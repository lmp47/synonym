{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Consolidation
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.SBP where

import Analysis.Axioms
import Analysis.Engine
import Analysis.Hoare
import Analysis.Invariant
import Analysis.Properties
import Analysis.Util
import Analysis.Types
import Analysis.Symmetry

import Control.Monad.State.Strict
import Control.Monad.ST.Safe

import Data.Map (Map)
import Data.Maybe

import Language.Java.Pretty
import Language.Java.Syntax

import System.IO.Unsafe
import Z3.Monad

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.List as L
import qualified Debug.Trace as T
import Data.Tuple

-- Move later:

data Composition = Composition { rest  :: [(Int, Block)]
                               , loops :: [(Int, Block)]
                               , conds :: [(Int, Block)] }

--

verifySBP :: Bool -> ClassMap -> [Comparator] -> Prop -> Z3 (Result,Maybe String)
verifySBP opt classMap _comps prop = do
 let comps = map rewrite _comps
     -- a = unsafePerformIO $ mapM_ (\(Comp _ f) -> putStrLn $ prettyPrint f) comps
 (objSort, pars, res, fields, gpidmap, idmap) <- prelude classMap comps
 (pre, post) <- trace ("after prelude:" ++ show (objSort, pars, res, fields)) $ prop (pars, res, fields)
 (fields', axioms) <- addAxioms objSort fields
 let blocks = zip [0..] $ getBlocks comps
 iSSAMap <- getInitialSSAMap
 let iCtrlMap = foldl (\m k -> M.insert k [] m) M.empty [0..length(comps) - 1]
-- let iEnv = Env objSort pars res fields' iSSAMap M.empty axioms pre post post opt False False 0
 -- set debug and fuse
 let iEnv = Env objSort pars res fields' iSSAMap M.empty axioms pre post post opt True True 0 M.empty idmap gpidmap iCtrlMap
 ((res, mmodel),_) <- runStateT (analyser (Composition blocks [] [])) iEnv
 case res of 
  Unsat -> return (Unsat, Nothing)
  Sat -> do
   str <- showModel $ fromJust mmodel
   return (Sat, Just str)
        
-- strongest post condition
_triple :: String -> String -> String -> String
_triple pre stm post =
 unlines
  ["-----------------"
  ,"Analyser State"
  ,pre
  ,stm
  ,post
  ,"-----------------"]

-- @ Analyser main function
analyser :: Composition -> EnvOp (Result,Maybe Model)
analyser stmts = do
 env@Env{..} <- get
 if _debug
 then analyser_debug stmts
 else analyse stmts

analyser_debug :: Composition -> EnvOp (Result,Maybe Model)
analyser_debug stmts = do
 env@Env{..} <- get
 preStr  <- lift $ astToString _pre
 postStr <- lift $ astToString _post
 case rest stmts of
  [] -> do 
   let k = T.trace (_triple preStr "end" postStr)
   k $ analyse stmts
  ((pid,Block []):rest) -> analyser_debug (Composition rest (loops stmts) (conds stmts))
  ((pid,Block (bstmt:r1)):rest) -> do
   let k = T.trace (_triple preStr (prettyPrint bstmt) postStr)
   k $ analyse stmts


analyse :: Composition -> EnvOp (Result,Maybe Model)   
analyse stmts = do
 env@Env{..} <- get
 case (rest stmts, loops stmts, conds stmts) of
  ([], [], []) -> lift $ local $ helper _axioms _pre _post
  ([], [], cs) -> analyse_conditionals cs
  ([], ls, cs)  -> analyse_loops ls cs []
  ((pid,Block []):rest, ls, cs) -> analyser (Composition rest ls cs)
  ((pid,Block (bstmt:r1)):rest, ls, cs) -> newStmt pid >> case bstmt of
   BlockStmt stmt -> analyser_stmt stmt (pid, Block r1) rest ls cs
   LocalVars mods ty vars -> do
    sort <- lift $ processType ty    
    (nssamap,nassmap,npre) <- 
      lift $ foldM (\(ssamap',assmap',pre') v -> 
        processNewVar (_objSort,_params,_res,_fields,ssamap',assmap',pre') sort v 1) (_ssamap,_assmap,_pre) vars
    updatePre npre
    updateSSAMap nssamap
    updateAssignMap nassmap
    mapM (\v ->
      case v of
        VarDecl varid _ ->
          case varid of
            VarId ident@(Ident str) ->
              case safeLookup "new vars" ident nssamap of
                (ast, _, _) -> addToPidMap ast pid >> addToIdMap ast ident) vars
    analyser (Composition ((pid, Block r1):rest) ls cs)

analyse_conditionals :: [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyse_conditionals conds = do
  env@Env{..} <- get
  tuples <- mapM convert conds
  let choices = map (\(x, _, _) -> x) tuples
  -- check that ctrl flow matches, and if it does, try to find symmetries
  let ctrl = map (\pid -> safeLookup "check ctrl" pid _ctrlmap) [0..(length conds) - 1]
  preSBP <- if and $ map (== head ctrl) (tail ctrl)
            then do
                 let pidCondMap = foldl (\m (x, (pid, _), _) -> M.insert pid x m) M.empty tuples
                 sbp <- getSBP pidCondMap
                 lift $ mkAnd [_pre, sbp]
            else return _pre
  choice <- lift $ mkOr choices
  decisions <- lift $ allSAT preSBP choices
  let decisions' = map (\(ast, bools) -> (ast, zipWith (\b (_, th, el) -> if b then th else el) bools tuples)) decisions
  combine env decisions tuples
 where
   analyse_branch (phi, bools) tuples = do
    env@Env{..} <- get
    newPre <- lift $ mkAnd [_pre, phi]
    updatePre newPre
    branches <- sequence $ zipWith
                  (\b (_, (thpid, Block th), (elpid, Block el)) ->
                    if b
                    then chooseThen thpid (length th) >> return (thpid, Block th)
                    else chooseElse elpid (length el) >> return (elpid, Block el))
                  bools tuples
    analyser (Composition branches [] [])
   combine e [] _ = return _default
   combine e (d:ds) tuples = do
     put e
     res <- analyse_branch d tuples
     case res of
       (Unsat,_) -> combine e ds tuples
       _ -> return res
   -- Convert each conditional-led program of form
   --   (pid, Block (BlockStmt (IfThenElse cond s1 s2):r1))
   -- to a tuple of form
   --   (cond, pid, Block (BlockStmt (s1:r1)), Block (BlockStmt (s2:r1)))
   convert :: (Int, Block) -> EnvOp (AST, (Int, Block), (Int, Block))
   convert (pid, Block (BlockStmt (IfThenElse cond s1 s2):r)) = do
     env@Env{..} <- get
     if cond == Nondet
     then do
       sort <- lift $ mkBoolSort
       cond' <- lift $ mkFreshConst "nondet" sort
       return (cond', (pid, Block (BlockStmt s1:r)), (pid, Block (BlockStmt s2:r)))
     else do
       cond' <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) cond
       return (cond', (pid, Block (BlockStmt s1:r)), (pid, Block (BlockStmt s2:r)))
     
-- Analyse If Then Else
analyse_loops :: [(Int,Block)] -> [(Int,Block)] -> [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyse_loops loops cs rest =
  case loops of
  (pid, Block (BlockStmt (While cond body):r1)):ls -> analyse_loop pid r1 ls cond body cs rest
  _ -> error "Expected While loop"

analyser_stmt :: Stmt -> (Int,Block) -> [(Int,Block)] -> [(Int,Block)] -> [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyser_stmt stmt (pid, Block r1) rest ls cs =
 case stmt of
  StmtBlock (Block block) -> newBlock pid (length block) >> analyser (Composition ((pid, Block (block ++ r1)):rest) ls cs)
  Assume expr -> do
   assume expr
   analyser (Composition ((pid, Block r1):rest) ls cs)
  Return mexpr -> do
   ret pid mexpr
   env@Env{..} <- get
   if _opt
   then do
    (check,_) <- lift $ local $ helper _axioms _pre _post
    if check == Unsat
    then return _default
    else analyser (Composition rest ls cs)
   else analyser (Composition rest ls cs)
  IfThen cond s1 -> do
   let ifthenelse = IfThenElse cond s1 (StmtBlock (Block []))
   analyser_stmt ifthenelse (pid, Block r1) rest ls cs
  IfThenElse cond s1 s2 -> -- analyse_conditional pid r1 rest cond s1 s2 
    analyser (Composition rest ls ((pid, Block(BlockStmt stmt:r1)):cs))
  ExpStmt expr -> analyse_exp pid ((pid,Block r1):rest) expr ls cs
  While _cond _body -> -- analyse_loop pid r1 rest _cond _body
    analyser (Composition rest ((pid, Block(BlockStmt stmt:r1)):ls) cs)

-- Analyse Expressions
analyse_exp :: Int -> [(Int, Block)] -> Exp -> [(Int, Block)] -> [(Int, Block)] -> EnvOp (Result, Maybe Model)
analyse_exp pid rest _exp ls cs =
 case _exp of
  MethodInv minv -> do 
   method_call minv
   analyser (Composition rest ls cs)
  Assign lhs aOp rhs -> do
   assign _exp lhs aOp rhs
   analyser (Composition rest ls cs)
  PostIncrement lhs -> do
   postOp _exp lhs Add "PostIncrement"
   analyser (Composition rest ls cs)
  PostDecrement lhs -> do
   postOp _exp lhs Sub "PostDecrement"
   analyser (Composition rest ls cs)

-- Analyse Loops
analyse_loop :: Int -> [BlockStmt] -> [(Int,Block)] -> Exp -> Stmt -> [(Int,Block)] -> [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyse_loop pid r1 ls _cond _body cs rest = do
 let bstmt = BlockStmt $ While _cond _body
 env@Env{..} <- get
 if _fuse
 --then if all isLoop rest - always the case
 then do 
   let (loops,rest) = unzip $ map takeHead ((pid,Block (bstmt:r1)):ls)
   (checkFusion,cont) <- applyFusion loops rest
   if checkFusion
   then analyser (Composition cont [] cs)
   else error "Fusion failed"
--      else analyse (Composition (rest ++ [(pid,Block (bstmt:r1))]) [] cs) -- apply commutativity
 else do
   invs <- guessInvariants (pid+1) _cond _body
   analyse_loop_w_inv invs
 where
   takeHead :: (Int, Block) -> ((Int, Stmt), (Int,Block))
   takeHead (pid, Block []) = error "takeHead"
   takeHead (pid, Block ((BlockStmt b):rest)) = ((pid,b), (pid, Block rest))
   isLoop :: (Int, Block) -> Bool
   isLoop (_, Block ((BlockStmt (While _ _)):ls)) = True
   isLoop _ = False
   analyse_loop_w_inv [] = error "none of the invariants was able to prove the property."
   analyse_loop_w_inv (inv:is) = do
    env@Env{..} <- get
    it_res <- _analyse_loop pid _cond _body inv
    if it_res
    then do
--     pre <- lift $ mkAnd [inv,_pre]
     put env
     updatePre inv -- pre
     case ls of
       [] -> analyser (Composition ((pid,Block r1):rest) ls cs)
       ls -> analyse_loops ls cs ((pid,Block r1):rest)
    else analyse_loop_w_inv is
   
--
_analyse_loop :: Int -> Exp -> Stmt -> AST -> EnvOp Bool
_analyse_loop pid _cond _body inv = do
 invStr  <- lift $ astToString inv
 env@Env{..} <- get
 (checkPre,_) <- lift $ local $ helper _axioms _pre inv
 case checkPre of
  Unsat -> do
   condAst <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) _cond
   ncondAst <- lift $ mkNot condAst
   (checkInv,_) <- lift $ mkAnd [inv,ncondAst] >>= \npre -> local $ helper _axioms npre inv
   case checkInv of
    Unsat -> do
     pre <- lift $ mkAnd [inv,condAst]
     let s = [(pid, Block [BlockStmt _body])]
     updatePre pre
     updatePost inv
     (bodyCheck,m) <- analyser (Composition s [] [])
     case bodyCheck of
      Unsat -> return True
      Sat -> do
       put env
       return  False -- {inv && cond} body {inv} failed
    Sat -> return False -- inv && not_cond =/=> inv
  Sat -> return False -- pre =/=> inv

applyFusion :: [(Int, Stmt)] -> [(Int, Block)] -> EnvOp (Bool,[(Int,Block)])
applyFusion [(pid, (While cond body))] rest = do
  env@Env{..} <- get
  invs <- guessInvariants (pid+1) cond body
  res <- analyse_loop_w_inv pid cond body invs
  return (res, rest)
 where
   analyse_loop_w_inv _ _ _ [] = error "none of the invariants was able to prove the property."
   analyse_loop_w_inv pid _cond _body (inv:is) = do
    env@Env{..} <- get
    it_res <- _analyse_loop pid _cond _body inv
    if it_res
    then do
     cond <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) _cond
     ncond <- lift $ mkNot cond
     pre <- lift $ mkAnd [inv,ncond]
     put env
     updatePre inv
     return True
    else analyse_loop_w_inv pid _cond _body is

applyFusion loops rest = do
 env@Env{..} <- get
 let (_conds,bodies) = unzip $ map splitLoop loops
     (pids,conds) = unzip _conds
 -- first, get cond counters
 counter <- case getCondCounter (head conds) of
              Nothing -> error "Could not find cond counter"
              Just c -> removeSubscript c
 -- use the found cond counter in astApps
 astApps <- lift $ mapM (makeApp _ssamap counter) pids
 let (asts,apps) = unzip astApps
 inv' <- lift $ mkExistsConst [] apps _pre
 -- equality constraints between the loop counter iterations: i1 = i2 and i1 = i3 ...
 eqs <- lift $ mapM (\c -> mkEq (head asts) c) (tail asts)
 eqInv <- lift $ mkAnd eqs
 -- frame rule 
 (a,_) <- (lift $ simplify _pre) >>= partitionAst pids
 -- the candidate invariant
 inv <- lift $ mkAnd (inv':eqInv:a)
 (checkInv,_) <- lift $ local $ helper _axioms _pre inv
 invStr <- lift $ astToString inv
 preStr <- lift $ astToString _pre
 case checkInv of
  Unsat -> do
   -- the new precondition inside the loop
   condsAsts <- lift $ mapM (processExp (_objSort,_params,_res,_fields,_ssamap)) conds
   ncondsAsts <- lift $ mapM mkNot condsAsts
   bodyPre <- lift $ mkAnd $ inv:condsAsts
   updatePre bodyPre
   updatePost inv
   (bodyCheck,_) <- analyser (Composition bodies [] [])
   case bodyCheck of
    Unsat -> do
     condsNAst <- lift $ mkAnd condsAsts >>= mkNot
     nPre <- lift $ mkAnd [inv,condsNAst]
     ncondAst <- lift $ mkAnd ncondsAsts
     (lastCheck, model) <- lift $ local $ helper _axioms nPre ncondAst
     case (lastCheck, model) of
      (Unsat, _) -> do
       put env
       updatePre nPre
       return (True,rest)
      (Sat, Just model) -> do --return (False,[]) -- "lastCheck failed"
        bools <- lift $  getAssignsInModel condsAsts model
        let (next', done') = L.partition (\(x,_) -> x) (zip bools loops)
            (next, done) = (map snd next', map snd done')
        put env
        -- analyse the "done" loops
        case done of
          [] -> error "applyFusion: at least one loop must have finished"
          ds -> do
                 (res, _) <- applyFusion ds []
                 if res
                 then -- try to analyse the "next" loops
                   applyFusion next rest >>= return
                 else error "applyFusion: fuse"
    Sat -> return (False,[]) -- "couldnt prove the loop bodies with invariant"
  Sat -> return (False,[]) -- "precondition does not imply the invariant"
 where
   -- Begin Fusion Utility Functions
   splitLoop :: (Int, Stmt) -> ((Int, Exp), (Int, Block))
   splitLoop (pid, While cond body) =
    case body of
     StmtBlock block -> ((pid, cond), (pid,block))
     _ -> error "splitLoop constructing block out of loop body"
   splitLoop _ = error "splitLoop"
   makeApp :: SSAMap -> String -> Int -> Z3 (AST,App)
   makeApp ssamap str pid = do
    let i = Ident $ str ++ show (pid+1)
        (iAST,_,_)  = safeLookup "guessInvariant: i" i ssamap
    iApp <- toApp iAST
    return (iAST,iApp)
   -- End Fusion Utility Functions
   analyse_loop_w_inv _ _ _ [] = error "none of the invariants was able to prove the property."
   analyse_loop_w_inv pid _cond _body (inv:is) = do
    env@Env{..} <- get
    it_res <- _analyse_loop pid _cond _body inv
    if it_res
    then do
     cond <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) _cond
     ncond <- lift $ mkNot cond
     pre <- lift $ mkAnd [inv,ncond]
     put env
     updatePre inv
     return True
    else analyse_loop_w_inv pid _cond _body is
