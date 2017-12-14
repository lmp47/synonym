{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Consolidation
-- Copyright :  (c) 2015 Marcelo Sousa
-------------------------------------------------------------------------------
module Analysis.Lockstep where

import Analysis.Axioms
import Analysis.Engine
import Analysis.Hoare
import Analysis.Invariant
import Analysis.Properties
import Analysis.Util
import Analysis.Types

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
import qualified Debug.Trace as T

-- Move later:

data Composition = Composition { rest  :: [(Int, Block)]
                               , loops :: [(Int, Block)]
                               , conds :: [(Int, Block)] }

--

verifyLs :: Bool -> ClassMap -> [Comparator] -> Prop -> Z3 (Result,Maybe String)
verifyLs opt classMap _comps prop = do
 let comps = map rewrite _comps
     -- a = unsafePerformIO $ mapM_ (\(Comp _ f) -> putStrLn $ prettyPrint f) comps
 (objSort, pars, res, fields) <- prelude classMap comps
 (pre, post) <- trace ("after prelude:" ++ show (objSort, pars, res, fields)) $ prop (pars, res, fields)
 (fields', axioms) <- addAxioms objSort fields
 let blocks = zip [0..] $ getBlocks comps
 iSSAMap <- getInitialSSAMap
 -- get initial pid map
 let iPidMap = foldl  (\m (i,r) -> M.insert r i m) M.empty (zip [0..] res)
-- let iEnv = Env objSort pars res fields' iSSAMap M.empty axioms pre post post opt False False 0
 -- set debug and fuse
 let iEnv = Env objSort pars res fields' iSSAMap M.empty axioms pre post post opt False True 0 iPidMap
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
   let k = T.trace (_triple preStr "end" postStr) $ unsafePerformIO $ getChar
   k `seq` analyse stmts
  ((pid,Block []):rest) -> analyser_debug (Composition rest [] [])
  ((pid,Block (bstmt:r1)):rest) -> do
   let k = T.trace (_triple preStr (prettyPrint bstmt) postStr) $ unsafePerformIO $ getChar
   k `seq` analyse stmts


analyse :: Composition -> EnvOp (Result,Maybe Model)   
analyse stmts = do
 env@Env{..} <- get
 case (rest stmts, loops stmts, conds stmts) of
  ([], [], []) -> lift $ local $ helper _axioms _pre _post
  ([], [], cs) -> analyse_conditionals cs []
  ([], ls, cs)  -> analyse_loops ls cs []
  ((pid,Block []):rest, ls, cs) -> analyser (Composition rest ls cs)
  ((pid,Block (bstmt:r1)):rest, ls, cs) -> case bstmt of
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
                (ast, _, _) -> addToPidMap ast pid) vars
    analyser (Composition ((pid, Block r1):rest) ls cs)

analyse_conditionals :: [(Int,Block)] -> [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyse_conditionals conds rest =
  case conds of
  (pid,Block (BlockStmt (IfThenElse cond s1 s2):r1)):cs -> analyse_conditional pid r1 cs cond s1 s2 rest
  _ -> error "Expected IfThenElse conditional"

analyse_loops :: [(Int,Block)] -> [(Int,Block)] -> [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyse_loops loops cs rest =
  case loops of
  (pid, Block (BlockStmt (While cond body):r1)):ls -> analyse_loop pid r1 ls cond body cs rest
  _ -> error "Expected While loop"

analyser_stmt :: Stmt -> (Int,Block) -> [(Int,Block)] -> [(Int,Block)] -> [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyser_stmt stmt (pid, Block r1) rest ls cs =
 case stmt of
  StmtBlock (Block block) -> analyser (Composition ((pid, Block (block ++ r1)):rest) ls cs)
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

-- Analyse If Then Else
analyse_conditional :: Int -> [BlockStmt] -> [(Int,Block)] -> Exp -> Stmt -> Stmt -> [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyse_conditional pid r1 cs cond s1 s2 rest =
 if cond == Nondet
 then do
  env@Env{..} <- get
  resThen <- analyser (Composition ((pid, Block (BlockStmt s1:r1)):rest) [] cs)
  put env
  resElse <- analyser (Composition ((pid, Block (BlockStmt s2:r1)):rest) [] cs)
  combine resThen resElse                
 else do
  env@Env{..} <- get
  condSmt <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) cond
  -- then branch
  preThen <- lift $ mkAnd [_pre, condSmt]
  resThen <- analyse_branch preThen s1
  -- else branch
  put env
  ncondSmt <- lift $ mkNot condSmt
  preElse <- lift $ mkAnd [_pre, ncondSmt]
  resElse <- analyse_branch preElse s2
  combine resThen resElse
 where
   next_cond r =
     case cs of
        [] -> analyser (Composition (r:rest) [] [])
        cs -> analyse_conditionals cs (r:rest)
   analyse_branch phi branch = do
    env@Env{..} <- get
    updatePre phi
    let r = (pid, Block (BlockStmt branch:r1))
    if _opt
    then do
      cPhi <- lift $ checkSAT phi
      if cPhi == Unsat
      then return _default
      else next_cond r
    else next_cond r
   combine :: (Result, Maybe Model) -> (Result, Maybe Model) -> EnvOp (Result, Maybe Model)
   combine (Unsat,_) (Unsat,_) = return _default
   combine (Unsat,_) res = return res
   combine res _ = return res

-- Analyse Loops
analyse_loop :: Int -> [BlockStmt] -> [(Int,Block)] -> Exp -> Stmt -> [(Int,Block)] -> [(Int,Block)] -> EnvOp (Result,Maybe Model)
analyse_loop pid r1 ls _cond _body cs rest = do
 let bstmt = BlockStmt $ While _cond _body
 env@Env{..} <- get
 invs <- guessInvariants (pid+1) _cond _body
 if T.trace "fuse" _fuse && length ls > 0
 --then if all isLoop rest - always the case
 then do 
   (checkFusion,cont) <- applyFusion ((pid,Block (bstmt:r1)):ls)
   if checkFusion
   then analyse (Composition cont [] cs)
   else do
        T.trace "failed" $ put env
        analyse_loop_w_inv invs       
--      else analyse (Composition (rest ++ [(pid,Block (bstmt:r1))]) [] cs) -- apply commutativity
 else if invs == []
      then error "no invs"
      else analyse_loop_w_inv invs
 where
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
 case T.trace "checkPre" checkPre of
  Unsat -> do
   condAst <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) _cond
   ncondAst <- lift $ mkNot condAst
   (checkInv,_) <- lift $ mkAnd [inv,ncondAst] >>= \npre -> local $ helper _axioms npre inv
   case T.trace "checkInv" checkInv of
    Unsat -> do
     pre <- lift $ mkAnd [inv,condAst]
     let s = [(pid, Block [BlockStmt _body])]
     updatePre pre
     updatePost inv
     (bodyCheck,m) <- analyser (Composition s [] [])
     case T.trace "bodyCheck" bodyCheck of
      Unsat -> T.trace "body succeeded" $ return True
      Sat -> do
       put env
       T.trace "failed body " $ return  False -- {inv && cond} body {inv} failed
    Sat -> return False -- inv && not_cond =/=> inv
  Sat -> return False -- pre =/=> inv

applyFusion :: [(Int, Block)] -> EnvOp (Bool,[(Int,Block)])
applyFusion list = do
 env@Env{..} <- get
 let (loops,rest) = unzip $ map takeHead list
     (_conds,bodies) = unzip $ map splitLoop loops
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
 let k = T.trace ("\nPrecondition:\n"++ preStr ++ "\nInvariant:\n" ++ invStr) $ unsafePerformIO $ getChar
 case k `seq` checkInv of
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
     (lastCheck,_) <- lift $ local $ helper _axioms nPre ncondAst
     case lastCheck of
      Unsat -> do
       put env
       updatePre nPre
       return (True,rest)
      Sat -> return (False,[]) -- "lastCheck failed"
    Sat -> return (False,[]) -- "couldnt prove the loop bodies with invariant"
  Sat -> return (False,[]) -- "precondition does not imply the invariant"
 where
   -- Begin Fusion Utility Functions
   takeHead :: (Int, Block) -> ((Int, Stmt), (Int,Block))
   takeHead (pid, Block []) = error "takeHead"
   takeHead (pid, Block ((BlockStmt b):rest)) = ((pid,b), (pid, Block rest))
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
