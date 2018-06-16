{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Invariant
-- Copyright :  (c) 2015 Marcelo Sousa
--                  2018 Lauren Pick
-------------------------------------------------------------------------------
module Analysis.Invariant where

import Analysis.Engine
import Analysis.Types
import Analysis.Util
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map as M
import Language.Java.Syntax
import Z3.Monad hiding (Params)

import qualified Debug.Trace as T

-- check if AST contains instance of any of a list of pids
containsPids :: [Int] -> [String] -> AST -> EnvOp Bool
containsPids pids except ast = do
  env@Env{..} <- get
  kind <- lift $ getAstKind ast
  case kind of
    Z3_NUMERAL_AST    -> return False
    Z3_APP_AST        -> do
      app <- lift $ toApp ast
      fn <- lift $ getAppDecl app
      sym <- lift $ getDeclName fn >>= getSymbolString
      nParams <- lift $ getAppNumArgs app
      args <- mapM (\i -> lift $ getAppArg app i) [0..(nParams-1)]
      args' <- mapM (containsPids pids except) args
      if sym `elem` except
      then do
        str <- lift $ astToString ast
        return (or args')
      else do
        (case M.lookup ast _pidmap of
          Nothing -> do
            str <- lift $ astToString ast
            return (or args')
          Just pid -> do
            str <- lift $ astToString ast
            return (pid `elem` pids || (or args')))
    Z3_VAR_AST        -> return False
    Z3_QUANTIFIER_AST -> do
      -- get variables that are bound as Strings
      nBoundVars <- lift $ getQuantifierNumBound ast
      boundVars <- mapM (\i -> lift $ getQuantifierBoundName ast i) [0..(nBoundVars - 1)]
      boundVarsStrings <- mapM (\i -> lift $ getSymbolString i) boundVars
      -- get body of quantifier
      body <- lift $ getQuantifierBody ast
      res <- containsPids pids (except ++ boundVarsStrings) body
      return res
    Z3_SORT_AST       -> return True
    Z3_FUNC_DECL_AST  -> return True
    Z3_UNKNOWN_AST    -> return True

-- given a list of pids, partition conjuncts into (A, B),
-- where A conjuncts have no instances of the pids in them
-- B conjuncts have some instances of the pids in them
partitionAst :: [Int] -> AST -> EnvOp ([AST], [AST])
partitionAst pids ast = do
  env@Env{..} <- get
  kind <- lift $ getAstKind ast
  case kind of
    Z3_APP_AST        -> do
      app <- lift $ toApp ast
      fn <- lift $ getAppDecl app
      sym <- lift $ getDeclName fn >>= getSymbolString
      nParams <- lift $ getAppNumArgs app
      args <- mapM (\i -> lift $ getAppArg app i) [0..(nParams-1)]
      if (sym == "and")
      then do
        resBool <- mapM (containsPids pids []) args
        let res = zip resBool args
        return (foldl (\(ind, dep) (haspid, arg) ->
                       if haspid then (ind, arg:dep) else (arg:ind, dep))
                      ([],[]) res)
      else do
        haspid <- containsPids pids [] ast
        if haspid
        then return ([], [ast])
        else return ([ast], [])
    _                 -> return ([], [])
  

guessInvariants :: Int -> Exp -> Stmt -> EnvOp [AST]
guessInvariants pid cond body = do
  env@Env{..} <- get
  let fnCalls = getFnCalls cond ++ getFnCalls body
  fnNames' <- lift $ foldM (search _fields) [] fnCalls 
  let fnNames = nub fnNames'
  increasing <- guessInvariant fnNames mkGe mkLe mkLt pid cond
  decreasing <- guessInvariant fnNames mkLe mkGe mkGt pid cond
  (a,b) <- (lift $ simplify _pre) >>= partitionAst [pid - 1]
  case a of
    [] -> return $ increasing ++ decreasing
    _ -> do
           frame <- lift $ mapM (\x -> mkAnd (x:a)) (increasing ++ decreasing)
           return $ frame ++ increasing ++ decreasing

search fields list (Name n) =
  case n of 
    [ident@(Ident "nondet")] ->
      case M.lookup ident fields of
        Nothing -> return list
        Just fn -> return $ (fn,0):list
    [obj,field] ->
      case M.lookup field fields of
        Nothing -> return list
        Just fn -> return $ (fn,1):list
{-
    [Ident "Character",fnName] -> return list
        Just fn -> return $ fn:list
    [Ident "Double",Ident "compare"] -> 
      let fnName = Ident "compareDouble"
      in case M.lookup fnName fields of
        Nothing -> return list
        Just fn -> return $ fn:list
    [Ident "Int",Ident "compare"] -> 
      let fnName = Ident "compareInt"
      in case M.lookup fnName fields of
        Nothing -> return list
        Just fn -> return $ fn:list
    [Ident "String",Ident "compareIgnoreCase"] -> 
      let fnName = Ident "compareIgnoreCaseString"
      in case M.lookup fnName fields of
        Nothing -> return list
        Just fn -> return $ fn:list
 -}
    _ -> return list
        
type Z3Op = (AST -> AST -> Z3 AST)
--
guessInvariant :: [(FuncDecl,Int)] -> Z3Op -> Z3Op -> Z3Op -> Int -> Exp -> EnvOp [AST]
guessInvariant fnNames op op' op'' pid cond = do
 env@Env{..} <- get 
 case getCondCounter cond of 
  Nothing -> do 
    t <- lift $ mkTrue
    T.trace "guessing True inv" $ return [t]
  Just i -> do
   let (iAST,_,_)  = safeLookup "guessInvariant: i" i _ssamap
       e = safeLookup ("getting last condition assignment" ++ show i) i _assmap
   -- i `op` Init
   i0 <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) e 
   c1 <- lift $ op iAST i0
   -- exists i. pre
   iApp <- lift $ toApp iAST
   ex1 <- lift $ mkExistsConst [] [iApp] _pre
   -- forall j. i_0 <= j < i => cond
   gen <- lift $ generalizeCond fnNames op' op'' (_objSort,_params,_res,_fields,_ssamap) i0 i iAST cond pid
   bound <- getBound op (_objSort,_params,_res,_fields,_ssamap) iAST cond
   case gen of
     [] -> do
             and_bound <- lift $ mkAnd [ex1, c1, bound]
             and <- lift $ mkAnd [ex1, c1]
             return [and_bound, and]
     _ -> lift $ mapM (\genInv -> mkAnd [ex1, genInv, c1]) gen
   --T.trace ("gen size: " ++ (show (length gen))) $ lift $ mapM (\genInv -> mkAnd [ex1, genInv, c1]) gen

getBound :: Z3Op -> (Sort, Params, [AST], Fields, SSAMap) -> AST -> Exp -> EnvOp AST
getBound op (_objSort,_params,_res,_fields,_ssamap) iAST _cond =
  case _cond of
    BinOp (ExpName (Name [i])) _ expr -> mkCond expr iAST
    BinOp (BinOp (ExpName (Name [i])) _ expr) _ _ -> mkCond expr iAST
    BinOp (BinOp (BinOp (ExpName (Name [i])) _ expr) _ _) And _ -> mkCond expr iAST
 where
  mkCond expr iAST = do
    lhs <- lift $ processExp (_objSort,_params,_res,_fields,_ssamap) expr 
    c <- lift $ op lhs iAST
    return c
    
removeSubscript :: Ident -> EnvOp String
removeSubscript (Ident str) = do
  env@Env{..} <- get
  let (iAST,_,_) = safeLookup ("removeSubscript: " ++ str) (Ident str) _ssamap
  let pid = safeLookup ("removeSubscript: " ++ str) iAST _pidmap
  return $ take (length str - length (show pid)) str
  

getCondCounter :: Exp -> Maybe Ident
getCondCounter expr = 
  case expr of
    BinOp (ExpName (Name [i])) _ _ -> Just i
    BinOp (BinOp (ExpName (Name [i])) _ _) _ _ -> Just i
    BinOp (BinOp (BinOp (ExpName (Name [i])) _ _) _ _) And _ -> Just i
    _ -> Nothing --error $ "getCondCounter: " ++ show expr

generalizeCond :: [(FuncDecl,Int)] -> Z3Op -> Z3Op -> (Sort, Params, [AST], Fields, SSAMap) ->  AST -> Ident -> AST -> Exp -> Int -> Z3 [AST]
generalizeCond fnNames op op' env@(objSort, pars, res, fields, ssamap) i0 i iAST _cond pid =
  case _cond of 
    BinOp _ And cond -> do
      let jIdent = Ident $ "j" ++ show pid
          cond' = replaceExp i jIdent cond
      --sort <- T.trace ("generalizeCond Binop, fnames length: " ++ (show (length fnNames))) $ mkIntSort
      sort <- mkIntSort
      jSym <- mkStringSymbol $ "j" ++ show pid
      j <- mkConst jSym sort
      jApp <- toApp j
--      let (i0,_,_) = safeLookup "genCond" (Ident $ "myPosition"++show pid) ssamap
--      i0 <- mkIntNum 0
      -- c1: Init <= j < i or i < j <= Init
      c1 <- op i0 j >>= \left -> op' j iAST >>= \right -> mkAnd [left, right]
      -- c2: 
      let ssamap' = M.insert jIdent (j, sort, pid) ssamap
      c2 <- processExp (objSort, pars, res, fields, ssamap') cond'
      -- \forall j. c1 => c2
      mkImplies c1 c2 >>= \body -> mkForallConst [] [jApp] body >>= \inv -> return [inv]
    _ -> do
      let jIdent = Ident $ "j" ++ show pid
      sort <- T.trace ("generalizeCond _ , fnNames length: " ++ (show (length fnNames))) mkIntSort
      jSym <- mkStringSymbol $ "j" ++ show pid
      j <- mkConst jSym sort
      jApp <- toApp j
--      i0 <- mkIntNum 0
      -- c1: 0 <= j < i
      c1 <- op i0 j >>= \left -> op' j iAST >>= \right -> mkAnd [left, right]
      -- c2: 
      let ssamap' = M.insert jIdent (j, sort, pid) ssamap
      c2p <- processExp (objSort, pars, res, fields, ssamap) _cond
      c2s <- mapM (\fnName -> buildArtCond fnName (pars,fields) j pid) fnNames
      -- \forall j. c1 => c2
      mapM (\c2 -> mkImplies c1 c2 >>= \body -> mkForallConst [] [jApp] body) c2s
    
buildArtCond :: (FuncDecl,Int) -> (Params, Fields) -> AST -> Int -> Z3 AST
buildArtCond (fn,0) (pars,fields) j pid = do 
  get1 <- mkApp fn [j]
  get2 <- mkApp fn [j]
  mkEq get1 get2
buildArtCond (fn,1) (pars,fields) j pid = do
  let o1 = Ident $ "o1" ++ show pid
      o2 = Ident $ "o2" ++ show pid
      o1z3 = safeLookup "buildArtCond: o1" o1 pars
      o2z3 = safeLookup "buildArtCond: o2" o2 pars
  get1 <- mkApp fn [o1z3,j]
  get2 <- mkApp fn [o2z3,j]
  mkEq get1 get2
