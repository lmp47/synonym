{-# LANGUAGE RecordWildCards #-}
-------------------------------------------------------------------------------
-- Module    :  Analysis.Symmetry
-- Copyright :  (c) 2017 Lauren Pick
-------------------------------------------------------------------------------
module Analysis.Symmetry where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State.Strict
import Language.Java.Syntax
import Language.Java.Pretty
import Data.Foldable

import Analysis.Types
import Analysis.Util
import Analysis.Engine
import NautyTraces.NautyTraces

import Z3.Monad hiding (Params)

import qualified Debug.Trace as T
import qualified Data.Map as M
import Data.List
--
-- TODO: clean & fix

sepPid :: AST -> IdMap -> PidMap -> Z3 (String, Maybe Int)
sepPid ast idmap pidmap =
  case M.lookup ast pidmap of
    Just pid -> do
      astStr <- astToString ast
      let Ident str = safeLookup "sepPid" ast idmap
      return (take (length str - length (show pid)) str, Just pid)
    _ -> return ("nondet", Nothing)
    
--
data Graph = Graph
  { nv :: Int
  , nde :: Int
  , v :: [Int]
  , d :: [Int]
  , e :: [Int]
  , lab :: [Int]
  , ptn :: [Int]
  } deriving Show

fromPreGraph :: PreGraph -> Graph
fromPreGraph pg =
  let (v, d, e) = mkVDE (edges pg) (outDeg pg) (numEdges pg) 0 (numVerts pg - 1) ([],[],[])
      (lab, ptn) = mkLabPtn in
  Graph { nv = numVerts pg
        , nde = numEdges pg
        , v = v
        , d = d
        , e = e
        , lab = lab
        , ptn = ptn
        }
 where
  mkVDE edges outDegs nde ndeAcc vert (v, d, e) =
    if vert < 0
    then (v, d, e)
    else
      let deg = safeLookup "mkVDE" vert outDegs in
      if deg == 0
      then mkVDE edges outDegs nde ndeAcc (vert - 1) (0:v, 0:d, e)
      else let vde' = ((nde - ndeAcc - deg):v, deg:d, (safeLookup "mkVDE" vert edges)++e) in
        mkVDE edges outDegs nde (ndeAcc + deg) (vert - 1) vde'
  mkLabPtn =
    foldl
      (\(lab,ptn) (_,vals) -> (vals ++ lab, (take (length vals - 1) (repeat 1)) ++ 0:ptn))
      ([], []) (M.toList $ color pg)

getSymmetries :: EnvOp [[(Int, Int)]]
getSymmetries = do
  env@Env{..} <- get
  pre' <- lift $ simplify _pre
  post' <- lift $ simplify _post
  let pids = M.keys _ctrlmap
  pgr <- lift $ makePreGraph pre' post' pids _idmap (_pidmap `M.union` _gpidmap)
  let g = fromPreGraph pgr
  symm <- liftIO $ getSymm (nv g) (nde g) (v g) (d g) (e g) (lab g) (ptn g)
  let symm' = map (filter (\(x, y) -> x /= y) . zip [0..length pids - 1]) (perms symm)
  return (filter (/= []) symm')

getSBP :: Map Int AST -> EnvOp AST
getSBP m = do
  -- remove "last" part of cycle to eliminate redundancy
  symms <- getSymmetriesZ3
  pps <- T.trace (show symms) $ lift $ mapM (getPP m) symms
  res <- lift $ mkAnd (concat pps)
  sbpStr <- lift $ astToString res
  T.trace ("sbp: " ++ sbpStr) $ return res

getPP :: Map Int AST -> [(Int, Int)] -> Z3 [AST]
getPP m symm = do
  let symm' = filter (\(x,y) -> M.lookup x m /= Nothing && M.lookup y m /= Nothing) symm
  -- make gs
  lgs <- mapM (\(x,y) -> do
                         let x' = safeLookup ("getPP: " ++ show x) x m
                             y' = safeLookup ("getPP: " ++ show y) y m
                         le <- mkImplies x' y'
                         ge <- mkImplies y' x'
                         return (le, ge)) symm'
  case lgs of
    (lg:lgs) -> do
                p <- mkFreshBoolVar "p"
                acc <- mkAnd [fst lg, p]
                chain p lg lgs [acc]
    _ -> return []
 where
 chain :: AST -> (AST,AST) -> [(AST, AST)] -> [AST] -> Z3 [AST]
 chain _ _ [] acc = return acc
 chain prevp prevlg [lg] acc = do
   lhs <- mkImplies prevp (snd prevlg)
   pp <- mkImplies lhs (fst lg)
   return (pp:acc)
 chain prevp prevg (lg:lgs) acc = do
   currp <- mkFreshBoolVar "p"
   rhs <- mkAnd [fst lg, currp]
   lhs <- mkImplies prevp (snd prevg)
   pp <- mkImplies lhs rhs
   chain currp lg lgs (pp:acc)
  

data PreGraph = PreGraph
  { numVerts :: Int --nv
  , numEdges :: Int  --nde
  , edges :: Map Int [Int]
  , outDeg :: Map Int Int
  , color :: Map Int [Int] -- color int to vertex int
  , colorbk :: ColInt -- color label and int bookkeeping
  } deriving Show

getInitialPerms :: EnvOp ()
getInitialPerms = do
  env@Env{..} <- get
  let pids = M.keys _ctrlmap
  let (def:perms) = genPerms pids
  (m, defAST) <- permuteAST def (M.empty, _pre)
  perms' <- getSymmASTZ3 defAST (m, _pre) perms
  (m', defAST') <- permuteAST def (M.empty, _post)
  permres <- getSymmASTZ3 defAST' (m', _post) perms'
  put env{ _perms = (def:permres) }

getSymmetriesZ3 :: EnvOp [[(Int,Int)]]
getSymmetriesZ3 = do
  env@Env{..} <- get
  let pids = M.keys _ctrlmap
  let (def:perms) = _perms
  (m, defAST) <- permuteAST def (M.empty, _pre)
  perms' <- getSymmASTZ3 defAST (m, _pre) perms
  (m', defAST') <- permuteAST def (M.empty, _post)
  permres <- getSymmASTZ3 defAST' (m', _post) perms'
  return $ map (M.toList) permres

getSymmASTZ3 :: AST -> (Map (String, Int) AST, AST) -> [Map Int Int] -> EnvOp [Map Int Int]
getSymmASTZ3 defAST mast perms =
  filterM (checkIff defAST mast) perms
  
checkIff :: AST -> (Map (String, Int) AST, AST) -> Map Int Int -> EnvOp Bool
checkIff defAST mast perm = do
  (_, pAST) <- permuteAST perm mast
  iff <- lift $ mkIff defAST pAST
  niff <- lift $ mkNot iff
  res <- lift $ checkSAT niff
  case res of
    Unsat -> return True
    Sat -> return False

genPerms :: [Int] -> [Map Int Int]
genPerms pids =
  let (h:t) = permutations pids in
  map (M.fromList . zip h) (h:t)
  
permuteAST :: Map Int Int -> (Map (String, Int) AST, AST) -> EnvOp (Map (String, Int) AST, AST)
permuteAST perm (m, ast) = do
    kind <- lift $ getAstKind ast
    env@Env{..} <- get
    case kind of
      Z3_NUMERAL_AST    -> return (m, ast)
      Z3_APP_AST        -> do
        app <- lift $ toApp ast
        fn <- lift $ getAppDecl app
        nParams <- lift $ getAppNumArgs app
        args <- lift $ mapM (\i -> getAppArg app i) [0..(nParams-1)]
        if nParams == 0
        then do -- constant
             (str, pid) <- lift $ sepPid ast _idmap (M.union _pidmap _gpidmap)
             case pid of
               Just pid' -> do
                       sort <- lift $ getSort ast
                       case M.lookup pid' perm of
                         Just pid'' -> case M.lookup (str, pid'') m of
                                         Just v -> return (m, v)
                                         _ -> do
                                              v <- lift $ mkFreshConst (str ++ (show pid'')) sort
                                              return (M.insert (str, pid'') v m, v)
                         _ -> error "permuteAST: pid doesn't map to anything"
               _ -> return (m, ast)
        else do -- function application
             (m', args') <- foldrM (\arg (m,margs') -> do
                                                       (m', arg') <- permuteAST perm (m, arg)
                                                       return (m', arg':margs')) (m, []) args
             app <- lift $ mkApp fn args'
             return (m', app)
      Z3_QUANTIFIER_AST -> do
        numBoundVars <- lift $ getQuantifierNumBound ast 
        boundVars <- lift $ getQuantifierBoundVars ast 
        apps <- lift $ mapM (\i -> toApp i) boundVars
        body <- lift $ getQuantifierBody ast 
        (m', body') <- permuteAST perm (m, body)
        isExists <- lift $ isQuantifierExists ast 
        if (isExists)
        then do
             ast' <- lift $ mkExistsConst [] apps body'
             return (m', ast')
        else do
             ast' <-  lift $ mkForallConst [] apps body'
             return (m', ast')
      Z3_VAR_AST       -> return (m, ast)
      _                 -> do
        astStr <- lift $ astToString ast
        error ("unexpected AST: " ++ astStr)

  

data Tag = N
         | Arg Int deriving (Eq, Ord)

instance Show Tag where
  show N = "N"
  show (Arg i) = "Arg " ++ (show i)

data Color = OrPre
           | OrPost
           | Lte
           | Not
           | Eq
           | Plus
           | Times
           | Forall
           | Exists
           | Id
           | Num (Tag, Int)
           | Var (Tag, String) deriving (Eq, Ord)

instance Show Color where
  show OrPre = "OrPre"
  show OrPost = "OrPost"
  show Lte = "Lte"
  show Not = "Not"
  show Eq = "Eq"
  show Plus = "Plus"
  show Times = "Times"
  show Forall = "Forall"
  show Exists = "Exists"
  show Id = "Id"
  show (Num (t,i)) = show t ++ ", " ++ show i
  show (Var (t,s)) = show t ++ ", " ++ s

-- TODO: keep map of (NextInt, Map Color Int) for nums and vars instead
type ColInt = (Int, Map Color Int, Map Int Color)

defaultColInt :: ColInt
defaultColInt = (9, M.empty, M.empty)

colToInt :: Color -> ColInt -> (Int, ColInt)
colToInt OrPre m = (0, m)
colToInt OrPost m = (1, m)
colToInt Lte m = (2, m)
colToInt Not m = (3, m)
colToInt Eq m = (4, m)
colToInt Plus m = (5, m)
colToInt Times m = (6, m)
colToInt Forall m = (7, m)
colToInt Exists m = (8, m)
colToInt Id m = (9, m)
colToInt other (n, ci, ic) =
  case M.lookup other ci of
    Nothing -> (n + 1, (n + 1, M.insert other (n + 1) ci, M.insert (n + 1) other ic))
    Just n -> (n, (n, ci, ic))
    

defaultPreGraph :: PreGraph
defaultPreGraph = PreGraph { numVerts = 0
                         , numEdges = 0
                         , edges = M.empty
                         , outDeg = M.empty
                         , color = M.empty
                         , colorbk = defaultColInt }

updateColor :: Int -> Int -> Map Int [Int] -> Map Int [Int]
updateColor color i m =
  case M.lookup color m of
    Nothing -> M.insert color [i] m
    Just ls -> M.insert color (i:ls) m

-- Given a list of precondition conjuncts, postcondition conjuncts,
-- and pids, create graph
makePreGraph :: AST -> AST -> [Int] -> IdMap -> PidMap -> Z3 PreGraph
makePreGraph pre post pids idmap pidmap = do
  let len = length pids
  let (col, bk) = colToInt Id defaultColInt
  let startGraph = defaultPreGraph { numVerts = len
                                   , outDeg = foldl (\m x -> M.insert x 0 m) M.empty [0..len-1]
                                   , color = M.fromList $ [(col, [0..len - 1])]
                                   , colorbk = bk }
  astStr <- astToString pre
  astStr' <- astToString post
  pres <- extractConj pre
  posts <- extractConj post
  preGraph <- foldM (handleConj (fst $ colToInt OrPre defaultColInt)) startGraph pres
  foldM (handleConj (fst $ colToInt OrPost (colorbk preGraph))) preGraph posts
 where
  pidToVert :: Map Int Int
  pidToVert = M.fromList $ zip pids [0..]
  extractConj :: AST -> Z3 [AST]
  extractConj ast = do
    kind <- getAstKind ast
    case kind of
      Z3_APP_AST -> do
        app <- toApp ast
        fn <- getAppDecl app
        sym <- getDeclName fn >>= getSymbolString
        if sym == "and"
        then do
          nParams <- getAppNumArgs app
          mapM (\i -> getAppArg app i) [0..(nParams-1)]
        else do
          return [ast]
      _ -> return [ast]
  handleConj orp g ast = do
    kind <- getAstKind ast
    case kind of
      Z3_APP_AST -> do
        app <- toApp ast
        fn <- getAppDecl app
        sym <- getDeclName fn >>= getSymbolString
        if sym == "or"
        then do
          nParams <- getAppNumArgs app
          args <- mapM (\i -> getAppArg app i) [0..(nParams-1)]
          let g' = g { numVerts = numVerts g + 1
                     , numEdges = numEdges g + nParams
                     , outDeg = M.insert (numVerts g) nParams (outDeg g)
                     , color = updateColor orp (numVerts g) (color g) }
          let args' = map (\a -> (a, N)) args
          foldlM (\gr (arg, atag) -> handle (arg, atag) (numVerts g) gr) g' args'
        else
          let g' = g { numVerts = numVerts g + 1
                     , numEdges = numEdges g + 1
                     , outDeg = M.insert (numVerts g) 1 (outDeg g)
                     , color = updateColor orp (numVerts g) (color g) } in
          handle (ast, N) (numVerts g) g'
      _ -> 
          let g' = g { numVerts = numVerts g + 1
                     , numEdges = numEdges g + 1
                     , outDeg = M.insert (numVerts g) 1 (outDeg g)
                     , color = updateColor orp (numVerts g) (color g) } in
          handle (ast, N) (numVerts g) g'
  handle :: (AST, Tag) -> Int -> PreGraph -> Z3 PreGraph
  handle (ast, tag) parent g = do
    let id = numVerts g
    let g' = g { numVerts = numVerts g + 1
               , edges = case M.lookup parent (edges g) of
                           Nothing -> M.insert parent [id] (edges g)
                           Just es -> M.insert parent (id:es) (edges g) }
    kind <- getAstKind ast
    case kind of
      Z3_NUMERAL_AST    -> do
        str <- getNumeralString ast
        let (n, bk) = colToInt (Num (tag, read str)) (colorbk g')
        let g'' = g' { outDeg = M.insert id 0 (outDeg g')
                     , color = updateColor n id (color g')
                     , colorbk = bk }
        return g''
      Z3_APP_AST        -> do
        app <- toApp ast
        fn <- getAppDecl app
        sym <- getDeclName fn >>= getSymbolString
        nParams <- getAppNumArgs app
        args <- mapM (\i -> getAppArg app i) [0..(nParams-1)]
        let col = if sym == "<="
                  then Just Lte
                  else if sym == "="
                    then Just Eq
                  else if sym == "iff"
                    then Just Eq
                  else if sym == "not"
                    then Just Not
                  else if sym == "+"
                    then Just Plus
                  else if sym == "*"
                    then Just Times
                  else Nothing
        case col of
          Nothing ->
            if nParams == 0
            then do -- constant
              (str, pid) <- sepPid ast idmap pidmap
              let (n, bk) = colToInt (Var (tag, str)) (colorbk g')
              let g'' = case pid of
                         Nothing ->
                           g' { outDeg = M.insert id 0 (outDeg g')
                              , color = updateColor n id (color g')
                              , colorbk = bk }
                         Just pid ->
                           g' { numEdges = 1 + numEdges g'
                              , edges = M.insert id [safeLookup ("pidToVert: " ++ show pid) pid pidToVert] (edges g')
                              , outDeg = M.insert id 1 (outDeg g')
                              , color = updateColor n id (color g')
                              , colorbk = bk }
              return g''
            else do -- function application
              --let Ident str = safeLookup "handle" ast idmap
              let (n, bk) = colToInt (Var (tag, sym)) (colorbk g')
              let g'' = g' { numEdges = nParams + numEdges g'
                           , outDeg = M.insert id nParams (outDeg g')
                           , color = updateColor n id (color g')
                           , colorbk = bk }
              let args' = zipWith (\a i -> (a, Arg i)) args [1..]
              foldlM (\gr (arg, atag) -> handle (arg, atag) id gr) g'' args'
          Just col -> 
            case col of
              Not ->
                let (n, _) = colToInt Not (colorbk g') in
                let g'' = g' { numEdges = numEdges g' + 1
                             , outDeg = M.insert id 1 (outDeg g')
                             , color = updateColor n id (color g')} in
                handle (head args, N) id g''
              _ ->
                let (n, _) = colToInt col (colorbk g') in
                let g'' = g' { numEdges = numEdges g' + 2
                             , outDeg = M.insert id 2 (outDeg g')
                             , color = updateColor n id (color g')} in
                case args of
                  [left, right] ->
                    let (ltag, rtag) = if col == Lte then (Arg 1, Arg 2) else (N, N) in
                    handle (left, ltag) id g'' >>= handle (right, rtag) id
                  _ -> error "too many args"
      Z3_QUANTIFIER_AST -> do
        numBoundVars <- getQuantifierNumBound ast 
        boundVars <- getQuantifierBoundVars ast 
        body <- getQuantifierBody ast 
        isExists <- isQuantifierExists ast 
        let nParams = numBoundVars + 1
        let col = if isExists then Exists else Forall
        let (n, _) = colToInt col (colorbk g')
        let g'' = g' { numEdges = numEdges g' + nParams
                     , outDeg = M.insert id nParams (outDeg g')
                     , color = updateColor n id (color g') }
        let args = zipWith (\a i -> (a, Arg i)) boundVars [2..]
        foldlM (\gr (arg, atag) -> handle (arg, atag) id gr) g'' ((body, Arg 1):args)
      Z3_VAR_AST       -> do
        str <- astToString ast
        let (n, bk) = colToInt (Var (tag, str)) (colorbk g')
        let g'' = g' { outDeg = M.insert id 0 (outDeg g')
                     , color = updateColor n id (color g')
                     , colorbk = bk }
        return g''
      _                 -> do
        astStr <- astToString ast
        error ("unexpected AST: " ++ astStr)

-- Make sparse nauty/traces-compatible graph:
-- Need:
--  - nv : num vertices
--  - nde : number of directed edges
--  - v : index into e s.t. e[v[i]] ... e[v[i] + d[i] - 1]
--        are vertices that vertex i is joined to
--  - d : d[i] gives out-degree of vertex i
--  - e : nde long
--  - lab + ptn for colors
