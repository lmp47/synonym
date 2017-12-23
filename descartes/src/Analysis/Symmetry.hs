{-# LANGUAGE FlexibleInstances #-}
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

import Analysis.Types
import Analysis.Util
import NautyTraces.NautyTraces

import Z3.Monad hiding (Params)

import qualified Debug.Trace as T
import qualified Data.Map as M
--
-- TODO: clean & fix

sepPid :: AST -> IdMap -> PidMap -> Z3 (String, Maybe Int)
sepPid ast idmap pidmap =
  case M.lookup ast pidmap of
    Just pid -> do
      astStr <- astToString ast
      let Ident str = T.trace ("looking up " ++ (show ast)) $ safeLookup "sepPid" ast idmap
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

fromRGraph :: RGraph -> Graph
fromRGraph rg =
  let (lab, ptn) = mkLabPtn in
  Graph { nv = numVerts rg
        , nde = numEdges rg
        , v = reverse $ startEdge rg
        , d = reverse $ outDeg rg
        , e = reverse $ endpt rg
        , lab = lab
        , ptn = ptn
        }
 where
  mkLabPtn =
    foldl
      (\(lab,ptn) (_,vals) -> (vals ++ lab, (take (length vals - 1) (repeat 1)) ++ 0:ptn))
      ([], []) (M.toList $ color rg)

getSymmetries :: Graph -> Z3 Perms
getSymmetries g = do
  symm <- liftIO $ getSymm (nv g) (nde g) (v g) (d g) (e g) (lab g) (ptn g)
  --if count symm > 0 then error "several" else return symm
  return symm

data RGraph = RGraph
  { numVerts :: Int --nv
  , numEdges :: Int  --nde
  , startEdge :: [Int] --v (reversed)
  , outDeg :: [Int]  --d (reversed)
  , endpt :: [Int] --e (reversed)
  , color :: Map Int [Int] -- color int to vertex int
  , colorbk :: ColInt -- color label and int bookkeeping
  } deriving Show

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
    

defaultRGraph :: RGraph
defaultRGraph = RGraph { numVerts = 0
                       , numEdges = 0
                       , startEdge = []
                       , outDeg = []
                       , endpt = []
                       , color = M.empty
                       , colorbk = defaultColInt }

updateColor :: Int -> Int -> Map Int [Int] -> Map Int [Int]
updateColor color i m =
  case M.lookup color m of
    Nothing -> M.insert color [i] m
    Just ls -> M.insert color (i:ls) m

-- Given a list of precondition conjuncts, postcondition conjuncts,
-- and pids, create graph
makeRGraph :: AST -> AST -> [Int] -> IdMap -> PidMap -> Z3 RGraph
makeRGraph pre post pids idmap pidmap = do
  let len = length pids
  let (col, bk) = colToInt Id defaultColInt
  let startRGraph = defaultRGraph { numVerts = len
                                  , startEdge = take len $ repeat 0
                                  , outDeg = take len $ repeat 0
                                  , color = M.fromList $ [(col, [0..len - 1])]
                                  , colorbk = bk }
  astStr <- astToString pre
  let k = T.trace astStr
  pres <- k $ extractConj pre
  posts <- extractConj post
  foldM (handleConj (fst $ colToInt OrPre defaultColInt)) startRGraph pres
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
          arg:args <- mapM (\i -> getAppArg app i) [0..(nParams-1)]
          let g' = g { numVerts = numVerts g + 1 + nParams
                     , numEdges = numEdges g + nParams
                     , startEdge = (numEdges g):(startEdge g)
                     , outDeg = nParams:outDeg g
                     , endpt = (map (+ (numVerts g)) [1..nParams]) ++ endpt g
                     , color = updateColor orp (numVerts g) (color g) }
          let args' = map (\a -> (a, N)) args
          handle (numVerts g + 1, arg, numVerts g', N) [(numVerts g + 2, args')] g'
        else
          let g' = g { numVerts = numVerts g + 2
                     , numEdges = numEdges g + 1
                     , startEdge = (numEdges g):(startEdge g)
                     , outDeg = 1:outDeg g
                     , endpt = (numVerts g + 1) : endpt g
                     , color = updateColor orp (numVerts g) (color g) } in
          handle (numVerts g + 1, ast, numVerts g', N) [] g'
      _ -> 
          let g' = g { numVerts = numVerts g + 2
                     , numEdges = numEdges g + 1
                     , startEdge = (numEdges g):(startEdge g)
                     , outDeg = 1:outDeg g
                     , endpt = (numVerts g + 1) : endpt g
                     , color = updateColor orp (numVerts g) (color g) } in
          handle (numVerts g + 1, ast, numVerts g', N) [] g'
        
  handle :: (Int, AST, Int, Tag) -> [(Int, [(AST, Tag)])] -> RGraph -> Z3 RGraph
  handle (id, ast, ch, tag) siblings g = do
    kind <- getAstKind ast
    case kind of
      Z3_NUMERAL_AST    -> do
        str <- getNumeralString ast
        let (n, bk) = colToInt (Num (tag, read str)) (colorbk g)
        let g' = g { startEdge = 0:(startEdge g)
                   , outDeg = 0:outDeg g
                   , color = updateColor n id (color g)
                   , colorbk = bk }
        case siblings of
          (id',(sib, t):sibs):sibs' -> handle (id', sib, numVerts g', t) ((id' + 1, sibs):sibs') g'
          (_,[]):(id',(sib, t):sibs):sibs' -> handle (id', sib, numVerts g', t) ((id' + 1, sibs):sibs') g'
          _ -> return g'
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
              let k = T.trace str
              let (n, bk) = k $ colToInt (Var (tag, str)) (colorbk g)
              let g' = case pid of
                         Nothing ->
                           g { startEdge = 0:startEdge g
                             , outDeg = 0:outDeg g
                             , color = updateColor n id (color g)
                             , colorbk = bk }
                         Just pid ->
                           g { numEdges = numEdges g + 1
                             , startEdge = (numEdges g):(startEdge g)
                             , outDeg = 1:outDeg g
                             , endpt = safeLookup ("pidToVert: " ++ show pid) pid pidToVert : endpt g
                             , color = updateColor n id (color g)
                             , colorbk = bk }
              case siblings of
                (id',(sib, t):sibs):sibs' -> handle (id', sib, numVerts g', t) ((id' + 1, sibs):sibs') g'
                (_,[]):(id',(sib, t):sibs):sibs' -> handle (id', sib, numVerts g', t) ((id' + 1, sibs):sibs') g'
                _ -> return g'
            else do -- function application
              --let Ident str = safeLookup "handle" ast idmap
              let k = T.trace sym
              let (n, bk) = k $ colToInt (Var (tag, sym)) (colorbk g)
              let endpts = map (+ ch) [0..nParams - 1]
              let g' = g { numVerts = numVerts g + nParams
                         , numEdges = numEdges g + nParams
                         , startEdge = (numEdges g):(startEdge g)
                         , outDeg = nParams:outDeg g
                         , endpt = endpts ++ endpt g
                         , color = updateColor n id (color g)
                         , colorbk = bk }
              let (arg, atag):args' = zipWith (\a i -> (a, Arg i)) args [1..]
              handle (ch, arg, numVerts g', atag) ((ch + 1, args'):siblings) g'
          Just col -> 
            case col of
              Not ->
                let (n, _) = colToInt Not (colorbk g) in
                let g' = g { numVerts = numVerts g + 1
                           , numEdges = numEdges g + 1
                           , startEdge = (numEdges g):(startEdge g)
                           , outDeg = 1:outDeg g
                           , endpt = ch : endpt g
                           , color = updateColor n id (color g)} in
                handle (ch, head args, numVerts g', N) siblings g'
              _ ->
                let (n, _) = colToInt col (colorbk g) in
                let g' = g { numVerts = numVerts g + 2
                           , numEdges = numEdges g + 2
                           , startEdge = (numEdges g):(startEdge g)
                           , outDeg = 2:outDeg g
                           , endpt = ch + 1 : ch : endpt g
                           , color = updateColor n id (color g)} in
                case args of
                  [left, right] ->
                    let (ltag, rtag) = if col == Lte then (Arg 1, Arg 2) else (N, N) in
                    handle (ch, left, numVerts g', ltag) ((ch + 1, [(right, rtag)]):siblings) g'
                  _ -> error "too many args"
      Z3_QUANTIFIER_AST -> do
        numBoundVars <- getQuantifierNumBound ast 
        boundVars <- getQuantifierBoundVars ast 
        body <- getQuantifierBody ast 
        isExists <- isQuantifierExists ast 
        let nParams = numBoundVars + 1
        let col = if isExists then Exists else Forall
        let (n, _) = colToInt col (colorbk g)
        let endpts = map (+ ch) [0..nParams - 1]
        let g' = g { numVerts = numVerts g + nParams
                   , numEdges = numEdges g + nParams
                   , startEdge = (numEdges g):(startEdge g)
                   , outDeg = nParams:outDeg g
                   , endpt = endpts ++ endpt g
                   , color = updateColor n id (color g) }
        let args = zipWith (\a i -> (a, Arg i)) boundVars [2..]
        bodyStr <- astToString body
        handle (ch, body, numVerts g', Arg 1) ((ch + 1, args):siblings) g'
      Z3_VAR_AST       -> do
        str <- astToString ast
        let (n, bk) = colToInt (Var (tag, str)) (colorbk g)
        let g' = g { startEdge = 0:(startEdge g)
                   , outDeg = 0:outDeg g
                   , color = updateColor n id (color g)
                   , colorbk = bk }
        case siblings of
          (id',(sib, t):sibs):sibs' -> handle (id', sib, numVerts g', t) ((id' + 1, sibs):sibs') g'
          (_,[]):(id',(sib, t):sibs):sibs' -> handle (id', sib, numVerts g', t) ((id' + 1, sibs):sibs') g'
          _ -> return g'
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
