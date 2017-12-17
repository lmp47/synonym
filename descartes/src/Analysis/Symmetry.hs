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

import Z3.Monad hiding (Params)

import qualified Debug.Trace as T
import qualified Data.Map as M
--

-- A hacky way of getting pids for io;
-- relies on assuming:
--  * there are <10 inputs
--  * inputs are of the form "odi",
--    where d is which argument it is
--          i is the pid
--  * outputs are of the form "resi"
--  * all inputs and no outputs are
--    in the idmap
ioPid :: AST -> IdMap -> Z3 (String, Int)
ioPid ast idmap =
  case M.lookup ast idmap of
    Just (Ident str) ->
      return $ (\(x,y) -> (x, read y)) $ splitAt 2 str
    Nothing -> do
      str <- astToString ast
      return $ (\(x,y) -> (x, read y)) $ splitAt 3 str

sepPid :: AST -> IdMap -> PidMap -> Z3 (String, Int)
sepPid ast idmap pidmap =
  case M.lookup ast pidmap of
    Just pid ->
      let Ident str = safeLookup "sepPid" ast idmap in
      return (take (length str - length (show pid)) str, pid)
    _ -> ioPid ast idmap
    
--
data Graph = Graph
  { numVerts :: Int --nv
  , numEdges :: Int  --nde
  , startEdge :: [Int] --v
  , outDeg :: [Int]  --d
  , endpt :: [Int] --e
  , color :: Map Int [Int] -- color int to vertex int
  , colorbk :: ColInt -- color label and int bookkeeping
  } deriving Show

data Tag = N
         | L
         | R deriving (Eq, Ord)

instance Show Tag where
  show N = "N"
  show L = "L"
  show R = "R"

data Color = OrPre
           | OrPost
           | Lte
           | Not
           | Eq
           | Plus
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
  show Id = "Id"
  show (Num (t,i)) = show t ++ ", " ++ show i
  show (Var (t,s)) = show t ++ ", " ++ s

-- TODO: keep map of (NextInt, Map Color Int) for nums and vars instead
type ColInt = (Int, Map Color Int, Map Int Color)

defaultColInt :: ColInt
defaultColInt = (6, M.empty, M.empty)

colToInt :: Color -> ColInt -> (Int, ColInt)
colToInt OrPre m = (0, m)
colToInt OrPost m = (1, m)
colToInt Lte m = (2, m)
colToInt Not m = (3, m)
colToInt Eq m = (4, m)
colToInt Plus m = (5, m)
colToInt Id m = (6, m)
colToInt other (n, ci, ic) =
  case M.lookup other ci of
    Nothing -> (n + 1, (n + 1, M.insert other (n + 1) ci, M.insert (n + 1) other ic))
    Just n -> (n, (n, ci, ic))
    

defaultGraph :: Graph
defaultGraph = Graph { numVerts = 0
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
makeGraph :: AST -> AST -> [Int] -> IdMap -> PidMap -> Z3 Graph
makeGraph pre post pids idmap pidmap = do
  astStr <- astToString pre
  let k = T.trace astStr
  pres <- k $ extractConj pre
  posts <- extractConj post
  foldM (handleConj (fst $ colToInt OrPre defaultColInt)) defaultGraph pres
 where
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
          let g' = g { numVerts = numVerts g + 1
                     , numEdges = numEdges g + nParams
                     , startEdge = (numEdges g):(startEdge g)
                     , outDeg = nParams:outDeg g
                     , endpt = (map (+ (numVerts g + 1)) [1..nParams]) ++ endpt g
                     , color = updateColor orp (numVerts g) (color g) }
          let args' = map (\a -> (a, N)) args
          handle (numVerts g', arg, numVerts g' + nParams, N) [(numVerts g' + 1, args')] g'
        else
          let g' = g { numVerts = numVerts g + 1
                     , numEdges = numEdges g + 1
                     , startEdge = (numEdges g):(startEdge g)
                     , outDeg = 1:outDeg g
                     , endpt = (numVerts g + 1) : endpt g
                     , color = updateColor orp (numVerts g) (color g) } in
          handle (numVerts g', ast, numVerts g' + 1, N) [] g'
      _ -> 
          let g' = g { numVerts = numVerts g + 1
                     , numEdges = numEdges g + 1
                     , startEdge = (numEdges g):(startEdge g)
                     , outDeg = 1:outDeg g
                     , endpt = (numVerts g + 1) : endpt g
                     , color = updateColor orp (numVerts g) (color g) } in
          handle (numVerts g', ast, numVerts g' + 1, N) [] g'
        
  handle :: (Int, AST, Int, Tag) -> [(Int, [(AST, Tag)])] -> Graph -> Z3 Graph
  handle (id, ast, ch, tag) siblings g = do
    kind <- getAstKind ast
    case kind of
      Z3_NUMERAL_AST    -> do
        str <- getNumeralString ast
        let (n, bk) = colToInt (Num (tag, read str)) (colorbk g)
        let g' = g { numVerts = numVerts g + 1
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
                  else Nothing
        case col of
          Nothing -> do
            let Ident strpid = safeLookup "handle" ast idmap
            (str, pid) <- sepPid ast idmap pidmap
            let k = T.trace str
            let (n, bk) = k $ colToInt (Var (tag, str)) (colorbk g)
            let g' = g { numVerts = numVerts g + 1
                       , outDeg = 0:outDeg g
                       , color = updateColor n id (color g)
                       , colorbk = bk }
            case siblings of
              (id',(sib, t):sibs):sibs' -> handle (id', sib, numVerts g', t) ((id' + 1, sibs):sibs') g'
              (_,[]):(id',(sib, t):sibs):sibs' -> handle (id', sib, numVerts g', t) ((id' + 1, sibs):sibs') g'
              _ -> return g'
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
                let g' = g { numVerts = numVerts g + 1
                           , numEdges = numEdges g + 2
                           , startEdge = (numEdges g):(startEdge g)
                           , outDeg = 2:outDeg g
                           , endpt = ch + 1 : ch : endpt g
                           , color = updateColor n id (color g)} in
                case args of
                  [left, right] ->
                    let (ltag, rtag) = if col == Lte then (L, R) else (N, N) in
                    handle (ch, left, numVerts g', ltag) ((ch + 1, [(right, rtag)]):siblings) g'
                  _ -> error "too many args"
{-
        if nParams == 0
        then --constant
          -- TODO: fix
          let g' = g { numVerts = numVerts g + 1
                     , outDeg = 0:outDeg g
                     , color = updateColor (colToInt Var) id (color g)} in
          case siblings of
            (id',sib:sibs):sibs' -> handle (id', sib, numVerts g') ((id' + 1, sibs):sibs') g'
            (_,[]):(id',sib:sibs):sibs' -> handle (id', sib, numVerts g') ((id' + 1, sibs):sibs') g'
            _ -> return g'
        else --node
          let col = if sym == "<="
                    then Lte
                    else if sym == "="
                      then Eq
                    else if sym == "not"
                      then Not
                    else if sym == "+"
                      then Plus
                    else error ("Symbol: " ++ sym ++ ", " ++ (show nParams)) in
          case col of
            Not ->
              let g' = g { numVerts = numVerts g + 1
                         , numEdges = numEdges g + 1
                         , startEdge = (numEdges g):(startEdge g)
                         , outDeg = 1:outDeg g
                         , endpt = ch : endpt g
                         , color = updateColor (colToInt Not) id (color g)} in
              handle (ch, head args, numVerts g') ((ch + 1, tail args):siblings) g'
            _ ->
              let g' = g { numVerts = numVerts g + 1
                         , numEdges = numEdges g + 2
                         , startEdge = (numEdges g):(startEdge g)
                         , outDeg = 2:outDeg g
                         , endpt = ch : endpt g
                         , color = updateColor (colToInt col) id (color g)} in
              handle (ch, head args, numVerts g') ((ch + 1, tail args):siblings) g'
-}
      _                 -> error "unexpected AST"

-- Make sparse nauty/traces-compatible graph:
-- Need:
--  - nv : num vertices
--  - nde : number of directed edges
--  - v : index into e s.t. e[v[i]] ... e[v[i] + d[i] - 1]
--        are vertices that vertex i is joined to
--  - d : d[i] gives out-degree of vertex i
--  - e : nde long
--  - lab + ptn for colors
