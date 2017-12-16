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

import Z3.Monad hiding (Params)

import qualified Debug.Trace as T
import qualified Data.Map as M
--

data Graph = Graph
  { numVerts :: Int --nv
  , numEdges :: Int  --nde
  , startEdge :: [Int] --v
  , outDeg :: [Int]  --d
  , endpt :: [Int] --e
  , color :: Map Int Int
  } deriving Show

data Color = OrPre
           | OrPost
           | Lte
           | Not
           | Eq
           | Plus
           | Var
           | Num Int

colToInt :: Color -> Int
colToInt OrPre = 0
colToInt OrPost = 1
colToInt Lte = 2
colToInt Not = 3
colToInt Eq = 4
colToInt Plus = 5
colToInt Var = 6
colToInt (Num i) = 7 + i

defaultGraph :: Graph
defaultGraph = Graph { numVerts = 0
                     , numEdges = 0
                     , startEdge = []
                     , outDeg = []
                     , endpt = []
                     , color = M.empty }

-- Given a list of precondition conjuncts, postcondition conjuncts,
-- and pids, create graph
makeGraph :: AST -> AST -> [Int] -> Z3 Graph
makeGraph pre post pids = do
  astStr <- astToString pre
  let k = T.trace astStr
  pres <- k $ extractConj pre
  posts <- extractConj post
  handleConj (head pres) (colToInt OrPre) defaultGraph
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
  handleConj ast orp g = do
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
                     , numEdges = numEdges g + 2
                     , startEdge = (numEdges g):(startEdge g)
                     , outDeg = 2:outDeg g
                     , endpt = (map (+ (numVerts g + 1)) [1..nParams]) ++ endpt g
                     , color = M.insert orp (numVerts g) (color g) }
          handle (numVerts g', arg, numVerts g' + nParams) [(numVerts g' + 1, args)] g'
        else
          let g' = g { numVerts = numVerts g + 1
                     , numEdges = numEdges g + 1
                     , startEdge = (numEdges g):(startEdge g)
                     , outDeg = 1:outDeg g
                     , endpt = (numVerts g + 1) : endpt g
                     , color = M.insert orp (numVerts g) (color g) } in
          handle (numVerts g', ast, numVerts g' + 1) [] g'
      _ -> 
          let g' = g { numVerts = numVerts g + 1
                     , numEdges = numEdges g + 1
                     , startEdge = (numEdges g):(startEdge g)
                     , outDeg = 1:outDeg g
                     , endpt = (numVerts g + 1) : endpt g
                     , color = M.insert orp (numVerts g) (color g) } in
          handle (numVerts g', ast, numVerts g' + 1)  [] g'
        
  handle :: (Int, AST, Int) -> [(Int, [AST])] -> Graph -> Z3 Graph
  handle (id, ast, ch) siblings g = do
    kind <- getAstKind ast
    case kind of
      Z3_NUMERAL_AST    -> do
        str <- getNumeralString ast
        let g' = g { numVerts = numVerts g + 1
                   , outDeg = 0:outDeg g
                   , color = M.insert (colToInt $ Num (read str)) id (color g) }
        case siblings of
          (id',sib:sibs):sibs' -> handle (id', sib, numVerts g') ((id' + 1, sibs):sibs') g'
          (_,[]):(id',sib:sibs):sibs' -> handle (id', sib, numVerts g') ((id' + 1, sibs):sibs') g'
          _ -> return g'
      Z3_APP_AST        -> do
        app <- toApp ast
        fn <- getAppDecl app
        sym <- getDeclName fn >>= getSymbolString
        nParams <- getAppNumArgs app
        args <- mapM (\i -> getAppArg app i) [0..(nParams-1)]
        if nParams == 0
        then --constant
          -- TODO: fix
          let g' = g { numVerts = numVerts g + 1
                     , outDeg = 0:outDeg g
                     , color = M.insert (colToInt Var) id (color g)} in
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
                         , color = M.insert (colToInt Not) id (color g)} in
              handle (ch, head args, numVerts g') ((ch + 1, tail args):siblings) g'
            _ ->
              let g' = g { numVerts = numVerts g + 1
                         , numEdges = numEdges g + 2
                         , startEdge = (numEdges g):(startEdge g)
                         , outDeg = 2:outDeg g
                         , endpt = ch : endpt g
                         , color = M.insert (colToInt col) id (color g)} in
              handle (ch, head args, numVerts g') ((ch + 1, tail args):siblings) g'
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
