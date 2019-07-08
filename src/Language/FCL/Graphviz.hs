{-

Visualise a script as a workflow net.

TODO:
 - Factor out magic numbers like font size

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Language.FCL.Graphviz (
  Graphviz,
  callDot,
  fileWriteSVG,
  methodsToGraphviz,
  fileToGraphviz,
) where

import Protolude

import Control.Arrow ((>>>))
import qualified Data.Set as Set
import Data.Text (unlines)
import System.FilePath (replaceExtension)
import System.Process.Text (readProcessWithExitCode)

import Language.FCL.AST
import Language.FCL.Analysis (inferMethodsTransitions)
import Language.FCL.Parser (parseFile)
import Language.FCL.Pretty (hsep, prettyPrint, ppr, panicppr)
import Language.FCL.ReachabilityGraph (allPlaces)
import Language.FCL.Utils ((?))

type SVG = Text
type Graphviz = Text

fileToGraphviz :: FilePath -> IO Graphviz
fileToGraphviz fp = scriptToGraphviz <$> parseFile fp

fileToSVG :: FilePath -> IO SVG
fileToSVG = parseFile >=> scriptToSVG

scriptToGraphviz :: Script -> Graphviz
scriptToGraphviz = scriptMethods >>> methodsToGraphviz

scriptToSVG :: Script -> IO SVG
scriptToSVG = scriptToGraphviz >>> callDot

callDot :: Graphviz -> IO SVG
callDot g = do
  (_, out, err) <- readProcessWithExitCode "dot" ["-Tsvg"] g
  if err == "" then pure out else panicppr err

fileWriteGraphviz :: FilePath -> IO ()
fileWriteGraphviz path = fileToGraphviz path >>= writeFile (replaceExtension path ".dot")

fileWriteSVG :: FilePath -> IO ()
fileWriteSVG path = fileToSVG path >>= writeFile (replaceExtension path ".svg")

type Label = Name
type Id = Text

-- | Given a list of methods, produce their corresponding graphviz graph.
methodsToGraphviz :: [Method] -> Graphviz
methodsToGraphviz methods = digraph $ unlines
    [ options
    , graphvizPlaces
    , graphvizTransitions
    , graphvizArrows
    , graphvizRanks
    ]
  where
    digraph :: Graphviz -> Graphviz
    digraph body = unlines
      [ "digraph workflow {"
      , "edge [color=\"#000000\"]"
      , body
      , "}"
      ]

    graphvizPlaces :: Graphviz
    graphvizPlaces = unlines . map mkPlace $ places
      where
        places :: [Place]
        places = Set.toList . allPlaces . Set.fromList . fmap snd3 $ labelledTransitions

        mkPlace :: Place -> Graphviz
        mkPlace p@(Place _) = prettyPrint p
          <> " [shape=ellipse; fontname=\"Arial\"; fontsize=16; style=filled; color=black; fillcolor=white;]"
        mkPlace p@(PlaceEnd) = prettyPrint p
          <> " [shape=point; width=0.3; peripheries=2; style=filled; color=\"#d11010\"; label=\"\"]"
        mkPlace p@(PlaceStart) = prettyPrint p
          <> " [shape=point; width=0.3; style=filled; color=\"#0e64ce\"; label=\"\"]"


    graphvizTransitions :: Graphviz
    graphvizTransitions = unlines . map mkTransition $ labelledTransitions
      where
        mkTransition :: (Method, Transition, Id) -> Graphviz
        mkTransition (method, _, id) = mconcat
            [ id
            , "[label=<"
            , "<FONT POINT-SIZE=\"16\">" <> prettyPrint (methodName method) <> "</FONT>"
            , "<FONT POINT-SIZE=\"10\" COLOR=\"blue\"> "
            , graphvizPreconditions (methodPreconditions method)
            , "</FONT>"
            , ">"
            , "shape=box; fontname=\"Arial\"; style=filled; color=black; fillcolor=gray75;]"
            ]
          where
          graphvizPreconditions :: Preconditions -> Graphviz
          graphvizPreconditions (Preconditions ps) = mconcat $ map go ps
            where go (p,e) = "<BR/>" <> prettyPrint p <> ": " <> prettyPrint e

    graphvizArrows :: Graphviz
    graphvizArrows = unlines . map mkArrow $ labelledTransitions
      where
        mkArrow :: (Method, Transition, Id) -> Graphviz
        mkArrow (_, Arrow src dst, id)
          = prettyPrint $ hsep
            [ ppr src
            , "->"
            , ppr id
            , ";"
            , ppr id
            , "->"
            , ppr dst
            ]

    labelledTransitions :: [(Method, Transition, Id)]
    labelledTransitions = zipWith mkUnique (inferMethodsTransitions methods) [1..]
      where
        mkUnique (meth, tr) n = (meth, tr, show n)

    -- make ranks to hopefully bring some sanity to the layout of AND-splits
    graphvizRanks :: Graphviz
    graphvizRanks = unlines . mapMaybe mkRank . Set.toList $ workflowStates
      where
        workflowStates :: Set WorkflowState
        workflowStates = foldMap
          ((\(Arrow s d) -> Set.fromList [s, d]) . snd3)
          labelledTransitions

        mkRank :: WorkflowState -> Maybe Graphviz
        mkRank wfst = case Set.toList $ places wfst of
          [_] -> Nothing
          ps -> Just $ "{rank=same " <> (prettyPrint . hsep . map ppr) ps <> "}"

    -- These options can be toggled via the boolean
    options :: Graphviz
    options = unlines
      [ True  ? "graph [bgcolor=transparent]" -- transparent background
      , False ? "rankdir=LR;" -- lay out horizontally
      ]

    thd3 :: (a,b,c) -> c
    thd3 (_,_,z) = z

    snd3 :: (a,b,c) -> b
    snd3 (_,y,_) = y
