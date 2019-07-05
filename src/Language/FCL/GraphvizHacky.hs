{-

Visualise a script as a workflow net.

TODO:
 - Factor out magic numbers like font size

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Language.FCL.GraphvizHacky (
  Graphviz,
  callDot,
  fileWriteSVG,
  transitionsToGraphviz,
) where

import Protolude

import Control.Arrow ((>>>))
import qualified Data.Set as Set
import Data.Text (unlines)
import System.FilePath (replaceExtension, (</>))
import System.Process.Text (readProcessWithExitCode)

import Language.FCL.AST
import Language.FCL.Analysis (actualTransitions)
import Language.FCL.Parser (parseFile)
import Language.FCL.Pretty (hsep, prettyPrint, ppr, panicppr)
import Language.FCL.ReachabilityGraph (allPlaces)
import Language.FCL.Utils ((?))
import Language.FCL.WorkflowGen

type SVG = Text
type Graphviz = Text

-- fileToGraphviz :: FilePath -> IO Graphviz
-- fileToGraphviz fp = transitionsToGraphviz <$> parseFile fp

transitionsToSVG :: [Transition] -> IO SVG
transitionsToSVG = transitionsToGraphviz >>> callDot

callDot :: Graphviz -> IO SVG
callDot g = do
  (_, out, err) <- readProcessWithExitCode "dot" ["-Tsvg"] g
  if err == "" then pure out else panicppr err

-- fileWriteGraphviz :: FilePath -> IO ()
-- fileWriteGraphviz path = fileToGraphviz path >>= writeFile (replaceExtension path ".dot")

fileWriteSVG :: FilePath -> [Transition] -> IO ()
fileWriteSVG path trs = transitionsToSVG trs >>= writeFile (replaceExtension path ".svg")

safeWfNetWriteSVG :: FilePath -> SafeWorkflowNet -> IO ()
safeWfNetWriteSVG path = fileWriteSVG path . constructTransitions

examplesRelPath :: FilePath
examplesRelPath = "../../../examples"

examplesGenRelPath :: FilePath
examplesGenRelPath = examplesRelPath </> "gen"

examplesGenBasicRelPath :: FilePath
examplesGenBasicRelPath = examplesGenRelPath </> "basic"

examplesGenExamplesRelPath :: FilePath
examplesGenExamplesRelPath = examplesGenRelPath </> "examples"

netsToSVG :: FilePath -> [(SafeWorkflowNet, [Char])] -> IO ()
netsToSVG path = mapM_ (\(net, name) -> safeWfNetWriteSVG (path </> name) net)

generateBasicNetsSVGs :: IO ()
generateBasicNetsSVGs = netsToSVG examplesGenBasicRelPath namedBasicNets

generateExampleNetsSVGs :: IO ()
generateExampleNetsSVGs = netsToSVG examplesGenExamplesRelPath namedExampleNets

type Label = Name
type Id = Text

-- | Given a list of tarnsitions, produce their corresponding graphviz graph.
transitionsToGraphviz :: [Transition] -> Graphviz
transitionsToGraphviz transitions = digraph $ unlines
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
        places = Set.toList . allPlaces . Set.fromList . fmap fst $ labelledTransitions

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
        mkTransition :: (Transition, Id) -> Graphviz
        mkTransition (_, id) = mconcat
            [ id
            , "[label=<"
            , "<FONT POINT-SIZE=\"16\">" <> prettyPrint id <> "</FONT>"
            , "<FONT POINT-SIZE=\"10\" COLOR=\"blue\"> "
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
        mkArrow :: (Transition, Id) -> Graphviz
        mkArrow (Arrow src dst, id)
          = prettyPrint $ hsep
            [ ppr src
            , "->"
            , ppr id
            , ";"
            , ppr id
            , "->"
            , ppr dst
            ]

    labelledTransitions :: [(Transition, Id)]
    labelledTransitions = zipWith mkUnique transitions [1..]
      where
        mkUnique tr n = (tr, "T" <> show n)

    -- make ranks to hopefully bring some sanity to the layout of AND-splits
    graphvizRanks :: Graphviz
    graphvizRanks = unlines . mapMaybe mkRank . Set.toList $ workflowStates
      where
        workflowStates :: Set WorkflowState
        workflowStates = foldMap
          ((\(Arrow s d) -> Set.fromList [s, d]) . fst)
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
