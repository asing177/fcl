{-

Visualise a script as a workflow net.

TODO:
 - Factor out magic numbers like font size

-}

{-# LANGUAGE ScopedTypeVariables #-}

module Language.FCL.Graphviz where

import Protolude

import qualified Data.Set as Set
import Data.Text (unlines)
import System.FilePath ((-<.>))
import System.IO (BufferMode(NoBuffering), hSetBuffering, hClose)
import System.Process (StdStream(CreatePipe), createProcess, std_in, proc)

import Language.FCL.AST
import Language.FCL.Analysis (actualTransitions)
import Language.FCL.Pretty (hsep, prettyPrint, ppr)
import Language.FCL.ReachabilityGraph (allPlaces)
import Utils ((?), dieRed)

-- | Given a script and its filepath, pipe the graphviz representation of the
-- transitions into @dot@ to produce a visualisation.
doGraphviz :: FilePath -> Script -> IO FilePath
doGraphviz path script = do
    dotE <- try $ createProcess $
      (proc "dot"  ["-T" <> format, "-o", outfile]) { std_in = CreatePipe }
    case dotE of
      Left (_ :: IOException) ->
        dieRed "Couldn't launch `dot`. You probably need to install Graphviz."
      Right (Just hDotIn,_,_,_) -> do
        hSetBuffering hDotIn NoBuffering
        hPutStrLn hDotIn $ graphviz (scriptMethods script)
        hClose hDotIn
        pure outfile
      Right (Nothing,_,_,_) ->
        dieRed "Couldn't open a pipe to `dot`. Please open an issue."
  where
    outfile = (path -<.> format)
    format = "svg"

type Graphviz = Text
type Label = Name
type Id = Text

-- | Given a list of methods, produce their corresponding graphviz graph.
graphviz :: [Method] -> Graphviz
graphviz methods = digraph $ unlines
    [ options
    , graphvizPlaces
    , graphvizTransitions
    , graphvizArrows
    , graphvizRanks
    ]
  where
    digraph :: Graphviz -> Graphviz
    digraph body = "digraph workflow {\n" <> body <> "\n}"

    graphvizPlaces :: Graphviz
    graphvizPlaces = unlines . map mkPlace $ places
      where
        places :: [Place]
        places = Set.toList . allPlaces . Set.fromList . fmap snd3 $ labelledTransitions

        mkPlace :: Place -> Graphviz
        mkPlace p@(Place _) = prettyPrint p
            <> " [shape=ellipse; fontname=\"Arial\"; fontsize=16; style=filled; color=gray75;]"
        mkPlace p@(PlaceEnd) = prettyPrint p <> " [shape=point; width=0.3; peripheries=2; style=filled; color=gray75; label=\"\"]"
        mkPlace p@(PlaceStart) = prettyPrint p <> " [shape=point; width=0.3; style=filled; color=gray75; label=\"\"]"


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
            , "shape=box; fontname=\"Arial\"; style=filled; color=black; fillcolor=white;]"
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
    labelledTransitions = zipWith mkUnique (actualTransitions methods) [1..]
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

    options :: Graphviz
    options = unlines
      [ True  ? "graph [bgcolor=transparent]" -- transparent background
      , False ? "rankdir=LR;" -- lay out horizontally
      ]

    thd3 :: (a,b,c) -> c
    thd3 (_,_,z) = z

    snd3 :: (a,b,c) -> b
    snd3 (_,y,_) = y
