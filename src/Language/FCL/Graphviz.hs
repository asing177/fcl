{-

Visualise a script as a workflow net.

TODO:
 - Factor out magic numbers like font size

-}

{-# LANGUAGE ScopedTypeVariables, TypeFamilies, InstanceSigs #-}

module Language.FCL.Graphviz (
  Graphviz,
  callDot,
  fileWriteSVG,
  fileToGraphviz,
  DisplayableWorkflow(..),
  Methods(..),
  Transitions(..),
  methodsToGraphviz,
  transitionsToGraphviz,
  workflowWriteSVG,

  digraph,
  options,
  mkPlace,
  mkRank
  ) where

import Protolude

import Control.Arrow ((>>>))
import qualified Data.Set as Set
import Data.Text (unlines)
import System.FilePath (replaceExtension)
import System.Process.Text (readProcessWithExitCode)

import Language.FCL.AST
import Language.FCL.Analysis (inferMethodsTransitions, inferStaticWorkflowStates)
import Language.FCL.Parser (parseFile)
import Language.FCL.Pretty (hsep, prettyPrint, ppr, panicppr)
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

workflowWriteSVG :: DisplayableWorkflow wf => FilePath -> wf -> IO ()
workflowWriteSVG path = (callDot . renderToGraphviz) >=> writeFile (replaceExtension path ".svg")

methodsToGraphviz :: [Method] -> Graphviz
methodsToGraphviz = renderToGraphviz . Methods

transitionsToGraphviz :: [Transition] -> Graphviz
transitionsToGraphviz = renderToGraphviz . Transitions

type Label = Name
type Id = Text

-- | Class abstraction for workflows renderable to Graphviz format.
class DisplayableWorkflow a where
  {-# MINIMAL renderTransitionNode, renderTransitionArrows, annotatedTransitions, staticWorkflowStates #-}

  -- | Data type representing a transition in the worfklow.
  -- The transition can be annotated with any additional meta information.
  data AnnotatedTransition a

  -- | Renders the node for an annotated transition.
  renderTransitionNode :: AnnotatedTransition a -> Graphviz

  -- | Renders the arrows for an annotated transition.
  renderTransitionArrows :: AnnotatedTransition a -> Graphviz

  -- | Extracts the the annonated transitions from the workflow.
  annotatedTransitions :: a -> [AnnotatedTransition a]

  -- | Extracts the static workflow states (input/output places of transitions)
  -- from the workflow.
  staticWorkflowStates :: a -> Set WorkflowState

  -- | Render a displayable workflow to Graphviz format.
  renderToGraphviz :: a -> Graphviz
  renderToGraphviz wf = digraph $ unlines
    [ options
    , graphvizPlaces
    , graphvizTransitions
    , graphvizArrows
    , graphvizRanks
    ]
    where
      graphvizPlaces :: Graphviz
      graphvizPlaces = unlines
                     . map mkPlace
                     . Set.toList
                     . foldMap places
                     . staticWorkflowStates
                     $ wf

      graphvizTransitions :: Graphviz
      graphvizTransitions = unlines
                          . map renderTransitionNode
                          . annotatedTransitions
                          $ wf

      graphvizArrows :: Graphviz
      graphvizArrows = unlines
                     . map renderTransitionArrows
                     . annotatedTransitions
                     $ wf

      -- make ranks to hopefully bring some sanity to the layout of AND-splits
      graphvizRanks :: Graphviz
      graphvizRanks = unlines
                    . mapMaybe mkRank
                    . Set.toList
                    . staticWorkflowStates
                    $ wf

-- | Workflow represented as a list of methods.
newtype Methods = Methods [Method]
  deriving (Eq, Ord, Show)

instance DisplayableWorkflow Methods where
  -- | Transitions paired with their corresponding method.
  data AnnotatedTransition Methods
    = MTr { mtrMethod     :: Method
          , mtrTransition :: Transition
          , mtrId         :: Id
          }

  renderTransitionNode :: AnnotatedTransition Methods -> Graphviz
  renderTransitionNode (MTr method _ id) = mconcat
    [ id
    , "[label=<"
    , "<FONT POINT-SIZE=\"16\">" <> prettyPrint (methodName method) <> "</FONT>"
    , "<FONT POINT-SIZE=\"10\" COLOR=\"blue\"> "
    , graphvizPreconditions (methodPreconditions method)
    , "</FONT>"
    , ">"
    , "shape=box; fontname=\"Arial\"; style=filled; color=black; fillcolor=gray75;]"
    ]

  renderTransitionArrows :: AnnotatedTransition Methods -> Graphviz
  renderTransitionArrows (MTr _ (Arrow src dst) id) = prettyPrint $ hsep
    [ ppr src
    , "->"
    , ppr id
    , ";"
    , ppr id
    , "->"
    , ppr dst
    ]

  annotatedTransitions :: Methods -> [AnnotatedTransition Methods]
  annotatedTransitions (Methods ms) = zipWith annotateTransition methodsWithTransitions [1..]
    where
      methodsWithTransitions :: [(Method, Transition)]
      methodsWithTransitions = inferMethodsTransitions ms

      annotateTransition :: (Method, Transition) -> Int -> AnnotatedTransition Methods
      annotateTransition (meth, tr) n = MTr meth tr (show n)

  staticWorkflowStates :: Methods -> Set WorkflowState
  staticWorkflowStates ms = inferStaticWorkflowStates
                          . map mtrTransition
                          . annotatedTransitions
                          $ ms

-- | Workflow represented as a list of transitions.
-- Transitions will be named in their order of appearance
-- in the input list.
newtype Transitions = Transitions [Transition]
  deriving (Eq, Ord, Show)

instance DisplayableWorkflow Transitions where
  -- | Named transition.
  data AnnotatedTransition Transitions
    = NTr { ntrTransition :: Transition
          , ntrId         :: Id
          }

  renderTransitionNode :: AnnotatedTransition Transitions -> Graphviz
  renderTransitionNode (NTr _ id) = mconcat
    [ id
    , "[label=<"
    , "<FONT POINT-SIZE=\"16\">" <> prettyPrint id <> "</FONT>"
    , "<FONT POINT-SIZE=\"10\" COLOR=\"blue\"> "
    , "</FONT>"
    , ">"
    , "shape=box; fontname=\"Arial\"; style=filled; color=black; fillcolor=gray75;]"
    ]

  renderTransitionArrows :: AnnotatedTransition Transitions -> Graphviz
  renderTransitionArrows (NTr (Arrow src dst) id) = prettyPrint $ hsep
    [ ppr src
    , "->"
    , ppr id
    , ";"
    , ppr id
    , "->"
    , ppr dst
    ]

  annotatedTransitions :: Transitions -> [AnnotatedTransition Transitions]
  annotatedTransitions (Transitions trs) = zipWith annotateTransition trs [1..]
    where
      annotateTransition :: Transition -> Int -> AnnotatedTransition Transitions
      annotateTransition tr n = NTr tr ("T" <> show n)

  staticWorkflowStates :: Transitions -> Set WorkflowState
  staticWorkflowStates trs = inferStaticWorkflowStates
                           . map ntrTransition
                           . annotatedTransitions
                           $ trs

---------------
-- Utilities --
---------------

digraph :: Graphviz -> Graphviz
digraph body = unlines
  [ "digraph workflow {"
  , "edge [color=\"#000000\"]"
  , body
  , "}"
  ]

mkPlace :: Place -> Graphviz
mkPlace p@(Place _) = prettyPrint p
  <> " [shape=ellipse; fontname=\"Arial\"; fontsize=16; style=filled; color=black; fillcolor=white;]"
mkPlace p@(PlaceEnd) = prettyPrint p
  <> " [shape=point; width=0.3; peripheries=2; style=filled; color=\"#d11010\"; label=\"\"]"
mkPlace p@(PlaceStart) = prettyPrint p
  <> " [shape=point; width=0.3; style=filled; color=\"#0e64ce\"; label=\"\"]"

graphvizPreconditions :: Preconditions -> Graphviz
graphvizPreconditions (Preconditions ps) = mconcat $ map go ps
  where go (p,e) = "<BR/>" <> prettyPrint p <> ": " <> prettyPrint e

mkRank :: WorkflowState -> Maybe Graphviz
mkRank wfst = case Set.toList $ places wfst of
  [_] -> Nothing
  ps -> Just $ "{rank=same " <> (prettyPrint . hsep . map ppr) ps <> "}"

-- QUESTION: What's the point of this?
-- `False ? _` will always be `mempty`, wont it?
-- These options can be toggled via the boolean
options :: Graphviz
options = unlines
  [ True  ? "graph [bgcolor=transparent]" -- transparent background
  , False ? "rankdir=LR;" -- lay out horizontally
  ]
