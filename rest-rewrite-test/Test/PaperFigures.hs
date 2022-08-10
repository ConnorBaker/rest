{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.PaperFigures where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity

import Data.List (sort)

import qualified Data.HashSet as S
import Data.Hashable (Hashable)

import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (encodeUtf8)

import           Language.REST.Op
import Language.REST.Dot (DiGraph, GraphType (Tree), graphString)
import Language.REST.ExploredTerms (ExploreStrategy (ExploreAlways, ExploreWhenNeeded))
import Language.REST.Internal.Rewrite (Rewrite (..))
import Language.REST.Internal.WorkStrategy (bfs)
import Language.REST.Core (orient)
import Language.REST.KBO (kbo)
import Language.REST.LPO (lpo, lpoStrict)
import Language.REST.OCAlgebra (OCAlgebra, fuelOC, refine, top)
import Language.REST.OCToAbstract (lift)
import Test.Lib.Language.REST.ProofGen (toProof)
import Language.REST.RESTDot (PrettyPrinter (PrettyPrinter), toGraph, ShowRejectsOpt(..))
import Language.REST.RPO (rpo)
import Language.REST.Rest
import Language.REST.RuntimeTerm (RuntimeTerm)
import Language.REST.SMT (SolverHandle, withZ3)
import Language.REST.Types (toOrderedSet)
import Test.Lib.Language.REST.ConcreteOC (concreteOC)
import Language.REST.WQOConstraints (isSatisfiable)
import qualified Language.REST.WQOConstraints.ADT as AC
import qualified Language.REST.WQOConstraints.Strict as SC
import qualified Data.Set as DS

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)

import Test.Lib.DSL (
  assocL,
  assocR,
  distribL,
  distribR,
  v,
  x,
  y,
  (#+),
  (<~>),
  (~>),
 )
import Test.Lib.Nat (parseTerm, pp)
import Test.Lib.Set (
  emptyset,
  s0,
  s1,
  (/\),
  (\/),
 )

data ConsType = Strict | Lazy | ADT

data GraphParams = GraphParams
  { gShowConstraints :: Bool
  , gTarget :: Maybe String
  , gGraphType :: GraphType
  , gShowRejects :: ShowRejectsOpt
  , gUseETOpt :: Bool
  }

explain :: (Show t2, Show t3, Show a) => (t2 -> t3 -> a) -> (t2, t3) -> IO ()
explain f0 (t, u) = putStrLn $ show t ++ " ≥ " ++ show u ++ " requires:\n" ++ show (f0 t u) ++ "\n\n"

explainOrient :: [String] -> IO ()
explainOrient ts0 = withZ3 go where
  go :: SolverHandle -> IO ()
  go _z3 =
    let
      ts             = map parseTerm ts0
      pairs          = zip ts (tail ts)
    in
      do
        mapM_ (explain (refine impl (top impl))) pairs
        putStrLn $ "Result:\n" ++ show (orient impl ts)
        isSatisfiable SC.strictOC (orient impl ts) >>= print
    where
      impl :: OCAlgebra (SC.StrictOC Op) RuntimeTerm Identity
      impl = lift SC.strictOC lpo

defaultParams :: GraphParams
defaultParams = GraphParams False Nothing Tree ShowRejectsWithoutRule True

withTarget :: String -> GraphParams -> GraphParams
withTarget target0 gp = gp{gTarget = Just target0}

withShowConstraints :: GraphParams -> GraphParams
withShowConstraints gp = gp{gShowConstraints = True}

withNoETOpt :: GraphParams -> GraphParams
withNoETOpt gp = gp{gUseETOpt = False}

withHideRejects :: GraphParams -> GraphParams
withHideRejects gp = gp{gShowRejects = HideRejects}

withShowRejectsRule :: GraphParams -> GraphParams
withShowRejectsRule gp = gp{gShowRejects = ShowRejectsWithRule}

data SolverType = LPOStrict | LPO | RPO | RPOConcrete [Op] | KBO | Fuel Int


mkRESTParams ::
  (MonadIO m, Show oc, Hashable oc, Ord oc) =>
  OCAlgebra oc RuntimeTerm m ->
  S.HashSet rule ->
  S.HashSet rule ->
  GraphParams ->
  RESTParams m rule RuntimeTerm oc PathsResult
mkRESTParams impl evalRWs0 userRWs0 params =
  RESTParams
    { re = evalRWs0
    , ru = userRWs0
    , target = fmap parseTerm (gTarget params)
    , workStrategy = bfs
    , ocImpl = impl
    , initRes = pathsResult
    , etStrategy = if gUseETOpt params then ExploreWhenNeeded else ExploreAlways
    }

mkRESTDiGraphHelper ::
  SolverType ->
  S.HashSet Rewrite ->
  S.HashSet Rewrite ->
  String ->
  GraphParams ->
  IO DiGraph
mkRESTDiGraphHelper LPOStrict evalRWs0 userRWs0 term0 params =
  withZ3 $ \z3 -> mkRESTDiGraph (lift (AC.adtOC z3) lpoStrict) evalRWs0 userRWs0 term0 params
mkRESTDiGraphHelper LPO evalRWs0 userRWs0 term0 params =
  withZ3 $ \z3 -> mkRESTDiGraph (lift (AC.adtOC z3) lpo) evalRWs0 userRWs0 term0 params
mkRESTDiGraphHelper RPO evalRWs0 userRWs0 term0 params =
  withZ3 $ \z3 -> mkRESTDiGraph (lift (AC.adtOC z3) rpo) evalRWs0 userRWs0 term0 params
mkRESTDiGraphHelper (RPOConcrete ops) evalRWs0 userRWs0 term0 params =  mkRESTDiGraph (concreteOC $ DS.fromList ops) evalRWs0 userRWs0 term0 params
mkRESTDiGraphHelper KBO evalRWs0 userRWs0 term0 params =
  withZ3 $ \z3 -> mkRESTDiGraph (kbo z3) evalRWs0 userRWs0 term0 params
mkRESTDiGraphHelper (Fuel n) evalRWs0 userRWs0 term0 params =
  mkRESTDiGraph (fuelOC n) evalRWs0 userRWs0 term0 params

mkRESTDiGraph ::
  (MonadIO m, Show oc, Hashable oc, Ord oc) =>
  OCAlgebra oc RuntimeTerm m ->
  S.HashSet Rewrite ->
  S.HashSet Rewrite ->
  String ->
  GraphParams ->
  m DiGraph
mkRESTDiGraph impl evalRWs0 userRWs0 term0 params =
  do
    (PathsResult paths, targetPath) <- rest (mkRESTParams impl evalRWs0 userRWs0 params) (parseTerm term0)
    let showCons = if gShowConstraints params then show else const ""
    let prettyPrinter = PrettyPrinter ppRewrite pp showCons (gShowRejects params)
    let digraph = toGraph (gGraphType params) prettyPrinter (toOrderedSet paths)
    case gTarget params of
      Just target1 ->
        case targetPath of
          Just tp -> error $ "FOUND TARGET. Proof:\n" ++ toProof tp ++ "\n"
          Nothing -> error $ "TARGET " ++ (pp . parseTerm) target1 ++ " NOT FOUND\n"
      Nothing -> pure digraph

ppRewrite :: Rewrite -> String
ppRewrite (Rewrite t u _) = pp t ++ " → " ++ pp u

ppRewrites :: S.HashSet Rewrite -> String
ppRewrites = unlines . sort . map ppRewrite . S.toList

writeDotFile :: FilePath -> DiGraph -> IO ()
writeDotFile filepath graph = writeFile filepath (graphString graph)

setDistribRules :: S.HashSet Rewrite
setDistribRules = S.fromList
  [ distribL (/\) (\/)
  , distribR (/\) (\/)
  , distribL (\/) (/\)
  , distribR (\/) (/\)
  ]

challengeRulesNoCommute :: S.HashSet Rewrite
challengeRulesNoCommute = S.union setDistribRules $ S.fromList
  [ x /\ x        ~> x
  , x \/ x        ~> x
  , x \/ emptyset ~> x
  , x /\ emptyset ~> emptyset
  , assocL (\/)
  , assocR (\/)
  ]

fig4 :: IO (S.HashSet Rewrite, S.HashSet Rewrite, DiGraph)
fig4  = do
  let impl = RPO
  let evalRWs = S.empty
  let userRWs = S.insert (s1 /\ s0 ~> emptyset) challengeRulesNoCommute
  digraph <- mkRESTDiGraphHelper impl evalRWs userRWs "f(intersect(union(s₀,s₁), s₀))" (withNoETOpt defaultParams)
  pure (evalRWs, userRWs, digraph)

fig8NoOpt ::IO (S.HashSet Rewrite, S.HashSet Rewrite, DiGraph)
fig8NoOpt  = do
  let impl = RPO
  let evalRWs = S.empty
  let userRWs = S.fromList $ (x #+ y ~> y #+ x) : ((x #+ y) #+ v <~> x #+ (y #+ v))
  digraph <- mkRESTDiGraphHelper impl evalRWs userRWs "a + (b + a)" (withNoETOpt defaultParams)
  pure (evalRWs, userRWs, digraph)

fig8Opt :: IO (S.HashSet Rewrite, S.HashSet Rewrite, DiGraph)
fig8Opt  = do
  let impl = RPO
  let evalRWs = S.empty
  let userRWs = S.fromList $ (x #+ y ~> y #+ x) : ((x #+ y) #+ v <~> x #+ (y #+ v))
  digraph <- mkRESTDiGraphHelper impl evalRWs userRWs "a + (b + a)" defaultParams
  pure (evalRWs, userRWs, digraph)

figComponents :: [(String, IO (S.HashSet Rewrite, S.HashSet Rewrite, DiGraph))]
figComponents = [("fig4", fig4), ("fig8-noopt", fig8NoOpt), ("fig8-opt", fig8Opt)]

testTreeConstructor :: String -> IO (S.HashSet Rewrite, S.HashSet Rewrite, DiGraph) -> IO TestTree
testTreeConstructor  figName fig = do
  (evalRWs, userRWs, digraph) <- fig
  pure $
    testGroup
      figName
      [ goldenVsString
          "TXT rewrites file generation"
          (goldenPrefix ++ figName ++ ".txt")
          ((pure . encodeUtf8 . T.pack) (ppRewrites evalRWs ++ ppRewrites userRWs))
      , goldenVsString
          "DOT file generation"
          (goldenPrefix ++ figName ++ ".dot")
          ((pure . encodeUtf8 . T.pack . graphString) digraph)
      ]
 where
  goldenPrefix = "test/graphs/"

goldenTests :: IO TestTree
goldenTests  = testGroup "Golden Tests" <$> mapM (uncurry testTreeConstructor) figComponents

figureExecutor ::  String ->  IO (S.HashSet Rewrite, S.HashSet Rewrite, DiGraph) -> IO ()
figureExecutor  figName fig = do
  (evalRWs, userRWs, digraph) <- fig
  writeFile (outputPrefix ++ figName ++ ".txt") (ppRewrites evalRWs ++ ppRewrites userRWs)
  writeFile (outputPrefix ++ figName ++ ".dot") (graphString digraph)
 where
  outputPrefix = "test/graphs/"

createFigures :: IO ()
createFigures  = mapM_ (uncurry figureExecutor) figComponents
