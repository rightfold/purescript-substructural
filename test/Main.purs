module Test.Main
  ( main
  ) where

import Control.Monad.Eff (Eff)
import Prelude
import Test.Data.Array.Unique as Data.Array.Unique
import Test.Data.Tuple.Sub as Data.Tuple.Sub
import Test.Spec.Reporter.Tap (tapReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: ∀ eff. Eff (RunnerEffects eff) Unit
main = run [tapReporter] do
  Data.Array.Unique.spec
  Data.Tuple.Sub.spec
