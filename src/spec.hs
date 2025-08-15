
import Test.Hspec

import Domain.GameSpec

main :: IO ()
main = hspec $ do
  Domain.GameSpec.spec

