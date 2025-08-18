
import Test.Hspec

import Game.BoardSpec
-- import Game.GameSpec

main :: IO ()
main = hspec $ do
  Game.BoardSpec.spec
  -- Game.GameSpec.spec

