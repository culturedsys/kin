import System.Exit
import Test.Hspec

import RelationSpec

main :: IO ()
main = hspec $ do
   describe "Relation" RelationSpec.spec