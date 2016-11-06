import System.Exit
import Test.Hspec

import RelationSpec
import LoaderSpec

main :: IO ()
main = hspec $ do
   describe "Relation" RelationSpec.spec
   describe "Loader" LoaderSpec.spec