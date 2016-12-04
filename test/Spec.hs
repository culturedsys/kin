import System.Exit
import Test.Hspec

import RelationSpec
import LoaderSpec
import PrinterSpec
import ParserSpec

main :: IO ()
main = hspec $ do
   describe "Relation" RelationSpec.spec
   describe "Loader" LoaderSpec.spec
   describe "Printer" PrinterSpec.spec
   describe "Parser" ParserSpec.spec