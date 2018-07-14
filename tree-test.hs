import qualified Tree as T
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tree Tests" [treeTests]

treeTests :: TestTree
treeTests = testGroup "Tree" [emptyTreeTests]

emptyTree = T.Empty :: (T.Tree String)
emptyTreeTests :: TestTree
emptyTreeTests =  testGroup "Empty Tree" 
  [
    testCase "Value of Empty Tree shall be Nothing" $
      T.valueOf emptyTree @=? Nothing
  , testCase "empty Tree shall be Empty" $
    T.isEmpty emptyTree @=? True
  , testCase "Left of Empty Tree shall be Empty" $
    T.left emptyTree @=? emptyTree
  , testCase "Right of Empty Tree shall be Empty" $
    T.right emptyTree @=? emptyTree
  , testCase "children of Empty Tree shall be Empty" $
    T.children emptyTree @=? []
  ]
