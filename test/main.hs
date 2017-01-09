
import Test.Hspec (hspec)
import qualified BasicTests
import Init (runConn)

import Database.Persist.Sql (runMigration)

main = do
  runConn $
    runMigration BasicTests.testMigrate
  hspec $ do
    BasicTests.specs
    BasicTests.specsCDC
