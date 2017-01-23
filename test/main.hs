{-# LANGUAGE CPP #-}

import Test.Hspec (hspec)

#ifdef DO_BASIC_TESTS
import qualified BasicTests
-- Not supported
#undef DO_MULTI_MODEL_TESTS  
#endif

#ifndef DO_MULTI_MODEL_TESTS
import qualified CDCTests
import qualified Model
#else
import qualified MultiModel
import qualified MultiModelTests
#endif

import Init (runConn)

import Database.Persist.Sql (runMigration)

main = do
  runConn $ do
#ifndef DO_MULTI_MODEL_TESTS
    runMigration Model.testMigrate
#else
    runMigration MultiModel.migrate1
    runMigration MultiModel.migrate2
#endif

  hspec $ do
#ifdef DO_BASIC_TESTS
    BasicTests.specs
#endif
#ifndef DO_MULTI_MODEL_TESTS
    CDCTests.specs
#else
    MultiModelTests.specs
#endif
