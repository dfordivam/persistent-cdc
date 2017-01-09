{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE CPP                        #-}

module Database.Persist.CDC.TH
  (share)
  where

import           Database.Persist
import qualified Database.Persist.TH (share)
import           Control.Monad (forM)
import           Data.Text (pack, unpack, Text)
import           Data.Char (toLower, toUpper)
import           Data.Maybe (mapMaybe)

import           Language.Haskell.TH.Syntax
import           Database.Persist.CDC

#define DEBUG_TRACE_ENABLED 1
#ifdef DEBUG_TRACE_ENABLED
import           Debug.Trace
#endif

-- Things needed to do
-- 1. Create a 'History' 'EntityDef' for each of the user defined
--    Have a reference to the original
-- 2. Create instances the PersistRecordCDC for each EntityDef
--
-- The new EntityDef have a correspoding field to each of the original field
--
-- Original 
--  data MyData = {
--      heading :: String
--    , 

-- For field of type 'Maybe a' we ideally need to create a type 'Maybe (Maybe a)'
-- But this is not a valid PersistValue, therefore we create '(Maybe a, Bool)'
-- where the Bool value of True represent change in field data.
-- 
share editAuthorType fs defs = do
  ndefs <- mapM (createNewDefAndNewTypeDecs editAuthorType) defs
  let newTypeDecs = concatMap snd ndefs
      newDefs = map fst ndefs

      instDec = mkPersistStoreCDCTypeInstance editAuthorType

  -- Call the usual 'share' of persistent-template
  standardRoutines <- Database.Persist.TH.share fs (defs ++ newDefs)

  newRoutines <- mkHistory defs
  

  return $
#ifdef DEBUG_TRACE_ENABLED
    trace ("New Routines:\n" ++ (show newRoutines) ++
      "\nNew Defs\n" ++ (show newDefs) ++
      "\nNew Type Decs\n" ++ (show newTypeDecs) ++
      "\nOrig Decs\n" ++ (show defs))
#endif
    (newTypeDecs ++ standardRoutines ++ newRoutines ++ [instDec])

-------------------------------------------------------------------------------------
-- New EntityDef creation
-- The Q Monad is required for doing newName in tuple type creation
createNewDefAndNewTypeDecs :: String -> EntityDef -> Q (EntityDef, [Dec])
createNewDefAndNewTypeDecs editAuthorType def = do
  a <- mapM getNewField $ entityFields def
  let newFields = map fst a
      newTypeDecs = mapMaybe snd a

      newEntityDef = def {
        entityHaskell = newHaskellName -- "MyDataHistory"
          , entityDB = newDBName -- "my_data_history"
          , entityFields = editAuthorId : refField : newFields
          , entityId = newId
          , entityUniques = []}
          -- No change in these
          -- entityAttrs
          -- entityForeigns
          -- entityDerives
          -- entityExtra
          -- entitySum

  return (newEntityDef, newTypeDecs)

  where
    -- "MyDataHistory"
    newHaskellName = concatString def HaskellName unHaskellName entityHaskell "History"
    -- "my_data_history"
    newDBName = concatString def DBName unDBName entityDB "_history"

    editAuthorIdType = pack $ editAuthorType ++ "Id"
    -- User specified EditAuthorType' Id
    editAuthorId = FieldDef {
        fieldHaskell = HaskellName $ "editAuthorId"
      , fieldDB = DBName $ "edit_author_id"
      , fieldType = FTTypeCon Nothing editAuthorIdType
      , fieldSqlType = fieldSqlType (entityId def)
      , fieldAttrs = []
      , fieldStrict = True
      , fieldReference = NoReference
    }

    -- This stores the reference of original Entity
    refField = FieldDef {
        fieldHaskell = entityHaskell def
      , fieldDB = entityDB def
      , fieldType = FTTypeCon Nothing
        (concatString def id unHaskellName entityHaskell "Id")
      , fieldSqlType = fieldSqlType (entityId def)
      , fieldAttrs = []
      , fieldStrict = True
      , fieldReference = NoReference
    }

    -- Id field for the new history entity
    newId = f { fieldType = idType, fieldReference = ref}
      where f = entityId def
            -- MyDataHistoryId
            idType = FTTypeCon Nothing (pack $ (unpack (unHaskellName newHaskellName)) ++ "Id" )
            ref = ForeignRef newHaskellName refFieldType
              where refFieldType = (\(ForeignRef _ x) -> x) (fieldReference f)

    -- For all the original fields, obtain new fielddefs
    getNewField :: FieldDef -> Q (FieldDef, Maybe Dec)
    getNewField field = do
      
      (newFieldType, dec) <-
        if isMayBeAttr field
          -- Please refer to 'Handling Maybe type'
          then do
            -- XXX check for list type
            let t = (\(FTTypeCon _ x) -> x) $ fieldType field
            (d, n) <- mkMaybeTupleType $ t
            return (FTTypeCon Nothing n, Just d)
          else return (fieldType field, Nothing)

      let

        -- If we have Maybe in original field, then we don't need it in history
        -- as we create a tuple with Maybe inside it
        newAttr = if isMayBeAttr field then [] else ["Maybe"]

        -- XXX Review this: 
        newFieldRef =if isMayBeAttr field then NoReference else fieldReference field

        newField = field {
            fieldType = newFieldType
          , fieldAttrs = newAttr
          , fieldReference = newFieldRef}
          -- No change in these
          -- fieldHaskell
          -- fieldDB
          -- fieldType
          -- fieldSqlType
          -- fieldStrict

      return (newField, dec)

-- General utility to concat string to a Text kind of field
concatString :: d -> (Text -> a) -> (b -> Text) -> (d -> b) -> String -> a
concatString def c u f str = c (pack ((unpack $ u $ f def) ++ str))

-------------------------------------------------------------------------------------
-- Code creation part
-- Create type for Maybe fields
mkMaybeTupleType :: Text -> Q (Dec, Text)
mkMaybeTupleType fieldType = do
  tupTypeNewName <- newName "TupType"
  let
    -- Remove _ from name, as it is invalid for type
    tupType = mkName $ filter ((/=) '_') $ show $ tupTypeNewName

    dec = TySynD tupType [] $ TupleT 2 `AppT` maybeT `AppT` ConT ''Bool
    maybeT = ConT ''Maybe `AppT` (ConT $ mkName $ unpack fieldType)
    tupTypeText = pack $ show tupType
  return (dec, tupTypeText)


-- Create the diff functionality
mkHistory :: [EntityDef] -> Q [Dec]
mkHistory decs = do
  a <- mapM mkGetEntityHistoryFun decs
  let b = map mkPersistRecordCDCInstance decs
  return $ a ++ b

-- Type of update function for data A
-- A -> A -> Key A -> Maybe (EntityHistory A)
-- old -> new -> id
mkGetEntityHistoryFun :: EntityDef -> Q Dec
mkGetEntityHistoryFun def = do
  editAuthorId <- newName "editAuthorId"
  old <- newName "old"
  new <- newName "new"
  idn <- newName "id"
  conVarName <- newName "con"

  fields <- mapM (mkFieldDiffStatements def (old, new)) (entityFields def)

  -- For the 'backend' type, not used anywhere
  dummy <- newName "dummy"
  let
    clause = 
      Clause [VarP dummy, VarP editAuthorId, VarP old, VarP new, VarP idn] body decs

    -- AHistory aField1 aField2 ...
    body = NormalB (ConE 'Just `AppE` VarE conVarName)
    decs = con : map fst fields

    -- Constructor Dec - AppE all the fields to construct History data
    con = ValD (VarP conVarName) 
            (NormalB (zipAppE (ConE conName : VarE editAuthorId : fieldVars))) []
      where conName = mkName (historyDataName def)
            zipAppE :: [Exp] -> Exp
            zipAppE (x:y:xs) =
              case xs of
                [] -> x `AppE` y
                _ -> zipAppE ((x `AppE` y) : xs)

            fieldVars = VarE idn : map VarE (map snd fields)

  return $ FunD (mkName (getEntityHistoryFunName def)) [clause]

isMayBeAttr field = elem "Maybe" (fieldAttrs field)

-- Create Dec like this
-- field_23 :: Maybe FieldType
-- field_23 = diffOldAndNew entitiNameFieldName old new
--
-- field_24 :: (Maybe FieldType, Bool)
-- field_24 = diffMaybeValues entityNameFieldName old new
--
mkFieldDiffStatements :: EntityDef -> (Name, Name) -> FieldDef -> Q (Dec, Name)
mkFieldDiffStatements def (old, new) fdef = do
  n <- newName "field"
  let decRhs = NormalB $
        VarE diffFun `AppE` (VarE recordFun) `AppE` (VarE old) `AppE` (VarE new)

      diffFun = if isMayBeAttr fdef
                    then 'diffMaybeValues
                    else 'diffOldAndNew

      -- entityNameFieldName :: Entity -> FieldValue
      recordFun = mkName $ ent ++ field
      
      -- Convert 'fieldName' to 'FieldName'
      fieldName = (unpack (unHaskellName (fieldHaskell fdef)))
      field = (toUpper (head fieldName)) : (tail fieldName)

      -- Convert 'EntityName' to 'entityName'
      entName = (unpack (unHaskellName (entityHaskell def)))
      ent = (toLower (head entName)) : (tail entName)
  return $ (FunD n [Clause [] decRhs []], n)

instanceD :: Cxt -> Type -> [Dec] -> Dec
#if MIN_VERSION_template_haskell(2,11,0)
instanceD = InstanceD Nothing
#else
instanceD = InstanceD
#endif

mkPersistStoreCDCTypeInstance :: String -> Dec
mkPersistStoreCDCTypeInstance editAuthorType =
  instanceD cxt typeN decs
  where 
    cxt = [ConT ''PersistStoreWrite `AppT` backendT]
    typeN = ConT ''PersistStoreCDCType `AppT` backendT

    backendT = VarT (mkName "backend")

    decs = [typeInst]
    typeInst = TySynInstD ''EditAuthorType $
                TySynEqn [backendT] (ConT $ mkName editAuthorType)

mkPersistRecordCDCInstance :: EntityDef -> Dec
mkPersistRecordCDCInstance def =
  instanceD cxt typeN decs
  where 
    cxt = [ConT ''PersistRecordBackend `AppT` entType `AppT` backendT,
           ConT ''PersistStoreWrite `AppT` backendT]
    typeN = ConT ''PersistRecordCDC `AppT` entType `AppT` backendT

    entType = ConT (mkName entName)
    entName = unpack (unHaskellName (entityHaskell def))

    backendT = VarT (mkName "backend")

    decs = [typeInst, getEnt]
    typeInst = TySynInstD ''EntityHistory $
                TySynEqn [entType] (ConT $ mkName $ historyDataName def)
    getEnt = FunD 'getEntityHistory
              [Clause [] (NormalB $ VarE $ mkName $ getEntityHistoryFunName def) []]
        

getEntityHistoryFunName def = "get" ++ (unpack (unHaskellName (entityHaskell def))) ++ "EntityHistory"

historyDataName :: EntityDef -> String
historyDataName def =
  (unpack (unHaskellName (entityHaskell def))) ++ "History"

-- Utility APIs
diffOldAndNew :: (Eq b) => (a -> b) -> a -> a -> Maybe b
diffOldAndNew f old new =
  if f old == f new
    then Nothing
    else Just $ f old

diffMaybeValues :: (Eq b) => (a -> Maybe b) -> a -> a -> (Maybe b, Bool)
diffMaybeValues f old new =
  if f old == f new
    then (Nothing, False)
    else (f old, True)
