{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module QueueModel.Judge.TaskResult (TaskResult(..), result_id, tests_results) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Control.Lens.TH
import qualified QueueModel.Judge.TaskResult.TestResult as Judge.TaskResult (TestResult)

data TaskResult = TaskResult{_result_id :: !(P'.Maybe P'.Int64), _tests_results :: !(P'.Seq Judge.TaskResult.TestResult)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

Control.Lens.TH.makeLenses ''TaskResult

instance P'.Mergeable TaskResult where
  mergeAppend (TaskResult x'1 x'2) (TaskResult y'1 y'2) = TaskResult (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default TaskResult where
  defaultValue = TaskResult P'.defaultValue P'.defaultValue

instance P'.Wire TaskResult where
  wireSize ft' self'@(TaskResult x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 3 x'1 + P'.wireSizeRep 1 11 x'2)
  wirePut ft' self'@(TaskResult x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 3 x'1
             P'.wirePutRep 18 11 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{_result_id = Prelude'.Just new'Field}) (P'.wireGet 3)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{_tests_results = P'.append (_tests_results old'Self) new'Field})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TaskResult) TaskResult where
  getVal m' f' = f' m'

instance P'.GPB TaskResult

instance P'.ReflectDescriptor TaskResult where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Judge.TaskResult\", haskellPrefix = [MName \"QueueModel\"], parentModule = [MName \"Judge\"], baseName = MName \"TaskResult\"}, descFilePath = [\"QueueModel\",\"Judge\",\"TaskResult.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.TaskResult.result_id\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"TaskResult\"], baseName' = FName \"result_id\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.TaskResult.tests_results\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"TaskResult\"], baseName' = FName \"tests_results\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Judge.TaskResult.TestResult\", haskellPrefix = [MName \"QueueModel\"], parentModule = [MName \"Judge\",MName \"TaskResult\"], baseName = MName \"TestResult\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = True}"

instance P'.TextType TaskResult where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TaskResult where
  textPut msg
   = do
       P'.tellT "result_id" (_result_id msg)
       P'.tellT "tests_results" (_tests_results msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'_result_id, parse'_tests_results]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'_result_id
         = P'.try
            (do
               v <- P'.getT "result_id"
               Prelude'.return (\ o -> o{_result_id = v}))
        parse'_tests_results
         = P'.try
            (do
               v <- P'.getT "tests_results"
               Prelude'.return (\ o -> o{_tests_results = P'.append (_tests_results o) v}))