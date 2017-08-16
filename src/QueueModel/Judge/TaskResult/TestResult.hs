{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module QueueModel.Judge.TaskResult.TestResult (TestResult(..), source_test_id, status, user_time, system_time, ram_usage) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Control.Lens.TH
import qualified QueueModel.Judge.TaskResult.Status as Judge.TaskResult (Status)

data TestResult = TestResult{_source_test_id :: !(P'.Maybe P'.Int64), _status :: !(P'.Maybe Judge.TaskResult.Status),
                             _user_time :: !(P'.Maybe P'.Int64), _system_time :: !(P'.Maybe P'.Int64),
                             _ram_usage :: !(P'.Maybe P'.Int64)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

Control.Lens.TH.makeLenses ''TestResult

instance P'.Mergeable TestResult where
  mergeAppend (TestResult x'1 x'2 x'3 x'4 x'5) (TestResult y'1 y'2 y'3 y'4 y'5)
   = TestResult (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)

instance P'.Default TestResult where
  defaultValue = TestResult P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire TestResult where
  wireSize ft' self'@(TestResult x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 3 x'1 + P'.wireSizeOpt 1 14 x'2 + P'.wireSizeOpt 1 3 x'3 + P'.wireSizeOpt 1 3 x'4 +
             P'.wireSizeOpt 1 3 x'5)
  wirePut ft' self'@(TestResult x'1 x'2 x'3 x'4 x'5)
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
             P'.wirePutOpt 16 14 x'2
             P'.wirePutOpt 24 3 x'3
             P'.wirePutOpt 32 3 x'4
             P'.wirePutOpt 40 3 x'5
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{_source_test_id = Prelude'.Just new'Field}) (P'.wireGet 3)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{_status = Prelude'.Just new'Field}) (P'.wireGet 14)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{_user_time = Prelude'.Just new'Field}) (P'.wireGet 3)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{_system_time = Prelude'.Just new'Field}) (P'.wireGet 3)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{_ram_usage = Prelude'.Just new'Field}) (P'.wireGet 3)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TestResult) TestResult where
  getVal m' f' = f' m'

instance P'.GPB TestResult

instance P'.ReflectDescriptor TestResult where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16, 24, 32, 40])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Judge.TaskResult.TestResult\", haskellPrefix = [MName \"QueueModel\"], parentModule = [MName \"Judge\",MName \"TaskResult\"], baseName = MName \"TestResult\"}, descFilePath = [\"QueueModel\",\"Judge\",\"TaskResult\",\"TestResult.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.TaskResult.TestResult.source_test_id\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"TaskResult\",MName \"TestResult\"], baseName' = FName \"source_test_id\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.TaskResult.TestResult.status\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"TaskResult\",MName \"TestResult\"], baseName' = FName \"status\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Judge.TaskResult.Status\", haskellPrefix = [MName \"QueueModel\"], parentModule = [MName \"Judge\",MName \"TaskResult\"], baseName = MName \"Status\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.TaskResult.TestResult.user_time\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"TaskResult\",MName \"TestResult\"], baseName' = FName \"user_time\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.TaskResult.TestResult.system_time\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"TaskResult\",MName \"TestResult\"], baseName' = FName \"system_time\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.TaskResult.TestResult.ram_usage\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"TaskResult\",MName \"TestResult\"], baseName' = FName \"ram_usage\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = True}"

instance P'.TextType TestResult where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TestResult where
  textPut msg
   = do
       P'.tellT "source_test_id" (_source_test_id msg)
       P'.tellT "status" (_status msg)
       P'.tellT "user_time" (_user_time msg)
       P'.tellT "system_time" (_system_time msg)
       P'.tellT "ram_usage" (_ram_usage msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice [parse'_source_test_id, parse'_status, parse'_user_time, parse'_system_time, parse'_ram_usage])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'_source_test_id
         = P'.try
            (do
               v <- P'.getT "source_test_id"
               Prelude'.return (\ o -> o{_source_test_id = v}))
        parse'_status
         = P'.try
            (do
               v <- P'.getT "status"
               Prelude'.return (\ o -> o{_status = v}))
        parse'_user_time
         = P'.try
            (do
               v <- P'.getT "user_time"
               Prelude'.return (\ o -> o{_user_time = v}))
        parse'_system_time
         = P'.try
            (do
               v <- P'.getT "system_time"
               Prelude'.return (\ o -> o{_system_time = v}))
        parse'_ram_usage
         = P'.try
            (do
               v <- P'.getT "ram_usage"
               Prelude'.return (\ o -> o{_ram_usage = v}))