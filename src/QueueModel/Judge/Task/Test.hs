{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module QueueModel.Judge.Task.Test (Test(..), test_id, input, output, time_limit, ram_limit) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Control.Lens.TH

data Test = Test{_test_id :: !(P'.Maybe P'.Int64), _input :: !(P'.Maybe P'.Utf8), _output :: !(P'.Maybe P'.Utf8),
                 _time_limit :: !(P'.Maybe P'.Int64), _ram_limit :: !(P'.Maybe P'.Int64)}
          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

Control.Lens.TH.makeLenses ''Test

instance P'.Mergeable Test where
  mergeAppend (Test x'1 x'2 x'3 x'4 x'5) (Test y'1 y'2 y'3 y'4 y'5)
   = Test (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)

instance P'.Default Test where
  defaultValue = Test P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire Test where
  wireSize ft' self'@(Test x'1 x'2 x'3 x'4 x'5)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 3 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 9 x'3 + P'.wireSizeOpt 1 3 x'4 +
             P'.wireSizeOpt 1 3 x'5)
  wirePut ft' self'@(Test x'1 x'2 x'3 x'4 x'5)
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
             P'.wirePutOpt 18 9 x'2
             P'.wirePutOpt 26 9 x'3
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{_test_id = Prelude'.Just new'Field}) (P'.wireGet 3)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{_input = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{_output = Prelude'.Just new'Field}) (P'.wireGet 9)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{_time_limit = Prelude'.Just new'Field}) (P'.wireGet 3)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{_ram_limit = Prelude'.Just new'Field}) (P'.wireGet 3)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Test) Test where
  getVal m' f' = f' m'

instance P'.GPB Test

instance P'.ReflectDescriptor Test where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 26, 32, 40])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Judge.Task.Test\", haskellPrefix = [MName \"QueueModel\"], parentModule = [MName \"Judge\",MName \"Task\"], baseName = MName \"Test\"}, descFilePath = [\"QueueModel\",\"Judge\",\"Task\",\"Test.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.Test.test_id\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\",MName \"Test\"], baseName' = FName \"test_id\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.Test.input\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\",MName \"Test\"], baseName' = FName \"input\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.Test.output\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\",MName \"Test\"], baseName' = FName \"output\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.Test.time_limit\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\",MName \"Test\"], baseName' = FName \"time_limit\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.Test.ram_limit\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\",MName \"Test\"], baseName' = FName \"ram_limit\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = True}"

instance P'.TextType Test where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Test where
  textPut msg
   = do
       P'.tellT "test_id" (_test_id msg)
       P'.tellT "input" (_input msg)
       P'.tellT "output" (_output msg)
       P'.tellT "time_limit" (_time_limit msg)
       P'.tellT "ram_limit" (_ram_limit msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'_test_id, parse'_input, parse'_output, parse'_time_limit, parse'_ram_limit]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'_test_id
         = P'.try
            (do
               v <- P'.getT "test_id"
               Prelude'.return (\ o -> o{_test_id = v}))
        parse'_input
         = P'.try
            (do
               v <- P'.getT "input"
               Prelude'.return (\ o -> o{_input = v}))
        parse'_output
         = P'.try
            (do
               v <- P'.getT "output"
               Prelude'.return (\ o -> o{_output = v}))
        parse'_time_limit
         = P'.try
            (do
               v <- P'.getT "time_limit"
               Prelude'.return (\ o -> o{_time_limit = v}))
        parse'_ram_limit
         = P'.try
            (do
               v <- P'.getT "ram_limit"
               Prelude'.return (\ o -> o{_ram_limit = v}))