{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module QueueModel.Judge.Task (Task(..), task_id, configuration, files, tests) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Control.Lens.TH
import qualified QueueModel.Judge.Task.File as Judge.Task (File)
import qualified QueueModel.Judge.Task.Test as Judge.Task (Test)

data Task = Task{_task_id :: !(P'.Maybe P'.Int64), _configuration :: !(P'.Maybe P'.Utf8), _files :: !(P'.Seq Judge.Task.File),
                 _tests :: !(P'.Seq Judge.Task.Test)}
          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

Control.Lens.TH.makeLenses ''Task

instance P'.Mergeable Task where
  mergeAppend (Task x'1 x'2 x'3 x'4) (Task y'1 y'2 y'3 y'4)
   = Task (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default Task where
  defaultValue = Task P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire Task where
  wireSize ft' self'@(Task x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 3 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeRep 1 11 x'4)
  wirePut ft' self'@(Task x'1 x'2 x'3 x'4)
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
             P'.wirePutRep 26 11 x'3
             P'.wirePutRep 34 11 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{_task_id = Prelude'.Just new'Field}) (P'.wireGet 3)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{_configuration = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{_files = P'.append (_files old'Self) new'Field}) (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{_tests = P'.append (_tests old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Task) Task where
  getVal m' f' = f' m'

instance P'.GPB Task

instance P'.ReflectDescriptor Task where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 26, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Judge.Task\", haskellPrefix = [MName \"QueueModel\"], parentModule = [MName \"Judge\"], baseName = MName \"Task\"}, descFilePath = [\"QueueModel\",\"Judge\",\"Task.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.task_id\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\"], baseName' = FName \"task_id\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.configuration\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\"], baseName' = FName \"configuration\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.files\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\"], baseName' = FName \"files\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Judge.Task.File\", haskellPrefix = [MName \"QueueModel\"], parentModule = [MName \"Judge\",MName \"Task\"], baseName = MName \"File\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.tests\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\"], baseName' = FName \"tests\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Judge.Task.Test\", haskellPrefix = [MName \"QueueModel\"], parentModule = [MName \"Judge\",MName \"Task\"], baseName = MName \"Test\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = True}"

instance P'.TextType Task where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Task where
  textPut msg
   = do
       P'.tellT "task_id" (_task_id msg)
       P'.tellT "configuration" (_configuration msg)
       P'.tellT "files" (_files msg)
       P'.tellT "tests" (_tests msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'_task_id, parse'_configuration, parse'_files, parse'_tests]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'_task_id
         = P'.try
            (do
               v <- P'.getT "task_id"
               Prelude'.return (\ o -> o{_task_id = v}))
        parse'_configuration
         = P'.try
            (do
               v <- P'.getT "configuration"
               Prelude'.return (\ o -> o{_configuration = v}))
        parse'_files
         = P'.try
            (do
               v <- P'.getT "files"
               Prelude'.return (\ o -> o{_files = P'.append (_files o) v}))
        parse'_tests
         = P'.try
            (do
               v <- P'.getT "tests"
               Prelude'.return (\ o -> o{_tests = P'.append (_tests o) v}))