{-# LANGUAGE TemplateHaskell, BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module QueueModel.Judge.Task.File (File(..), name, content) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Control.Lens.TH

data File = File{_name :: !(P'.Maybe P'.Utf8), _content :: !(P'.Maybe P'.ByteString)}
          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

Control.Lens.TH.makeLenses ''File

instance P'.Mergeable File where
  mergeAppend (File x'1 x'2) (File y'1 y'2) = File (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default File where
  defaultValue = File P'.defaultValue P'.defaultValue

instance P'.Wire File where
  wireSize ft' self'@(File x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 12 x'2)
  wirePut ft' self'@(File x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 9 x'1
             P'.wirePutOpt 18 12 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{_name = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{_content = Prelude'.Just new'Field}) (P'.wireGet 12)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> File) File where
  getVal m' f' = f' m'

instance P'.GPB File

instance P'.ReflectDescriptor File where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Judge.Task.File\", haskellPrefix = [MName \"QueueModel\"], parentModule = [MName \"Judge\",MName \"Task\"], baseName = MName \"File\"}, descFilePath = [\"QueueModel\",\"Judge\",\"Task\",\"File.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.File.name\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\",MName \"File\"], baseName' = FName \"name\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Judge.Task.File.content\", haskellPrefix' = [MName \"QueueModel\"], parentModule' = [MName \"Judge\",MName \"Task\",MName \"File\"], baseName' = FName \"content\", baseNamePrefix' = \"_\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = True}"

instance P'.TextType File where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg File where
  textPut msg
   = do
       P'.tellT "name" (_name msg)
       P'.tellT "content" (_content msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'_name, parse'_content]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'_name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{_name = v}))
        parse'_content
         = P'.try
            (do
               v <- P'.getT "content"
               Prelude'.return (\ o -> o{_content = v}))