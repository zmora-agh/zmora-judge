{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module QueueModel.Judge.TaskResult.Status (Status(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Status = OK
            | RTE
            | MEM
            | TLE
            | ANS
            | CME
            deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Status

instance Prelude'.Bounded Status where
  minBound = OK
  maxBound = CME

instance P'.Default Status where
  defaultValue = OK

toMaybe'Enum :: Prelude'.Int -> P'.Maybe Status
toMaybe'Enum 0 = Prelude'.Just OK
toMaybe'Enum 1 = Prelude'.Just RTE
toMaybe'Enum 2 = Prelude'.Just MEM
toMaybe'Enum 3 = Prelude'.Just TLE
toMaybe'Enum 4 = Prelude'.Just ANS
toMaybe'Enum 5 = Prelude'.Just CME
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum Status where
  fromEnum OK = 0
  fromEnum RTE = 1
  fromEnum MEM = 2
  fromEnum TLE = 3
  fromEnum ANS = 4
  fromEnum CME = 5
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type QueueModel.Judge.TaskResult.Status") .
      toMaybe'Enum
  succ OK = RTE
  succ RTE = MEM
  succ MEM = TLE
  succ TLE = ANS
  succ ANS = CME
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type QueueModel.Judge.TaskResult.Status"
  pred RTE = OK
  pred MEM = RTE
  pred TLE = MEM
  pred ANS = TLE
  pred CME = ANS
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type QueueModel.Judge.TaskResult.Status"

instance P'.Wire Status where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB Status

instance P'.MessageAPI msg' (msg' -> Status) Status where
  getVal m' f' = f' m'

instance P'.ReflectEnum Status where
  reflectEnum = [(0, "OK", OK), (1, "RTE", RTE), (2, "MEM", MEM), (3, "TLE", TLE), (4, "ANS", ANS), (5, "CME", CME)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Judge.TaskResult.Status") ["QueueModel"] ["Judge", "TaskResult"] "Status")
      ["QueueModel", "Judge", "TaskResult", "Status.hs"]
      [(0, "OK"), (1, "RTE"), (2, "MEM"), (3, "TLE"), (4, "ANS"), (5, "CME")]

instance P'.TextType Status where
  tellT = P'.tellShow
  getT = P'.getRead