module Cardano.Node.Rpc.Server
  (

  )
  where 

import Network.GRPC.Common.Protobuf

data Era = Byron
  | Shelley
  | Allegra
  | Mary
  | Alonzo
  | Babbage
  | Conway
  deriving (Show, Eq)


-- Individual handlers 

_getEra :: FakeNode -> Proto () -> IO (Proto Era)
_getEra nodeEra _ = return nodeEra 

-- Server top level 

 -- methods 

-- Predefined era 
type FakeNode = Proto Era

-- Mock node
_nodeCurrentEra :: Proto Era 
_nodeCurrentEra = Proto Conway