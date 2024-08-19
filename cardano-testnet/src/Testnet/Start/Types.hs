{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetOptions(..)
  , PerNodeConfiguration(..)
  , cardanoDefaultTestnetOptions

  , anyEraToString
  , eraToString

  , TestnetNodeOptions(..)
  , extraSpoNodeCliArgs
  , cardanoDefaultTestnetNodeOptions

  , NodeLoggingFormat(..)
  , Conf(..)
  , NodeConfigurationYaml(..)
  , mkConf
  ) where

import           Cardano.Api hiding (cardanoEra)

import           Prelude

import           Data.Char (toLower)
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           System.FilePath (addTrailingPathSeparator)

import           Testnet.Filepath

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras as H


data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | List of node options. Each option will result in a single node being
    -- created.
    cardanoNodes :: [TestnetNodeOptions]
  , cardanoNodeEra :: AnyCardanoEra -- ^ The era to start at
  , cardanoEpochLength :: Int -- ^ An epoch's duration, in number of slots
  , cardanoSlotLength :: Double -- ^ Slot length, in seconds
  , cardanoTestnetMagic :: Int
  , cardanoActiveSlotsCoeff :: Double
  , cardanoMaxSupply :: Word64 -- ^ The amount of ADA you are starting your testnet with (forwarded to shelley genesis)
  , cardanoEnableP2P :: Bool
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  , cardanoNumDReps :: Int -- ^ The number of DReps to generate at creation
  , cardanoEnableNewEpochStateLogging :: Bool -- ^ if epoch state logging is enabled
  } deriving (Eq, Show)

cardanoDefaultTestnetOptions :: CardanoTestnetOptions
cardanoDefaultTestnetOptions = CardanoTestnetOptions
  { cardanoNodes = cardanoDefaultTestnetNodeOptions
  , cardanoNodeEra = AnyCardanoEra BabbageEra
  , cardanoEpochLength = 500
  , cardanoSlotLength = 0.1
  , cardanoTestnetMagic = 42
  , cardanoActiveSlotsCoeff = 0.05
  , cardanoMaxSupply = 100_020_000_000 -- 100 billions. This amount should be bigger than the 'byronTotalBalance' in Testnet.Start.Byron
  , cardanoEnableP2P = False
  , cardanoNodeLoggingFormat = NodeLoggingFormatAsJson
  , cardanoNumDReps = 3
  , cardanoEnableNewEpochStateLogging = True
  }

-- | Specify a BFT node (Pre-Babbage era only) or an SPO (Shelley era onwards only)
data TestnetNodeOptions
  = SpoTestnetNodeOptions (Maybe NodeConfigurationYaml) [String]
    -- ^ These arguments will be appended to the default set of CLI options when
    -- starting the node.
  | PerNodeOption FilePath
    -- ^ Path of the file containing the configuration
  deriving (Eq, Show)

-- | The content of a specific node configuration file, as written to disk in JSON
data PerNodeConfiguration = PerNodeConfiguration
  { executable :: Maybe FilePath
  } deriving (Eq, Show, Generic)

instance FromJSON PerNodeConfiguration where

instance ToJSON PerNodeConfiguration where

extraSpoNodeCliArgs :: TestnetNodeOptions -> [String]
extraSpoNodeCliArgs =
  \case
    SpoTestnetNodeOptions _ args -> args
    PerNodeOption _ -> []

cardanoDefaultTestnetNodeOptions :: [TestnetNodeOptions]
cardanoDefaultTestnetNodeOptions =
  [ SpoTestnetNodeOptions Nothing []
  , SpoTestnetNodeOptions Nothing []
  , SpoTestnetNodeOptions Nothing []
  ]

data NodeLoggingFormat = NodeLoggingFormatAsJson | NodeLoggingFormatAsText deriving (Eq, Show)


newtype NodeConfigurationYaml = NodeConfigurationYaml
  { unYamlFilePath :: FilePath
  } deriving (Eq, Show)

newtype Conf = Conf
  { tempAbsPath :: TmpAbsolutePath
  } deriving (Eq, Show)

-- | Create a 'Conf' from a temporary absolute path. Logs the argument in the test.
mkConf :: (HasCallStack, MonadTest m) => FilePath -> m Conf
mkConf tempAbsPath' = withFrozenCallStack $ do
  H.note_ tempAbsPath'
  pure $ Conf
    { tempAbsPath = TmpAbsolutePath (addTrailingPathSeparator tempAbsPath')
    }

-- | @anyEraToString (AnyCardanoEra ByronEra)@ returns @"byron"@
anyEraToString :: AnyCardanoEra -> String
anyEraToString (AnyCardanoEra e) = eraToString e

-- | @eraToString ByronEra@ returns @"byron"@
eraToString :: CardanoEra a -> String
eraToString = map toLower . docToString . pretty
