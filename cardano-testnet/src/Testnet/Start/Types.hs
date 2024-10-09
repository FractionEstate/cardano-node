{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Testnet.Start.Types
  ( CardanoTestnetCliOptions(..)
  , CardanoTestnetOptions(..)

  , anyEraToString
  , anyShelleyBasedEraToString
  , eraToString

  , TestnetNodeOptions(..)
  , extraSpoNodeCliArgs
  , cardanoDefaultTestnetNodeOptions
  , GenesisOptions(..)

  , NodeLoggingFormat(..)
  , Conf(..)
  , NodeConfigurationYaml(..)
  , mkConf
  ) where

import           Cardano.Api hiding (cardanoEra)

import           Prelude

import           Data.Char (toLower)
import           Data.Default.Class
import           Data.Word
import           GHC.Stack
import           System.FilePath (addTrailingPathSeparator)

import           Testnet.Filepath

import           Hedgehog (MonadTest)
import qualified Hedgehog.Extras as H

-- | Command line options for the @cardano-testnet@ executable. They are used
-- in the parser, and then get split into 'CardanoTestnetOptions' and
-- 'GenesisOptions'
data CardanoTestnetCliOptions = CardanoTestnetCliOptions
  { cliTestnetOptions :: CardanoTestnetOptions
  , cliGenesisOptions :: GenesisOptions
  } deriving (Eq, Show)

instance Default CardanoTestnetCliOptions where
  def = CardanoTestnetCliOptions
    { cliTestnetOptions = def
    , cliGenesisOptions = def
    }

-- | Options which, contrary to 'GenesisOptions' are not implemented
-- by tuning the genesis files.
data CardanoTestnetOptions = CardanoTestnetOptions
  { -- | List of node options. Each option will result in a single node being
    -- created.
    cardanoNodes :: [TestnetNodeOptions]
  , cardanoNodeEra :: AnyShelleyBasedEra -- ^ The era to start at
  , cardanoMaxSupply :: Word64 -- ^ The amount of Lovelace you are starting your testnet with (forwarded to shelley genesis)
                               -- TODO move me to GenesisOptions when https://github.com/IntersectMBO/cardano-cli/pull/874 makes it to cardano-node
  , cardanoEnableP2P :: Bool
  , cardanoNodeLoggingFormat :: NodeLoggingFormat
  , cardanoNumDReps :: Int -- ^ The number of DReps to generate at creation
  , cardanoEnableNewEpochStateLogging :: Bool -- ^ if epoch state logging is enabled
  } deriving (Eq, Show)

instance Default CardanoTestnetOptions where
  def = CardanoTestnetOptions
    { cardanoNodes = cardanoDefaultTestnetNodeOptions
    , cardanoNodeEra = AnyShelleyBasedEra ShelleyBasedEraBabbage
    , cardanoMaxSupply = 100_000_020_000_000 -- 100 000 billions Lovelace, so 100 millions ADA. This amount should be bigger than the 'byronTotalBalance' in Testnet.Start.Byron
    , cardanoEnableP2P = False
    , cardanoNodeLoggingFormat = NodeLoggingFormatAsJson
    , cardanoNumDReps = 3
    , cardanoEnableNewEpochStateLogging = True
    }

-- | Options that are implemented by writing fields in the Shelley genesis file.
data GenesisOptions = GenesisOptions
  { genesisTestnetMagic :: Int -- TODO Use the NetworkMagic type from API
  , genesisEpochLength :: Int -- ^ An epoch's duration, in number of slots
  , genesisSlotLength :: Double -- ^ Slot length, in seconds
  , genesisActiveSlotsCoeff :: Double
  } deriving (Eq, Show)

instance Default GenesisOptions where
  def = GenesisOptions
    { genesisTestnetMagic = 42
    , genesisEpochLength = 500
    , genesisSlotLength = 0.1
    , genesisActiveSlotsCoeff = 0.05
    }

-- | Specify a BFT node (Pre-Babbage era only) or an SPO (Shelley era onwards only)
data TestnetNodeOptions
  = TestnetNodeOptions (Maybe NodeConfigurationYaml) [String]
    -- ^ These arguments will be appended to the default set of CLI options when
    -- starting the node.
  deriving (Eq, Show)

extraSpoNodeCliArgs :: TestnetNodeOptions -> [String]
extraSpoNodeCliArgs (TestnetNodeOptions _ args) = args


cardanoDefaultTestnetNodeOptions :: [TestnetNodeOptions]
cardanoDefaultTestnetNodeOptions =
  [ TestnetNodeOptions Nothing []
  , TestnetNodeOptions Nothing []
  , TestnetNodeOptions Nothing []
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
eraToString :: Pretty (eon era) => eon era -> String
eraToString = map toLower . docToString . pretty

-- | @anyShelleyBasedEraToString $ AnyShelleyBasedEra ShelleyBasedEraConway@ returns @"conway"@
anyShelleyBasedEraToString :: AnyShelleyBasedEra -> String
anyShelleyBasedEraToString (AnyShelleyBasedEra sbe) = eraToString sbe
