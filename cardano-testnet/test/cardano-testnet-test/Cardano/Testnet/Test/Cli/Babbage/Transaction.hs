{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}

{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Use let" -}

module Cardano.Testnet.Test.Cli.Babbage.Transaction
  ( hprop_transaction
  ) where

import           Cardano.Api

import           Cardano.Testnet

import           Prelude

import           Control.Monad (void)
import qualified Data.Text as Text
import           System.FilePath ((</>))
import qualified System.Info as SYS

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras.Test.Base as H
import qualified Hedgehog.Extras.Test.File as H

import qualified Cardano.Api.Ledger.Lens as A
import qualified Data.Map as Map
import           Testnet.Components.SPO
import qualified Testnet.Process.Run as H
import           Testnet.Process.Run
import qualified Testnet.Property.Utils as H
import           Testnet.Runtime

import qualified Cardano.Api.Ledger as L
import qualified Data.List as List
import           Lens.Micro

hprop_transaction :: Property
hprop_transaction = H.integrationRetryWorkspace 0 "babbage-transaction" $ \tempAbsBasePath' -> do
  H.note_ SYS.os
  conf@Conf { tempAbsPath } <- H.noteShowM $ mkConf tempAbsBasePath'
  let tempAbsPath' = unTmpAbsPath tempAbsPath
  work <- H.createDirectoryIfMissing $ tempAbsPath' </> "work"

  let
    sbe = ShelleyBasedEraBabbage
    era = toCardanoEra sbe
    tempBaseAbsPath = makeTmpBaseAbsPath $ TmpAbsolutePath tempAbsPath'
    options = cardanoDefaultTestnetOptions
      { cardanoNodes = cardanoDefaultTestnetNodeOptions
      , cardanoSlotLength = 0.1
      , cardanoNodeEra = AnyCardanoEra era -- TODO: We should only support the latest era and the upcoming era
      }

  TestnetRuntime
    { testnetMagic
    , poolNodes
    , wallets
    } <- cardanoTestnetDefault options conf

  poolNode1 <- H.headM poolNodes

  poolSprocket1 <- H.noteShow $ nodeSprocket $ poolRuntime poolNode1

  execConfig <- H.mkExecConfig tempBaseAbsPath poolSprocket1


  txbodyFp <- H.note $ work </> "tx.body"
  txbodySignedFp <- H.note $ work </> "tx.body.signed"

  void $ execCli' execConfig
    [ "babbage", "query", "utxo"
    , "--address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]

  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  UTxO utxo1 <- H.noteShowM $ decodeEraUTxO sbe utxo1Json
  txin1 <- H.noteShow =<< H.headM (Map.keys utxo1)

  void $ execCli' execConfig
    [ "babbage", "transaction", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", Text.unpack $ paymentKeyInfoAddr $ head wallets
    , "--tx-in", Text.unpack $ renderTxIn txin1
    , "--tx-out", Text.unpack (paymentKeyInfoAddr (head wallets)) <> "+" <> show @Int 5_000_001
    , "--out-file", txbodyFp
    ]

  void $ execCli' execConfig
    [ "babbage", "transaction", "sign"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-body-file", txbodyFp
    , "--signing-key-file", paymentSKey $ paymentKeyInfoPair $ wallets !! 0
    , "--out-file", txbodySignedFp
    ]

  void $ execCli' execConfig
    [ "babbage", "transaction", "submit"
    , "--testnet-magic", show @Int testnetMagic
    , "--tx-file", txbodySignedFp
    ]

  H.byDurationM 1 15 "Expected UTxO found" $ do
    void $ execCli' execConfig
      [ "babbage", "query", "utxo"
      , "--address", Text.unpack $ paymentKeyInfoAddr $ head wallets
      , "--cardano-mode"
      , "--testnet-magic", show @Int testnetMagic
      , "--out-file", work </> "utxo-2.json"
      ]

    utxo2Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-2.json"
    UTxO utxo2 <- H.noteShowM $ decodeEraUTxO sbe utxo2Json
    txouts2 <- H.noteShow $ L.unCoin . txOutValueLovelace . txOutValue . snd <$> Map.toList utxo2

    H.assert $ 5_000_001 `List.elem` txouts2

txOutValue :: TxOut ctx era -> TxOutValue era
txOutValue (TxOut _ v _ _) = v

txOutValueLovelace ::TxOutValue era -> L.Coin
txOutValueLovelace = \case
  TxOutValueShelleyBased sbe v -> v ^. A.adaAssetL sbe
  TxOutValueByron (Lovelace v) -> L.Coin v
