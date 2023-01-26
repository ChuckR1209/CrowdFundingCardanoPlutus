#!/usr/bin/env bash
set -e
set -o pipefail

source helpers.sh
getInputTx $1
FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}
FROM_BALANCE=${SELECTED_UTXO_LOVELACE}

read -p 'Lovelace to send: ' LOVELACE_TO_SEND
read -p 'Receiving wallet name: ' TO_WALLET_NAME

echo ${TO_WALLET_NAME}

if [[ $TO_WALLET_NAME != \addr_* ]];
then 
    TO_WALLET_ADDRESS=$(cat $BASE/.priv/Wallets/$TO_WALLET_NAME/$TO_WALLET_NAME.addr)
else
    TO_WALLET_ADDRESS=$TO_WALLET_NAME
fi
echo "sendFromWallet - After to wallet address and before txn build"
echo ${FROM_UTXO}
$CARDANO_CLI transaction build \
--tx-in ${FROM_UTXO} \
--tx-out ${TO_WALLET_ADDRESS}+${LOVELACE_TO_SEND} \
--change-address=${FROM_WALLET_ADDRESS} \
--testnet-magic ${TESTNET_MAGIC} \
--out-file $BASE/tx/tx.draft \
--babbage-era

$CARDANO_CLI transaction sign \
--tx-body-file $BASE/tx/tx.draft \
--signing-key-file $BASE/.priv/Wallets/${FROM_WALLET_NAME}/${FROM_WALLET_NAME}.skey \
--out-file $BASE/tx/tx.signed

$CARDANO_CLI transaction submit --tx-file $BASE/tx/tx.signed --testnet-magic $TESTNET_MAGIC
