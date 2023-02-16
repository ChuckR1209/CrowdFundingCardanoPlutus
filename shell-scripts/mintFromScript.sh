#!/usr/bin/env bash

set -e
set -o pipefail

source helpers.sh

set -x

REQUIRED_SIGNER_ARRAY=()
SIGNING_KEY_FILE_ARRAY=()

# Input for this run only 
SCRIPT_NAME=MintBurn
UTXOWALLET=forPlutus
LOVELACE_TO_SEND=12000000    # send more than min 2Ada otherwise issues in PayToScript
TO_WALLET_NAME=forPlutus
COLLATERAL=Collateral
TOKEN_NAME=MyCrowdFund
TOKEN_QUANTITY=1
REDEEMER_FILE=redeemer-mint.json

# Collateral
SIGNER1=0d29d2f72ba11f3381783dda5501139f397d81e83244fce13e7a711a
SIGNER_FILE1=$BASE/.priv/Wallets/Collateral/Collateral.skey
# forPlutus
SIGNER2=dbbab47cf610921db8e266c3747cd393db6f9d4b7eb8e348ddeb3971
SIGNER_FILE2=$BASE/.priv/Wallets/forPlutus/forPlutus.skey

if [ -z ${SIGNER1} ];
then
  echo "no pre-set signers provided"
else
  REQUIRED_SIGNER_ARRAY+='--required-signer-hash '
  REQUIRED_SIGNER_ARRAY+=$SIGNER1
  REQUIRED_SIGNER_ARRAY+=' '
  #SIGNING_KEY_FILE_ARRAY+='--required-signer '
  SIGNING_KEY_FILE_ARRAY+='--signing-key-file '
  
  SIGNING_KEY_FILE_ARRAY+=$SIGNER_FILE1
  SIGNING_KEY_FILE_ARRAY+=' '
fi
if [ -z ${SIGNER2} ];
then
  echo "no pre-set signers provided"
else
  REQUIRED_SIGNER_ARRAY+='--required-signer-hash '
  REQUIRED_SIGNER_ARRAY+=$SIGNER2
  REQUIRED_SIGNER_ARRAY+=' '
  SIGNING_KEY_FILE_ARRAY+='--signing-key-file '
  SIGNING_KEY_FILE_ARRAY+=$SIGNER_FILE2
  SIGNING_KEY_FILE_ARRAY+=' '
fi


if [ -z ${SCRIPT_NAME} ];
then
    read -p 'Script name to spend from: ' SCRIPT_NAME
fi


read -p 'Do you need to pick a UTXO spend for NFT? [Y/N]: ' inputUtxoSpend
REQUIRED_TX_IN_ARRAY=()
case $inputUtxoSpend in
    [yY][eE][sS]|[yY])
        echo "You say Yes"
        if [ -z ${UTXOWALLET} ];
        then
            read -p 'Input wallet: ' UTXOWALLET
        fi
        getInputTx ${UTXOWALLET}
        UTXOWALLET_TX=$SELECTED_UTXO
        FEE_ADDR=$SELECTED_WALLET_ADDR
        REQUIRED_TX_IN_ARRAY+='--tx-in '
        REQUIRED_TX_IN_ARRAY+=$UTXOWALLET_TX
        ;;
    [nN][oO]|[nN])
        echo "You say No"
        ;;
    *)
        echo "Invalid input..."
        ;;
esac

#read -p 'From WalletName if diff, if same leave blank: ' FROM_WALLET
# if [ -z "$FROM_WALLET" ]
# then 
#   #echo "condition met"
#   FROM_WALLET="$SCRIPT_NAME"
# fi
# echo "Script name to spend from = ${SCRIPT_NAME}"
# echo "From Wallet name = ${FROM_WALLET}"    # if needed.

#SCRIPT_FILE=$BASE/plutus-scripts/${SCRIPT_NAME}.plutus 
SCRIPT_FILE=$BASE/plutus-scripts/${SCRIPT_NAME}.plutus 
SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)
mkdir -p $BASE/.priv/Wallets/${SCRIPT_NAME}
echo $SCRIPT_ADDRESS > $BASE/.priv/Wallets/${SCRIPT_NAME}/${SCRIPT_NAME}.addr

POLICY_ID=$($CARDANO_CLI transaction policyid --script-file $SCRIPT_FILE)

if [ -z ${LOVELACE_TO_SEND} ];
then 
    read -p 'Lovelace to send: ' LOVELACE_TO_SEND
fi

if [ -z ${TO_WALLET_NAME} ];
then
    read -p 'Receiving wallet name: ' TO_WALLET_NAME
fi


# i think if wallet you dont give address directly - it pattern matches with addr*
if [[ $TO_WALLET_NAME != \addr_* ]];
then 
    TO_WALLET_ADDRESS=$(cat $BASE/.priv/Wallets/$TO_WALLET_NAME/$TO_WALLET_NAME.addr)
else
    TO_WALLET_ADDRESS=$TO_WALLET_NAME
fi

section "Select Collateral UTxO"
if [ -z ${COLLATERAL} ];
then
    read -p 'Collateral wallet name: ' COLLATERAL
fi
getInputTx ${COLLATERAL}
COLLATERAL_TX=$SELECTED_UTXO
FEE_ADDR=$SELECTED_WALLET_ADDR


REQUIRED_TX_IN_ARRAY+=' --tx-in '
REQUIRED_TX_IN_ARRAY+=$COLLATERAL_TX


section "Token Creation"

if [ -z ${TOKEN_NAME} ];
then
    read -p 'Token Name ' TOKEN_NAME
fi
if [ -z ${TOKEN_QUANTITY} ];
then
    read -p 'Token quantity ' TOKEN_QUANTITY
fi

# this i guess only creates a hex and not a public key hash
TOKEN_NAME_HEX=$(echo -n "$TOKEN_NAME" | xxd -p)

echo $TOKEN_NAME_HEX

if [ -z ${REDEEMER_FILE} ];
then
    read -p 'Redeemer file name: ' REDEEMER_FILE
fi


# below we have a while loop to add multiple signatures. WHen input is yes it will take the 
#     Signer hash and the signer key file. and since its while loop again asks. 
#                       Once you say no it breaks.
# you can say Y or Yes and also handles case . There is or operator |
# and we are using += to keep adding in to same variable REQUIRED_SIGNER_ARRAY / SIGNING_KEY_FILE_ARRAY


while true; do
read -p 'Add required-signer-hash? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        echo "You say Yes"
        read -p 'Input required-signer-hash: ' REQUIRED_SIGNER
        read -p 'Input path and file to skey: ' SIGNING_KEY_FILE
        REQUIRED_SIGNER_ARRAY+='--required-signer-hash '
        REQUIRED_SIGNER_ARRAY+=$REQUIRED_SIGNER
        REQUIRED_SIGNER_ARRAY+=' '
        SIGNING_KEY_FILE_ARRAY+='--signing-key-file '
        SIGNING_KEY_FILE_ARRAY+=$SIGNING_KEY_FILE
        SIGNING_KEY_FILE_ARRAY+=' '
        ;;
    [nN][oO]|[nN])
        echo "You say No"
        break
        ;;
    *)
        echo "Invalid input..."
        exit 1
        ;;
esac
done

build=($CARDANO_CLI transaction build \
--babbage-era \
--cardano-mode \
--testnet-magic $TESTNET_MAGIC \
#--tx-in ${COLLATERAL_TX} \
${REQUIRED_TX_IN_ARRAY} \
--tx-out ${TO_WALLET_ADDRESS}+${LOVELACE_TO_SEND}+"$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \
--change-address=${TO_WALLET_ADDRESS} \
--mint="$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \
--mint-script-file ${SCRIPT_FILE} \
--mint-redeemer-file $BASE/plutus-scripts/${REDEEMER_FILE} \
--tx-in-collateral=${COLLATERAL_TX} \
${REQUIRED_SIGNER_ARRAY} \
#--protocol-params-file $WORK/transactions/pparams.json \
--protocol-params-file $BASE/tx/pparams.json \
#--out-file $WORK/transactions/tx.draft)
--out-file $BASE/tx/tx.draft)

# print the cardano transaction build
# cat $build
# execute the cardano transaction build
"${build[@]}"

#--tx-body-file $WORK/transactions/tx.draft \
$CARDANO_CLI transaction sign \
--tx-body-file $BASE/tx/tx.draft \
${SIGNING_KEY_FILE_ARRAY} \
--testnet-magic $TESTNET_MAGIC \
--out-file $BASE/tx/tx.signed \

#$CARDANO_CLI transaction submit --tx-file $WORK/transactions/tx.signed --testnet-magic $TESTNET_MAGIC
$CARDANO_CLI transaction submit --tx-file $BASE/tx/tx.signed --testnet-magic $TESTNET_MAGIC
