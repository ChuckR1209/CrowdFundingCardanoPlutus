#!/usr/bin/env bash

set -e
set -o pipefail

source helpers.sh

set -x

REQUIRED_SIGNER_ARRAY=()
SIGNING_KEY_FILE_ARRAY=()

# Set this for this Current run so for multiple runs no issues
SCRIPT_NAME=CrowdFunding
TO_WALLET_NAME=forPlutus
COLLATERAL=Collateral
DATUM_HASH_FILE=CF-datum
REDEEMER_FILE=CFC-redeem

SIGNER1=0d29d2f72ba11f3381783dda5501139f397d81e83244fce13e7a711a
SIGNER_FILE1=$BASE/.priv/Wallets/Collateral/Collateral.skey

if [ -z ${SIGNER1} ];
then
  echo "no pre-set signers provided"
else
  # REQUIRED_SIGNER_ARRAY+='--required-signer-hash '
  # REQUIRED_SIGNER_ARRAY+=$SIGNER1
  # REQUIRED_SIGNER_ARRAY+=' '
  SIGNING_KEY_FILE_ARRAY+='--required-signer '
  SIGNING_KEY_FILE_ARRAY+=$SIGNER_FILE1
  SIGNING_KEY_FILE_ARRAY+=' '
fi


if [ -z ${SCRIPT_NAME} ];
then
  read -p 'Script name to spend from: ' SCRIPT_NAME
fi

SCRIPT_FILE=$WORK/plutus-scripts/${SCRIPT_NAME}.plutus 
SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $SCRIPT_FILE --testnet-magic $TESTNET_MAGIC)
mkdir -p $BASE/.priv/Wallets/${SCRIPT_NAME}
echo $SCRIPT_ADDRESS > $BASE/.priv/Wallets/${SCRIPT_NAME}/${SCRIPT_NAME}.addr

if [ -z ${TO_WALLET_NAME} ];
then
  read -p 'Receiving wallet name: ' TO_WALLET_NAME
fi

if [[ $TO_WALLET_NAME != \addr_* ]];
then 
    TO_WALLET_ADDRESS=$(cat $BASE/.priv/Wallets/$TO_WALLET_NAME/$TO_WALLET_NAME.addr)
else
    TO_WALLET_ADDRESS=$TO_WALLET_NAME
fi

echo ${TO_WALLET_ADDRESS}

section "Select Script UTxO"
getInputTx ${SCRIPT_NAME}
SCRIPT_UTXO=$SELECTED_UTXO
PAYMENT=$SELECTED_UTXO_LOVELACE
UTXO_POLICY_ID=${SELECTED_UTXO_POLICYID}

# this will also add the Token in the script address not just LoveLace
if [ -z ${UTXO_POLICY_ID} ];
then 
    echo "no token to send"    
    TX_OUT=${SCRIPT_ADDRESS}+${SELECTED_UTXO_LOVELACE}
else 
    #PAYMENT=${PAYMENT}+'"'1 ${UTXO_POLICY_ID}'"'
    Q1="\"1"
    Q2=" "
    #PAYMENT=${PAYMENT}+$Q1" "${UTXO_POLICY_ID}'"'
    TX_OUT=${SCRIPT_ADDRESS}+${SELECTED_UTXO_LOVELACE}+${Q1}" "${UTXO_POLICY_ID}'"'
fi
echo "Your tx-out payment from script is : ${PAYMENT}"

echo ${TX_OUT}


section "Select Collateral UTxO"

if [ -z ${COLLATERAL} ];
then
  read -p 'Collateral wallet name: ' COLLATERAL
fi

getInputTx ${COLLATERAL}
COLLATERAL_TX=$SELECTED_UTXO
FEE_ADDR=$SELECTED_WALLET_ADDR

if [ -z ${DATUM_HASH_FILE} ];
then
  read -p 'Datum hash file name: ' DATUM_HASH_FILE
fi

if [ -z ${REDEEMER_FILE} ];
then
  read -p 'Redeemer file name: ' REDEEMER_FILE
fi

DATUM_HASH_FILE=${DATUM_HASH_FILE}.json
REDEEMER_FILE=${REDEEMER_FILE}.json


while true; do
read -p 'Add required-signer-hash? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        echo "You say Yes"
        read -p 'Input required-signer-hash: ' REQUIRED_SIGNER
        read -p 'Input path to skey: ' SIGNING_KEY_FILE
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

INVALID_BEFORE_ARRAY=()
INVALID_HEREAFTER_ARRAY=()
read -p 'Is the script constraint by deadline or time? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        ./currentSlot.sh
        echo "You say Yes"
        echo "[X, X+epochs_valid) validity range in slots"
        read -p 'Input the starting validity slot number (X): ' VALIDITY
        # echo 'Current epoch is: ' 
        read -p 'Input the number of epochs for validity (i.e current slot + 200): ' EPOCHS_VALID
        INVALID_BEFORE_ARRAY+='--invalid-before '
        INVALID_BEFORE_ARRAY+=$((VALIDITY))
        INVALID_BEFORE_ARRAY+=' '
        INVALID_HEREAFTER_ARRAY+='--invalid-hereafter '
        INVALID_HEREAFTER_ARRAY+=$((EPOCHS_VALID))
        INVALID_HEREAFTER_ARRAY+=' '
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

# Check if wanted to add additional outputs


TO_WALLET_NAME_ARRAY=()
while true; do
read -p 'Do you want to add additional outputs? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        echo "You say Yes"
        read -p 'Lovelace to send: ' LOVELACE_TO_SEND
        read -p 'Receiving wallet name: ' TO_WALLET_NAME

        echo TO_WALLET_NAME

        if [[ $TO_WALLET_NAME != \addr_* ]];
        then 
            TO_WALLET_ADDRESS=$(cat $BASE/.priv/Wallets/$TO_WALLET_NAME/$TO_WALLET_NAME.addr)
        else
            TO_WALLET_ADDRESS=$TO_WALLET_NAME
        fi
        TO_WALLET_NAME_ARRAY+='--tx-out '
        TO_WALLET_NAME_ARRAY+=$TO_WALLET_ADDRESS+$LOVELACE_TO_SEND 
        PAYMENT=$(expr $PAYMENT - $LOVELACE_TO_SEND)
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

$CARDANO_CLI query protocol-parameters --testnet-magic $TESTNET_MAGIC > $BASE/tx/pparams.json

#Section to allow the new feature for reference scripts

read -p 'Is the script existing in a reference utxo? [Y/N]: ' input
case $input in
    [yY][eE][sS]|[yY])
        ./currentSlot.sh
        echo "You say Yes"
        # echo 'Current epoch is: ' 
        read -p 'Witness wallet name: ' WITNESS
        getInputTx ${WITNESS}
        WITNESS_TX=$SELECTED_UTXO
        # WITNESS_ADDR=$SELECTED_WALLET_ADDR
        # WITNESS_NAME=${SELECTED_WALLET_NAME}

        build=($CARDANO_CLI transaction build \
        --babbage-era \
        --cardano-mode \
        --testnet-magic $TESTNET_MAGIC \
        ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY} \
        --change-address=${FEE_ADDR} \
        --tx-in ${SCRIPT_UTXO} \
        --spending-tx-in-reference ${WITNESS_TX} \
        --spending-plutus-script-v2 \
        --spending-reference-tx-in-datum-file $WORK/plutus-scripts/${DATUM_HASH_FILE} \
        --spending-reference-tx-in-redeemer-file $WORK/plutus-scripts/${REDEEMER_FILE} \
        --tx-in ${COLLATERAL_TX} \
        --tx-in-collateral=${COLLATERAL_TX} \
        #--tx-out ${TO_WALLET_ADDRESS}+${PAYMENT} \
        --tx-out ${TX_OUT} \
        ${TO_WALLET_NAME_ARRAY} \
        ${REQUIRED_SIGNER_ARRAY} \
        --protocol-params-file $WORK/transactions/pparams.json \
        --out-file $WORK/transactions/tx.draft)
        ;;
    [nN][oO]|[nN])
      if [ -z ${UTXO_POLICY_ID} ];
      then 
        echo "No token"
        # No token to spend
        $CARDANO_CLI transaction build \
        --babbage-era \
        --cardano-mode \
        --testnet-magic $TESTNET_MAGIC \
        ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY} \
        --change-address=${FEE_ADDR} \
        --tx-in ${SCRIPT_UTXO} \
        --tx-in-script-file ${SCRIPT_FILE} \
        --tx-in-datum-file $WORK/plutus-scripts/${DATUM_HASH_FILE} \
        --tx-in-redeemer-file $WORK/plutus-scripts/${REDEEMER_FILE} \
        --tx-in ${COLLATERAL_TX} \
        --tx-in-collateral=${COLLATERAL_TX} \
        #--tx-out ${TO_WALLET_ADDRESS}+${PAYMENT} \
        --tx-out ${TO_WALLET_ADDRESS}+${SELECTED_UTXO_LOVELACE} \
        #--tx-out "'${TX_OUT}'" \
        ${TO_WALLET_NAME_ARRAY} \
        ${REQUIRED_SIGNER_ARRAY} \
        --protocol-params-file $WORK/transactions/pparams.json \
        --out-file $WORK/transactions/tx.draft
      else
        echo "Token is there"
        echo ""
        # Token to spend is there
        build=("$CARDANO_CLI transaction build --babbage-era --cardano-mode --testnet-magic $TESTNET_MAGIC ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY} --change-address=${FEE_ADDR} --tx-in ${SCRIPT_UTXO} --tx-in-script-file ${SCRIPT_FILE} --tx-in-datum-file $BASE/tx/${DATUM_HASH_FILE} --tx-in-redeemer-file $BASE/tx/${REDEEMER_FILE} --tx-in ${COLLATERAL_TX} --tx-in-collateral=${COLLATERAL_TX} --tx-out ${TO_WALLET_ADDRESS}+${SELECTED_UTXO_LOVELACE}+\"1 ${UTXO_POLICY_ID}\" ${TO_WALLET_NAME_ARRAY} ${SIGNING_KEY_FILE_ARRAY} --protocol-params-file $BASE/tx/pparams.json --out-file $BASE/tx/tx.draft")
      fi
      ;;
    *)
        echo "Invalid input..."
        exit 1
        ;;
esac

# print the cardano transaction build
#echo "${build[@]}"
# execute the cardano transaction build
"${build[@]}"

TX_HASH=$($CARDANO_CLI transaction txid --tx-body-file $WORK/transactions/tx.draft)
# TX_ANALYZE=$($CARDANO_CLI transaction view --tx-body-file $WORK/transactions/tx.draft)

echo 'Transaction with id: ' $TX_HASH

# echo 'User transaction with id: ' $TX_ANALYZE
read -p 'Sign and submit Pay to Script Tx? [Y/N]: ' input

case $input in
      [yY][eE][sS]|[yY])
            echo "You say Yes"

            $CARDANO_CLI transaction sign \
            --tx-body-file $WORK/transactions/tx.draft \
            ${SIGNING_KEY_FILE_ARRAY} \
            --testnet-magic $TESTNET_MAGIC \
            --out-file $WORK/transactions/tx.signed \

            $CARDANO_CLI transaction submit --tx-file $WORK/transactions/tx.signed --testnet-magic $TESTNET_MAGIC

            ;;
      [nN][oO]|[nN])
            echo "You say No"
            ;;
      *)
            echo "Invalid input..."
            exit 1
            ;;
esac