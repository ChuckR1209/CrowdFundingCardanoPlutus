#!/usr/bin/env bash

set -e
set -o pipefail

source helpers.sh

set -x

REQUIRED_SIGNER_ARRAY=()
SIGNING_KEY_FILE_ARRAY=()

# Set this for this Current run so for multiple runs no issues
SCRIPT_NAME=CrowdFunding

# TO_WALLET is contributor when redeem is Contribute and When its close its the Beneficiary wallet
# TO_WALLET_NAME=Contributor    
# COLLATERAL=Contributor
# TO_WALLET_NAME=Collateral    # contributor 1 for 2nd run
# COLLATERAL=Collateral        # we use the same contributor 1 wallet - one of the UTXO as colleteral too

# TO_WALLET_NAME=Beneficiary    # Contributor 2 for the 2nd run
# COLLATERAL=Beneficiary        # collateral for the Txn from same Contributor 2 wallet

TO_WALLET_NAME=forPlutus    
COLLATERAL=forPlutus
CLOSE_PAYMENT=302000000

#DATUM_HASH_FILE=crowdFunding-datumOut    # first contribution
DATUM_HASH_FILE=crowdFunding-datumOut2   # 2nd contribution
#REDEEMER_FILE=crowdFundingContribute-redeem
#REDEEMER_FILE=crowdFundingContribute-redeem2
REDEEMER_FILE=crowdFundingClose-redeem
#WRITING_BACK_TO_SCRIPT=Y      # when we contribute this needs to be uncommented
TOKEN_QUANTITY_SCRIPT=1

SIGNER1=$(cat $BASE/.priv/Wallets/$TO_WALLET_NAME/$TO_WALLET_NAME.pubKeyHash)
# SIGNER1=0d29d2f72ba11f3381783dda5501139f397d81e83244fce13e7a711a
SIGNER_FILE1=$BASE/.priv/Wallets/$TO_WALLET_NAME/$TO_WALLET_NAME.skey

if [ -z ${SIGNER1} ];
then
  echo "no pre-set signers provided"
else
  REQUIRED_SIGNER_ARRAY+='--required-signer '
  REQUIRED_SIGNER_ARRAY+=$SIGNER_FILE1
  REQUIRED_SIGNER_ARRAY+=' '
  SIGNING_KEY_FILE_ARRAY+='--signing-key-file '
  SIGNING_KEY_FILE_ARRAY+=$SIGNER_FILE1
  SIGNING_KEY_FILE_ARRAY+=' '
fi

# SIGNER2=8c573e818f35a8fa8a693933c396561b0622a88bbf34952c4d572cd7
# SIGNER_FILE2=$BASE/.priv/Wallets/Contributor/Contributor.skey

if [ -z ${SIGNER2} ];
then
  echo "no pre-set signers provided"
else
  REQUIRED_SIGNER_ARRAY+='--required-signer  '
  REQUIRED_SIGNER_ARRAY+=$SIGNER_FILE2
  REQUIRED_SIGNER_ARRAY+=' '
  SIGNING_KEY_FILE_ARRAY+='--signing-key-file '
  SIGNING_KEY_FILE_ARRAY+=$SIGNER_FILE2
  SIGNING_KEY_FILE_ARRAY+=' '
fi


if [ -z ${SCRIPT_NAME} ];
then
  read -p 'Script name to spend from: ' SCRIPT_NAME
fi

SCRIPT_FILE=$BASE/plutus-scripts/${SCRIPT_NAME}.plutus 
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
# UTXO_POLICY_ID=${SELECTED_UTXO_POLICYID}
UTXO_TOKEN_NAME_HEX=${SELECTED_UTXO_TOKEN_NAME_HEX}

if [ -z ${WRITING_BACK_TO_SCRIPT} ];
then
    echo " not writing back to script"
else
    #echo "writing back to script - Also Lovelace to add"
    read -p 'writing back to script - Lovelace to send writing back to Script [Y/N]: ' inputLl
    case $inputLl in
      [yY][eE][sS]|[yY])
        echo "You say Yes"
        read -p 'Input more lovelace to send: ' ADDL_LOVELACE
        #PAYMENT=`expr $SELECTED_UTXO_LOVELACE + $ADDL_LOVELACE`
        PAYMENT=$ADDL_LOVELACE     # should not send any other min lovelace
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
fi


# this will also add the Token in the script address not just LoveLace
if [ -z ${UTXO_POLICY_ID} ];
then 
    echo "no token to send"    
    TX_OUT=${SCRIPT_ADDRESS}+${PAYMENT}
else 
    #PAYMENT=${PAYMENT}+'"'1 ${UTXO_POLICY_ID}'"'
    Q1="\"1"
    Q2=" "
    #PAYMENT=${PAYMENT}+$Q1" "${UTXO_POLICY_ID}'"'
    #TX_OUT=${SCRIPT_ADDRESS}+${PAYMENT}+${Q1}" "${UTXO_POLICY_ID}'"'
    TX_OUT=${SCRIPT_ADDRESS}+${PAYMENT}+"$TOKEN_QUANTITY_SCRIPT ${UTXO_POLICY_ID}.${UTXO_TOKEN_NAME_HEX}"
#   --tx-out ${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND}+"$TOKEN_QUANTITY_SCRIPT ${UTXO_POLICY_ID}.${UTXO_TOKEN_NAME_HEX}" \
fi
echo "Your tx-out payment to/from script is : ${PAYMENT}"

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


# Do this below if you did not set the SIGNING_KEY_FILE_ARRAY up at the top
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
        if ! [ -z ${WRITING_BACK_TO_SCRIPT} ];
        then 
          echo "Token is there & writing back to script"
          echo ""
          # Token to spend is there
          #build=("$CARDANO_CLI transaction build --babbage-era --cardano-mode --testnet-magic $TESTNET_MAGIC ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY} --change-address=${FEE_ADDR} --tx-in ${SCRIPT_UTXO} --tx-in-script-file ${SCRIPT_FILE} --tx-in-datum-file $BASE/tx/${DATUM_HASH_FILE} --tx-in-redeemer-file $BASE/tx/${REDEEMER_FILE} --tx-in ${COLLATERAL_TX} --tx-in-collateral=${COLLATERAL_TX} --tx-out ${TO_WALLET_ADDRESS}+${SELECTED_UTXO_LOVELACE}+\"1 ${UTXO_POLICY_ID}\" ${TO_WALLET_NAME_ARRAY} ${SIGNING_KEY_FILE_ARRAY} --protocol-params-file $BASE/tx/pparams.json --out-file $BASE/tx/tx.draft")
          #build=("$CARDANO_CLI transaction build --babbage-era --cardano-mode --testnet-magic $TESTNET_MAGIC ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY} --change-address=${FEE_ADDR} --tx-in ${SCRIPT_UTXO} --tx-in-script-file ${SCRIPT_FILE} --tx-in-datum-file $BASE/tx/${DATUM_HASH_FILE} --tx-in-redeemer-file $BASE/tx/${REDEEMER_FILE} --tx-in ${COLLATERAL_TX} --tx-in-collateral=${COLLATERAL_TX} --tx-out ${SCRIPT_ADDRESS}+${SELECTED_UTXO_LOVELACE}+\"1 ${UTXO_POLICY_ID}\" ${TO_WALLET_NAME_ARRAY} ${SIGNING_KEY_FILE_ARRAY} --protocol-params-file $BASE/tx/pparams.json --out-file $BASE/tx/tx.draft")
          build=($CARDANO_CLI transaction build \
          --babbage-era \
          --cardano-mode \
          --testnet-magic $TESTNET_MAGIC \
          --tx-in ${COLLATERAL_TX} \
          --tx-in ${SCRIPT_UTXO} \
          --tx-in-script-file ${SCRIPT_FILE} \
          --tx-in-inline-datum-present \
          #  --tx-in-datum-file $BASE/tx/${DATUM_HASH_FILE} \
          --tx-in-redeemer-file $BASE/plutus-scripts/${REDEEMER_FILE} \
          ${REQUIRED_SIGNER_ARRAY} \
          #--required-signer /home/chakravarti/emurgoCardano/.priv/Wallets/Contributor/Contributor.skey \
          --tx-in-collateral=${COLLATERAL_TX} \
          #--tx-out ${SCRIPT_ADDRESS}+${SELECTED_UTXO_LOVELACE}+\"1 ${UTXO_POLICY_ID}\" ${TO_WALLET_NAME_ARRAY} \
          --tx-out ${SCRIPT_ADDRESS}+${PAYMENT}+"$TOKEN_QUANTITY_SCRIPT ${UTXO_POLICY_ID}.${UTXO_TOKEN_NAME_HEX}"
          # ${SIGNING_KEY_FILE_ARRAY} \
          --tx-out-inline-datum-file $BASE/plutus-scripts/${DATUM_HASH_FILE} \
          --change-address=${FEE_ADDR}  \
          ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY}   \
          --protocol-params-file $BASE/tx/pparams.json \
          --out-file $BASE/tx/tx.draft)
        else
          echo "Token is there & not writing back to script - only spend"
          echo ""
          # Token to spend is there
          #build=("$CARDANO_CLI transaction build --babbage-era --cardano-mode --testnet-magic $TESTNET_MAGIC ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY} --change-address=${FEE_ADDR} --tx-in ${SCRIPT_UTXO} --tx-in-script-file ${SCRIPT_FILE} --tx-in-datum-file $BASE/tx/${DATUM_HASH_FILE} --tx-in-redeemer-file $BASE/tx/${REDEEMER_FILE} --tx-in ${COLLATERAL_TX} --tx-in-collateral=${COLLATERAL_TX} --tx-out ${TO_WALLET_ADDRESS}+${SELECTED_UTXO_LOVELACE}+\"1 ${UTXO_POLICY_ID}\" ${TO_WALLET_NAME_ARRAY} ${SIGNING_KEY_FILE_ARRAY} --protocol-params-file $BASE/tx/pparams.json --out-file $BASE/tx/tx.draft")
          #build=("$CARDANO_CLI transaction build --babbage-era --cardano-mode --testnet-magic $TESTNET_MAGIC ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY} --change-address=${FEE_ADDR} --tx-in ${SCRIPT_UTXO} --tx-in-script-file ${SCRIPT_FILE} --tx-in-datum-file $BASE/tx/${DATUM_HASH_FILE} --tx-in-redeemer-file $BASE/tx/${REDEEMER_FILE} --tx-in ${COLLATERAL_TX} --tx-in-collateral=${COLLATERAL_TX} --tx-out ${SCRIPT_ADDRESS}+${SELECTED_UTXO_LOVELACE}+\"1 ${UTXO_POLICY_ID}\" ${TO_WALLET_NAME_ARRAY} ${SIGNING_KEY_FILE_ARRAY} --protocol-params-file $BASE/tx/pparams.json --out-file $BASE/tx/tx.draft")
          build=($CARDANO_CLI transaction build \
          --babbage-era \
          --cardano-mode \
          --testnet-magic $TESTNET_MAGIC \
          --tx-in ${COLLATERAL_TX} \
          --tx-in ${SCRIPT_UTXO} \
          --tx-in-script-file ${SCRIPT_FILE} \
          --tx-in-inline-datum-present \
          #  --tx-in-datum-file $BASE/tx/${DATUM_HASH_FILE} \
          --tx-in-redeemer-file $BASE/plutus-scripts/${REDEEMER_FILE} \
          ${REQUIRED_SIGNER_ARRAY} \
          #--required-signer /home/chakravarti/emurgoCardano/.priv/Wallets/Contributor/Contributor.skey \
          --tx-in-collateral=${COLLATERAL_TX} \
          #--tx-out ${SCRIPT_ADDRESS}+${SELECTED_UTXO_LOVELACE}+\"1 ${UTXO_POLICY_ID}\" ${TO_WALLET_NAME_ARRAY} \
          --tx-out ${TO_WALLET_ADDRESS}+${CLOSE_PAYMENT}+"$TOKEN_QUANTITY_SCRIPT ${UTXO_POLICY_ID}.${UTXO_TOKEN_NAME_HEX}"
          # ${SIGNING_KEY_FILE_ARRAY} \
          # --tx-out-inline-datum-file $BASE/plutus-scripts/${DATUM_HASH_FILE} \
          --change-address=${FEE_ADDR}  \
          ${INVALID_BEFORE_ARRAY} ${INVALID_HEREAFTER_ARRAY}   \
          --protocol-params-file $BASE/tx/pparams.json \
          --out-file $BASE/tx/tx.draft)
        fi
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

TX_HASH=$($CARDANO_CLI transaction txid --tx-body-file $BASE/tx/tx.draft)
# TX_ANALYZE=$($CARDANO_CLI transaction view --tx-body-file $WORK/transactions/tx.draft)

echo 'Transaction with id: ' $TX_HASH

# echo 'User transaction with id: ' $TX_ANALYZE
read -p 'Sign and submit Pay to Script Tx? [Y/N]: ' input

case $input in
      [yY][eE][sS]|[yY])
            echo "You say Yes"

            $CARDANO_CLI transaction sign \
            --tx-body-file $BASE/tx/tx.draft \
            ${SIGNING_KEY_FILE_ARRAY} \
            --testnet-magic $TESTNET_MAGIC \
            --out-file $BASE/tx/tx.signed \

            $CARDANO_CLI transaction submit --tx-file $BASE/tx/tx.signed --testnet-magic $TESTNET_MAGIC

            ;;
      [nN][oO]|[nN])
            echo "You say No"
            ;;
      *)
            echo "Invalid input..."
            exit 1
            ;;
esac