#!/usr/bin/env bash

set -e
# Set –e is used within the Bash to stop execution instantly as a query exits while having a non-zero status. 
#    This function is also used when you need to know the error location in the running code.

set -o pipefail
# This setting prevents errors in a pipeline from being masked. If any command in a pipeline fails, 
#        that return code will be used as the return code of the whole pipeline. 
#        By default, the pipeline's return code is that of the last command even if it succeeds.
# set -x

source helpers.sh

# Set this for this Current run so for multiple runs no issues
SCRIPT_NAME=CrowdFunding
LOVELACE_TO_SEND=2000000  #-- 2 Ada initial
DATUM_HASH_FILE=crowdFunding-datum
SELECTED_WALLET_NAME=forPlutus
TOKEN_QUANTITY_SCRIPT=1

if [ -z ${SELECTED_WALLET_NAME} ];
then
    getInputTx $1
else
    getInputTx $SELECTED_WALLET_NAME
fi



FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}
FROM_BALANCE=${SELECTED_UTXO_LOVELACE}
echo "payToScript - FROM_BALANCE = ${FROM_BALANCE}"
RETURN_BALANCE=`expr $FROM_BALANCE - 4000000`
echo "payToScript - RETURN_BALANCE = ${RETURN_BALANCE}"
UTXO_POLICY_ID=${SELECTED_UTXO_POLICYID}
UTXO_TOKEN_NAME_HEX=${SELECTED_UTXO_TOKEN_NAME_HEX}
echo "payToScript - SELECTED_UTXO_TOKENS = ${SELECTED_UTXO_TOKENS}"
if [ $SELECTED_UTXO_TOKENS -gt 1 ]
then
  TOKENS_TO_SEND_BACK=`expr $SELECTED_UTXO_TOKENS - 1`
else
  TOKENS_TO_SEND_BACK=0
fi
echo "payToScript - UTXO_POLICY_ID = ${UTXO_POLICY_ID}"


if [ -z ${LOVELACE_TO_SEND} ];
then
    read -p 'Lovelace to send: ' LOVELACE_TO_SEND
fi
if [ -z ${SCRIPT_NAME} ];
then
    read -p 'Receiving script name: ' SCRIPT_NAME
fi 

echo $SCRIPT_NAME

if [[ $SCRIPT_NAME != \addr_* ]];
then 
    SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $BASE/plutus-scripts/${SCRIPT_NAME}.plutus --testnet-magic $TESTNET_MAGIC)
#   When the -p option is used, the command creates the directory only if it doesn’t exist.
    mkdir -p $BASE/.priv/Wallets/${SCRIPT_NAME}
    echo $SCRIPT_ADDRESS > $BASE/.priv/Wallets/${SCRIPT_NAME}/${SCRIPT_NAME}.addr
else
    SCRIPT_ADDRESS=$SCRIPT_NAME
fi

if [ -z ${DATUM_HASH_FILE} ];
then
    read -p 'Datum hash file name: ' DATUM_HASH_FILE
fi

DATUM_HASH_FILE=${DATUM_HASH_FILE}.json


if [ -z ${UTXO_POLICY_ID} ]
then 
    TX_OUT=${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND}
else 
    echo "Policy and token=  ${SELECTED_UTXO_POLICYID}"
    read -p 'Send Tokens (will send 1 token) ?: [Y/N]' POL_INPUT
    case $POL_INPUT in 
        [yY][eE][sS]|[yY])
        echo "You say Yes"
        TX_OUT="${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND}+\"1 ${UTXO_POLICY_ID}\""
        ;;
        [nN][oO]|[nN])
            echo "You say No"
        ;;
    esac
fi
echo "Your tx-out is : ${TX_OUT}"

# FROM_TOKENS=${SELECTED_UTXO_TOKENS}
# --tx-out ${TO_WALLET_ADDRESS}+${LOVELACE_TO_SEND}+"$TOKEN_QUANTITY ${POLICY_ID}.${UTXO_TOKEN_NAME_HEX}" \      -- need to use this to build it.

echo "Your from UTXO is : ${FROM_UTXO}"

# build=($CARDANO_CLI transaction build \
# --babbage-era \
# --cardano-mode \
# --testnet-magic $TESTNET_MAGIC \
# --tx-in ${FROM_UTXO} \
# --tx-out ${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND}+"$TOKEN_QUANTITY ${UTXO_POLICY_ID}.${UTXO_TOKEN_NAME_HEX}" \
# --tx-out-datum-hash-file $BASE/plutus-scripts/${DATUM_HASH_FILE} \
# --change-address=${FROM_WALLET_ADDRESS} \
# --protocol-params-file $BASE/tx/pparams.json \
# --out-file $BASE/tx/tx.draft)


if [ $TOKENS_TO_SEND_BACK -gt 0 ]
then
  build=($CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in ${FROM_UTXO} \
  --tx-out ${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND}+"$TOKEN_QUANTITY_SCRIPT ${UTXO_POLICY_ID}.${UTXO_TOKEN_NAME_HEX}" \
  --tx-out-inline-datum-file $BASE/plutus-scripts/${DATUM_HASH_FILE} \
  --tx-out ${FROM_WALLET_ADDRESS}+${RETURN_BALANCE}+"$TOKENS_TO_SEND_BACK ${UTXO_POLICY_ID}.${UTXO_TOKEN_NAME_HEX}" \
  --change-address=${FROM_WALLET_ADDRESS} \
  --protocol-params-file $BASE/tx/pparams.json \
  --out-file $BASE/tx/tx.draft)
else
  build=($CARDANO_CLI transaction build \
  --babbage-era \
  --cardano-mode \
  --testnet-magic $TESTNET_MAGIC \
  --tx-in ${FROM_UTXO} \
  --tx-out ${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND}+"$TOKEN_QUANTITY_SCRIPT ${UTXO_POLICY_ID}.${UTXO_TOKEN_NAME_HEX}" \
  --tx-out-inline-datum-file $BASE/plutus-scripts/${DATUM_HASH_FILE} \
  --change-address=${FROM_WALLET_ADDRESS} \
  --protocol-params-file $BASE/tx/pparams.json \
  --out-file $BASE/tx/tx.draft)
fi


#build=("$CARDANO_CLI transaction build --babbage-era --cardano-mode --testnet-magic $TESTNET_MAGIC --tx-in ${FROM_UTXO} --tx-out ${TX_OUT} --tx-out-datum-hash-file $BASE/plutus-scripts/${DATUM_HASH_FILE} --change-address=${FROM_WALLET_ADDRESS} --protocol-params-file $BASE/tx/pparams.json --out-file $BASE/tx/tx.draft")
#--babbage-era
# --tx-out ${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND} \
#--testnet-magic ${TESTNET_MAGIC}  \

# print the cardano transaction build
echo "${build[@]}"

# https://stackoverflow.com/questions/18135451/what-is-the-difference-between-var-var-and-var-in-the-bash-shell - 
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
            --signing-key-file $BASE/.priv/Wallets/${FROM_WALLET_NAME}/${FROM_WALLET_NAME}.skey \
            --testnet-magic $TESTNET_MAGIC \
            --out-file $BASE/tx/tx.signed

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