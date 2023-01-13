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
getInputTx $1
FROM_UTXO=${SELECTED_UTXO}
FROM_WALLET_NAME=${SELECTED_WALLET_NAME}
FROM_WALLET_ADDRESS=${SELECTED_WALLET_ADDR}
FROM_BALANCE=${SELECTED_UTXO_LOVELACE}
UTXO_POLICY_ID=${SELECTED_UTXO_POLICYID}

read -p 'Lovelace to send: ' LOVELACE_TO_SEND

read -p 'Receiving script name: ' SCRIPT_NAME

echo $SCRIPT_NAME

if [[ $SCRIPT_NAME != \addr_* ]];
then 
    SCRIPT_ADDRESS=$($CARDANO_CLI address build --payment-script-file $WORK/plutus-scripts/${SCRIPT_NAME}.plutus --testnet-magic $TESTNET_MAGIC)
#   When the -p option is used, the command creates the directory only if it doesn’t exist.
    mkdir -p $BASE/.priv/Wallets/${SCRIPT_NAME}
    echo $SCRIPT_ADDRESS > $BASE/.priv/Wallets/${SCRIPT_NAME}/${SCRIPT_NAME}.addr
else
    SCRIPT_ADDRESS=$SCRIPT_NAME
fi

read -p 'Datum hash file name: ' DATUM_HASH_FILE

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
        TX_OUT=${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND}+'"'1 ${UTXO_POLICY_ID}'"'
        ;;
        [nN][oO]|[nN])
            echo "You say No"
        ;;
    esac
fi
echo "Your tx-out is : ${TX_OUT}"

# FROM_TOKENS=${SELECTED_UTXO_TOKENS}
# --tx-out ${TO_WALLET_ADDRESS}+${LOVELACE_TO_SEND}+"$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}" \      -- need to use this to build it.

echo "Your from UTXO is : ${FROM_UTXO}"

build=($CARDANO_CLI transaction build \
--babbage-era \
--cardano-mode \
--testnet-magic $TESTNET_MAGIC \
--tx-in ${FROM_UTXO} \
--tx-out ${TX_OUT} \
#--tx-out addr_test1wp02taqyn6rp38g4wqn7h5sxccgwkdzex9cegexxsny4qlczfn2al+2000000+"1 d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be.4d7943726f776446756e64" \
--tx-out-datum-hash-file $WORK/plutus-scripts/${DATUM_HASH_FILE} \
--change-address=${FROM_WALLET_ADDRESS} \
--protocol-params-file $WORK/transactions/pparams.json \
--out-file $WORK/transactions/tx.draft)
#--babbage-era
# --tx-out ${SCRIPT_ADDRESS}+${LOVELACE_TO_SEND} \
#--testnet-magic ${TESTNET_MAGIC}  \

# print the cardano transaction build
echo "${build[@]}"

# https://stackoverflow.com/questions/18135451/what-is-the-difference-between-var-var-and-var-in-the-bash-shell - 
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
            --signing-key-file $BASE/.priv/Wallets/${FROM_WALLET_NAME}/${FROM_WALLET_NAME}.skey \
            --testnet-magic $TESTNET_MAGIC \
            --out-file $WORK/transactions/tx.signed

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