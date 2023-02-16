#!/usr/bin/env bash

# read -p 'Script name to spend from: ' SCRIPT_NAME
# read -p 'From WalletName if diff, if same leave blank: ' FROM_WALLET
# if [ -z "${FROM_WALLET}" ]
# then 
#   echo "condition met"
#   FROM_WALLET="$SCRIPT_NAME"
# fi
# echo "Script name to spend from = ${SCRIPT_NAME}"
# echo "From Wallet name = ${FROM_WALLET}"

# TOKEN_NAME="myCrowdFund"
# TOKEN_NAME_HEX=$(echo -n "$TOKEN_NAME" | xxd -p)
# echo "Token name hex - ${TOKEN_NAME_HEX}"


set -e
set -o pipefail

source helpers.sh

#set -x

# read -p 'Do you need to pick a UTXO spend for NFT? [Y/N]: ' inputUtxoSpend
# REQUIRED_TX_IN_ARRAY=()
# case $inputUtxoSpend in
#     [yY][eE][sS]|[yY])
#         echo "You say Yes"
#         read -p 'Input wallet: ' UTXOWALLET
#         getInputTx ${UTXOWALLET}
#         UTXOWALLET_TX=$SELECTED_UTXO
#         FEE_ADDR=$SELECTED_WALLET_ADDR
#         REQUIRED_TX_IN_ARRAY+='--tx-in '
#         REQUIRED_TX_IN_ARRAY+=$UTXOWALLET_TX
#         ;;
#     [nN][oO]|[nN])
#         echo "You say No"
#         ;;
#     *)
#         echo "Invalid input..."
#         ;;
# esac


# section "Select Collateral UTxO"
# read -p 'Collateral wallet name: ' COLLATERAL
# getInputTx ${COLLATERAL}
# COLLATERAL_TX=$SELECTED_UTXO
# FEE_ADDR=$SELECTED_WALLET_ADDR


# REQUIRED_TX_IN_ARRAY+=' --tx-in '
# REQUIRED_TX_IN_ARRAY+=$COLLATERAL_TX


# echo " final line = ${REQUIRED_TX_IN_ARRAY} "

# TX_ROW_NUM=3
# BALANCE_FILE=/home/chakravarti/emurgocdp2/emurgocdp/scripts/work/transactions/Jan10-mintingOne/walletBalances.txt
# #TX_ROW=$(sed "${TX_ROW_NUM}q;d" $BALANCE_FILE)
# TX_ROW="b202a10788f6a33ecfdf4abda70897dc13e9530bdd16499ea8776feeba2595ef     0        2000000 lovelace + 1 d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be.4d7943726f776446756e64 + TxOutDatumNone"
# #echo ${TX_ROW}
# SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
# SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
# SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
# SELECTED_UTXO_POLICYID=$(echo $TX_ROW | awk '{ print $7 }')
# echo ${SELECTED_UTXO}
# echo ${SELECTED_UTXO_LOVELACE}
# echo ${SELECTED_UTXO_TOKENS}
# #echo ${SELECTED_UTXO_POLICYID}
# if [ -z ${SELECTED_UTXO_POLICYID} ]
# then 
#     echo " no policy id found"
# else 
#     echo ${SELECTED_UTXO_POLICYID}
# fi
# # chakravarti@chakravarti-Latitude-7280:~/emurgocdp2/emurgocdp/scripts/work/shell-scripts$ ./test.sh
# # 5c23f8d4f6fe3aa4f9ac767fdae9685c196ce07a01be5169b88a37658978ad63#0
# # 50000000
# # TxOutDatumNone



# build=(cardano-cli query utxo \
# --address addr_test1vrdm4dru7cgfy8dcufnvxaru6wfakmuafdlt3c6gmh4njugn794kg \
# --testnet-magic 2)
# # build=($CARDANO_CLI transaction build \
# # --babbage-era \
# # --cardano-mode \
# # --testnet-magic $TESTNET_MAGIC \
# # --tx-in ${FROM_UTXO} \
# # --tx-out ${TX_OUT} \
# # --tx-out-datum-hash-file $WORK/plutus-scripts/${DATUM_HASH_FILE} \
# # --change-address=${FROM_WALLET_ADDRESS} \
# # --protocol-params-file $WORK/transactions/pparams.json \
# # --out-file $WORK/transactions/tx.draft)

# echo "${build[@]}"
# "${build[@]}"

# TO_WALLET=forPlutus
# LOVELACE_TO_SEND=2000000
# TOKEN_QUANTITY=1
# POLICY_ID=d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be
# TOKEN_NAME="CrowdFunding"
# TOKEN_NAME_HEX=$(echo -n "$TOKEN_NAME" | xxd -p)
# echo $TOKEN_NAME_HEX
# #TOKEN_NAME_HEX=4d7943726f776446756e64
# POL="$TOKEN_QUANTITY ${POLICY_ID}.${TOKEN_NAME_HEX}"
# txOut=(
# --tx-out ${TO_WALLET_ADDRESS}+${LOVELACE_TO_SEND}+'"'${POL}'"' \
# )
# echo "${txOut[@]}"

# LOVELACE_TO_SEND=2000000
# PAYMENT=$LOVELACE_TO_SEND
# TOKEN_QUANTITY=1
# POLICY_ID=d1c14384a6e806c521bff39b0c98518576a29727ac2b5f029cf5b9be.43726f776446756e64696e67
# Q1='"'1
# PAYMENT=${PAYMENT}+$Q1" "${POLICY_ID}'"'
# #PAYMENT=
# echo $Q1
# echo ${PAYMENT}
# #echo $PAYMENT
# TO_WALLET_ADDRESS=addr_test1vrdm4dru7cgfy8dcufnvxaru6wfakmuafdlt3c6gmh4njugn794kg
# final=${TO_WALLET_ADDRESS}+${PAYMENT}
# echo $final


# Q:
# for i in 01 02 03 04; do ssh web.${i}.domain.com 'echo "<img src=beacon.gif?cluster=${i}>" >> /var/www/index.html'; done

# Ans:
# for i in 01 02 03 04; do
#     ssh web.${i}.domain.com 'echo "<img src=beacon.gif?cluster='${i}'>" >> /var/www/index.html'
# done

# USER_UID="$1"
# echo "generate_token(\"$USER_UID\")"
# echo "generate_token('"'$USER_UID'"')"]
# Q1=("ex \"1 asdfaf")
# echo "$Q1"
# Q2=$Q1"afdfd"
# echo $Q2

# TX_ROW="12fd13deadc8248eac08dc9ea782fff58350abf37a06abad36bbb28cdbff0989     0        2000000 lovelace + 10 b7047182a00354f8c4cd7b01c2faab230e01d2f33a6dcfd0c781f7ec.4d7943726f776446756e64 + TxOutDatumNone"
# SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
# SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
# SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
# SELECTED_UTXO_POLICYID=$(echo $TX_ROW | awk '{ print $7 }' | awk -F '.' '{print $1}')
# SELECTED_UTXO_TOKEN_NAME_HEX=$(echo $TX_ROW | awk '{ print $7 }' | awk -F '.' '{print $2}')
# TOKENS_TO_SEND_BACK=$(expr $SELECTED_UTXO_TOKENS - 1)
# echo $SELECTED_UTXO
# echo $SELECTED_UTXO_LOVELACE
# echo $SELECTED_UTXO_TOKENS
# echo $SELECTED_UTXO_POLICYID
# echo $SELECTED_UTXO_TOKEN_NAME_HEX
# echo $TOKENS_TO_SEND_BACK

# source helpers.sh
# TO_WALLET_NAME=Contributor 
# TMPHASH=$(cat $BASE/.priv/Wallets/$TO_WALLET_NAME/$TO_WALLET_NAME.pubKeyHash)
# echo " temp hash = $TMPHASH"

# SELECTED_UTXO_TOKENS=9
# TOKENS_TO_SEND_BACK=`expr $SELECTED_UTXO_TOKENS - 1`
# if [ $TOKENS_TO_SEND_BACK -gt 0 ]
# then
#   echo $TOKENS_TO_SEND_BACK
# else 
#   echo "Zero tokens"
# fi


BALANCE_FILE=/home/chakravarti/emurgoCardano/tx/walletBalances.txt
cat $BALANCE_FILE
read -p 'TX row number starting in 1: ' TMP
TX_ROW_NUM="$(($TMP+2))"
echo "Row num ${TX_ROW_NUM}"

TX_ROW=$(sed "${TX_ROW_NUM}q;d" $BALANCE_FILE)
echo "Row num ${TX_ROW}"

SELECTED_UTXO="$(echo $TX_ROW | awk '{ print $1 }')#$(echo $TX_ROW | awk '{ print $2 }')"
SELECTED_UTXO_LOVELACE=$(echo $TX_ROW | awk '{ print $3 }')
SELECTED_UTXO_TOKENS=$(echo $TX_ROW | awk '{ print $6 }')
#SELECTED_UTXO_POLICYID=$(echo $TX_ROW | awk '{ print $7 }')
SELECTED_UTXO_POLICYID=$(echo $TX_ROW | awk '{ print $7 }' | awk -F '.' '{print $1}')
SELECTED_UTXO_TOKEN_NAME_HEX=$(echo $TX_ROW | awk '{ print $7 }' | awk -F '.' '{print $2}')

echo "SELECTED_UTXO=  ${SELECTED_UTXO}"
echo "SELECTED_UTXO_LOVELACE=  ${SELECTED_UTXO_LOVELACE}"
echo "SELECTED_UTXO_TOKENS=  ${SELECTED_UTXO_TOKENS}"
echo "SELECTED_UTXO_POLICYID=  ${SELECTED_UTXO_POLICYID}"
echo "SELECTED_UTXO_TOKEN_NAME_HEX=  ${SELECTED_UTXO_TOKEN_NAME_HEX}"


#TOKENS_TO_SEND_BACK=`expr $SELECTED_UTXO_TOKENS - 1`
#TOKENS_TO_SEND_BACK=$SELECTED_UTXO_TOKENS
if [ $TOKENS_TO_SEND_BACK -gt 1 ]
then
  echo "tokens to send back= ${TOKENS_TO_SEND_BACK}"
else 
  echo "Zero tokens"
fi
