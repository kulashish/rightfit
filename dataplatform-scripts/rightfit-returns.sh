#!/bin/bash

. /home/akulkarni/right-fit/rightfit-var.sh
echo "Creating returns shoes data"
echo "Ticket details $TICKET_DETAILS_PATH"
echo "Order details $ORDER_DETAILS_PATH"
echo "Product details $PRODUCT_DETAILS_PATH"
Rscript "$PROJECT_PATH/make_returns_shoes_data.R" $TICKET_DETAILS_PATH $ORDER_DETAILS_PATH $PRODUCT_DETAILS_PATH $TICKET_PRODUCTS_PATH $CATALOG_SIMPLE_SHOES_DATA_JSON_PATH $RETURNS_SHOES_DATA_JSON_PATH
echo "Returns shoes data done."

RETURNS_SHOES_MERGED_JSON_PATH="$PROJECT_PATH/returns_shoes_data.json"
echo "Merging the returns shoes data json"
hadoop fs -getmerge $RETURNS_SHOES_DATA_JSON_PATH $RETURNS_SHOES_MERGED_JSON_PATH
echo "Returns shoes data json merged. Creating file archive."
gzip -f $RETURNS_SHOES_MERGED_JSON_PATH
scp "${RETURNS_SHOES_MERGED_JSON_PATH}.gz" $TARGET_PATH
echo "${RETURNS_SHOES_MERGED_JSON_PATH}.gz copied to remote."
