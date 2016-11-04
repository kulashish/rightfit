#!/bin/bash

. /home/akulkarni/right-fit/rightfit-var.sh

#Rscript make_sales_data.R $SALES_ORDER_PATH $SALES_ORDER_ITEM_PATH $SALES_ORDER_ITEM_STATUS_PATH $SALES_ORDER_DATA_JSON_PATH
echo "Creating shoes sales data"
Rscript "$PROJECT_PATH/make_sales_shoes_data.R" $SALES_ORDER_PATH $SALES_ORDER_ITEM_PATH $SALES_ORDER_ITEM_STATUS_PATH $CATALOG_SIMPLE_SHOES_DATA_JSON_PATH $SALES_SHOES_DATA_JSON_PATH
echo "Shoes sales data done."

SALES_SHOES_MERGED_JSON_PATH="$PROJECT_PATH/sales_shoes_data.json"
hadoop fs -getmerge $SALES_SHOES_DATA_JSON_PATH $SALES_SHOES_MERGED_JSON_PATH
echo "Merged the shoes sales data json. Creating the file archive"
gzip -f $SALES_SHOES_MERGED_JSON_PATH
scp "${SALES_SHOES_MERGED_JSON_PATH}.gz" $TARGET_PATH
echo "${SALES_SHOES_MERGED_JSON_PATH}.gz copied to remote."
