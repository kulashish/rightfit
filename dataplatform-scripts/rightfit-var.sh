#!/bin/bash
export USER="akulkarni"
export PROJECT_PATH="/home/akulkarni/right-fit"
export DS_SERVER="172.16.84.194"
export TARGET_PATH="${USER}@${DS_SERVER}:./right-fit/model_input/"
export BOB_DATE=$(date -d "-1 month" +'%Y/%m/%d')
export CRM_DATE=$(date -d "-1 day" +'%Y/%m/%d')
export CLUSTER_PATH_PREFIX="hdfs://dataplatform-master.jabong.com:8020"
export CLUSTER_BOB_PATH_PREFIX="${CLUSTER_PATH_PREFIX}/data/input/bob/"
export CLUSTER_BOB_PATH_SUFFIX="/full/${BOB_DATE}/*/*/"
export CLUSTER_CRM_PATH_PREFIX="/data/input/crm/"
export CLUSTER_CRM_PATH_SUFFIX="/full/${CRM_DATE}/*/"
export LOCAL_PATH_PREFIX="${CLUSTER_PATH_PREFIX}/user/akulkarni/rightfit/data/"

export CATALOG_SIMPLE_PATH="${CLUSTER_BOB_PATH_PREFIX}catalog_simple${CLUSTER_BOB_PATH_SUFFIX}"
export CATALOG_CONFIG_PATH="${CLUSTER_BOB_PATH_PREFIX}catalog_config${CLUSTER_BOB_PATH_SUFFIX}"
export CATALOG_ATTRIB_SET_PATH="${CLUSTER_BOB_PATH_PREFIX}catalog_attribute_set${CLUSTER_BOB_PATH_SUFFIX}"
export CATALOG_SIMPLE_DATA_JSON_PATH="${LOCAL_PATH_PREFIX}catalog_simple_json"

export CATALOG_SIMPLE_SHOES_PATH="${CLUSTER_BOB_PATH_PREFIX}catalog_simple_shoes${CLUSTER_BOB_PATH_SUFFIX}"
export CATALOG_ATTRIBUTE_OPTION_SHOES_SH_SIZE_PATH="${CLUSTER_BOB_PATH_PREFIX}catalog_attribute_option_shoes_sh_size${CLUSTER_BOB_PATH_SUFFIX}"
export CATALOG_SIMPLE_SHOES_DATA_JSON_PATH="${LOCAL_PATH_PREFIX}catalog_simple_shoes_json"

export CATALOG_CONFIG_ADDINFO_PATH="${CLUSTER_BOB_PATH_PREFIX}catalog_config_additional_info${CLUSTER_BOB_PATH_SUFFIX}"
export CATALOG_DISTINCT_SIZECHART_PATH="${CLUSTER_BOB_PATH_PREFIX}catalog_distinct_sizechart${CLUSTER_BOB_PATH_SUFFIX}"
export CATALOG_SIZECHART_PATH="${CLUSTER_BOB_PATH_PREFIX}catalog_sizechart${CLUSTER_BOB_PATH_SUFFIX}"
export CATEGORY_SEGMENT_PATH="${CLUSTER_BOB_PATH_PREFIX}catalog_category_has_catalog_segment${CLUSTER_BOB_PATH_SUFFIX}"
export CATALOG_SHOES_SIZECHART_DATA_JSON_PATH="${LOCAL_PATH_PREFIX}catalog_shoes_sizechart_json"

export SALES_ORDER_PATH="${CLUSTER_BOB_PATH_PREFIX}sales_order${CLUSTER_BOB_PATH_SUFFIX}"
export SALES_ORDER_ITEM_PATH="${CLUSTER_BOB_PATH_PREFIX}sales_order_item${CLUSTER_BOB_PATH_SUFFIX}"
export SALES_ORDER_ITEM_STATUS_PATH="${CLUSTER_BOB_PATH_PREFIX}sales_order_item_status${CLUSTER_BOB_PATH_SUFFIX}"
export SALES_ORDER_DATA_JSON_PATH="${LOCAL_PATH_PREFIX}sales_order_json"
export SALES_SHOES_DATA_JSON_PATH="${LOCAL_PATH_PREFIX}sales_shoes_json"

export TICKET_DETAILS_PATH="${CLUSTER_CRM_PATH_PREFIX}tblTicketDetails${CLUSTER_CRM_PATH_SUFFIX}"
export ORDER_DETAILS_PATH="${CLUSTER_CRM_PATH_PREFIX}tblOrderDetails${CLUSTER_CRM_PATH_SUFFIX}"
export PRODUCT_DETAILS_PATH="${CLUSTER_CRM_PATH_PREFIX}tblProductDetails${CLUSTER_CRM_PATH_SUFFIX}"
export TICKET_PRODUCTS_PATH="${CLUSTER_CRM_PATH_PREFIX}tblTicketProducts${CLUSTER_CRM_PATH_SUFFIX}"
export RETURNS_SHOES_DATA_JSON_PATH="${LOCAL_PATH_PREFIX}returns_shoes_json"
