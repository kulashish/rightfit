#!/bin/bash
. /home/akulkarni/right-fit/rightfit-var.sh

echo "Creating catalog simple data"
Rscript "$PROJECT_PATH/make_catalog_simple_data.R" $CATALOG_SIMPLE_PATH $CATALOG_CONFIG_PATH $CATALOG_ATTRIB_SET_PATH $CATALOG_SIMPLE_DATA_JSON_PATH
echo "Catalog simple data done"

echo "Creating catalog simple shoes data"
Rscript "$PROJECT_PATH/make_catalog_simple_shoes_data.R" $CATALOG_SIMPLE_SHOES_PATH $CATALOG_ATTRIBUTE_OPTION_SHOES_SH_SIZE_PATH $CATALOG_SIMPLE_DATA_JSON_PATH $CATALOG_SIMPLE_SHOES_DATA_JSON_PATH
echo "Catalog simple shoes data done"

echo "Creating catalog sizechart data"
Rscript "$PROJECT_PATH/make_catalog_sizechart_data.R" $CATALOG_CONFIG_PATH $CATALOG_CONFIG_ADDINFO_PATH $CATALOG_DISTINCT_SIZECHART_PATH $CATALOG_SIZECHART_PATH $CATEGORY_SEGMENT_PATH $CATALOG_SIMPLE_SHOES_DATA_JSON_PATH $CATALOG_SHOES_SIZECHART_DATA_JSON_PATH
echo "Catalog sizechart data done"

CATALOG_SHOES_SIZECHART_MERGED_JSON_PATH="$PROJECT_PATH/catalog_shoes_sizechart_data.json"
echo "Merging the sizechart data"
hadoop fs -getmerge $CATALOG_SHOES_SIZECHART_DATA_JSON_PATH $CATALOG_SHOES_SIZECHART_MERGED_JSON_PATH
echo "Merging complete"

gzip -f $CATALOG_SHOES_SIZECHART_MERGED_JSON_PATH
echo "Created file archive; copying it to remote"
scp "${CATALOG_SHOES_SIZECHART_MERGED_JSON_PATH}.gz" ${TARGET_PATH}
echo "${CATALOG_SHOES_SIZECHART_MERGED_JSON_PATH}.gz copied to remote."


