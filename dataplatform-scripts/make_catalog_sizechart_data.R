source('/home/akulkarni/right-fit/spark_init.R')
catalog_config_path			<- args[1]
catalog_config_addinfo_path		<- args[2]
catalog_distinct_sizechart_path		<- args[3]
catalog_sizechart_path			<- args[4]
category_segment_path			<- args[5]
catalog_simple_shoes_data_json_path	<- args[6]
catalog_shoes_sizechart_data_json_path	<- args[7]

catalog.config <- parquetFile(sqlContext, catalog_config_path)
catalog.config <- select(catalog.config, "id_catalog_config", "sku", "status", "name", "fk_catalog_brand", "fk_catalog_config_group", "fk_catalog_attribute_set", "fk_catalog_attribute_option_global_brick", "fk_catalog_attribute_option_global_business_unit", "fk_catalog_attribute_option_global_class", "fk_catalog_attribute_option_global_family", "fk_catalog_attribute_option_global_segment")
catalog.config.add <- parquetFile(sqlContext, catalog_config_addinfo_path)
catalog.config.add <- select(catalog.config.add, "fk_catalog_config", "fk_catalog_distinct_sizechart", "id_catalog_config_additional_info")
catalog.config.data <- join(catalog.config, catalog.config.add, catalog.config$id_catalog_config==catalog.config.add$fk_catalog_config)
catalog.sizechart.distinct <- parquetFile(sqlContext, catalog_distinct_sizechart_path)
catalog.sizechart.distinct <- select(catalog.sizechart.distinct, "id_catalog_distinct_sizechart", "fk_catalog_category", "sizechart_name", "sizechart_type")
catalog.config.data <- join(catalog.config.data, catalog.sizechart.distinct, catalog.sizechart.distinct$id_catalog_distinct_sizechart==catalog.config.data$fk_catalog_distinct_sizechart)
catalog.config.data <- select(catalog.config.data, "id_catalog_config", "sku", "status", "name", "fk_catalog_brand", "fk_catalog_config_group", "fk_catalog_attribute_set", "fk_catalog_attribute_option_global_brick", "fk_catalog_attribute_option_global_business_unit", "fk_catalog_attribute_option_global_class", "fk_catalog_attribute_option_global_family", "fk_catalog_attribute_option_global_segment", "id_catalog_config_additional_info", "id_catalog_distinct_sizechart", "fk_catalog_category", "sizechart_name", "sizechart_type")
catalog.sizechart <- parquetFile(sqlContext, catalog_sizechart_path) 
catalog.sizechart <- select(catalog.sizechart, "id_catalog_sizechart", "fk_catalog_distinct_sizechart", "brand", "column_header", "row_header_name", "row_header_type", "value")
catalog.sizechart <- join(catalog.config.data, catalog.sizechart, catalog.config.data$id_catalog_distinct_sizechart==catalog.sizechart$fk_catalog_distinct_sizechart)
catalog.sizechart <- select(catalog.sizechart, 'brand', 'column_header', 'fk_catalog_brand', 'fk_catalog_category', 'id_catalog_config', 'row_header_type', 'value')

category.segment <- parquetFile(sqlContext, category_segment_path)
category.segment <- select(category.segment, 'fk_catalog_category', 'fk_catalog_segment')
category.segment <- withColumnRenamed(category.segment, 'fk_catalog_category', 'catalog_category')
catalog.sizechart <- join(catalog.sizechart, category.segment, catalog.sizechart$fk_catalog_category==category.segment$catalog_category)

catalog.simple.shoes <- jsonFile(sqlContext, catalog_simple_shoes_data_json_path)
catalog.simple.shoes <- select(catalog.simple.shoes, 'catalog_simple_size', 'id_catalog_config', 'id_catalog_simple', 'id_catalog_simple_shoes', 'sku_simple')
registerTempTable(catalog.sizechart, "catalog_sizechart")
registerTempTable(catalog.simple.shoes, "catalog_simple_shoes")
catalog.shoes.sizechart <- sql(sqlContext, "select shoes.id_catalog_config, shoes.id_catalog_simple, shoes.id_catalog_simple_shoes, shoes.sku_simple, shoes.catalog_simple_size, size.brand, size.column_header, size.fk_catalog_brand, size.fk_catalog_category, size.row_header_type, size.value, size.fk_catalog_segment from catalog_simple_shoes shoes, catalog_sizechart size where shoes.id_catalog_config=size.id_catalog_config and shoes.catalog_simple_size=size.row_header_type")

write.df(catalog.shoes.sizechart, catalog_shoes_sizechart_data_json_path, source='json', mode='overwrite')
