source('/home/akulkarni/right-fit/spark_init.R')
catalog_simple_shoes_path			<- args[1]
catalog_attribute_option_shoes_sh_size_path	<- args[2]
catalog_simple_json_path			<- args[3]
catalog_simple_shoes_data_json_path		<- args[4]
catalog.simple.shoes <- parquetFile(sqlContext, catalog_simple_shoes_path)
catalog.attribute_option.shoes <- parquetFile(sqlContext, catalog_attribute_option_shoes_sh_size_path)
catalog.simple.shoes <- join(catalog.simple.shoes, catalog.attribute_option.shoes, catalog.simple.shoes$fk_catalog_attribute_option_shoes_sh_size==catalog.attribute_option.shoes$id_catalog_attribute_option_shoes_sh_size)
catalog.simple.shoes <- select(catalog.simple.shoes, "id_catalog_simple_shoes", "fk_catalog_simple", "id_catalog_attribute_option_shoes_sh_size", "name")
catalog.simple.shoes <- withColumnRenamed(catalog.simple.shoes, 'name', 'catalog_simple_size')

catalog.simple <- jsonFile(sqlContext, catalog_simple_json_path)
catalog.simple <- withColumnRenamed(catalog.simple, 'sku', 'sku_simple')
catalog.simple.shoes.data <- join(catalog.simple, catalog.simple.shoes, catalog.simple$id_catalog_simple==catalog.simple.shoes$fk_catalog_simple)
catalog.simple.shoes.data <- select(catalog.simple.shoes.data, 'id_catalog_simple_shoes', 'id_catalog_attribute_option_shoes_sh_size', 'catalog_simple_size', 'attribute_set_name', 'catalog_config_name', 'fk_catalog_brand', 'id_catalog_attribute_set', 'id_catalog_config', 'id_catalog_simple', 'sku_simple')
write.df(catalog.simple.shoes.data, catalog_simple_shoes_data_json_path, source='json', mode='overwrite')
