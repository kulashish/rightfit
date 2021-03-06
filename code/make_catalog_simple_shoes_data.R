source('spark_init.R')
catalog.simple.shoes <- parquetFile(sqlContext, '/data/input/bob1/catalog_simple_shoes/full/2015/12/20/*')
catalog.attribute_option.shoes <- parquetFile(sqlContext, '/data/input/bob1/catalog_attribute_option_shoes_sh_size/full/2015/12/20/*')
catalog.simple.shoes <- join(catalog.simple.shoes, catalog.attribute_option.shoes, catalog.simple.shoes$fk_catalog_attribute_option_shoes_sh_size==catalog.attribute_option.shoes$id_catalog_attribute_option_shoes_sh_size)
catalog.simple.shoes <- select(catalog.simple.shoes, "id_catalog_simple_shoes", "fk_catalog_simple", "id_catalog_attribute_option_shoes_sh_size", "name")
catalog.simple.shoes <- withColumnRenamed(catalog.simple.shoes, 'name', 'catalog_simple_size')

catalog.simple <- jsonFile(sqlContext, '/data/input/bob1/catalog_simple_data')
catalog.simple <- withColumnRenamed(catalog.simple, 'sku', 'sku_simple')
catalog.simple.shoes.data <- join(catalog.simple, catalog.simple.shoes, catalog.simple$id_catalog_simple==catalog.simple.shoes$fk_catalog_simple)
catalog.simple.shoes.data <- select(catalog.simple.shoes.data, 'id_catalog_simple_shoes', 'id_catalog_attribute_option_shoes_sh_size', 'catalog_simple_size', 'attribute_set_name', 'catalog_config_name', 'fk_catalog_brand', 'id_catalog_attribute_set', 'id_catalog_config', 'id_catalog_simple', 'sku_simple')
write.df(catalog.simple.shoes.data, '/data/input/bob1/catalog_simple_shoes_data', source='json', mode='overwrite')
