source('/home/akulkarni/right-fit/spark_init.R')
options(echo=TRUE)
args 	<- commandArgs(trailingOnly = TRUE)
catalog_simple_path		<- args[1]
#'/data/input/bob1/catalog_simple/full/2015/12/20/*'
catalog_config_path		<- args[2]
#'/data/input/bob1/catalog_config/full/2015/12/20/*'
catalog_attribute_set_path	<- args[3]
#'/data/input/bob1/catalog_attribute_set/full/2015/12/20/*'
catalog_simple_data_json_path	<- args[4]

paste('Reading catalog simple data from', catalog_simple_path)
catalog.simple <- parquetFile(sqlContext, catalog_simple_path)
catalog.simple <- select(catalog.simple, "id_catalog_simple", "fk_catalog_config", "sku")
paste('Reading catalog config data from', catalog_config_path)
catalog.config <- parquetFile(sqlContext, catalog_config_path)
catalog.config <- select(catalog.config, "id_catalog_config", "name", "fk_catalog_brand", "fk_catalog_attribute_set")
catalog.config <- withColumnRenamed(catalog.config, 'name', 'catalog_config_name')
catalog.simple.data <- join(catalog.simple, catalog.config, catalog.simple$fk_catalog_config==catalog.config$id_catalog_config)
paste('Reading catalog attribute set data from', catalog_attribute_set_path)
catalog.attribute.set <- parquetFile(sqlContext, catalog_attribute_set_path)
catalog.attribute.set <- select(catalog.attribute.set, "id_catalog_attribute_set", "name")
catalog.attribute.set <- withColumnRenamed(catalog.attribute.set, 'name', 'attribute_set_name')
catalog.simple.data <- join(catalog.simple.data, catalog.attribute.set, catalog.simple.data$fk_catalog_attribute_set==catalog.attribute.set$id_catalog_attribute_set)
catalog.simple.data <- select(catalog.simple.data, "id_catalog_simple", "sku", "id_catalog_config", "catalog_config_name", "fk_catalog_brand", "id_catalog_attribute_set", "attribute_set_name")
paste('Writing processed catalog simple data to', catalog_simple_data_json_path)
write.df(catalog.simple.data, catalog_simple_data_json_path, source='json', mode='overwrite')
