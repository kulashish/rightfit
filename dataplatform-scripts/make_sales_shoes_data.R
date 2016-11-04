source('/home/akulkarni/right-fit/spark_init.R')

sales_order_path			<- args[1]
sales_order_item_path			<- args[2]
sales_order_item_status_path		<- args[3]
catalog_simple_shoes_data_json_path     <- args[4]
sales_shoes_data_json_path              <- args[5]

sales <- parquetFile(sqlContext, sales_order_path)
sales <- select(sales, "id_sales_order", "fk_sales_order_address_billing", "fk_sales_order_address_shipping", "fk_customer", "cod_charge", "id_sales_order_gift_wrap", "gw_amount")

paste('Reading sales order item data from', sales_order_item_path)
sales.item <- parquetFile(sqlContext, sales_order_item_path)
sales.item <- select(sales.item, "id_sales_order_item", "fk_sales_order", "fk_sales_order_item_status", "sku", "created_at", "is_cod", "is_gift_wrapped")

paste('Reading sales order item status data from', sales_order_item_status_path)
sales.item.status <- parquetFile(sqlContext, sales_order_item_status_path) 
sales.item.status <- select(sales.item.status, "id_sales_order_item_status", "name")

sales.data <- join(sales, sales.item, sales$id_sales_order==sales.item$fk_sales_order)
sales.data <- join(sales.data, sales.item.status, sales.item.status$id_sales_order_item_status==sales.data$fk_sales_order_item_status)
sales.data <- select(sales.data, "id_sales_order", "fk_sales_order_address_billing", "fk_sales_order_address_shipping", "fk_customer", "cod_charge", "id_sales_order_gift_wrap", "gw_amount", "id_sales_order_item", "sku", "created_at", "is_cod", "is_gift_wrapped", "id_sales_order_item_status", "name")

catalog.simple.shoes    <- jsonFile(sqlContext, catalog_simple_shoes_data_json_path)
sales.shoes.data        <- join(sales.data, catalog.simple.shoes, sales.data$sku==catalog.simple.shoes$sku_simple)
sales.shoes.success     <- subset(sales.shoes.data, sales.shoes.data$id_sales_order_item_status %in% c(3,4,5,6,7,11,17,24,33,34))
write.df(sales.shoes.success, sales_shoes_data_json_path, source='json', mode='overwrite')

