source('spark_init.R')
sales <- parquetFile(sqlContext, '/data/input/bob1/sales_order/full/2015/12/20/*')
sales <- select(sales, "id_sales_order", "fk_customer")

sales.item <- parquetFile(sqlContext, '/data/input/bob1/sales_order_item/full/2015/12/20/*')
sales.item <- select(sales.item, "id_sales_order_item", "fk_sales_order", "fk_sales_order_item_status", "sku", "created_at")

sales.item.status <- parquetFile(sqlContext, '/data/input/bob1/sales_order_item_status/full/2015/12/20/*')
sales.item.status <- select(sales.item.status, "id_sales_order_item_status", "name")

sales.data <- join(sales, sales.item, sales$id_sales_order==sales.item$fk_sales_order)
sales.data <- join(sales.data, sales.item.status, sales.item.status$id_sales_order_item_status==sales.data$fk_sales_order_item_status)
sales.data <- select(sales.data, "id_sales_order", "fk_customer", "id_sales_order_item", "sku", "created_at", "id_sales_order_item_status", "name")
write.df(sales.data, '/data/input/bob1/sales_data', source='json', mode='overwrite')
