source('/home/akulkarni/right-fit/spark_init.R')

ticket_details_path			<- args[1]
order_details_path			<- args[2]
product_details_path			<- args[3]
ticket_products_path			<- args[4]
catalog_simple_shoes_data_json_path 	<- args[5]
returns_shoes_data_json_path		<- args[6]

hc	<- sparkRHive.init(sc)

ticket.details <- loadDF(hc, ticket_details_path, source = 'orc')
ticket.details <- select(ticket.details, 'ticketid', 'issueid', 'orderno', 'customerno', 'issuecreationdate', 'ticketdetailid', 'neworderno', 'bobrefno')
ticket.returns <- subset(ticket.details, ticket.details$issueid %in% c(1, 56))

order.details <- loadDF(hc, order_details_path, source = 'orc')
order.details <- select(order.details, 'orderid', 'orderno')

product.details <- loadDF(hc, product_details_path, source = 'orc')
product.details <- select(product.details, 'productid', 'orderno', 'sku', 'updatedate')

ticket.products <- loadDF(hc, ticket_products_path, source = 'orc')
ticket.products <- select(ticket.products, 'ticketproductsid', 'ticketid', 'productid', 'returnreason', 'adddate', 'secondryreturnreason')

order.details	<- withColumnRenamed(order.details, 'orderno', 'o_orderno')
ticket.products <- withColumnRenamed(ticket.products, 'ticketid', 'tp_ticketid')
ticket.products <- withColumnRenamed(ticket.products, 'productid', 'tp_productid')
product.details <- withColumnRenamed(product.details, 'orderno', 'p_orderno')

returns.data <- join(ticket.returns, order.details, ticket.returns$orderno == order.details$o_orderno)
returns.data <- join(returns.data, ticket.products, returns.data$ticketid == ticket.products$tp_ticketid)
returns.data <- join(returns.data, product.details, returns.data$tp_productid == product.details$productid)
returns.data <- select(returns.data, 'ticketid', 'productid', 'customerno', 'orderid', 'issuecreationdate', 'returnreason', 'secondryreturnreason', 'sku', 'updatedate')

shoes.simples		<- jsonFile(sqlContext, catalog_simple_shoes_data_json_path)
shoes.simples		<- select(shoes.simples, 'id_catalog_config', 'id_catalog_simple', 'sku_simple')
returns.shoes.data	<- join(returns.data, shoes.simples, returns.data$sku == shoes.simples$sku_simple)

write.df(returns.shoes.data, returns_shoes_data_json_path, source = 'json', mode = 'overwrite')
