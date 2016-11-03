library(lubridate)
#-------------Begin Read Returns Data------------------------------------------
shoes.returns <- read.json('/home/akulkarni/right-fit/returns_shoes_data.json')
shoes.returns$issuecreationdate <- ymd_hms(shoes.returns$issuecreationdate)
shoes.returns$updatedate        <- ymd_hms(shoes.returns$updatedate)
nrow(shoes.returns)
#804,305
#Check that customerno and id_sales_order are not NA
shoes.returns.clean <- shoes.returns[complete.cases(shoes.returns[,2:3]),]

#---------------begin ignore-----------------
missing_ids <- setdiff(sales.shoes.returns$id_sales_order, shoes.returns$id_sales_order)
length(missing_ids)
#28,808 (These IDs were observed to be missing in one or more of the returns tables)
head(missing_ids)

returns.missing_ids <- setdiff(shoes.returns.clean$id_sales_order, sales.shoes.returns$id_sales_order)
length(returns.missing_ids)
#169,052 (These are the IDs observed in the returns tables but marked as a successful sale in the sales tables)
#---------------end ignore-----------------

shoes.returns.sizeissue <- shoes.returns.clean[grep('size', shoes.returns.clean$returnreason, ignore.case = T),]
nrow(shoes.returns.sizeissue)
#531,677
#-------------End Read Returns Data------------------------------------------
#-------------Begin Read Sales Data---------------------------------------------------------
shoes.sales            <- read.json('/home/akulkarni/right-fit/sales_shoes_custdata.json')
nrow(shoes.sales)
#8,269,191
setnames(shoes.sales, "fk_customer", "CustomerNo")
shoes.sales$created_at <- ymd_hms(shoes.sales$created_at)
return.itemids         <- intersect(shoes.sales$id_sales_order_item, shoes.returns.sizeissue$productid)
length(return.itemids)
#464,059
shoes.sales.good       <- shoes.sales[shoes.sales$id_sales_order_item_status %in% c(3,4,5,6,7,11,17,24,33,34),]
nrow(shoes.sales.good)
#4,782,599
length(intersect(shoes.sales.good$id_sales_order_item, shoes.returns.sizeissue$productid))
#88,531 = size issue related returns incorrectly marked as successful sales. Remove them from sales data.
shoes.sales.invalid <- shoes.sales.good[shoes.sales.good$id_sales_order_item %in% intersect(shoes.sales.good$id_sales_order_item, shoes.returns.sizeissue$productid), c('id_sales_order', 'id_sales_order_item', 'id_sales_order_item_status', 'created_at')]
shoes.sales.good <- shoes.sales.good[!(shoes.sales.good$id_sales_order_item %in% intersect(shoes.sales.good$id_sales_order_item, shoes.returns.sizeissue$productid)),]
#shoes.returns.sizeissue - shoes returns (due to size issue) data 
#shoes.sales.good - successful sales data


generate.training <- function(max_date) {
  shoes.sales.good[shoes.sales.good$created_at < max_date,]
}
generate.training.grouped <- function(training_data) {
  cust.shoes.data.merged <- merge(training_data, catalog.shoes.sizechart.merged, by = 'id_catalog_simple')
  cust.shoes.data.merged <- subset(cust.shoes.data.merged, select = -c(sku_simple.y, id_catalog_category_has_catalog_segment))
  setnames(cust.shoes.data.merged, "sku_simple.x", "sku_simple")
  cust.shoes.data.labeled <- merge(cust.shoes.data.merged, catalog.shoes.sizechart.combined[,c('id_catalog_simple', 'sku', 'cluster_index')])
  cust.shoes.data.labeled %>% group_by(CustomerNo, fk_catalog_segment, cluster_index) %>% summarize(cluster_count = n())
}
generate.test.pos <- function(min_date, max_date, customers_list) {
  test.pos     <- shoes.sales.good[shoes.sales.good$created_at >= min_date & shoes.sales.good$created_at < max_date,]
  test.pos     <- merge(test.pos, catalog.shoes.sizechart.merged[, c(2,6,7,9)], by = 'id_catalog_simple')
  test.pos     <- test.pos[test.pos$CustomerNo %in% customers_list,]
}

generate.test.neg <- function(min_date, max_date, customers_list) {
  test.neg      <- shoes.sales[shoes.sales$id_sales_order_item %in% return.itemids & shoes.sales$created_at >= min_date & shoes.sales$created_at < max_date,]
  test.neg      <- subset(test.neg, select = -id_catalog_simple)
  test.neg      <- merge(test.neg, catalog.shoes.sizechart.merged, by = 'sku_simple')
  test.neg.eval <- test.neg[test.neg$CustomerNo %in% customers_list,]
}

evalrun.sequence <- function(min_date, max_date) {
  training.pos <- generate.training(min_date)
  training.pos.by_customer <- generate.training.grouped(training.pos)
  training.pos.by_customer <- training.pos.by_customer %>% mutate(cluster_total = sum(cluster_count), profiles = n())
  training.pos.customers   <- unique(training.pos.by_customer$CustomerNo)
  score.customer_cluster_support.matrix_list <<- support_score.matrices(training.pos.by_customer)
  test.pos     <- generate.test.pos(min_date, max_date, training.pos.customers)
  test.neg     <- generate.test.neg(min_date, max_date, training.pos.customers)
  system.time(combined_scoring.evalrun.neg <- evalrun.clustermodel.combined_scoring(test.neg))
  combined_scoring.evalrun.neg <- merge(combined_scoring.evalrun.neg, unique(training.pos.by_customer[, c('CustomerNo', 'fk_catalog_segment', 'cluster_total', 'profiles')]), by = c('CustomerNo', 'fk_catalog_segment'))
  conf.tn <- length(which(combined_scoring.evalrun.neg$prediction == FALSE))
  conf.fp <- length(which(combined_scoring.evalrun.neg$prediction == TRUE))
  system.time(combined_scoring.evalrun.pos <- evalrun.clustermodel.combined_scoring(test.pos))
  combined_scoring.evalrun.pos <- merge(combined_scoring.evalrun.pos, unique(training.pos.by_customer[, c('CustomerNo', 'fk_catalog_segment', 'cluster_total', 'profiles')]), by = c('CustomerNo', 'fk_catalog_segment'))
  conf.tp <- length(which(combined_scoring.evalrun.pos$prediction == TRUE))
  conf.fn <- length(which(combined_scoring.evalrun.pos$prediction == FALSE))
  c(conf.tn, conf.fp, conf.tp, conf.fn)
}
conf.run1 <- evalrun.sequence('2015-01-01', '2015-03-01')
conf.run2 <- evalrun.sequence('2015-03-01', '2015-05-01')
conf.run3 <- evalrun.sequence('2015-05-01', '2015-07-01')

#Single purchase history
length(which(combined_scoring.evalrun.neg$prediction == FALSE & combined_scoring.evalrun.neg$cluster_total==1))
