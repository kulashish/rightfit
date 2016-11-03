source("rightfit_header.R")
library(flexclust)

#CATALOG SHOES SIZECHART DATA
file.sizechart  <- "/home/akulkarni/right-fit/catalog_shoes_sizechart_1.json"
catalog.shoes.sizechart <- read.json(file.sizechart)

length(unique(catalog.shoes.sizechart$id_catalog_simple))
#No. of total shoes simples : 627,418
catalog.shoes.sizechart.cm <- catalog.shoes.sizechart[catalog.shoes.sizechart$column_header %in% c("In CM", "In Cm", "Foot Size (In Cm)", "IN CM", "In cm", "Foot Size (CM)", "In Mc", "IN Cm", "In  Cm", "Foot Size (in CM)", "IN cm", "Foot Size in CM"), ]
length(which(catalog.shoes.sizechart.cm$value==""))
#68 blanks
catalog.shoes.sizechart.cm <- catalog.shoes.sizechart.cm[!catalog.shoes.sizechart.cm$value=="",]
length(unique(catalog.shoes.sizechart.cm$id_catalog_simple))
#No. of shoes simples with cm data : 610,166

#id_catalog_simple, sku_simple, catalog_simple_size, fk_catalog_brand, fk_catalog_category, value
catalog.shoes.sizechart.cm.sub <- catalog.shoes.sizechart.cm[, c(2,4,5,8,9,11)]
#From the cm range, compute avg size
catalog.shoes.sizechart.cm.sub$avg_size <- unlist(lapply(lapply(str_extract_all(catalog.shoes.sizechart.cm$value, "[0-9\\.]+"), as.numeric), mean))
View(head(catalog.shoes.sizechart.cm.sub))
min(catalog.shoes.sizechart.cm.sub$avg_size)
#9.5
max(catalog.shoes.sizechart.cm.sub$avg_size)
#32.7

#Read the category-segment mapping
file.cat_seg <- "/home/akulkarni/right-fit/cat_seg.json"
mapping.cat_seg <- read.json(file.cat_seg)

#MERGE SHOES SIZECHART DATA WITH THE CATEGORY-SEGMENT DATA
catalog.shoes.sizechart.merged <- merge(catalog.shoes.sizechart.cm.sub, mapping.cat_seg)
nrow(catalog.shoes.sizechart.cm.sub)
#610,168
nrow(catalog.shoes.sizechart.merged)
#609,720
category.missing <- catalog.shoes.sizechart.cm.sub[!(catalog.shoes.sizechart.cm.sub$fk_catalog_category %in% mapping.cat_seg$fk_catalog_category),]
nrow(category.missing)
#521
unique(category.missing$fk_catalog_category)
#3245 3243 3248 3246
#catalog categories (3245, 3243, 3248, 3246), corresponding to 521 id_catalog_simple (for shoes), 
#that are not mapped to any segment in the catalog_category_has_catalog_segment table
catalog.shoes.sizechart.merged %>% group_by(fk_catalog_segment) %>% summarize(count=n())
# fk_catalog_segment  count
#                  1 391222 (Women)
#                  2 202026 (Men)
#                  3  16472 (Kids)
catalog.shoes.sizechart.women <- catalog.shoes.sizechart.merged[catalog.shoes.sizechart.merged$fk_catalog_segment==1,]
catalog.shoes.sizechart.men   <- catalog.shoes.sizechart.merged[catalog.shoes.sizechart.merged$fk_catalog_segment==2,]
catalog.shoes.sizechart.kids  <- catalog.shoes.sizechart.merged[catalog.shoes.sizechart.merged$fk_catalog_segment==3,]
range(catalog.shoes.sizechart.men$avg_size)
#10.3 32.7 (22.4cm)
range(catalog.shoes.sizechart.women$avg_size)
#19.3 30.0 (10.7cm)
range(catalog.shoes.sizechart.kids$avg_size)
#9.50 28.24 (18.74cm)

#Compute dist measure between every two shoes simples
shoes.dist <- dist(catalog.shoes.sizechart.cm.sub$avg_size)

#BEGIN CLUSTER COMPUTATION
#========================================================================================================================
#Find clusters in the shoes simples data
#-----------------------------------------
#CLUSTER MEN'S SHOES DATA
#-----------------------------------------
#using k-means
system.time(clusters_women.kemans <- kmeans(catalog.shoes.sizechart.women$avg_size, centers = 12, nstart = 10))
#using k-medoids
library(cluster)
#Following code fails due to large data size
system.time(pam(catalog.shoes.sizechart.men$avg_size, k = 20, diss = F, cluster.only = T))
#Following failed to return
system.time(pam(dist(catalog.shoes.sizechart.men$avg_size), k = 20, cluster.only = T))
#Clara works great with large data
system.time(clusters_men <- clara(catalog.shoes.sizechart.men$avg_size, k = 20, samples = 10, pamLike = T))
#user  system elapsed 
#0.322   0.001   0.338 
clusplot(clusters_men, main = "Clusters of Men Shoes")
nrow(catalog.shoes.sizechart.men[catalog.shoes.sizechart.men$avg_size <= 12,])
#1,361 <- Minority cluster with size <= 12 (Most likely belongs to "Boys")
nrow(catalog.shoes.sizechart.men[catalog.shoes.sizechart.men$avg_size > 12 & catalog.shoes.sizechart.men$avg_size <= 21,])
#0 <- No data with size 12 < size <= 21
#Clustering after removing the minority cluster data
catalog.shoes.sizechart.men.original <- catalog.shoes.sizechart.men
catalog.shoes.sizechart.men <- catalog.shoes.sizechart.men[catalog.shoes.sizechart.men$avg_size > 21, ]
cluster_count.men <- floor(2 * (max(catalog.shoes.sizechart.men$avg_size) - min(catalog.shoes.sizechart.men$avg_size)))
#23 clusters 
cluster_count.men <- floor(1 * (max(catalog.shoes.sizechart.men$avg_size) - min(catalog.shoes.sizechart.men$avg_size)))
#11 clusters
cluster_count.men <- floor(0.5 * (max(catalog.shoes.sizechart.men$avg_size) - min(catalog.shoes.sizechart.men$avg_size)))
#5 clusters
system.time(clusters_men <- clara(catalog.shoes.sizechart.men$avg_size, k = cluster_count.men, samples = 10, sampsize = 5000, rngR = T, pamLike = T))
#67s-140s
catalog.shoes.sizechart.men$cluster_index <- clusters_men$clustering
#Compute the closeness-score between each sku simple and a cluster
catalog.shoes.distance.matrix.men           <- closeness.score(catalog.shoes.sizechart.men$avg_size, clusters_men$medoids)
rownames(catalog.shoes.distance.matrix.men) <- catalog.shoes.sizechart.men$id_catalog_simple

data.temp <- catalog.shoes.sizechart.men[catalog.shoes.sizechart.men$cluster_index==11,]
min(data.temp$avg_size)
max(data.temp$avg_size)
nrow(data.temp)
system.time(clusters_temp <- clara(data.temp$avg_size, k = 5, samples = 10, sampsize = 5000, rngR = T, pamLike = T))
system.time(clusters_temp <- pam(data.temp$avg_size, k = 5, diss = F))
#-----------------------------------------
#CLUSTER WOMEN'S SHOES DATA
#-----------------------------------------
cluster_count.women <- floor(2 * (max(catalog.shoes.sizechart.women$avg_size) - min(catalog.shoes.sizechart.women$avg_size)))
#21 clusters
cluster_count.women <- floor(1 * (max(catalog.shoes.sizechart.women$avg_size) - min(catalog.shoes.sizechart.women$avg_size)))
#10 clusters
cluster_count.women <- floor(0.5 * (max(catalog.shoes.sizechart.women$avg_size) - min(catalog.shoes.sizechart.women$avg_size)))
#5 clusters 
system.time(clusters_women <- clara(catalog.shoes.sizechart.women$avg_size, k = cluster_count.women, samples = 10, sampsize = 5000, rngR = T, pamLike = T))
#user   system elapsed 
#146.040   0.007 145.976 
clusplot(clusters_women, main = "Clusters of Women Shoes")
catalog.shoes.sizechart.women$cluster_index <- clusters_women$clustering
#Compute the closeness-score between each sku simple and a cluster
catalog.shoes.distance.matrix.women           <- closeness.score(catalog.shoes.sizechart.women$avg_size, clusters_women$medoids)
rownames(catalog.shoes.distance.matrix.women) <- catalog.shoes.sizechart.women$id_catalog_simple

#-----------------------------------------
#CLUSTER KIDS' SHOES DATA
#-----------------------------------------
cluster_count.kids <- floor(2 * (max(catalog.shoes.sizechart.kids$avg_size) - min(catalog.shoes.sizechart.kids$avg_size)))
#37
cluster_count.kids <- floor(1 * (max(catalog.shoes.sizechart.kids$avg_size) - min(catalog.shoes.sizechart.kids$avg_size)))
#18
cluster_count.kids <- floor(0.5 * (max(catalog.shoes.sizechart.kids$avg_size) - min(catalog.shoes.sizechart.kids$avg_size)))
#9 clusters
system.time(clusters_kids <- clara(catalog.shoes.sizechart.kids$avg_size, k = cluster_count.kids, samples = 10, pamLike = T))
#user   system elapsed 
#  0.127   0.000   0.127 
clusplot(clusters_kids, main = "Clusters of Kids Shoes")
catalog.shoes.sizechart.kids$cluster_index <- clusters_kids$clustering
#Compute the closeness-score between each sku simple and a cluster
catalog.shoes.distance.matrix.kids           <- closeness.score(catalog.shoes.sizechart.kids$avg_size, clusters_kids$medoids)
rownames(catalog.shoes.distance.matrix.kids) <- catalog.shoes.sizechart.kids$id_catalog_simple

score.simple_cluster_closeness.matrix_list <<- list(catalog.shoes.distance.matrix.women, catalog.shoes.distance.matrix.men, catalog.shoes.distance.matrix.kids)

columns.subset <- c('id_catalog_simple', 'sku_simple', 'avg_size', 'fk_catalog_segment', 'cluster_index')
catalog.shoes.sizechart.combined <- rbind(catalog.shoes.sizechart.men[, columns.subset], catalog.shoes.sizechart.women[, columns.subset], catalog.shoes.sizechart.kids[, columns.subset])
catalog.shoes.sizechart.combined$sku <- sapply(catalog.shoes.sizechart.combined$sku_simple, function(x) unlist(strsplit(x, "-"))[1])

clusters.df.list <- list(data.frame(centers = clusters_women$medoids), data.frame(centers = clusters_men$medoids), data.frame(centers = clusters_kids$medoids))

#END CLUSTER COMPUTATION
#============================================================================================================================
#PRIMARY O/P OBJECTS AFTER CLUSTERING :-
#CLOSENESS SCORE MATRICES:
#catalog.shoes.distance.matrix.men, catalog.shoes.distance.matrix.women, catalog.shoes.distance.matrix.kids
#score.simple_cluster_closeness.matrix_list contains a list of the above closeness score matrices
#COMBINED SIZECHART DATA:
#catalog.shoes.sizechart.combined
#CLUSTERS:
#clusters.df.list
#============================================================================================================================

#BEGIN ANALYZE SALES DATA - UNSUCCESSFUL
#============================================================================================================================
sales.shoes.bad <- read.json('/home/akulkarni/right-fit/sales_shoes_bad.json')
nrow(sales.shoes.bad)
#3,486,592
unique(sales.shoes.bad$id_sales_order_item_status)
length(which(sales.shoes.bad$id_sales_order_item_status %in% c(8,12,18,32,35,36,37,38)))
#603,858 <- returned
sales.shoes.bad.returned <- sales.shoes.bad[sales.shoes.bad$id_sales_order_item_status %in% c(8,12,18,32,35,36,37,38), ]
length(which(sales.shoes.bad$id_sales_order_item_status %in% c(14,15,16,23,25,26,27,28,29)))
#1,123,119 <- cancelled
length(which(sales.shoes.bad$id_sales_order_item_status==10))
#907,435 <- invalid
load('/home/akulkarni/right-fit/bad_shoes_return_reason.Rdata')
sales.shoes.sizeissue <- both[grep('size', both$ReturnReason, ignore.case = T),] %>% group_by(CustomerNo)
nrow(sales.shoes.sizeissue)
#396,547
sales.shoes.sizeissue <- sales.shoes.sizeissue[, c(1,2,3,5,10)]
sales.shoes.sizeissue.merged <- merge(sales.shoes.sizeissue, catalog.shoes.sizechart.merged, by = 'sku_simple')
#============================================================================================================================
#END ANALYZE SALES DATA - UNSUCCESSFUL

#============================================================================================================================
#Following code has been moved to the function support_score.matrices
#============================================================================================================================
# Customer - Shoes purchase data
cust.shoes.data <- read.json('/home/akulkarni/right-fit/sales_shoes_custdata.json')
# Total number of shoes sales
nrow(cust.shoes.data)
#4,782,599
# Number of unique shoes in the sales data
length(unique(cust.shoes.data$id_catalog_simple))
#472,910
indices.notpresent <- which(!(unique(cust.shoes.data$id_catalog_simple) %in% catalog.shoes.sizechart.merged$id_catalog_simple))
length(indices.notpresent)
#106,838 - shoes not present in the catalog sizechart data for shoes.
length(unique(cust.shoes.data[indices.notpresent, 'id_catalog_config']))
#These missing shoes simples correspond to 24,365 id_catalog_config                 
#Some of this sizechart data seems to be available in the more recent data.

cust.shoes.data.grouped <- cust.shoes.data %>% group_by(fk_customer) %>% summarize(count=n()) %>% arrange(desc(count))
# Number of customers who have purchased shoes
nrow(cust.shoes.data.grouped)
#2,366,972
cust.shoes.data.merged <- merge(cust.shoes.data, catalog.shoes.sizechart.merged, by = 'id_catalog_simple')
nrow(cust.shoes.data.merged)
#3,010,537
length(unique(cust.shoes.data.merged$id_catalog_simple))
#366,072 <- number of unique shoes simples in the sales data with associated sizechart information
cust.shoes.data.merged <- cust.shoes.data.merged[, c(-7, -12)]
library(data.table)
setnames(cust.shoes.data.merged, "sku_simple.x", "sku_simple")
cust.shoes.data.labeled <- merge(cust.shoes.data.merged, catalog.shoes.sizechart.combined)
#cust.shoes.data.labeled <- merge(cust.shoes.data.labeled, catalog.shoes.sizechart.women[, c('id_catalog_simple', 'cluster_index')], all.x = T)
#cust.shoes.data.labeled <- merge(cust.shoes.data.labeled, catalog.shoes.sizechart.kids[, c('id_catalog_simple', 'cluster_index')], all.x = T)

cust.shoes.data.by_segment <- cust.shoes.data.labeled %>% group_by(fk_customer, fk_catalog_segment) %>% summarize(seg_count = n())
cust.shoes.data.by_cluster <- cust.shoes.data.labeled %>% group_by(fk_customer, fk_catalog_segment, cluster_index) %>% summarize(cluster_count = n()) %>% mutate(confidence = (cluster_count + 2) / (4 + sum(cluster_count))) %>% arrange(desc(confidence))
cust.shoes.data.by_cluster_new <- cust.shoes.data.labeled %>% group_by(fk_customer, fk_catalog_segment, cluster_index) %>% summarize(cluster_count = n())
score.customer_cluster.matrix.women <- support.score(cust.shoes.data.by_cluster_new, cluster_count.women, seg = 1)
score.customer_cluster.matrix.men   <- support.score(cust.shoes.data.by_cluster_new, cluster_count.men, seg = 2)
score.customer_cluster.matrix.kids  <- support.score(cust.shoes.data.by_cluster_new, cluster_count.kids, seg = 3)
score.customer_cluster_support.matrix_list <- list(score.customer_cluster.matrix.women, score.customer_cluster.matrix.men, score.customer_cluster.matrix.kids)
score.customer_cluster_support.matrix_list <- support_score.matrices(training.pos)
#=============================================Function end=============================================
#===================================================================================================================
#PRIMARY O/P OBJECTS FROM THE SALES DATA :-
#SALES DATA GROUPED BY CLUSTER:
#cust.shoes.data.by_cluster_new
#SUPPORT SCORE MATRICES:
#score.customer_cluster.matrix.men, score.customer_cluster.matrix.women, score.customer_cluster.matrix.kids
#score.customer_cluster_support.matrix_list is a list of the above three matrices
#===================================================================================================================
#COMBINE THE SUPPORT SCORE AND CLOSENESS SCORE INTO A SINGLE SCORE
score.combined.matrix.men <- apply(catalog.shoes.distance.matrix.men, 1, function(x) apply(t(t(score.customer_cluster.matrix.men) * x), 1, max))

#BEGIN EVALUATION
#===================================================================================================================
#cust.shoes.data.by_cluster AND sales.shoes.sizeissue.merged
#===================================================================================================================
customers.sizeissue <- unique(sales.shoes.sizeissue.merged$CustomerNo)
length(customers.sizeissue) #177,281
customers.sales <- unique(cust.shoes.data.by_cluster$fk_customer)
length(customers.sales) #1,617,970
length(which(customers.sizeissue %in% customers.sales)) #131,082
sales.shoes.sizeissue.eval <- sales.shoes.sizeissue.merged[sales.shoes.sizeissue.merged$CustomerNo %in% customers.sales,]
nrow(sales.shoes.sizeissue.eval) #208,467
nrow(sales.shoes.sizeissue.merged) #265,139
#For each row in sales.shoes.sizeissue.eval, check if the sku_simple is there in the preferred cluster (top-k) of the customer for that segment
#It should not be there!! That would justify the size issue!

#cl <- makePSOCKcluster(ncores)
registerDoParallel(ncores)
sales.shoes.sizeissue.eval$result <- foreach(m = isplitRows(sales.shoes.sizeissue.eval, chunks = ncores), .combine = c) %dopar% {
  #apply(m[, c('CustomerNo', 'fk_catalog_segment', 'id_catalog_simple', 'sku_simple')], 1, function(x) check_valid.neighborhood.by_sku(x[1], x[2], x[3], x[4]))
  apply(m[, c('CustomerNo', 'fk_catalog_segment', 'id_catalog_simple')], 1, function(x) check_valid.neighborhood(x[1], x[2], x[3]))
}

stopCluster(cl)
#system.time(apply(head(sales.shoes.sizeissue.eval)[, c('CustomerNo', 'fk_catalog_segment', 'id_catalog_simple', 'sku_simple')], 1, function(x) check_valid.neighborhood.by_sku(x[1], x[2], x[3], x[4])))

length(which(sales.shoes.sizeissue.eval$result==FALSE))
#(23-21-37) --> 135,007 <- in these many cases, the shoe did not belong to customer's preferred clusters
#(11-10-18) --> 122,657
#(5-5-9) --> 88,495
length(which(sales.shoes.sizeissue.eval$result==TRUE))
#(23-21-37) --> 73,052 <- in these many cases, the shoe did belong to customer's preferred clusters
#(11-10-18) --> 85,402
#(5-5-9) --> 119,564
# Sanity check: Remove entries with empty result - happens when the test SKU simple doesn't have a size chart.
sales.shoes.sizeissue.eval$len <- unlist(lapply(sales.shoes.sizeissue.eval$result, length))
sales.shoes.sizeissue.eval <- sales.shoes.sizeissue.eval[sales.shoes.sizeissue.eval$len!=0,]
sales.shoes.sizeissue.eval$result <- unlist(sales.shoes.sizeissue.eval$result)
write.csv(sales.shoes.sizeissue.eval[, -15], file = 'returns_sizeissue_test.csv', row.names = FALSE)

#------------------------------------------------------
# EVALUATION AFTER COMBINED SCORING
#------------------------------------------------------
system.time(evalrun.combined_scoring.df <- evalrun.clustermodel.combined_scoring(sales.shoes.sizeissue.eval))
#user    system   elapsed 
#32906.731   164.731  1127.849 
length(which(evalrun.combined_scoring.df$prediction == FALSE))
length(which(evalrun.combined_scoring.df$prediction == TRUE))
write.csv(evalrun.combined_scoring.df[, c(1, 2, 5, 7, 10, 11, 13, 16, 17)], file = 'test_neg_combined_scoring.csv', row.names = FALSE)


check_valid <- function(customer, segment, simple_id)   {
  cust.clusters <- cust.shoes.data.by_cluster[cust.shoes.data.by_cluster$fk_customer == customer & cust.shoes.data.by_cluster$fk_catalog_segment == segment, 'cluster_index']
  prod.cluster  <- catalog.shoes.sizechart.combined[catalog.shoes.sizechart.combined$id_catalog_simple == simple_id & catalog.shoes.sizechart.combined$fk_catalog_segment == segment, 'cluster_index']
  prod.cluster %in% cust.clusters
}

check_valid.neighborhood <- function(customer, segment, simple_id) {
  cust.clusters <- cust.shoes.data.by_cluster[cust.shoes.data.by_cluster$fk_customer == customer & cust.shoes.data.by_cluster$fk_catalog_segment == segment,]$cluster_index
  if(length(cust.clusters) > 0) {
    clusters.df <- switch(segment, 
                         data.frame(clusters_women$medoids),
                         data.frame(clusters_men$medoids),
                         data.frame(clusters_kids$medoids))
    colnames(clusters.df) <- 'centers'
    cust.clusters.df  <- order_by.clust.dist.points(clusters.df, clusters.df[cust.clusters,])
    cust.clusters.all <- rownames(cust.clusters.df[cust.clusters.df$d < 0.5,])
    prod.cluster  <- catalog.shoes.sizechart.combined[catalog.shoes.sizechart.combined$id_catalog_simple == simple_id & catalog.shoes.sizechart.combined$fk_catalog_segment == segment, 'cluster_index']
    prod.cluster %in% cust.clusters.all
  }
  else {
    return(FALSE)
  }
}

check_valid.neighborhood.by_sku <- function(customer, segment, simple_id, sku_simple) {
  cust.clusters <- cust.shoes.data.by_cluster[cust.shoes.data.by_cluster$fk_customer == customer & cust.shoes.data.by_cluster$fk_catalog_segment == segment,]$cluster_index
  sku            <- unlist(strsplit(sku_simple, "-"))[1]
  sku.catalog    <- catalog.shoes.sizechart.combined[catalog.shoes.sizechart.combined$sku == sku,]
  
  if(length(cust.clusters) > 0 && nrow(sku.catalog) > 0) {
    if(Reduce('|', sku.catalog$cluster_index %in% cust.clusters) == TRUE) {
      simples.topk <- sku.catalog[sku.catalog$cluster_index %in% cust.clusters,]$id_catalog_simple
    }
    else {
      #cust.pref_cluster <- cust.clusters[1]
      clusters.df <- clusters.df.list[[segment]]
      cust.pref_cluster.centers <- clusters.df[cust.clusters, 'centers']
      
      sku.catalog    <- order_by.vect.dist(sku.catalog, sku.catalog$avg_size, cust.pref_cluster.centers)
  #    simple.smaller <- sku.catalog[sku.catalog$avg_size < cust.pref_cluster.center,]$id_catalog_simple[1]
  #    simple.bigger  <- sku.catalog[sku.catalog$avg_size > cust.pref_cluster.center,]$id_catalog_simple[1]
      simples.topk   <- head(sku.catalog[sku.catalog$d < 0.6,]$id_catalog_simple, 2)
    }
    #simple_id %in% c(simple.smaller, simple.bigger)
    simple_id %in% simples.topk
  }
  else {
    return(FALSE)
  }
}

#=============================================================================
#Evaluation on sales data between '2015-09-10' to '2015-10-20'
#=============================================================================
sales.shoes.test <- read.json('/home/akulkarni/right-fit/sales_shoes_test.json')
nrow(sales.shoes.test)
#355,312
sales.shoes.test.success <- sales.shoes.test[sales.shoes.test$id_sales_order_item_status %in% c(3,4,5,6,7,11,17,24,33,34), ]
nrow(sales.shoes.test.success)
#207,797 <- evaluate only the successful orders
sales.shoes.test.success <- sales.shoes.test.success[, c(3,15,16,19,20,22)]
sales.shoes.test.success <- merge(sales.shoes.test.success, catalog.shoes.sizechart.merged[, c(2,6,7,9)], by = 'id_catalog_simple')
length(unique(sales.shoes.test.success$fk_customer))
#78,153 <- number of unique customers in the sales test data.
length(which(unique(sales.shoes.test.success$fk_customer) %in% customers.sales))
#31,712 <- repeating customers for which we already have sales data (for shoes)
#filter sales test data to retain only the repeating customers
sales.shoes.test.success <- sales.shoes.test.success[sales.shoes.test.success$fk_customer %in% customers.sales,]
nrow(sales.shoes.test.success)
#44,316
setnames(sales.shoes.test.success, "fk_customer", "CustomerNo")

#cl <- makePSOCKcluster(ncores)
registerDoParallel(ncores)
sales.shoes.test.success$result <- foreach(m = isplitRows(sales.shoes.test.success, chunks = ncores), .combine = c) %dopar% {
  #apply(m[, c('CustomerNo', 'fk_catalog_segment', 'id_catalog_simple', 'sku_simple')], 1, function(x) check_valid.neighborhood.by_sku(x[1], x[2], x[3], x[4]))
  apply(m[, c('CustomerNo', 'fk_catalog_segment', 'id_catalog_simple')], 1, function(x) check_valid.neighborhood(x[1], x[2], x[3]))
}

#stopCluster(cl)
length(which(sales.shoes.test.success$result == TRUE))
#(23-21-37) --> 13,510
#(11-10-18) --> 16,680
#(5-5-9) --> 22,887
length(which(sales.shoes.test.success$result == FALSE))
#(23-21-37) --> 30,446
#(11-10-18) --> 27,276
#(5-5-9) --> 21,069
#sales.shoes.test.success$result <- clustermodel.eval(sales.shoes.test.success)
sales.shoes.test.success$len <- unlist(lapply(sales.shoes.test.success$result, length))
sales.shoes.test.success <- sales.shoes.test.success[sales.shoes.test.success$len != 0,]
sales.shoes.test.success$result <- unlist(sales.shoes.test.success$result)
write.csv(sales.shoes.test.success[, -11], file = 'sales_shoes_test.csv', row.names = FALSE)
#------------------------------------------------------------------
#EVALUATION AFTER COMBINED SCORING
#------------------------------------------------------------------
remove(evalrun.combined_scoring.test.pos.df)
system.time(evalrun.combined_scoring.test.pos.df <- evalrun.clustermodel.combined_scoring(sales.shoes.test.success))
#user   system  elapsed 
#7918.469   75.836  230.697 
write.csv(evalrun.combined_scoring.test.pos.df[, c(1, 2, 6, 7, 8, 9, 12, 13)], file = 'test_pos_combined_scoring.csv', row.names = FALSE)

#---------------------------------------------------------------
# MAP WITH THE BRICK DATA
#---------------------------------------------------------------
shoes.brick   <- read.json('/home/akulkarni/right-fit/shoes_brick_data.json')
shoes.brick   <- shoes.brick[, c(2, 4, 5, 7)]
setnames(shoes.brick, "fk_catalog_attribute_option_global_brick", "id_brick")
bricks.global <- read.delim('/home/akulkarni/right-fit/global_bricks_data.tsv')
setnames(bricks.global, "id_catalog_attribute_option_global_brick", "id_brick")
shoes.brick   <- merge(shoes.brick, bricks.global)

evalrun.combined_scoring.test.pos.df        <- merge(evalrun.combined_scoring.test.pos.df, shoes.brick[, c(1, 2, 5)])
evalrun.combined_scoring.df                 <- merge(evalrun.combined_scoring.df, shoes.brick[, c(1, 2, 5)])
evalrun.combined_scoring.test.pos.brickwise <- evalrun.combined_scoring.test.pos.df %>% group_by(id_brick) %>% summarize(tp = length(which(prediction == TRUE)), fn = length(which(prediction == FALSE)))
evalrun.combined_scoring.test.neg.brickwise <- evalrun.combined_scoring.df %>% group_by(id_brick) %>% summarize(fp = length(which(prediction == TRUE)), tn = length(which(prediction == FALSE)))
evalrun.combined_scoring.test.brickwise     <- merge(evalrun.combined_scoring.test.pos.brickwise, evalrun.combined_scoring.test.neg.brickwise, all = TRUE)
evalrun.combined_scoring.test.brickwise[is.na(evalrun.combined_scoring.test.brickwise)] <- 0
evalrun.combined_scoring.test.brickwise     <- merge(evalrun.combined_scoring.test.brickwise, bricks.global)
