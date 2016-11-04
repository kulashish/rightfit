source("/home/akulkarni/right-fit/code/rightfit_header.R")
library(flexclust)
library(cluster)
library(mongolite)

options(echo=TRUE)
args		<- commandArgs(trailingOnly = TRUE)
file.sizechart	<- args[1]
file.sales	<- args[2] 
param.cluster	<- 1

#CATALOG SHOES SIZECHART DATA
catalog.shoes.sizechart <- read.json(file.sizechart)
catalog.shoes.sizechart.cm <- catalog.shoes.sizechart[catalog.shoes.sizechart$column_header %in% c("In CM", "In Cm", "Foot Size (In Cm)", "IN CM", "In cm", "Foot Size (CM)", "In Mc", "IN Cm", "In  Cm", "Foot Size (in CM)", "IN cm", "Foot Size in CM"), ]
#remove blanks
catalog.shoes.sizechart.cm <- catalog.shoes.sizechart.cm[!catalog.shoes.sizechart.cm$value=="",]
catalog.shoes.sizechart.cm <- catalog.shoes.sizechart.cm[, c('id_catalog_simple', 'sku_simple', 'catalog_simple_size', 'fk_catalog_brand', 'fk_catalog_category', 'value')]
catalog.shoes.sizechart.cm.$avg_size <- unlist(lapply(lapply(str_extract_all(catalog.shoes.sizechart.cm$value, "[0-9\\.]+"), as.numeric), mean))

catalog.shoes.sizechart.women	<- catalog.shoes.sizechart.cm[catalog.shoes.sizechart.cm$fk_catalog_segment==1,]
catalog.shoes.sizechart.men   	<- catalog.shoes.sizechart.cm[catalog.shoes.sizechart.cm$fk_catalog_segment==2,]
catalog.shoes.sizechart.kids  	<- catalog.shoes.sizechart.cm[catalog.shoes.sizechart.cm$fk_catalog_segment==3,]

#-----------------------------------------
#CLUSTER MEN'S SHOES DATA
#-----------------------------------------
#0 <- No data with size 12 < size <= 21
#Clustering after removing the minority cluster data
catalog.shoes.sizechart.men	<- catalog.shoes.sizechart.men[catalog.shoes.sizechart.men$avg_size > 21, ]
cluster_count.men		<- floor(param.cluster * (max(catalog.shoes.sizechart.men$avg_size) - min(catalog.shoes.sizechart.men$avg_size)))
system.time(clusters_men 	<- clara(catalog.shoes.sizechart.men$avg_size, k = cluster_count.men, samples = 10, sampsize = 5000, rngR = T, pamLike = T))
catalog.shoes.sizechart.men$cluster_index	<- clusters_men$clustering

#Compute the closeness-score between each sku simple and a cluster
catalog.shoes.distance.matrix.men           <- closeness.score(catalog.shoes.sizechart.men$avg_size, clusters_men$medoids)
rownames(catalog.shoes.distance.matrix.men) <- catalog.shoes.sizechart.men$id_catalog_simple

#-----------------------------------------
#CLUSTER WOMEN'S SHOES DATA
#-----------------------------------------
cluster_count.women		<- floor(param.cluster * (max(catalog.shoes.sizechart.women$avg_size) - min(catalog.shoes.sizechart.women$avg_size)))
system.time(clusters_women	<- clara(catalog.shoes.sizechart.women$avg_size, k = cluster_count.women, samples = 10, sampsize = 5000, rngR = T, pamLike = T))
catalog.shoes.sizechart.women$cluster_index <- clusters_women$clustering
catalog.shoes.distance.matrix.women           <- closeness.score(catalog.shoes.sizechart.women$avg_size, clusters_women$medoids)
rownames(catalog.shoes.distance.matrix.women) <- catalog.shoes.sizechart.women$id_catalog_simple

#-----------------------------------------
#CLUSTER KIDS' SHOES DATA
#-----------------------------------------
cluster_count.kids		<- floor(param.cluster * (max(catalog.shoes.sizechart.kids$avg_size) - min(catalog.shoes.sizechart.kids$avg_size)))
system.time(clusters_kids 	<- clara(catalog.shoes.sizechart.kids$avg_size, k = cluster_count.kids, samples = 10, pamLike = T))
catalog.shoes.sizechart.kids$cluster_index <- clusters_kids$clustering
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
#COMBINED SIZECHART DATA WITH CLUSTER MEMBERSHIP:
#catalog.shoes.sizechart.combined
#CLUSTERS:
#clusters.df.list
#============================================================================================================================

cust.shoes.data			<- read.json(file.sales)
cust.shoes.labeled		<- merge(cust.shoes.data, catalog.shoes.sizechart.combined)
cust.shoes.data.by_cluster	<- cust.shoes.data.labeled %>% group_by(fk_customer, fk_catalog_segment, cluster_index) %>% summarize(cluster_count = n())
cust.shoes.data.by_cluster	<- cust.shoes.data.by_cluster %>% mutate(cluster_total = sum(cluster_count), profiles = n())
score.customer_cluster.matrix.women <- support.score(cust.shoes.data.by_cluster, cluster_count.women, seg = 1)
score.customer_cluster.matrix.men   <- support.score(cust.shoes.data.by_cluster, cluster_count.men, seg = 2)
score.customer_cluster.matrix.kids  <- support.score(cust.shoes.data.by_cluster, cluster_count.kids, seg = 3)
score.customer_cluster_support.matrix_list <- list(score.customer_cluster.matrix.women, score.customer_cluster.matrix.men, score.customer_cluster.matrix.kids)

#===================================================================================================================
#PRIMARY O/P OBJECTS FROM THE SALES DATA :-
#SALES DATA GROUPED BY CLUSTER:
#cust.shoes.data.by_cluster_new
#SUPPORT SCORE MATRICES:
#score.customer_cluster.matrix.men, score.customer_cluster.matrix.women, score.customer_cluster.matrix.kids
#score.customer_cluster_support.matrix_list is a list of the above three matrices
#===================================================================================================================

mongo_insert <- function(collection_name, data) {
  #mongo(collection = "test", db = "test", url = "mongodb://localhost", verbose = TRUE)
  m <- mongo(collection = collection_name, url = "mongodb://rightfit:R!gHtF!te831@172.16.90.53:27017/admin", verbose = TRUE)
  df <- as.data.frame(data)
  row_id <- rownames(df)
  df <- cbind(df, row_id)
  m$insert(df)
}

mongo_insert("catalog_shoes_proximity_men", catalog.shoes.distance.matrix.men)
mongo_insert("catalog_shoes_proximity_women", catalog.shoes.distance.matrix.women)
mongo_insert("catalog_shoes_proximity_kids", catalog.shoes.distance.matrix.kids)

mongo_insert("score_customer_cluster_support_women", score.customer_cluster_support.matrix_list[[1]])
mongo_insert("score_customer_cluster_support_men", score.customer_cluster_support.matrix_list[[2]])
mongo_insert("score_customer_cluster_support_kids", score.customer_cluster_support.matrix_list[[3]])

