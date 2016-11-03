library(jsonlite)
library(reshape2)
library(doParallel)
library(itertools)
library(data.table)

ncores <- 32
#cl <- makePSOCKcluster(ncores)

read.json <- function(filename) {
  fromJSON(sprintf("[%s]", paste(readLines(filename), collapse=",")))
}

euc.dist <- function(x1, x2) {
  sqrt(sum((x1 - x2) ^ 2))
}

euc.dist.points <- function(x, y.list) {
  min(unlist(lapply(y.list, euc.dist, x)))
}

order_by.clust.dist <- function(clusters.df, point) {
  clusters.df$d <- unlist(lapply(clusters.df$centers, euc.dist, point))
  clusters.df[order(clusters.df$d),]
}

order_by.clust.dist.points <- function(clusters.df, points) {
  clusters.df$d <- unlist(lapply(clusters.df$centers, euc.dist.points, points))
  clusters.df[order(clusters.df$d),]
}

order_by.vect.dist <- function(df, points_vect, point) {
  df$d <- unlist(lapply(points_vect, euc.dist, point))
  df[order(df$d),]
}

order_by.vect.dist.points <- function(df, points_vect, points) {
  df$d <- unlist(lapply(points_vect, euc.dist.points, points))
  df[order(df$d),]
}

closeness.score <- function(size_vec, center_vec) {
  score.mat         <- dist2(size_vec, center_vec)
  1 - score.mat / max(dist(center_vec))
  #1 - mat / apply(mat, 1, max)
}

support.score.each <- function(df.part, count.total) {
  count.clust <- 0
  if(nrow(df.part) > 0) 
    count.clust <- df.part$cluster_count
  (count.clust + 2) / (count.total + 4)
}

support.score.old <- function(grouped.df, clusters.count, seg) {
  customers.vec <- unique(grouped.df$fk_customer)
  score.df <- data.frame(customer = customers.vec, segment = rep(seg, length(customers.vec)))
  score.df <- merge(score.df, grouped.df, by.x = c('customer', 'segment'), by.y = c('fk_customer', 'fk_catalog_segment'), all.x = T) %>%
    group_by(customer, segment) %>% summarize(count.total = sum(cluster_count))
  score.df[is.na(score.df$count.total),]$count.total <- 0
  registerDoParallel(32)
  score.mat <- foreach(cust = customers.vec, .combine = 'rbind') %:%
    foreach(cluster = seq(1:clusters.count), .combine = 'c') %dopar% {
      support.score.each(grouped.df[grouped.df$fk_customer == cust & grouped.df$fk_catalog_segment == seg & grouped.df$cluster_index == cluster,], score.df[score.df$customer == cust, ]$count.total)
    }
}

support.score <- function(grouped.df, clusters.count, seg) {
  customers.vec <- unique(grouped.df$CustomerNo)
  score.df   <- data.frame(customer = customers.vec, segment = rep(seg, length(customers.vec)))
  cluster.df <- data.frame(cluster_index = seq(1:clusters.count)) 
  score.df   <- merge(score.df, cluster.df)
  score.df   <- merge(score.df, grouped.df, by.x = c('customer', 'segment', 'cluster_index'), by.y = c('CustomerNo', 'fk_catalog_segment', 'cluster_index'), all.x = T)
  score.df[is.na(score.df)]   <- 0
  score.df <- score.df %>% group_by(customer, segment) %>% mutate(count.total = sum(cluster_count), score = (cluster_count + 2) / (count.total + 4))
  acast(score.df[, c('customer', 'cluster_index', 'score')], customer~cluster_index, value.var = "score")
}

eval.clustermodel.supp_closeness_score.old <- function(customer, segment, simple_id, sku_simple) {
  score.support.matrix   <- score.customer_cluster_support.matrix_list[[as.integer(segment)]]
  score.closeness.matrix <- score.simple_cluster_closeness.matrix_list[[as.integer(segment)]]
  score.support.vec      <- score.support.matrix[rownames(score.support.matrix) == as.integer(customer),]
  sku                    <- unlist(strsplit(sku_simple, "-"))[1]
  simple_id.list         <- catalog.shoes.sizechart.combined[catalog.shoes.sizechart.combined$sku == sku,]$id_catalog_simple
  score.closeness.matrix.part <- score.closeness.matrix[rownames(score.closeness.matrix) %in% simple_id.list,]
  score.combined         <- apply(t(t(score.closeness.matrix.part) * score.support.vec), 1, max)
  order.indices          <- order(score.combined, decreasing = T)
  score.combined.ordered <- score.combined[order.indices]
  simple_id.list.ordered <- simple_id.list[order.indices]
  result.df              <- data.frame(simple = simple_id.list.ordered, score = format(round(score.combined.ordered, 2), nsmall = 2)) %>% 
    mutate(readable = paste(simple, score, sep = ':'))
  result.simple_list     <- paste(result.df$readable, collapse = ',')
  result.prediction      <- simple_id == simple_id.list.ordered[1]
  data.frame(prediction = result.prediction, simple_list = result.simple_list)
}

eval.clustermodel.supp_closeness_score <- function(eval.vec, topk = 1) {
  score.support.matrix   <- score.customer_cluster_support.matrix_list[[as.integer(eval.vec[2])]]
  score.closeness.matrix <- score.simple_cluster_closeness.matrix_list[[as.integer(eval.vec[2])]]
  score.support.vec      <- score.support.matrix[rownames(score.support.matrix) == as.integer(eval.vec[1]),]
  sku                    <- unlist(strsplit(eval.vec[4], "-"))[1]
  simple_id.list         <- catalog.shoes.sizechart.combined[catalog.shoes.sizechart.combined$sku == sku,]$id_catalog_simple # handle length 0?
  if(length(simple_id.list) == 0) return (data.frame(prediction = 'NOPRED', simple_list = 'NOPRED'))
  
  score.closeness.matrix.part <- score.closeness.matrix[rownames(score.closeness.matrix) %in% simple_id.list,]
  score.combined         <- apply(t(t(score.closeness.matrix.part) * score.support.vec), 1, max)
  order.indices          <- order(score.combined, decreasing = T)
  score.combined.ordered <- score.combined[order.indices]
  simple_id.list.ordered <- simple_id.list[order.indices]
  result.df              <- data.frame(simple = simple_id.list.ordered, score = format(round(score.combined.ordered, 2), nsmall = 2)) %>% 
    mutate(readable = paste(simple, score, sep = ':'))
  scores.topk            <- head(result.df$score, topk)
  result.simple_list     <- paste(result.df$readable, collapse = ',')
#  result.prediction      <- simple_id == simple_id.list.ordered[1]
  if((max(score.combined) - min(score.combined)) <= 0.05)
    result.prediction   <- 'NOPRED'
  else
    result.prediction   <- as.integer(eval.vec[3]) %in% result.df[result.df$score %in% scores.topk,]$simple
  data.frame(prediction = result.prediction, simple_list = result.simple_list, stringsAsFactors = FALSE)
}

evalrun.clustermodel.combined_scoring <- function(eval.df) {
  registerDoParallel(24)
  result.df <- foreach(m = isplitRows(eval.df, chunks = ncores), .combine = rbind) %dopar% {
    do.call("rbind", apply(m[, c('CustomerNo', 'fk_catalog_segment', 'id_catalog_simple', 'sku_simple')], 1, function(x) eval.clustermodel.supp_closeness_score(x, 3)))
  }
  eval.df <- cbind(eval.df, result.df)
}

support_score.matrices <- function(cust.shoes.data.by_cluster_new) {
  score.customer_cluster.matrix.women <- support.score(cust.shoes.data.by_cluster_new, cluster_count.women, seg = 1)
  score.customer_cluster.matrix.men   <- support.score(cust.shoes.data.by_cluster_new, cluster_count.men, seg = 2)
  score.customer_cluster.matrix.kids  <- support.score(cust.shoes.data.by_cluster_new, cluster_count.kids, seg = 3)
  score.customer_cluster_support.matrix_list <- list(score.customer_cluster.matrix.women, score.customer_cluster.matrix.men, score.customer_cluster.matrix.kids)
}