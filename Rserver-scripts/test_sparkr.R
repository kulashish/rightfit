Sys.setenv(SPARK_HOME='/home/akulkarni/spark-1.5.0-bin-hadoop2.6')
.libPaths(c(file.path(Sys.getenv('SPARK_HOME')), .libPaths()))
library(SparkR)
sc <- sparkR.init("yarn-client", "SparkR")
