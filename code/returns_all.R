require(ffbase)
require(datasets)
require(data.table)

require(stats)
require(dplyr)
require(gmodels)
require(ggplot2)

getwd()
setwd("/home/rstudio/rightfit/temp")
--options(fftempdir = (fftempdir=[/home/ashok/temp])
          
          --method-1 read csv file        
          #returns <- read.csv.ffdf(file="/home/ashok/Returns_Data_14Sep2015.csv",sep="|", header=T, VERBOSE=T, next.rows=500000, colClasses=NA)
          
          --method-2  read csv file
          #returns<- read.table.ffdf(file="/home/ashok/Desktop/data/18sep2014_18sep2014_195_Server.csv", sep = "|")
          
          #create data frame 
          returns_all <- read.table.ffdf(file="/home/akatara/returns_data_all.csv", sep = "|",header=TRUE)
          
          
          salesrule<- read.table.ffdf(file= "/home/ashok/salesrule.csv",sep="|")
          
          class(returns )
          dim(returns )
          str(returns s[1:10,])
          head(returns )
          names(returns )
          
          --save ddff in the current directory
          save.ffdf(hhp,dir =".")
          
          --remove ddff
          rm(hhp)
          
          --load ddff 
          save.ffdf(hhp,dir =".")