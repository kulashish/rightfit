library(jsonlite)
json.file  <- "/home/akulkarni/right-fit/sales_data_grouped.json"
sales.data.grouped <- fromJSON(sprintf("[%s]", paste(readLines(json.file), collapse=",")))
sales.data.grouped.300 <- sales.data.grouped[sales.data.grouped$count < 300,]
sales.data.grouped.50 <- sales.data.grouped[sales.data.grouped$count < 50,]
nrow(sales.data.grouped[sales.data.grouped$count < 50,])
nrow(sales.data.grouped[sales.data.grouped$count > 5000,])

library(ggplot2)
ggplot(data=sales.data.grouped.50, aes(sales.data.grouped.50$count)) +
  geom_histogram(breaks=seq(1, 50, by=1), aes(y = ..density..)) +
  xlim(c(1, 50))

ggplot(data=sales.data.grouped.50, aes(x=factor(0), y=sales.data.grouped.50$count)) +
  geom_boxplot() + theme(axis.text.x=element_blank(), axis.title.x=element_blank())

boxplot(data=sales.data.grouped.50$count)

#-------------------
json.file  <- "/home/akulkarni/right-fit/joined_sku.json"
joined.sku.grouped <- fromJSON(sprintf("[%s]", paste(readLines(json.file), collapse=",")))

p = ggplot(data=joined.sku.grouped, 
           aes(x=factor(1),
               y=count,
               fill = attribute_set_name
           )
) 

p <- p + geom_bar(width = 1, stat = "identity")
at <- nrow(joined.sku.grouped) - as.numeric(cumsum(sort(table(joined.sku.grouped)))-0.5*sort(table(joined.sku.grouped)))
label=paste0(round(sort(table(joined.sku.grouped))/sum(table(joined.sku.grouped)),2) * 100,"%")
p + coord_polar("y", start=0) + theme(axis.text.x=element_blank()) + 
  annotate(geom = "text", y = at, x = 1, label = label)
  
