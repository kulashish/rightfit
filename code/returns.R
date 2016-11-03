library(jsonlite)
json.file  <- "/home/akulkarni/right-fit/sales_data_grouped.json"
sales.data.grouped <- fromJSON(sprintf("[%s]", paste(readLines(json.file), collapse=",")))
colnames(sales.data.grouped) <- c('customerno', 'sales_count')

returns.data <- read.table('/home/akulkarni/right-fit/returns_data_all.csv', header = T, sep = "|", quote="", stringsAsFactors = FALSE, fill = T)
returns.data <- returns.data[, c(-21, -23)]
colnames(returns.data) <- c('orderno', 'issueid', 'status', 'customerno', 'creationdate', 'productid', 'reason', 'otherreason', 'sku', 'bu', 'brick', 'brand', 'category', 'gender', 'description', 'size', 'price', 'discount', 'specialprice', 'orderid', 'orderdate')
returns.data[, 'issueid'] <- as.numeric(returns.data[, 'issueid'])
returns.data <- returns.data[returns.data$issueid==1 | returns.data$issueid==56,]
library(dplyr)
return.data.grouped <- returns.data %>%
  group_by(customerno) %>%
  summarize(count = n())
colnames(return.data.grouped) <- c('customerno', 'return_count')

sales_returns <- merge(sales.data.grouped, return.data.grouped)
sales_returns.more_returns <- sales_returns[sales_returns$return_count > sales_returns$sales_count,]
write.csv(sales_returns.more_returns, 'more_returns_than_sales.csv', row.names = F)

cor(sales_returns[-1], method = 'kendall')
plot(sales_returns[, 'sales'], sales_returns[, 'returns'])
abline(lm(sales_returns[, 'returns']~sales_returns[, 'sales']))

library(ggplot2)
sales_returns <- sales_returns %>%
  mutate(r_by_s = returns / sales)
ggplot(data=sales_returns, aes(sales_returns$r_by_s)) +
  geom_histogram(breaks=seq(0, 1, by=.01)) +
  xlim(c(0, 1))


ggplot(data=sales_returns[sales_returns$r_by_s<=1,], aes(x=factor(0), y=sales_returns$r_by_s)) +
  geom_boxplot() + theme(axis.text.x=element_blank(), axis.title.x=element_blank())
max(sales_returns$r_by_s)
nrow(sales_returns[sales_returns$r_by_s>1,])
