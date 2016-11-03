file.path <- '/home/akulkarni/right-fit/cat_seg.tsv'
data.tsv <- read.delim(file.path)
library(jsonlite)
data.json <- toJSON(data.tsv)
stream_out(data.tsv, con = file('cat_seg.json'))
