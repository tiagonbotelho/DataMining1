library(gdata)
library(ggplot2)
data_path <- "./crime.xls"
info <- read.xls(data_path, sheet=1)

split <- strsplit(as.character(info$BlockRange), "-")
info$BlockRange <- order(sapply(split, "[", 1))

ola <- info[startsWith(info$Beat, "10"),]
ggplot(ola, aes(x=ola$Beat, y=ola$BlockRange, color=ola$Type)) + geom_point()