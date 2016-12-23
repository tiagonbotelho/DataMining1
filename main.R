library(gdata)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
data_path <- "./crime.xls"
info <- read.xls(data_path, sheet=1)
info$BlockRange[info$BlockRange=='UNK'] <- NA
info$Type[info$Type == '-'] <- NA
info$Suffix[info$Suffix == '-'] <- NA

split <- strsplit(as.character(info$BlockRange), "-")
info$BlockRange <- order(sapply(split, "[", 1))

ola <- info[startsWith(info$Beat, "10"),]
ggplot(ola, aes(x=ola$Beat, y=ola$BlockRange, color=ola$Type)) + geom_point()



#number of crimes per Address (block range, streetname, type, suffix) - all count 1
info.df <- tbl_df(info)
by_address <- group_by(info.df,BlockRange, StreetName, Type, Suffix)
head(by_address)
count <- arrange(tally(by_address), desc(n))
head(count)
#criar a morada com as outras colunas coladas
count$d <- paste(count$BlockRange, count$StreetName,count$Type,count$Suffix)
#remover os NA's depois, antes tava dificil
count$d <- gsub(" NA","",count$d)
count$d <- gsub("NA ","",count$d)
head(count)
#reorder porque o x ficava ordenado por ordem alfabetica e quero pelo n, e -n para ficar por ordem descendente.
ggplot(head(count), aes(x=reorder(d,-n), y=n)) + geom_bar(stat="identity") + ggtitle("Distribution of crimes per Address")


#number of crimes per street name, Type and Suffix
info.df <- tbl_df(info)
by_street_type_suffix <- group_by(info.df, StreetName, Type, Suffix)
count <- arrange(tally(by_street_type_suffix), desc(n))
#criar a morada com as outras colunas coladas
count$d <- paste(count$StreetName,count$Type,count$Suffix)
#remover os NA's depois, antes tava dificil
count$d <- gsub(" NA","",count$d)
head(count)
#reorder porque o x ficava ordenado por ordem alfabetica e quero pelo n, e -n para ficar por ordem descendente.
ggplot(head(count), aes(x=reorder(d,-n), y=n)) + geom_bar(stat="identity") + ggtitle("Distribution of crimes per StreetName, Type and Suffix")

#number of crimes per street name
info.df <- tbl_df(info)
by_street <- group_by(info.df, StreetName)
count <- arrange(tally(by_street), desc(n))
head(count)
#reorder porque o x ficava ordenado por ordem alfabetica e quero pelo n, e -n para ficar por ordem descendente.
ggplot(head(count), aes(x=reorder(StreetName,-n), y=n)) + geom_bar(stat="identity") + ggtitle("Distribution of crimes per StreetName")