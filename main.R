library(gdata)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(lubridate)
data_path <- "./crime.xls"
info <- read.xls(data_path, sheet=1)
info
info$BlockRange[info$BlockRange=='UNK'] <- NA
info$Type[info$Type == '-'] <- NA
info$Suffix[info$Suffix == '-'] <- NA

# Removes outliers by unix time days
dates <- info$Date
n_days <- day(days(ymd(date)))
info <- info[n_days >= quantile(n_days, .25) - 1.5*IQR(n_days) & n_days <= quantile(n_days, .75) + 1.5*IQR(n_days), ]

#number of crimes per beat with the types of crimes
info.df <- tbl_df(info) %>% drop_na(Beat, BlockRange)
by_beat_with_group <- group_by(info.df, Beat, BlockRange)
by_beat <- group_by(info.df, Beat)
crime_count_with_group <- arrange(tally(by_beat_with_group), desc=-n)
crime_count <- arrange(tally(by_beat), desc=-n)
# we only want the top ones
top_beats <- head(crime_count)
top_crime_count_with_group <- crime_count_with_group[crime_count_with_group$Beat %in% top_beats$Beat]
top_crime_count_with_group <- subset(crime_count_with_group, Beat %in% top_beats$Beat)
ggplot(top_crime_count_with_group, aes(reorder(Beat, -n), n, fill=BlockRange, order=BlockRange)) + geom_bar(stat="identity") + ggtitle("Distribution of crimes per beat")

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
#number of crimes per hour and type
ggplot(info, aes(x=Hour, color=Offense.Type)) + geom_histogram(binwidth = 1) + ggtitle("Distribution of crimes per hour and type")


