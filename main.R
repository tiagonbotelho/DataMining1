library(gdata)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(lubridate)
library(DMwR)
<<<<<<< HEAD
library(nnet)

=======
library(rpart.plot)
>>>>>>> c183283d75bc6e23fc7a5a2dfbcef60984d142a2
data_path <- "./crime.xls"
info <- read.xls(data_path, sheet=1)

# Removes outliers by unix time days
dates <- info$Date
n_days <- day(days(ymd(dates)))
info <- info[n_days >= quantile(n_days, .25) - 1.5*IQR(n_days) & n_days <= quantile(n_days, .75) + 1.5*IQR(n_days), ]

info$Beat[info$Beat == 'UNK'] <- NA
info <- knnImputation(info, k=3)
info$BlockRange[info$BlockRange=='UNK'] <- NA
info$Type[info$Type == '-'] <- NA
info$Suffix[info$Suffix == '-'] <- NA
info$Beat <- as.character(info$Beat)

# Creates the new table with the proper Weekdays
# dataset_prep <- function(x) {
#   x$Date <- ifelse(as.integer(x$Hour) < 8, as.character(as.Date(x$Date) - 1), as.character(x$Date))
#   x$DayInterval <- 0
#   x[as.integer(x$Hour) < 8 | as.integer(x$Hour) >= 19,]$DayInterval <- 3
#   x[as.integer(x$Hour) >= 12 & as.integer(x$Hour) < 19,]$DayInterval <- 2
#   x[as.integer(x$Hour) >= 8 & as.integer(x$Hour) < 12,]$DayInterval <- 1
#   
#   result <- data.frame(WeekDay = as.integer(strftime(x$Date, "%u")),
#                        DayInterval = x$DayInterval,
#                        Beat = x$Beat,
#                        Offenses = x$X..offenses,
#                        stringsAsFactors = FALSE)
#   
#   
#   
#   return(result)
# }

dataset_prep <- function(x) {
  x$Date <- ifelse(as.integer(x$Hour) < 8, as.character(as.Date(x$Date) - 1), as.character(x$Date))
  x$DayInterval <- 0
  x[as.integer(x$Hour) < 8 | as.integer(x$Hour) >= 19,]$DayInterval <- 3
  x[as.integer(x$Hour) >= 12 & as.integer(x$Hour) < 19,]$DayInterval <- 2
  x[as.integer(x$Hour) >= 8 & as.integer(x$Hour) < 12,]$DayInterval <- 1
<<<<<<< HEAD

=======
>>>>>>> c183283d75bc6e23fc7a5a2dfbcef60984d142a2
  
  result <- data.frame(WeekDay = as.integer(strftime(x$Date, "%u")),
                       DayInterval = x$DayInterval,
                       Beat = x$Beat,
                       Offenses = x$X..offenses,
<<<<<<< HEAD
                       Day = day(x$Date),
                       Month = month(x$Date),
                       Year = year(x$Date),
=======
>>>>>>> c183283d75bc6e23fc7a5a2dfbcef60984d142a2
                       stringsAsFactors = FALSE)
  
  
  
  return(result)
}

preprocessed <- dataset_prep(info)
<<<<<<< HEAD
preprocessed.group <- group_by(preprocessed, WeekDay, DayInterval, Beat, Day, Month, Year) %>% summarize(Offenses = sum(Offenses))

idx.tr <- sample(1:nrow(preprocessed.group),as.integer(0.7*nrow(preprocessed.group)))
train <- preprocessed.group[idx.tr,]
test <- preprocessed.group[-idx.tr,]

nn <- nnet(Offenses ~ ., train, size=5, decay=0.01, maxit=1000)
nn$xlevels[["Beat"]] <- union(nn$xlevels[["Beat"]], levels(test$Beat))
(mtrx <- table(predict(nn, newdata=test, class='integer'), test$Offenses))
=======
preprocessed.group <- group_by(preprocessed, WeekDay, DayInterval, Beat) %>% summarize(Offenses = sum(Offenses))
>>>>>>> c183283d75bc6e23fc7a5a2dfbcef60984d142a2

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


#Regression Trees
sp <- sample(1:nrow(preprocessed.group), as.integer(nrow(preprocessed.group)*0.80))
tr <- preprocessed.group[sp,]
ts <- preprocessed.group[-sp,]
ac <- rpartXse(Offenses ~ ., tr)
ac$xlevels[["y"]] <- union(ac$xlevels[["y"]], levels(ts$Offenses))
ps <- predict(ac, ts, type="vector")
mae <- mean(abs(ps-ts$Offenses))
mae
cr <- cor(ps, ts$Offenses)
cr
prp(ac, type=1, extra=101)
#media erro 1.89