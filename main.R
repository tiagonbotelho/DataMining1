library(gdata)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(lubridate)
library(DMwR)
library(nnet)
library(rpart.plot)
library(MASS)
data_path <- "./crime.xls"

remove_outliers <- function(info) {
  n_days <- day(days(ymd(info$Date)))
  return(info[n_days >= quantile(n_days, .25) - 1.5*IQR(n_days) & n_days <= quantile(n_days, .75) + 1.5*IQR(n_days),])
}

na_handler <- function(info) {
  info$Beat[info$Beat == 'UNK'] <- NA
  info <- knnImputation(info, k=3)
  info$BlockRange[info$BlockRange=='UNK'] <- NA
  info$Type[info$Type == '-'] <- NA
  info$Suffix[info$Suffix == '-'] <- NA
  info$Beat <- as.character(info$Beat)
  
  return(info)
}

dataset_prep <- function(x, only.week=FALSE) {
  x$Date <- ifelse(as.integer(x$Hour) < 8, as.character(as.Date(x$Date) - 1), as.character(x$Date))
  
  # Split info in time intervals
  x$DayInterval <- 0
  x[as.integer(x$Hour) < 8 | as.integer(x$Hour) >= 19,]$DayInterval <- 3
  x[as.integer(x$Hour) >= 12 & as.integer(x$Hour) < 19,]$DayInterval <- 2
  x[as.integer(x$Hour) >= 8 & as.integer(x$Hour) < 12,]$DayInterval <- 1

  if(only.week){
    return(data.frame(WeekDay = as.integer(strftime(x$Date, "%u")),
                      DayInterval = x$DayInterval,
                      Beat = x$Beat,
                      Offenses = x$X..offenses,
                      stringsAsFactors = FALSE))
  }
  
  return(data.frame(WeekDay = as.integer(strftime(x$Date, "%u")),
                    DayInterval = x$DayInterval,
                    Beat = x$Beat,
                    Offenses = x$X..offenses,
                    Day = day(x$Date),
                    Month = month(x$Date),
                    Year = year(x$Date),
                    stringsAsFactors = FALSE))
}

interval_beat_perms <- function(x) { return(expand.grid(DayInterval = unique(x$DayInterval), Beat = unique(x$Beat))) }

get_days_between <- function(info) {
  firstdate <- info$Date[order(format(as.Date(info$Date)))[1]]
  lastdate <- info$Date[tail(order(format(as.Date(info$Date))), n=1)] 
  
  return(seq(as.Date(firstdate), as.Date(lastdate), by="days"))
}

create_total_perm <- function(preprocessed, only.week = FALSE) {
  days.between <- get_days_between(info)
  unique_beats <- unique(preprocessed$Beat)
  unique_day_intervals <- unique(preprocessed$DayInterval)
  
  if(only.week){
    unique_weekdays <- unique(preprocessed$WeekDay)
    all_beats_perm <- data.frame(WeekDay = rep(unique_weekdays, times=length(unique_beats) * length(unique_day_intervals)),
                                 DayInterval = rep(unique_day_intervals, times=length(unique_beats) * length(days.between)),
                                 Beat = rep(unique_beats, times=length(unique_day_intervals) * length(days.between)),
                                 Offenses = rep(0, times=length(unique_day_intervals) * length(days.between) * length(unique_beats)))
  } else{
    all_beats_perm <- data.frame(Date = rep(days.between, times=length(unique_beats) * length(unique_day_intervals)),
                                 DayInterval = rep(unique_day_intervals, times=length(unique_beats) * length(days.between)),
                                 Beat = rep(unique_beats, times=length(unique_day_intervals) * length(days.between)),
                                 Offenses = rep(0, times=length(unique_day_intervals) * length(days.between) * length(unique_beats)))
    all_beats_perm$DayInterval <- as.integer(all_beats_perm$DayInterval)
    all_beats_perm$Offenses <- as.character(all_beats_perm$Offenses)
    all_beats_perm$WeekDay = as.integer(strftime(all_beats_perm$Date, "%u"))  
    all_beats_perm$Day <- day(as.Date(all_beats_perm$Date))
    all_beats_perm$Month <- month(as.Date(all_beats_perm$Date))
    all_beats_perm$Year <- year(as.Date(all_beats_perm$Date))
    all_beats_perm <- all_beats_perm[,!colnames(all_beats_perm) %in% c("Date")]
  }
  
  return(all_beats_perm)
}

info <- read.xls(data_path, sheet=1) %>% remove_outliers %>% na_handler
info.preprocessed <- dataset_prep(info)
info.preprocessed.group <- group_by(info.preprocessed, WeekDay, DayInterval, Beat, Day, Month, Year) %>% summarize(Offenses = sum(Offenses))
info.preprocessed.total_perm <- create_total_perm(info.preprocessed)
info.preprocessed.joined <- merge(info.preprocessed.group, info.preprocessed.total_perm, 
                                  by=c("WeekDay", "DayInterval", "Beat", "Day", "Month", "Year"), all=TRUE)
info.preprocessed.joined[is.na(info.preprocessed.joined$Offenses.x),]$Offenses.x <- 0
info.preprocessed.joined$Offenses <- info.preprocessed.joined$Offenses.x
info.preprocessed.joined <- info.preprocessed.joined[,!colnames(info.preprocessed.joined) %in% c("Offenses.y", "Offenses.x")]


info.preprocessed.onlyweek <- dataset_prep(info, only.week = TRUE)
info.preprocessed.onlyweek.group <- group_by(info.preprocessed.onlyweek, WeekDay, DayInterval, Beat) %>% summarize(Offenses = sum(Offenses))
info.preprocessed.onlyweek.total_perm <- create_total_perm(info.preprocessed, only.week = TRUE)

######## Neural Network ##########
training_index <- sample(1:nrow(info.preprocessed.joined),as.integer(0.95*nrow(info.preprocessed.joined)))
train <- info.preprocessed.joined[training_index,]
test <- info.preprocessed.joined[-training_index,]

max.offenses <- max(info.preprocessed.joined$Offenses)
nn <- nnet(Offenses / max.offenses ~ ., data = train, size=5, decay=0.05, maxit=1000)
pred <- predict(nn, test) * max.offenses
MSE <- mean((pred - test$Offenses)^2)
perc <- mean(abs(test$Offenses - pred)/test$Offenses)
  
  
######## Neural Network ##########


######## Regression Trees ##########
info.preprocessed.group$Offenses <- as.integer(info.preprocessed.group$Offenses)
sp <- sample(1:nrow(info.preprocessed.group), as.integer(nrow(info.preprocessed.group)*0.80))
tr <- info.preprocessed.group[sp,]
ts <- info.preprocessed.group[-sp,]
ac <- rpartXse(Offenses ~ ., tr)
ac$xlevels[["y"]] <- union(ac$xlevels[["y"]], levels(ts$Offenses))
ps <- predict(ac, ts, type="vector")
#root mean squared error = 2.50
mse = sqrt(mean((ts$Offenses-ps)^2))
#mean absolute error = 1.89
mae <- mean(abs(ps - ts$Offenses))
#correlation between the predictions and the true values = 0.70
cr <- cor(ps, ts$Offenses)
#mean average percentage error = 0.63
mape <- mean(abs(ts$Offenses-ps)/ts$Offenses)
prp(ac, type=1, extra=101)
######## Regression Trees ##########


######## Linear Discriminant Analysis ##########
sp <- sample(1:nrow(info.preprocessed.group), as.integer(nrow(info.preprocessed.group)*0.99))
tr <- info.preprocessed.group[sp,]
ts <- info.preprocessed.group[-sp,]
ac <- lda(Offenses ~ ., tr)
ac$xlevels[["y"]] <- union(ac$xlevels[["y"]], levels(ts$Offenses))
ps <- predict(ac, ts)
######## Linear Discriminant Analysis ##########


######## Multiple Linear Regression ##########
linearRegression <- lm(Offenses ~ ., info.preprocessed.group)
summary(linearRegression)
######## Multiple Linear Regression ##########


get_total_dataset <- function(info) {
  all_perms <- dataset_prep() %>% create_total_perm()
}


#number of crimes per beat with the types of crimes
info.df <- tbl_df(info) %>% drop_na(Beat, BlockRange)
by_beat_with_group <- group_by(info.df, Beat, BlockRange)
by_beat <- group_by(info.df, Beat)
crime_count_with_group <- arrange(tally(by_beat_with_group), desc=-n)
crime_count <- arrange(tally(by_beat), desc=-n)
# we only want the top ones
top_beats <- head(crime_count)
top_crime_count_with_group <- crime_count_with_group[crime_count_with_group$Beat %in% top_beats$Beat,]
top_crime_count_with_group <- subset(crime_count_with_group, Beat %in% top_beats$Beat)
ggplot(top_crime_count_with_group, aes(reorder(Beat, -n), n, fill=BlockRange, order=BlockRange)) + geom_bar(stat="identity") + ggtitle("Distribution of crimes per beat")

#number of crimes per Address (block range, streetname, type, suffix) - all count 1
info.df <- tbl_df(info)
by_address <- group_by(info.df,BlockRange, StreetName, Type, Suffix)
count <- arrange(tally(by_address), desc(n))
#criar a morada com as outras colunas coladas
count$d <- paste(count$BlockRange, count$StreetName,count$Type,count$Suffix)
#remover os NA's depois, antes tava dificil
count$d <- gsub(" NA","",count$d)
count$d <- gsub("NA ","",count$d)
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
#reorder porque o x ficava ordenado por ordem alfabetica e quero pelo n, e -n para ficar por ordem descendente.
ggplot(head(count), aes(x=reorder(d,-n), y=n)) + geom_bar(stat="identity") + ggtitle("Distribution of crimes per StreetName, Type and Suffix")

#number of crimes per street name
info.df <- tbl_df(info)
by_street <- group_by(info.df, StreetName)
count <- arrange(tally(by_street), desc(n))
#reorder porque o x ficava ordenado por ordem alfabetica e quero pelo n, e -n para ficar por ordem descendente.
ggplot(head(count), aes(x=reorder(StreetName,-n), y=n)) + geom_bar(stat="identity") + ggtitle("Distribution of crimes per StreetName")
#number of crimes per hour and type
ggplot(info, aes(x=Hour, color=Offense.Type)) + geom_histogram(binwidth = 1) + ggtitle("Distribution of crimes per hour and type")

