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
  info$Offense.Type[info$Offense.Type == "1"] <- NA
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
    ret <- data.frame(WeekDay = as.integer(strftime(x$Date, "%u")),
                      DayInterval = x$DayInterval,
                      Beat = x$Beat,
                      Offenses = x$X..offenses,
                      stringsAsFactors = FALSE)
    ret <- group_by(ret, WeekDay, DayInterval, Beat) %>% summarize(Offenses = mean(Offenses))
    return(ret)
  }
  ret <- data.frame(WeekDay = as.integer(strftime(x$Date, "%u")),
                    DayInterval = x$DayInterval,
                    Beat = x$Beat,
                    Offenses = x$X..offenses,
                    Day = day(x$Date),
                    Month = month(x$Date),
                    Year = year(x$Date),
                    stringsAsFactors = FALSE)
  ret <- group_by(ret, WeekDay, DayInterval, Beat, Day, Month, Year) %>% summarize(Offenses = sum(Offenses))
  return(ret)
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
    all_beats_perm <- expand.grid(Beat=unique_beats, WeekDay=unique_weekdays, DayInterval=unique_day_intervals, Offenses=0)
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
split <- strsplit(as.character(info$BlockRange), "-")
info$BlockRange <- sapply(split, "[", 1)
info.preprocessed <- dataset_prep(info)
info.preprocessed.total_perm <- create_total_perm(info.preprocessed)
info.preprocessed.joined <- merge(info.preprocessed, info.preprocessed.total_perm, 
                                  by=c("WeekDay", "DayInterval", "Beat", "Day", "Month", "Year"), all=TRUE)
info.preprocessed.joined[is.na(info.preprocessed.joined$Offenses.x),]$Offenses.x <- 0
info.preprocessed.joined$Offenses <- info.preprocessed.joined$Offenses.x
info.preprocessed.joined <- info.preprocessed.joined[,!colnames(info.preprocessed.joined) %in% c("Offenses.y", "Offenses.x")]



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

info.preprocessed.onlyweek <- dataset_prep(info, only.week = TRUE)
info.preprocessed.onlyweek.total_perm <- create_total_perm(info.preprocessed, only.week = TRUE)
info.preprocessed.onlyweek.joined <- merge(info.preprocessed.onlyweek, info.preprocessed.onlyweek.total_perm, 
                                  by=c("WeekDay", "DayInterval", "Beat"), all=TRUE)
info.preprocessed.onlyweek.joined[is.na(info.preprocessed.onlyweek.joined$Offenses.x),]$Offenses.x <- 0
info.preprocessed.onlyweek.joined$Offenses <- info.preprocessed.onlyweek.joined$Offenses.x
info.preprocessed.onlyweek.joined <- info.preprocessed.onlyweek.joined[,!colnames(info.preprocessed.onlyweek.joined) %in% c("Offenses.y", "Offenses.x")]

######## Regression Trees ##########
sp <- sample(1:nrow(info.preprocessed.onlyweek.joined), as.integer(nrow(info.preprocessed.onlyweek.joined)*0.80))
tr <- info.preprocessed.onlyweek.joined[sp,]
ts <- info.preprocessed.onlyweek.joined[-sp,]
ac <- rpartXse(Offenses ~ ., tr)
ac$xlevels[["y"]] <- union(ac$xlevels[["y"]], levels(ts$Offenses))
ps <- predict(ac, ts, type="vector")
#root mean squared error = 0.29
mse = sqrt(mean((ts$Offenses-ps)^2))
#mean absolute error = 1.89
mae <- mean(abs(ps - ts$Offenses))
#correlation between the predictions and the true values = 0.60
cr <- cor(ps, ts$Offenses)
prp(ac, type=1, extra=101)
######## Regression Trees ##########


######## Linear Discriminant Analysis ##########
sp <- sample(1:nrow(info.preprocessed.onlyweek.joined), as.integer(nrow(info.preprocessed.onlyweek.joined)*0.99))
tr <- info.preprocessed.onlyweek.joined[sp,]
ts <- info.preprocessed.onlyweek.joined[-sp,]
ac <- lda(Offenses ~ ., tr)
ps <- predict(ac, ts)
#root mean squared error = N.a.N.
mse = sqrt(mean((ts$Offenses-as.numeric(levels(ps$class)[ps$class])^2)))
#mean absolute error = 0.248
mae <- mean(abs(as.numeric(levels(ps$class)[ps$class]) - ts$Offenses))
#correlation between the predictions and the true values = 0.42
cr <- cor(as.numeric(levels(ps$class)[ps$class]), ts$Offenses)
######## Linear Discriminant Analysis ##########


######## Multiple Linear Regression ##########
sp <- sample(1:nrow(info.preprocessed.onlyweek.joined), as.integer(nrow(info.preprocessed.onlyweek.joined)*0.99))
tr <- info.preprocessed.onlyweek.joined[sp,]
ts <- info.preprocessed.onlyweek.joined[-sp,]
lin.reg <- lm(Offenses ~ ., tr)
final.lin.reg <- step(lin.reg)
ps <- predict(final.lin.reg)
regr.eval(ts$Offenses, ps)
######## Multiple Linear Regression ##########


get_total_dataset <- function(info) {
  all_perms <- dataset_prep() %>% create_total_perm()
}

#number of crimes per beat with the types of crimes
info.df <- tbl_df(info) %>% drop_na(Beat, BlockRange)
by_beat <- group_by(info.df, Beat)
crime_count <- arrange(tally(by_beat), desc(n))
# we only want the top ones
top_beats <- head(crime_count)
top_beat_crimes <- filter(info.df,info.df$Beat %in% top_beats$Beat)
ggplot(top_beat_crimes, aes(x = Beat, fill=BlockRange)) + geom_bar(stat="count") + ggtitle("Distribution of crimes per beat")

#number of crimes per Address (block range, streetname, type, suffix)
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
ggplot(info, aes(x=Hour)) + geom_histogram(binwidth = 1) + facet_wrap(~ Offense.Type) + ggtitle("Distribution of crimes per hour and type")

