---
title: "Report"
author: "Pedro Belém, Rui Fonseca, Tiago Botelho"
date: "December 23, 2016"
output: 
  pdf_document: 
    fig_height: 6
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(gdata)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
```

## Data Pre processing
The data is read from the crime.xls file into a data frame
```{r}
data_path <- "./crime.xls"
info <- read.xls(data_path, sheet=1)
```
Then, the unknown values (NA) are treated
```{r}
info$BlockRange[info$BlockRange=='UNK'] <- NA
info$Type[info$Type == '-'] <- NA
info$Suffix[info$Suffix == '-'] <- NA
info$Offense.Type[info$Offense.Type == '1'] <- NA
```
and the block range variable is modified. Since it is always a string like "X-Y", with X being a multiple of 100 and Y=X+99, we keep only X/100
```{r}
split <- strsplit(as.character(info$BlockRange), "-")
info$BlockRange <- order(sapply(split, "[", 1))
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
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

#number of crimes per street name
info.df <- tbl_df(info)
by_street <- group_by(info.df, StreetName)
count <- arrange(tally(by_street), desc(n))
#reorder porque o x ficava ordenado por ordem alfabetica e quero pelo n, e -n para ficar por ordem descendente.
ggplot(head(count), aes(x=reorder(StreetName,-n), y=n)) + geom_bar(stat="identity") + ggtitle("Distribution of crimes per StreetName")
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
