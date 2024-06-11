library(stringr)
library(quanteda)

install.packages("lubridate")
library(lubridate)

## data laden
setwd("~/A Thesis/Rstudio")
data <- read.csv("data_fleur.csv")
View(data)

## 310 artikelen, 5 variabelen

## hoeveelheid woorden 
sum(str_count(data$publisher, "Trouw"))


data$date2 <- as.Date(data$date) #datum van maken
floor_date(data$date2) # maanden van maken om vraag te beantwoorden
data$date3 <- floor_date(data$date2,
           unit = c("month"))
## frequentie per maand
library(dplyr)
freq_data <- data %>%
  group_by(date3) %>%
  count()

## lijngrafiek
library(ggplot2)
ggplot(freq_data, aes(x=date3, y=n)) +
  geom_line()
?geom_line

## filteren op publisher
gefilterd <- data %>%
  filter(str_detect(publisher, "De Telegraaf"))

dfm = data %>%
  corpus(text_field = "text") %>%
  tokens(remove_punct = T)%>%
  tokens_tolower()%>%
  tokens_remove(stopwords('dutch'))%>%
  dfm()

dtm_final <- dfm

## wordcloud
install.packages("corpustools")
library(corpustools)
dtm_wordcloud(dtm_final, nterms = 50, scale = c(1.5, 0.5), rot.per = 0.3)
## heeft betrekking op de 50 vaakst voorkomende woorden in het dtm

