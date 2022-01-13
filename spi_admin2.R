library(SPEI)
library(stringr)
library(glue)
library(zoo)
setwd("~/Dr Arinze Nwokolo/Projects/Drought and Mental Outcomes")

data = read.csv("Admin2.csv")
months = c(unique(data$month))
months =as.data.frame(months)
months$num = seq(12)

for (i in seq(nrow(months))){
  mnth = months$months[i]
  num = months$num[i]
  a = glue("data[data == '{mnth}'] = {num}") 
  print(a)
}

data[data == 'january'] = 01
data[data == 'february'] = 02
data[data == 'march'] = 03
data[data == 'april'] = 04
data[data == 'may'] = 05
data[data == 'june'] = 06
data[data == 'july'] = 07
data[data == 'august'] = 08
data[data == 'september'] = 09
data[data == 'october'] = 10
data[data == 'november'] = 11
data[data == 'december'] = 12



get_spi1 = function(state){
  newdf = data[data$admin2==state  ,]
  newdf <- ts(newdf[,-c(1,2)], end=c(1994,12), frequency=12)
  spi1 = spi(newdf[,'prep'], 1)
  return(spi1)
}
get_spi12 = function(state){
  newdf = data[data$admin2==state  ,]
  newdf <- ts(newdf[,-c(1,2)], end=c(1994,12), frequency=12)
  spi12 = spi(newdf[,'prep'], 12)
  return(spi12)
}


#create empty dataframe for appending for loop
newdf1 = as.data.frame(c(""))
newdf1$year=c("")
newdf1$month=c("")
newdf1$admin2=c("")
newdf1$spi12 =c("")
names(newdf1) = c("spi12", "date", "admin2", "month", "year")

for (state in unique(data$admin2)){
  aaa = get_spi12(state)$fitted     #####change to spi1 if necessary
  aaa = data.frame(Y=as.matrix(aaa), date=as.Date(as.yearmon(time(aaa))))
  aaa$admin2 = state 
  aaa$month = strftime(aaa$date, "%m")
  aaa$month = as.numeric(aaa$month)
  aaa$year = strftime(aaa$date, "%Y")
  names(aaa) = c("spi12", "date", "admin2", "month", "year" )
  newdf1 =  rbind(newdf1, aaa)
  
}


data2 = merge(data, newdf1, by = c("admin2",  "month", "year"), all.x = T)

write.csv(data2, "~/Dr Arinze Nwokolo/Projects/Drought and Mental Outcomes/spi12_admin2.csv")


#create empty dataframe for appending for loop
newdf1 = as.data.frame(c(""))
newdf1$year=c("")
newdf1$month=c("")
newdf1$admin2=c("")
newdf1$spi1 =c("")
names(newdf1) = c("spi1", "date", "admin2", "month", "year")


for (state in unique(data$admin2)){
  aaa = get_spi1(state)$fitted     #####change to spi1 if necessary
  aaa = data.frame(Y=as.matrix(aaa), date=as.Date(as.yearmon(time(aaa))))
  aaa$admin2 = state 
  aaa$month = strftime(aaa$date, "%m")
  aaa$month = as.numeric(aaa$month)
  aaa$year = strftime(aaa$date, "%Y")
  names(aaa) = c("spi1", "date", "admin2", "month", "year" )
  newdf1 =  rbind(newdf1, aaa)
  
}


data2 = merge(data, newdf1, by = c("admin2",  "month", "year"), all.x = T)

write.csv(data2, "~/Dr Arinze Nwokolo/Projects/Drought and Mental Outcomes/spi1_admin2.csv")


spi1_admin2 <- read.csv("spi1_admin2.csv")
spi12_admin2 <- read.csv("spi12_admin2.csv")

spi_admin2 = merge(spi1_admin2, spi12_admin2, by = c("admin2",  "month", "year", "date", "prep"), all = TRUE)

write.csv(spi_admin2, "~/Dr Arinze Nwokolo/Projects/Drought and Mental Outcomes/spi_admin2.csv")


