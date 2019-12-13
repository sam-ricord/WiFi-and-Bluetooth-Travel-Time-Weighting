rm(list=ls()) #clear variables
setwd("\Users\timri_000\Desktop\SAM") #set working directroy
#import libraries
library(data.table)
library(dplyr)
library(reshape)
library(stringr)
library(DataCombine)
library(tidyverse)
library(chron)
#import smart.csv to global env. dataframe
dataframe1 <- read.csv("smart1_new.csv", header = FALSE)
dataframe2 <- read.csv("smart2_new.csv", header = FALSE)
dataframe3 <- read.csv("smart3_new.csv", header = FALSE)
dataframe4 <- read.csv("smart4_new.csv", header = FALSE)
dataframe5 <- read.csv("smart5_new.csv", header = FALSE)
dataframe <- rbind(dataframe1, dataframe2)
dataframe <- rbind(dataframe, dataframe3)
dataframe <- rbind(dataframe, dataframe4)
dataframe <- rbind(dataframe, dataframe5)

final.dataframe <- data.frame(TimeDiffernce="", Start="", End="")

dataframe <- dataframe[-c(1), ]
dataframe <- dataframe[-c(6252), ]
dataframe <- dataframe[-c(12913), ]
dataframe <- dataframe[-c(26113), ]
dataframe <- dataframe[-c(39286), ]
dataframe$V12 <- chron(times=dataframe$V12)

unique.mac.address <- unique(dataframe$V3)
unique.mac.address <- as.vector.factor(unique.mac.address)
cider <- as.numeric(length(unique.mac.address))
for (n in 1:cider) {
  instance.unique.mac.address <- unique.mac.address[n]
  new.unique.mac.address <- dataframe[ which(dataframe$V3==instance.unique.mac.address), ]
  new.unique.mac.address.dfed <- new.unique.mac.address$V12
  test <- unique(new.unique.mac.address$V2)
  test <- as.numeric(test)
  j <- nrow(new.unique.mac.address)
  j <- as.numeric(j)
  if(test == 1){
      for (i in 1:j) {
        dataframe<-dataframe[!(dataframe$V3==instance.unique.mac.address),]
        i <- i + 1
      }
  }
  n <- n+1
}
new.unique.mac.address <- unique(dataframe$V3)
new.unique.mac.address <- as.vector.factor(new.unique.mac.address)
x <- length(new.unique.mac.address)
x <- as.numeric(x)
for (y in 1:x) {
  y <- 3
  new.unique.mac.address1 <- new.unique.mac.address[y]
  new.unique.mac.address.ata <- dataframe[ which(dataframe$V3==new.unique.mac.address1), ]
  if(new.unique.mac.address.ata$V2=="SmartCity001" && new.unique.mac.address.ata$V2=="SmartCity005"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity001"), ]
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity005"), ]
    if(new.unique.mac.address.ata.2$V12 > new.unique.mac.address.ata.1$V12){
      time.mac.add <- new.unique.mac.address.ata.2$V12 - new.unique.mac.address.ata.1$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity001", End="SmartCity005")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1$V12 - new.unique.mac.address.ata.2$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity005", End="SmartCity001")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  }
  if(new.unique.mac.address.ata$V2=="SmartCity001" && new.unique.mac.address.ata$V2=="SmartCitySRSBaes"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity001"), ]
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCitySRSBaes"), ]
    if(new.unique.mac.address.ata.2$V12 > new.unique.mac.address.ata.1$V12){
      time.mac.add <- new.unique.mac.address.ata.2$V12 - new.unique.mac.address.ata.1$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity001", End="SmartCitySRSBaes")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1$V12 - new.unique.mac.address.ata.2$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCitySRSBaes", End="SmartCity001")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  }
  if(new.unique.mac.address.ata$V2=="SmartCitySRSBaes" && new.unique.mac.address.ata$V2=="SmartCity003"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCitySRSBaes"), ]
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity003"), ]
    if(new.unique.mac.address.ata.2$V12 > new.unique.mac.address.ata.1$V12){
      time.mac.add <- new.unique.mac.address.ata.2$V12 - new.unique.mac.address.ata.1$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity001", End="SmartCitySRSBaes")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1$V12 - new.unique.mac.address.ata.2$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCitySRSBaes", End="SmartCity001")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  }
  if(new.unique.mac.address.ata$V2=="SmartCity003" && new.unique.mac.address.ata$V2=="SmartCity004"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity003"), ]
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity004"), ]
    if(new.unique.mac.address.ata.2$V12 > new.unique.mac.address.ata.1$V12){
      time.mac.add <- new.unique.mac.address.ata.2$V12 - new.unique.mac.address.ata.1$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity003", End="SmartCity004")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1$V12 - new.unique.mac.address.ata.2$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity004", End="SmartCity003")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  }
  if(new.unique.mac.address.ata$V2=="SmartCity004" && new.unique.mac.address.ata$V2=="SmartCity005"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity004"), ]
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity005"), ]
    if(new.unique.mac.address.ata.2$V12 > new.unique.mac.address.ata.1$V12){
      time.mac.add <- new.unique.mac.address.ata.2$V12 - new.unique.mac.address.ata.1$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity004", End="SmartCity005")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1$V12 - new.unique.mac.address.ata.2$V12
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity005", End="SmartCity004")
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  }
  y <- y+1
}
###
#n <- nrow(new.unq.mac.df)
#for (i in 1:n) {
#  
#  if ( new.unq.mac.df[i,2] == "SmartCity001" &&) {
#    print("fe")
#  }
#  i <- i+1
#}

#as.data.frame.numeric()
#new.unq.mac.dfed <- as.POSIXct(new.unq.mac.dfed, format='%H:%M:%OS')
#new.unq.mac.dfed <- as.numeric(new.unq.mac.dfed)
#new.unq.mac.df <- dat
#i <- 1

A = 50
B = 35
C = 15
D = 20
E = 15
F = 5
G = 7
H = 4
I = 2
Time = 2
LL = 2
car = 22
bike = 22
ped = 22

for(j in 1:nrow(df)) {
  if(df[j, Time] <= (((df[j, LL])/(A))*60)){
    break
  }else if(df[j, Time] <= (((df[j, LL])/(B))*60)){
    
    df[j, car] = ((1/(((df[j, LL] / B)*60) - ((df[j, LL] / A)*60)))*(df[j, time] - ((df[j, LL] / A)*60)))
    
  }else if(df[j, Time] <= (((df[j, LL])/(D))*60)){
    
    df[j, car] = ((1/(((df[j, LL] / B)*60) - ((df[j, LL] / C)*60)))*(df[j, time] - ((df[j, LL] / C)*60)))
    
  }else if(df[j, Time] <= (((df[j, LL])/(C))*60)){
    
    df[j, car] = ((1/(((df[j, LL] / B)*60) - ((df[j, LL] / C)*60)))*(df[j, time] - ((df[j, LL] / C)*60)))
    df[j, bike] = ((1/(((df[j, LL] / E)*60) - ((df[j, LL] / D)*60)))*(df[j, time] - ((df[j, LL] / D)*60)))
    
  }else if(df[j, Time] <= (((df[j, LL])/(G))*60)){
    
    df[j, bike] = ((1/(((df[j, LL] / E)*60) - ((df[j, LL] / F)*60)))*(df[j, time] - ((df[j, LL] / F)*60)))
    
  }else if(df[j, Time] <= (((df[j, LL])/(F))*60)){
    
    df[j, bike] = ((1/(((df[j, LL] / E)*60) - ((df[j, LL] / F)*60)))*(df[j, time] - ((df[j, LL] / F)*60)))
    df[j, ped] = ((1/(((df[j, LL] / H)*60) - ((df[j, LL] / G)*60)))*(df[j, time] - ((df[j, LL] / G)*60)))
    
  }else if(df[j, Time] <= (((df[j, LL])/(H))*60)){
    
    df[j, ped] = ((1/(((df[j, LL] / H)*60) - ((df[j, LL] / G)*60)))*(df[j, time] - ((df[j, LL] / G)*60)))
  
  }else if(df[j, Time] <= (((df[j, LL])/(I))*60)){
    
    df[j, ped] = ((1/(((df[j, LL] / H)*60) - ((df[j, LL] / I)*60)))*(df[j, time] - ((df[j, LL] / I)*60)))
    
  }
}

w = 0
x = 0
for(k in 1:nrow(df)){
  w = w + (df[k, time] * df[k, car])
  x = x + (df[k, car])
}
carAverage = (w/x)

w = 0
x = 0
for(k in 1:nrow(df)){
  w = w + (df[k, time] * df[k, bike])
  x = x + (df[k, bike])
}
bikeAverage = (w/x)

w = 0
x = 0
for(k in 1:nrow(df)){
  w = w + (df[k, time] * df[k, ped])
  x = x + (df[k, ped])
}
pedAverage = (w/x)

print(carAverage)
print(bikeAverage)
print(pedAverage)

#histogram density
#average travel times by time of day
