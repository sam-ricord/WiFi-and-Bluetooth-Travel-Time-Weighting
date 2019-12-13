rm(list=ls()) #clear variables
setwd("/Users/timri_000/Desktop/SAM") #set working directroy
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

#creates the final dataframe
final.dataframe <- data.frame(TimeDiffernce="", Start="", End="", car="", bike="", ped="", LL="", stringsAsFactors=FALSE)

dataframe <- dataframe[-c(1), ]
dataframe <- dataframe[-c(6252), ]
dataframe <- dataframe[-c(12913), ]
dataframe <- dataframe[-c(26113), ]
dataframe <- dataframe[-c(39286), ]
dataframe$V12 <- chron(times=dataframe$V12)

#filters the list of MAC addresses for uniqueness
unique.mac.address <- unique(dataframe$V3)
unique.mac.address <- as.vector.factor(unique.mac.address)
numRow <- as.numeric(length(unique.mac.address))
for (n in 1:numRow) {
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

#Logic to pair times together 
new.unique.mac.address <- unique(dataframe$V3)
new.unique.mac.address <- as.vector.factor(new.unique.mac.address)
x <- length(new.unique.mac.address)
x <- as.numeric(x)
y <- 1 

for (y in 1:x) {
  new.unique.mac.address1 <- new.unique.mac.address[y]
  new.unique.mac.address.ata <- dataframe[ which(dataframe$V3==new.unique.mac.address1), ]
  #ifelse(new.unique.mac.address.ata$V2=="SmartCity001" & new.unique.mac.address.ata$V2=="SmartCity005"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity001"), ]
    new.unique.mac.address.ata.1 <- max(new.unique.mac.address.ata.1$V12)
#    new.unique.mac.address.ata.1 <- new.unique.mac.address.ata.1(times=new.unique.mac.address.ata.1)
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity005"), ]
    new.unique.mac.address.ata.2 <- max(new.unique.mac.address.ata.2$V12)
#    new.unique.mac.address.ata.2 <- new.unique.mac.address.ata.2(times=new.unique.mac.address.ata.2)
    if(chron(new.unique.mac.address.ata.2) > chron(new.unique.mac.address.ata.1)){
      time.mac.add <- new.unique.mac.address.ata.2 - new.unique.mac.address.ata.1
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity001", End="SmartCity005", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1 - new.unique.mac.address.ata.2
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity005", End="SmartCity001", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  #}
  #ifelse(new.unique.mac.address.ata$V2=="SmartCity001" & new.unique.mac.address.ata$V2=="SmartCitySRSBaes"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity001"), ]
    new.unique.mac.address.ata.1 <- max(new.unique.mac.address.ata.1$V12)
#    new.unique.mac.address.ata.1 <- new.unique.mac.address.ata.1(times=new.unique.mac.address.ata.1)
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCitySRSBaes"), ]
    new.unique.mac.address.ata.2 <- max(new.unique.mac.address.ata.2$V12)
#    new.unique.mac.address.ata.2 <- new.unique.mac.address.ata.2(times=new.unique.mac.address.ata.2)
    if(new.unique.mac.address.ata.2 > new.unique.mac.address.ata.1){
      time.mac.add <- new.unique.mac.address.ata.2 - new.unique.mac.address.ata.1
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity001", End="SmartCitySRSBaes", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1 - new.unique.mac.address.ata.2
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCitySRSBaes", End="SmartCity001", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  #}
  #ifelse(new.unique.mac.address.ata$V2=="SmartCitySRSBaes" & new.unique.mac.address.ata$V2=="SmartCity003"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCitySRSBaes"), ]
    new.unique.mac.address.ata.1 <- max(new.unique.mac.address.ata.1$V12)
#    new.unique.mac.address.ata.1 <- new.unique.mac.address.ata.1(times=new.unique.mac.address.ata.1)
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity003"), ]
    new.unique.mac.address.ata.2 <- max(new.unique.mac.address.ata.2$V12)
#    new.unique.mac.address.ata.2 <- new.unique.mac.address.ata.2(times=new.unique.mac.address.ata.2)
    if(new.unique.mac.address.ata.2 > new.unique.mac.address.ata.1){
      time.mac.add <- new.unique.mac.address.ata.2 - new.unique.mac.address.ata.1
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity003", End="SmartCitySRSBaes", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1 - new.unique.mac.address.ata.2
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCitySRSBaes", End="SmartCity003", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  #}###
  #ifelse(new.unique.mac.address.ata$V2=="SmartCity003" & new.unique.mac.address.ata$V2=="SmartCity004"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity003"), ]
    new.unique.mac.address.ata.1 <- max(new.unique.mac.address.ata.1$V12)
#    new.unique.mac.address.ata.1 <- new.unique.mac.address.ata.1(times=new.unique.mac.address.ata.1)
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity004"), ]
    new.unique.mac.address.ata.2 <- max(new.unique.mac.address.ata.2$V12)
#    new.unique.mac.address.ata.2 <- new.unique.mac.address.ata.2(times=new.unique.mac.address.ata.2)
    if(new.unique.mac.address.ata.2 > new.unique.mac.address.ata.1){
      time.mac.add <- new.unique.mac.address.ata.2 - new.unique.mac.address.ata.1
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity003", End="SmartCity004", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1 - new.unique.mac.address.ata.2
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity004", End="SmartCity003", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  #}##
  #ifelse(new.unique.mac.address.ata$V2=="SmartCity004" & new.unique.mac.address.ata$V2=="SmartCity005"){
    new.unique.mac.address.ata.1 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity004"), ]
    new.unique.mac.address.ata.1 <- max(new.unique.mac.address.ata.1$V12)
#    new.unique.mac.address.ata.1 <- new.unique.mac.address.ata.1(times=new.unique.mac.address.ata.1)
    new.unique.mac.address.ata.2 <- dataframe[ which(new.unique.mac.address.ata$V2=="SmartCity005"), ]
    new.unique.mac.address.ata.2 <- max(new.unique.mac.address.ata.2$V12)
#    new.unique.mac.address.ata.2 <- new.unique.mac.address.ata.2(times=new.unique.mac.address.ata.2)
    if(new.unique.mac.address.ata.2 > new.unique.mac.address.ata.1){
      time.mac.add <- new.unique.mac.address.ata.2 - new.unique.mac.address.ata.1
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity004", End="SmartCity005", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    } else {
      time.mac.add <- new.unique.mac.address.ata.1 - new.unique.mac.address.ata.2
      time.mac.add <- chron(times=time.mac.add)
      final.dataframe1 <- data.frame(TimeDiffernce=time.mac.add, Start="SmartCity005", End="SmartCity004", car="", bike="", ped="", LL="", stringsAsFactors = FALSE)
      final.dataframe <- rbind(final.dataframe, final.dataframe1)
    }
  #}
  y <- y+1
}

#eliminates infinite recursion errors
Time = 1
LL = 7
rowsToDelete = vector()

for(q in 1:nrow(final.dataframe)) {
  if(final.dataframe[q, Time] > 1) {
    rowsToDelete <- c(rowsToDelete, q)
  }
}
final.dataframe <- final.dataframe[-c(rowsToDelete),]

#converts the time to minutes
final.dataframe$TimeDiffernce <- as.double(final.dataframe$TimeDiffernce)
final.dataframe$TimeDiffernce <- final.dataframe$TimeDiffernce  * 1440

#assigns the link length
for(u in 1:nrow(final.dataframe)) {
  if(((final.dataframe[u,2] == ("SamartCity005")) | (final.dataframe[u,2] == ("SmartCity001"))) & ((final.dataframe[u,3] == ("SamartCity005")) | (final.dataframe[u,2] == ("SmartCity001")))){
    final.dataframe[u,LL] <- 3.55
  } else if(((final.dataframe[u,2] == ("SamartCity005")) | (final.dataframe[u,2] == ("SmartCity004"))) & ((final.dataframe[u,3] == ("SamartCity005")) | (final.dataframe[u,2] == ("SmartCity004")))){
    final.dataframe[u,LL] <- 0.95
  } else if(((final.dataframe[u,2] == ("SamartCity004")) | (final.dataframe[u,2] == ("SmartCity003"))) & ((final.dataframe[u,3] == ("SamartCity004")) | (final.dataframe[u,2] == ("SmartCity003")))){
    final.dataframe[u,LL] <- 0.60
  } else if(((final.dataframe[u,2] == ("SamartCity003")) | (final.dataframe[u,2] == ("SmartCitySRSBaes"))) & ((final.dataframe[u,3] == ("SamartCity003")) | (final.dataframe[u,2] == ("SmartCitySRSBaes")))){
    final.dataframe[u,LL] <- 0.79
  } else {
    final.dataframe[u,LL] <- 1.21
  }
}  

#gives the weights to the different modes for each travel time
A = 50
B = 35
C = 15
D = 20
E = 15
J = 5
G = 7
H = 4
I = 2
Time = 1
LL = 7
car = 4
bike = 5
ped = 6

final.dataframe$LL <- as.double(final.dataframe$LL)

for(j in 2:nrow(final.dataframe)) {
  if(final.dataframe[j, Time] <= (((final.dataframe[j, LL])/(A))*60)){
    #break
  }else if(final.dataframe[j, Time] <= (((final.dataframe[j, LL])/(B))*60)){
    final.dataframe[j, car] <- ((1/(((final.dataframe[j, LL] / B)*60) - ((final.dataframe[j, LL] / A)*60)))*(final.dataframe[j, Time] - ((final.dataframe[j, LL] / A)*60)))
  }else if(final.dataframe[j, Time] <= (((final.dataframe[j, LL])/(D))*60)){
    final.dataframe[j, car] <- ((1/(((final.dataframe[j, LL] / B)*60) - ((final.dataframe[j, LL] / C)*60)))*(final.dataframe[j, Time] - ((final.dataframe[j, LL] / C)*60)))
  }else if(final.dataframe[j, Time] <= (((final.dataframe[j, LL])/(C))*60)){
    final.dataframe[j, car] <- ((1/(((final.dataframe[j, LL] / B)*60) - ((final.dataframe[j, LL] / C)*60)))*(final.dataframe[j, Time] - ((final.dataframe[j, LL] / C)*60)))
    final.dataframe[j, bike] <- ((1/(((final.dataframe[j, LL] / E)*60) - ((final.dataframe[j, LL] / D)*60)))*(final.dataframe[j, Time] - ((final.dataframe[j, LL] / D)*60)))
  }else if(final.dataframe[j, Time] <= (((final.dataframe[j, LL])/(G))*60)){
    final.dataframe[j, bike] <- ((1/(((final.dataframe[j, LL] / E)*60) - ((final.dataframe[j, LL] /J)*60)))*(final.dataframe[j, Time] - ((final.dataframe[j, LL] /J)*60)))
  }else if(final.dataframe[j, Time] <= (((final.dataframe[j, LL])/(J))*60)){
    final.dataframe[j, bike] <- ((1/(((final.dataframe[j, LL] / E)*60) - ((final.dataframe[j, LL] /J)*60)))*(final.dataframe[j, Time] - ((final.dataframe[j, LL] /J)*60)))
    final.dataframe[j, ped] <- ((1/(((final.dataframe[j, LL] / H)*60) - ((final.dataframe[j, LL] / G)*60)))*(final.dataframe[j, Time] - ((final.dataframe[j, LL] / G)*60)))
  }else if(final.dataframe[j, Time] <= (((final.dataframe[j, LL])/(H))*60)){
    final.dataframe[j, ped] <- ((1/(((final.dataframe[j, LL] / H)*60) - ((final.dataframe[j, LL] / G)*60)))*(final.dataframe[j, Time] - ((final.dataframe[j, LL] / G)*60)))
  }else if(final.dataframe[j, Time] <= (((final.dataframe[j, LL])/(I))*60)){
    final.dataframe[j, ped] <- ((1/(((final.dataframe[j, LL] / H)*60) - ((final.dataframe[j, LL] / I)*60)))*(final.dataframe[j, Time] - ((final.dataframe[j, LL] / I)*60)))
  }
}

#aggregates the weighted travel times together
carVector1_2 <- vector()
bikeVector1_2 <- vector()
pedVector1_2 <- vector()
carVector2_3 <- vector()
bikeVector2_3 <- vector()
pedVector2_3 <- vector()
carVector3_4 <- vector()
bikeVector3_4 <- vector()
pedVector3_4 <- vector()
carVector4_5 <- vector()
bikeVector4_5 <- vector()
pedVector4_5 <- vector()

final.dataframe$car <- as.double(final.dataframe$car)
final.dataframe$bike <- as.double(final.dataframe$bike)
final.dataframe$ped <- as.double(final.dataframe$ped)

for(u in 2:nrow(final.dataframe)) {
  if(((final.dataframe[u,2] == ("SamartCity005")) | (final.dataframe[u,2] == ("SmartCity004"))) & ((final.dataframe[u,3] == ("SamartCity005")) | (final.dataframe[u,2] == ("SmartCity004")))){
    if(!is.na(final.dataframe[u,car])) {
      carVector4_5 <- c(carVector4_5, u)
    }
    if(!is.na(final.dataframe[u,bike])) {
      bikeVector4_5 <- c(bikeVector4_5, u)
    }
    if(!is.na(final.dataframe[u,ped])) {
      pedVector4_5 <- c(pedVector4_5, u)
    }
  } else if(((final.dataframe[u,2] == ("SamartCity004")) | (final.dataframe[u,2] == ("SmartCity003"))) & ((final.dataframe[u,3] == ("SamartCity004")) | (final.dataframe[u,2] == ("SmartCity003")))){
    if(!is.na(final.dataframe[u,car])) {
      carVector3_4 <- c(carVector3_4, u)
    }
    if(!is.na(final.dataframe[u,bike])) {
      bikeVector3_4 <- c(bikeVector3_4, u)
    }
    if(!is.na(final.dataframe[u,ped])) {
      pedVector3_4 <- c(pedVector3_4, u)
    }
  } else if(((final.dataframe[u,2] == ("SamartCity003")) | (final.dataframe[u,2] == ("SmartCitySRSBaes"))) & ((final.dataframe[u,3] == ("SamartCity003")) | (final.dataframe[u,2] == ("SmartCitySRSBaes")))){
    if(!is.na(final.dataframe[u,car])) {
      carVector2_3 <- c(carVector2_3, u)
    }
    if(!is.na(final.dataframe[u,bike])) {
      bikeVector2_3 <- c(bikeVector2_3, u)
    }
    if(!is.na(final.dataframe[u,ped])) {
      pedVector2_3 <- c(pedVector2_3, u)
    }
  } else {
    if(!is.na(final.dataframe[u,car])) {
      carVector1_2 <- c(carVector1_2, u)
    }
    if(!is.na(final.dataframe[u,bike])) {
      bikeVector1_2 <- c(bikeVector1_2, u)
    }
    if(!is.na(final.dataframe[u,ped])) {
      pedVector1_2 <- c(pedVector1_2, u)
    }
  }
} 

carFinal <- vector()
bikeFinal <- vector()
pedFinal <- vector()

w <- 0
x <- 0
for(k in carVector1_2){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, car])
  x <- x + (final.dataframe[k, car])
}
carFinal <- c(carFinal, (w/x))
w <- 0
x <- 0
for(k in carVector2_3){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, car])
  x <- x + (final.dataframe[k, car])
}
carFinal <- c(carFinal, (w/x))
w <- 0
x <- 0
for(k in carVector3_4){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, car])
  x <- x + (final.dataframe[k, car])
}
carFinal <- c(carFinal, (w/x))
w <- 0
x <- 0
for(k in carVector4_5){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, car])
  x <- x + (final.dataframe[k, car])
}
carFinal <- c(carFinal, (w/x))

w <- 0
x <- 0
for(k in bikeVector1_2){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, bike])
  x <- x + (final.dataframe[k, bike])
}
bikeFinal <- c(bikeFinal, (w/x))
w <- 0
x <- 0
for(k in bikeVector2_3){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, bike])
  x <- x + (final.dataframe[k, bike])
}
bikeFinal <- c(bikeFinal, (w/x))
w <- 0
x <- 0
for(k in bikeVector3_4){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, bike])
  x <- x + (final.dataframe[k, bike])
}
bikeFinal <- c(bikeFinal, (w/x))
w <- 0
x <- 0
for(k in bikeVector4_5){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, bike])
  x <- x + (final.dataframe[k, bike])
}
bikeFinal <- c(bikeFinal, (w/x))

w <- 0
x <- 0
for(k in pedVector1_2){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, ped])
  x <- x + (final.dataframe[k, ped])
}
pedFinal <- c(pedFinal, (w/x))
w <- 0
x <- 0
for(k in pedVector2_3){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, ped])
  x <- x + (final.dataframe[k, ped])
}
pedFinal <- c(pedFinal, (w/x))
w <- 0
x <- 0
for(k in pedVector3_4){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, ped])
  x <- x + (final.dataframe[k, ped])
}
pedFinal <- c(pedFinal, (w/x))
w <- 0
x <- 0
for(k in pedVector4_5){
  w <- w + (final.dataframe[k, Time] * final.dataframe[k, ped])
  x <- x + (final.dataframe[k, ped])
}
pedFinal <- c(pedFinal, (w/x))

print(pedFinal)
print(bikeFinal)
print(carFinal)

#for weighting coefficients
print(max(final.dataframe[,car], na.rm = TRUE))
print(min(final.dataframe[,car], na.rm = TRUE))
print(max(final.dataframe[,bike], na.rm = TRUE))
print(min(final.dataframe[,bike], na.rm = TRUE))
print(sd(final.dataframe[,car], na.rm = TRUE))
print(sd(final.dataframe[,bike], na.rm = TRUE))

bikeTimes <- vector()
carTimes <- vector()
for(t in 2:nrow(final.dataframe)){
  if(!is.na(final.dataframe[t,car])){
    carTimes <- c(carTimes, final.dataframe[t,Time])
  }
  if(!is.na(final.dataframe[t,bike])){
    bikeTimes <- c(bikeTimes, final.dataframe[t,Time])
  }
}
print(max(carTimes, na.rm = TRUE))
print(min(carTimes, na.rm = TRUE))
print(max(bikeTimes, na.rm = TRUE))
print(min(bikeTimes, na.rm = TRUE))
print(sd(carTimes, na.rm = TRUE))
print(sd(bikeTimes, na.rm = TRUE))

#makeing the graphs
eliminateUnused <- vector()
for(t in 2:nrow(final.dataframe)){
  if(final.dataframe[t,Time] > 50){
    eliminateUnused <- c(eliminateUnused, t)
  }
}
for(t in 2:nrow(final.dataframe)){
  if(final.dataframe[t,Time] < (((final.dataframe[j, LL])/(A))*60)){
    eliminateUnused <- c(eliminateUnused, t)
  }
}
graph.dataframe <- final.dataframe[-c(eliminateUnused),]


hist(graph.dataframe$TimeDiffernce, breaks = 50)

carHist <- vector()
for(t in 2:nrow(final.dataframe)){
  if(!is.na(graph.dataframe[t, car])){
    carHist <- c(carHist, graph.dataframe[t, Time])
  }
}
hist(carHist, breaks = 50)

bikeHist <- vector()
for(t in 2:nrow(final.dataframe)){
  if(!is.na(graph.dataframe[t, bike])){
    bikeHist <- c(bikeHist, graph.dataframe[t, Time])
  }
}
hist(bikeHist, breaks = 50)

sd(carHist)
sd(bikeHist)

low <- vector()
mid <- vector()
high <- vector()
for(p in 2:nrow(graph.dataframe)){
  if(graph.dataframe[p,Time] < 1.6){
    low <- c(low, graph.dataframe[p, Time])
  }
  if(graph.dataframe[p, Time] > 3){
    high <- c(high, graph.dataframe[p, Time])
  }
  if(graph.dataframe[p, Time] < 3 & graph.dataframe[p, Time] > 1.6){
    mid <- c(mid, graph.dataframe[p, Time])
  }
}

hist(low, breaks = 10)
hist(mid, breaks = 10)
hist(high, breaks = 10)

sd(carVector1_2)
sd(carVector2_3)
sd(carVector3_4)
sd(carVector4_5)

sd(bikeVector1_2, na.rm=FALSE)
sd(bikeVector2_3)
sd(bikeVector3_4)
sd(bikeVector4_5)


