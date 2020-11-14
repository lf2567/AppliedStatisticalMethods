# Install following packages if you don't have them already:
library(jsonlite)
library(stringr)
library(dplyr)
library(tidyr)
library(ISOweek)
library(neuralnet)
library(ggplot2)
library(fields)
library(RANN)

#########################################################################################################
# PART 1: DATA WRANGLING

# Disabling scientific notation:
options(scipen=999)

# Finding files
GeoFiles <- list.files(path="/Users/larafernandez/Desktop/ASM/Project 2/GPS_data",pattern=".geojson$")
NumberOfFiles <- length(GeoFiles)

# Getting file path of first file
GeoFilePath <- paste('/Users/larafernandez/Desktop/ASM/Project 2/GPS_data/',GeoFiles[[1]],sep='')

# Function that converts file to dataframe 
GeoJsonToDF <- function(GeoJsonFilePath){
  Geo <- fromJSON(GeoJsonFilePath)
  Coordinates <- Geo$features$geometry$coordinates
  NumberOfSamples <- length(Geo$features$type)
  
  Times <- Geo$features$properties$time
  Latitudes <- sapply(Coordinates,function(coord)coord[2])
  Longitudes <- sapply(Coordinates,function(coord)coord[1])
  Accuracy <- Geo$features$properties$accuracy
  Speeds <- Geo$features$properties$speed
  
  Gps_df <- data.frame(Times=Times, Latitude=Latitudes, Longitude=Longitudes, Accuracy=Accuracy, Speeds=Speeds)
}


# Creating empty dataframe to store all data
Infos_df <- data.frame(Time=character(), Latitude=character(),Longitude=character(),
                       Accuracy=character(),Speeds=character())

# Looping over all file paths
for(FileIndex in 1:NumberOfFiles){
  GeoFilePath <- paste('/Users/larafernandez/Desktop/ASM/Project 2/GPS_data/',GeoFiles[[FileIndex]],sep='')
  Geo_df <- GeoJsonToDF(GeoFilePath)
  
  Infos_df <- rbind(Infos_df,Geo_df)
}

# Converting strings to date/time format:
Infos_df$Time2 <- as.POSIXct(Infos_df$Time,format="%Y-%m-%dT%H:%M:%OS")

# Splitting "Times" into date and time
Infos_df <- separate(data=Infos_df,col=Times,into=c("Date","Time"),sep="T")

# Adding weekday column
Infos_df$Weekday <- sapply(Infos_df$Date, function(date) ISOweekday(as.Date(date)))
Infos_df$Weekday <- as.numeric(Infos_df$Weekday)

# Removing August 27 (outlier)
Infos_df <- Infos_df[!Infos_df$Date=="2020-08-27",]

# Removing unnecessary columns
Keep <- c("Date","Weekday","Time2","Latitude","Longitude","Accuracy","Speeds")
Infos_df <- Infos_df[Keep]
colnames(Infos_df)[3] <- "Time"

# Ordering by date and time
Infos_df <- Infos_df %>% arrange(Date,Time)

# Finding time differences between rows in minutes
Infos_df <- Infos_df %>% mutate(diff=Time - lag(Time),TimeDiff_Minutes = as.numeric(diff, units = 'mins'))
Keep <- c("Date","Weekday","Time","TimeDiff_Minutes","Latitude","Longitude","Accuracy","Speeds")
Infos_df <- Infos_df[Keep]

#########################################################################################################
# PART 2: LOADING IN TEST DATA

GeoFiles <- list.files(path="/Users/larafernandez/Desktop/ASM/Project 2/test_gps",pattern=".geojson$")
NumberOfFiles <- length(GeoFiles)

# Getting file path of first file
GeoFilePath <- paste('/Users/larafernandez/Desktop/ASM/Project 2/test_gps/',GeoFiles[[1]],sep='')

# Function that converts file to dataframe 
GeoJsonToDF <- function(GeoJsonFilePath){
  Geo <- fromJSON(GeoJsonFilePath)
  Coordinates <- Geo$features$geometry$coordinates
  NumberOfSamples <- length(Geo$features$type)
  
  Times <- Geo$features$properties$time
  Latitudes <- sapply(Coordinates,function(coord)coord[2])
  Longitudes <- sapply(Coordinates,function(coord)coord[1])
  Accuracy <- Geo$features$properties$accuracy
  
  Gps_df <- data.frame(Times=Times, Latitude=Latitudes, Longitude=Longitudes, Accuracy=Accuracy)
}

# Creating empty dataframe to store all data
TestData_df <- data.frame(Time=character(), Latitude=character(),Longitude=character(), Accuracy=character())

# Looping over all file paths
for(FileIndex in 1:NumberOfFiles){
  GeoFilePath <- paste('/Users/larafernandez/Desktop/ASM/Project 2/test_gps/',GeoFiles[[FileIndex]],sep='')
  Geo_df <- GeoJsonToDF(GeoFilePath)
  
  TestData_df <- rbind(TestData_df,Geo_df)
}

# Converting strings to date/time format:
TestData_df$Times2 <- as.POSIXct(TestData_df$Times,format="%Y-%m-%dT%H:%M:%OS")

# Splitting "Times" into date and time
TestData_df <- separate(data=TestData_df,col=Times,into=c("Date","Time"),sep="T")

# Removing unnecessary columns
Keep <- c("Date","Times2","Latitude","Longitude","Accuracy")
TestData_df <- TestData_df[Keep]
colnames(TestData_df)[2] <- "Time"

#########################################################################################################
# PART 3: BOMBING ALGORITHM

# Function that returns best coordinates given starting position
GetOptimalPositions <- function(IdentifiedWeekday, DayPosition){
  # Convert 1st obs to matrix of long and lat:
  Matrix <- as.matrix(TestData_df[DayPosition,c("Longitude","Latitude")])
  
  # Convert identified weekday df (from old data) to matrix of long and lat:
  IdentifiedWeekday_df <- Infos_df[Infos_df$Weekday==IdentifiedWeekday,]
  IdentifiedWeekday_matrix <- as.matrix(IdentifiedWeekday_df[,c("Longitude","Latitude")])
  
  # Find distances between test observation and all points from identified weekday in METERS:
  DistanceTestToPoints <- (t(rdist.earth(Matrix, IdentifiedWeekday_matrix, miles = FALSE, R = NULL)))*1000
  
  # Combine distance-to-points with time, longs, and lats of identified weekday:
  Obs1 <- cbind(IdentifiedWeekday_matrix,DistanceTestToPoints)
  colnames(Obs1) <- c("Longitude","Latitude","DistFromStartingPoint")
  
  # Sort by shortest distance
  Obs1 <- Obs1[order(Obs1[,"DistFromStartingPoint"],decreasing=FALSE),]
  
  # Identify shortest distance index in Infos_df
  FirstRowName <- rownames(Obs1)[1]
  Index <- which(rownames(IdentifiedWeekday_df)==FirstRowName)
  
  # Subset Infos_df for identified weekday with Index as starting point
  WeekdayFromStartingPoint <- IdentifiedWeekday_df[Index:nrow(IdentifiedWeekday_df),]
  
  # Remove stationary points:
  # Assume all points are valid until verification:
  WeekdayNoStationary <- WeekdayFromStartingPoint
  # Find stationary points = minute differences > 2 minutes:
  DiffGreaterThan2 <- which(WeekdayFromStartingPoint$TimeDiff_Minutes > 2)
  # Concatenate
  cat('Found total of',length(DiffGreaterThan2),'stationary points\n')
  cat('Stationary point indices are:', DiffGreaterThan2,'\n')
  # If points found, remove them:
  if(length(DiffGreaterThan2)>0){
    # Remove rows directly below and above point found:
    RemoveRows <- c(DiffGreaterThan2,DiffGreaterThan2+1,DiffGreaterThan2-1)
    WeekdayNoStationary <- WeekdayFromStartingPoint[-RemoveRows, ]
  }
  
  # Finding number of points within 5m radius of each point
  radius <- 0.00005
  Points <- data.frame(WeekdayNoStationary$Longitude,WeekdayNoStationary$Latitude)
  colnames(Points) <- c("longitude","latitude")
  NeighbouringPoints <- nn2(Points, k=nrow(Points), searchtype="radius", radius=radius)
  NumberOfNeighbouringPoints <- rowSums(NeighbouringPoints$nn.idx > 0) - 1
  WeekdayNoStationary <- cbind(WeekdayNoStationary,NumberOfNeighbouringPoints)
  
  Optimal <- WeekdayNoStationary[which.max(WeekdayNoStationary$NumberOfNeighbouringPoints),]
  
  OptimalBomb <- data.frame(Optimal$Time,Optimal$Latitude,Optimal$Longitude,Optimal$NumberOfNeighbouringPoints)
}


# Creat empty dataframe to store optimal positions
OptimalPositions <- data.frame(Time <- vector(mode='character'), Latitude <- vector(mode='numeric'),
                               Longitude <- vector(mode='numeric'),
                               NumberOfNeighbouringPoints <- vector(mode='numeric'))

# Repeat for all observations in TestData_df:
for(indice in 1:nrow(TestData_df)){
  
  cat('\tProcessing list number:',indice,'\n')  
  IdentifiedWeekday <- ISOweekday(TestData_df$Date)[indice]
  
  OptimalBomb <- GetOptimalPositions(IdentifiedWeekday, indice)
  
  OptimalPositions <- rbind(OptimalPositions,OptimalBomb)
}

OptimalPositions <- OptimalPositions %>% distinct()
OptimalPositions <- OptimalPositions[order(OptimalPositions[,"Optimal.NumberOfNeighbouringPoints"],decreasing=TRUE),][1:2,]
OptimalPositions$Optimal.Weekday <- weekdays(OptimalPositions$Optimal.Time)
OptimalPositions$Optimal.Time <- format(OptimalPositions$Optimal.Time, format = "%H:%M:%S")

# FINAL BOMB DECISION
print(BombDecision <- data.frame(Weekday=OptimalPositions$Optimal.Weekday,
                                  Time=OptimalPositions$Optimal.Time,
                                  Latitude=OptimalPositions$Optimal.Latitude,
                                  Longitude=OptimalPositions$Optimal.Longitude,row.names=c("Bomb#1","Bomb#2")))
