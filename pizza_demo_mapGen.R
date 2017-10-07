#demonstration for pizzaAssigner
rm(list = ls())
#address
library(shiny)
library(leaflet)
library(tmap)
library(tmaptools)
library(OpenStreetMap)
library(maps)
library(sp)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(DT)
library(readr)
library(SpatialTools)
library(dplyr)
library(osmar)
library(ggmap)
source('~/pizzaprojekt/assignment/rndTime.R')
#This is a demonstration. With some mock data I assign orders to runners and plot the result on a leaflet.
#First, red a list of adresses from POS terminal. 
#This is the mock data. The adresses are prepared by developer.
#----------------------MOCK DATA GENERATION----------------------
mock_pizza=function(){
  print('lol')
cimlista <- read_csv("~/pizzaprojekt/assignment/cimlista.csv", 
                     col_names = FALSE)
#take a sample from it
set.seed(1)
sampleSize=4
sampleAddresses=cimlista[sample(nrow(cimlista),sampleSize),]
#Genereate ordertimes in specified timeframe
orderTimes=rndTime(sampleSize,st="2017/07/25 3:00:00",et="2017/07/25 3:30:00")
#combine with adresses
sampleData=cbind(sampleAddresses,orderTimes)
colnames(sampleData)<-c("Adress","orderTime")
#create string of actual adress
sampleCoords=data.frame()
#--------------GEOCODING ------------
#smart geocoding by line and tríing until it is successful!
sampleData$lon=rep(0,nrow(sampleData))
sampleData$lat=rep(0,nrow(sampleData))
print('lol')
for(i in 1:length(sampleData$Adress)){
  success=FALSE
  attempt=1
  while(!success&attempt<10){
    Sys.sleep(2)
    print('Attempting to geocode location')
    geocodes=rev(geocode(sampleData$Adress[i]))
    if(!is.na(geocodes)){
      success=TRUE
      print("Successful Geocoding")
      sampleData$lon[i]=geocodes$lat
      sampleData$lat[i]=geocodes$lon
    }
    attempt=attempt+1
  }
}
print(sampleData)
#sampleData=cbind(sampleData,rev(geocode(sampleData$Adress)))
#convert to SpatialPointsDataframe. This is important!!!!!!! From now on no shitting with column names and stuff
sampleSDF=SpatialPointsDataFrame(sampleData[,4:3],sampleData[,1:2])
print(sampleSDF)
#designate depot
depotAdress="Budapest IX. kerulet , Viola u. 48"
depotCoords=rev(geocode_OSM(depotAdress)[[2]])
#now solve the bin packing problem
timeConstraint=60*60
#convert time to km
#v=s/t
avgSpeed=20/3600
#------------------ASSIGNMENT-------------------------
print("STARTING ASSIGNMENT")
#the assignment is based on URGENCY. Urgency is made up of spatial distance, and order since time.
#If we group based on both spatial and temporal urgency ith get better results. Most pizza delivery boys do 2-4 adresses at a time
#Current implementation is not based on map and traffic, just airline distance. This can be improved!
urgencyDist=spDistsN1(sampleSDF@coords,depotCoords,longlat=TRUE)
#convert this to time
urgencyDist=urgencyDist/avgSpeed #in seconds
#add time since order
urgencyTime=as.POSIXct("2017/07/25 3:30:00")-orderTimes
urgencyAll=urgencyDist+urgencyTime
#Bin packing algorithm; sort by urgency!
#add urgency to df
sampleSDF@data=cbind(sampleSDF@data,urgencyAll)
sampleSDF=sampleSDF[rev(order(sampleSDF@data$urgencyAll)),]
sampleSDF@data
#calculate distmat
#create distance matrix of adresses and times
#distance based on adresses
adressDistMat=spDists(sampleSDF@coords,longlat = TRUE)
timeDistMat=outer(sampleSDF@data$orderTime,sampleSDF@data$orderTime,'-')
#sum the 2 matrices
timeDistMat=abs(timeDistMat*avgSpeed)*sqrt(2)
totalDistMat=adressDistMat+timeDistMat
#route contains route number for each delivery


#Source Optimization function
print("SOURCING OPT FUN")
source('~/pizzaprojekt/1007 - V2.0/pizzaRouting_1007.R')
#pizzaRouting returns the ROUTES from the below parameters

routes=pizzaRouting(totalDistMat,depotAdress,depotCoords,avgSpeed)
print("I GOT EM' TOUTES M8")
#every adress is matched to the route
sampleSDF@data=cbind(sampleSDF@data,routes)
#set center of map
lon=mean(sampleSDF@bbox[1,])
lat=mean(sampleSDF@bbox[2,])
#URGENCY LEVELS: time since ordering in seconds
urgencyLevels=c(0,5*60,10*60,Inf)
#add time since order to dataframe
print("MEGALOL")
sampleSDF@data=cbind(sampleSDF@data,as.POSIXct("2017/07/25 3:30:00")-sampleSDF@data$orderTime)
print(sampleSDF@data)
names(sampleSDF@data)=c(names(sampleSDF@data)[1:3],"routes","TimeSinceORder")
sampleSDF@data=mutate(sampleSDF@data,group=cut(as.double(urgencyTime),breaks = urgencyLevels,labels=c("green","yellow","red")))
#---------------------ROUTE CALCULATION---------------------
print("ROUTING")
urgencyIcons=awesomeIconList(green=makeAwesomeIcon(icon= 'flag', markerColor = 'green', iconColor = 'green'),
                      red=makeAwesomeIcon(icon= 'flag', markerColor = 'red', iconColor = 'red'),
                      yellow=makeAwesomeIcon(icon= 'flag', markerColor = 'yellow', iconColor = 'yellow'))
#now plot stuff on a map

routes_df=list()
i=1

from1 <- depotAdress

#number of routes
numRoutes=max(sampleSDF@data$routes)
routes_final=list()
#for every route
for(i in 1:numRoutes){
  Sys.sleep(1)
  #first, get route length
  #number of WAYPOINTS
  numWP=length(which(routes==i))
  #adresses
  adressList=sampleSDF$Adress[sampleSDF@data$route==i]
  print(adressList)
  print(i)
#GET A ROUTE FROM GOOGLE THANK YOU GOOGLE TOO KIND
#GOOGLE DATA STRUCTURE IS AWESOME, IT CONTAINS EVERYTHING INCLUDING DISTANCE IN TIME AND KM 
  
#GO TO FIRST POINT IN ADDRESSLIST FROM DEPOT
#I SHOULD WRITE AN ALGORITHM TO SOLVE THE TSP! A SIMPLE 3-OPT WILL DO.
Sys.sleep(1)
tempRoute <- NULL
attempt <- 1

while( is.null(tempRoute) && attempt <= 5 ) {
  attempt <- attempt + 1
  Sys.sleep(1)
  try(
    
    tempRoute <- route(from=from1, to=adressList[1], structure = 'route', mode = 'driving')
  )
} 

print(tempRoute)
routes_df[[i]]=tempRoute
#either 2 or 3 waypoints as specified by user
#we need stuff to make them able to modify this
if(numWP==2){
  Sys.sleep(1)
  print("lol1")
  tempRoute <- NULL
  attempt <- 1
  while( is.null(tempRoute) && attempt <= 5 ) {
    attempt <- attempt + 1
    Sys.sleep(1)
    try(
      
      tempRoute <- route(adressList[1], adressList[2], structure = 'route', mode = 'driving')
    )
  } 
  routes_df[[i]]=rbind(routes_df[[i]],tempRoute)
  print("lol2")
  tempRoute <- NULL
  attempt <- 1
  while( is.null(tempRoute) && attempt <= 5 ) {
    attempt <- attempt + 1
    Sys.sleep(1)
    try(
      
      tempRoute <- route(adressList[2], from1, structure = 'route', mode = 'driving')
    )
  } 
  Sys.sleep(1)
  routes_df[[i]]=rbind(routes_df[[i]],tempRoute)
  
}else{
  print("lol3")
  Sys.sleep(1)
  tempRoute <- NULL
  attempt <- 1
  while( is.null(tempRoute) && attempt <= 5 ) {
    attempt <- attempt + 1
    Sys.sleep(1)
    try(
      
      tempRoute <- route(adressList[1], from1, structure = 'route', mode = 'driving')
    )
  } 
  
  routes_df[[i]]=rbind(routes_df[[i]],tempRoute)
  
  }
#routes_df[[i]][[2]]=route(to1, to2, structure = 'route', mode = 'driving')

routes_df[[i]]$routeNum=i

}
routes_final=as.data.frame(do.call(rbind,routes_df))
#define color for each route
routes_final$color=rainbow(numRoutes)[routes_final$routeNum]
pal <- colorNumeric(c("red", "green", "blue"), 1:numRoutes)
#----------------------Creating Map -------------------------
print("MAPPING")
pizzaMap=leaflet(data=sampleSDF) %>%
  setView(lng = lon, lat = lat, zoom = 12)%>%
  addTiles()
  pizzaMap<-addAwesomeMarkers(pizzaMap,label=~as.factor(routes),icon = urgencyIcons[sampleSDF@data$group])
  for( i in 1:numRoutes){
    pizzaMap <- addPolylines(pizzaMap, lng=routes_final[routes_final$routeNum==i,]$lon,lat=routes_final[routes_final$routeNum==i,]$lat,data=routes_final[routes_final$routeNum==i,], color=pal(i))
  }


pizzaMap
return(pizzaMap)
}

mock_pizza()