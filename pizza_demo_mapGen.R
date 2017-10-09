#demonstration for pizzaAssigner
#rm(list = ls())
#address
library(shiny)
library(leaflet)
library(tmap)
library(tmaptools)
#library(OpenStreetMap)
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
#This is a demonstration. With some mock data I assign orders to runners and plot the result on a leaflet.
#First, red a list of adresses from POS terminal. 
#This is the mock data. The adresses are prepared by developer.
#mock data generator function
mock_pizza_read_data=function(){
  source('~/pizzaRoutingDemo/rndTime.R')
  cimlista <- read.table("~/pizzaRoutingDemo/cimlista.csv",sep=",",fileEncoding = "UTF-8")
  #cimlista=cimlista[,1]
  print(cimlista)
  #take a sample from it
  set.seed(1)
  sampleSize=4
  sampleAddresses=as.character(cimlista[sample(nrow(cimlista),sampleSize),]$V1)
  #Genereate ordertimes in specified timeframe
  orderTimes=as.character(rndTime(sampleSize,st="2017/07/25 3:00:00",et="2017/07/25 3:30:00"))
  #combine with adresses
  sampleData=as.data.frame(cbind(sampleAddresses,orderTimes))
  colnames(sampleData)<-c("Adress","orderTime")
  sampleData$orderTime=as.POSIXct(sampleData$orderTime,origin= "1960-01-01")
  #create string of actual adress
  sampleData$Adress=as.character(sampleData$Adress)
  print(sampleData)
  return(sampleData)
}
#geocoding function
mock_pizza_geocoding=function(sampleData){
  sampleData$Adress=as.character(sampleData$Adress)
  sampleData$Adress=iconv(as.character(sampleData$Adress),'Latin1', "ASCII//TRANSLIT")
  #smart geocoding by line and tr?ing until it is successful!
  sampleData$lon=rep(0,nrow(sampleData))
  sampleData$lat=rep(0,nrow(sampleData))
  cimlista <- read.table("~/pizzaRoutingDemo/cimlista.csv",sep=",",fileEncoding = "UTF-8")
  if(ncol(cimlista)==1){
    cimlista$lat=NA
    cimlista$lon=NA
  }
  cimlista[,1]=as.character(cimlista[,1])
  cimlista[,1]=iconv(as.character(cimlista[,1]),'Latin1',"ASCII//TRANSLIT")
  print(cimlista)
  names(cimlista)=c('Adress','lat','lon')
  print('lol')
  for(i in 1:length(sampleData$Adress)){
    success=FALSE
    attempt=1
    #checking if latlon has been recorded yet
    print(which(as.character(cimlista[,1])==sampleData$Adress[i]))
    # if(!is.na(cimlista[which(as.character(cimlista[,1])==sampleData$Adress[i]),2])){
    #   sampleData$lon[i]=as.numeric(cimlista[which(cimlista[,1]==sampleData$Adress[i]),2])
    #   sampleData$lat[i]=as.numeric(cimlista[which(cimlista[,1]==sampleData$Adress[i]),3])
    #   success=TRUE
    #   print("I Found it in the database!")
    # }
    while(!success&attempt<5){
      Sys.sleep(1)
      print('Attempting to geocode location')
      geocodes=rev(geocode(as.character(sampleData$Adress[i]),source = 'google'))
      print(geocodes)
      if(!is.na(geocodes)){
        success=TRUE
        print("Successful Geocoding")
        sampleData$lon[i]=geocodes$lat
        sampleData$lat[i]=geocodes$lon
        cimlista[which(cimlista$Adress==sampleData$Adress[i]),]$lat=geocodes$lat
        cimlista[which(cimlista$Adress==sampleData$Adress[i]),]$lon=geocodes$lon
        
      }
      attempt=attempt+1
    }
  }
  print(sampleData)
  #sampleData=cbind(sampleData,rev(geocode(sampleData$Adress)))
  #convert to SpatialPointsDataframe. This is important!!!!!!! From now on no shitting with column names and stuff
  sampleSDF=SpatialPointsDataFrame(sampleData[,4:3],sampleData[,1:2])
  print(sampleSDF)
  #write the geocoords to the original csv
  #cimlista=merge(cimlista,sampleData[,c(1,4,3)],by.x="X1",by.y="Adress",all.x=TRUE)
  #write.table(cimlista,file="~/pizzaRoutingDemo/cimlista.csv",row.names=FALSE,col.names=FALSE,fileEncoding = "UTF-8")
  return(sampleSDF)
}
mock_pizza_router=function(sampleSDF,depotAdress){
  print("ROUTING")
  
  
  routes_df=list()
  i=1
  
  from1 <- depotAdress
  sleepTime=2
  #number of routes
  numRoutes=max(sampleSDF@data$routes)
  
  routes_final=list()
  #for every route
  for(i in 1:numRoutes){
    Sys.sleep(sleepTime)
    #first, get route length
    #number of WAYPOINTS
    numWP=length(which(sampleSDF@data$routes==i))
    #adresses
    adressList=sampleSDF$Adress[sampleSDF@data$route==i]
    print(adressList)
    print(i)
    #GET A ROUTE FROM GOOGLE THANK YOU GOOGLE TOO KIND
    #GOOGLE DATA STRUCTURE IS AWESOME, IT CONTAINS EVERYTHING INCLUDING DISTANCE IN TIME AND KM 
    
    #GO TO FIRST POINT IN ADDRESSLIST FROM DEPOT
    #I SHOULD WRITE AN ALGORITHM TO SOLVE THE TSP! A SIMPLE 3-OPT WILL DO.
    Sys.sleep(sleepTime)
    tempRoute <- NULL
    attempt <- 1
    maxAttempt=10
    sleepTime=2
    while( is.null(tempRoute) && attempt <= maxAttempt ) {
      attempt <- attempt + 1
      Sys.sleep(sleepTime)
      try(
        
        tempRoute <- route(from=from1, to=adressList[1], structure = 'route', mode = 'driving')
      )
    } 
    tempRoute$section=1
    print(tempRoute)
    routes_df[[i]]=tempRoute
    #either 2 or 3 waypoints as specified by user
    #we need stuff to make them able to modify this
    if(numWP==2){
      Sys.sleep(sleepTime)
      print("lol1")
      tempRoute <- NULL
      attempt <- 1
      while( is.null(tempRoute) && attempt <= 5 ) {
        attempt <- attempt + 1
        Sys.sleep(sleepTime)
        try(
          
          tempRoute <- route(adressList[1], adressList[2], structure = 'route', mode = 'driving')
        )
      } 
      tempRoute$section=2
      
      routes_df[[i]]=rbind(routes_df[[i]],tempRoute)
      print("lol2")
      tempRoute <- NULL
      attempt <- 1
      while( is.null(tempRoute) && attempt <= 5 ) {
        attempt <- attempt + 1
        Sys.sleep(sleepTime)
        try(
          
          tempRoute <- route(adressList[2], from1, structure = 'route', mode = 'driving')
        )
      } 
      Sys.sleep(sleepTime)
      tempRoute$section=3
      
      routes_df[[i]]=rbind(routes_df[[i]],tempRoute)
      
    }else{
      print("lol3")
      Sys.sleep(sleepTime)
      tempRoute <- NULL
      attempt <- 1
      while( is.null(tempRoute) && attempt <= 5 ) {
        attempt <- attempt + 1
        Sys.sleep(sleepTime)
        try(
          
          tempRoute <- route(adressList[1], from1, structure = 'route', mode = 'driving')
        )
      } 
      tempRoute$section=2
      
      routes_df[[i]]=rbind(routes_df[[i]],tempRoute)
      
    }
    #routes_df[[i]][[2]]=route(to1, to2, structure = 'route', mode = 'driving')
    
    routes_df[[i]]$routeNum=i
    
  }
  routes_final=as.data.frame(do.call(rbind,routes_df))
  #define color for each route
  routes_final$color=rainbow(numRoutes)[routes_final$routeNum]
  routes_final$sectionID=paste(routes_final$routeNum,"_",routes_final$section)
  return(routes_final)
  print(routes_final)
}

mock_piza_waitTime=function(sampleSDF,routes_final){
  library(lubridate)
  numRoutes=max(sampleSDF@data$routes)
  sampleSDF@data$startTime=NA
  sampleSDF@data$arrivalTime=NA
  prepTime=10
  #calculate the time for starting
  for(i in 1:numRoutes){
    sampleSDF@data[sampleSDF@data$routes==i,]$startTime=max(sampleSDF@data[sampleSDF@data$routes==i,]$orderTime)+prepTime*60
    
  }
  
  numAdresses=nrow(sampleSDF@data)
  sampleSDF@data$startTime=as.POSIXct(sampleSDF@data$startTime,origin=origin)
  #calculate the time of arrival
  breaks=which(is.na(routes_final$leg))
  
  sectorTimes=c()
  for(i in 1:(length(breaks))){
    if(i==1){
      sectorTimes[i]=sum(routes_final$seconds[1:breaks[i]],na.rm=TRUE)/60
      }else{
    sectorTimes[i]=sum(routes_final$seconds[breaks[i-1]:breaks[i]],na.rm=TRUE)/60
      }
    }
  print("SECTORTIMES")
  print(sectorTimes)

    for(i in 1:numAdresses){
      currentroute=sampleSDF@data$routes[i]
      if(i==1){
      sampleSDF@data$arrivalTime[i]=sampleSDF@data$startTime[i]+sum(routes_final$seconds[1:breaks[i+currentroute-1]],na.rm = TRUE)
      }else if(sampleSDF@data$routes[i-1]!=sampleSDF@data$routes[i]){
        sampleSDF@data$arrivalTime[i]=sampleSDF@data$startTime[i]+sum(routes_final$seconds[breaks[i+currentroute-1-1]:breaks[i+currentroute-1]],na.rm = TRUE)
      }else{
        sampleSDF@data$arrivalTime[i]=sampleSDF@data$arrivalTime[i-1]+sum(routes_final$seconds[breaks[i+currentroute-1-1]:breaks[i+currentroute-1]],na.rm = TRUE)
      }
    }
    
  
  sampleSDF@data$arrivalTime=as.POSIXct(sampleSDF@data$arrivalTime,origin=origin)
  sampleSDF@data$waitingTime=sampleSDF@data$arrivalTime-sampleSDF@data$orderTime
  return(sampleSDF)
}


mock_pizza_assigner=function(sampleSDF,depotCoords,depotAdress,timeConstraint,avgSpeed){
  print("STARTING ASSIGNMENT")
  #the assignment is based on URGENCY. Urgency is made up of spatial distance, and order since time.
  #If we group based on both spatial and temporal urgency ith get better results. Most pizza delivery boys do 2-4 adresses at a time
  #Current implementation is not based on map and traffic, just airline distance. This can be improved!
  urgencyDist=spDistsN1(sampleSDF@coords,depotCoords,longlat=TRUE)
  #convert this to time
  urgencyDist=urgencyDist/avgSpeed #in seconds
  #add time since order
  urgencyTime=as.POSIXct("2017/07/25 3:30:00")-sampleSDF@data$orderTime
  urgencyAll=urgencyDist+urgencyTime
  #Bin packing algorithm; sort by urgency!
  #add urgency to df
  sampleSDF@data=cbind(sampleSDF@data,urgencyAll)
  sampleSDF=sampleSDF[rev(order(sampleSDF@data$urgencyAll)),]
  #sampleSDF@data
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
  source('~/pizzaRoutingDemo/pizzaRouting_1007.R')
  #pizzaRouting returns the ROUTES from the below parameters
  
  routes=pizzaRouting(totalDistMat,depotAdress,depotCoords,avgSpeed)
  print("I GOT EM' TOUTES M8")
  print(routes)
  #every adress is matched to the route
  sampleSDF@data=cbind(sampleSDF@data,routes)
  #set center of map
  lon=mean(sampleSDF@bbox[1,])
  lat=mean(sampleSDF@bbox[2,])
  #URGENCY LEVELS: time since ordering in seconds
  urgencyLevels=c(0,5*60,10*60,Inf)
  #add time since order to dataframe
  print("MEGALOL")
  timeWaitedNow=as.POSIXct("2017/07/25 3:30:00")-sampleSDF@data$orderTime
  sampleSDF@data=cbind(sampleSDF@data,timeWaitedNow)
  print(sampleSDF@data)
  names(sampleSDF@data)=c(names(sampleSDF@data)[1:3],"routes","section","TimeSinceORder")
  sampleSDF@data=mutate(sampleSDF@data,group=cut(as.double(urgencyTime),breaks = urgencyLevels,labels=c("green","yellow","red"))) 
  return(sampleSDF)
}

mock_pizza_mapGen=function(sampleSDF,routes_final,depotAdress,depotCoords){
  print("MAPPING")
  numRoutes=max(sampleSDF@data$routes)
  pal <- colorNumeric(c("red", "green", "blue"), 1:numRoutes)
  lon=mean(sampleSDF@bbox[1,])
  lat=mean(sampleSDF@bbox[2,])
  urgencyIcons=awesomeIconList(green=makeAwesomeIcon(icon= 'flag', markerColor = 'green', iconColor = 'green'),
                               red=makeAwesomeIcon(icon= 'flag', markerColor = 'red', iconColor = 'red'),
                               yellow=makeAwesomeIcon(icon= 'flag', markerColor = 'yellow', iconColor = 'yellow'),
                               depot=makeAwesomeIcon(icon= 'flag', markerColor = 'red', iconColor = 'red'))
                               #now plot stuff on a map
  pizzaMap=leaflet(data=sampleSDF) %>%
    setView(lng = lon, lat = lat, zoom = 12)%>%
    addTiles()
  pizzaMap<-addAwesomeMarkers(pizzaMap,layerId =sampleSDF@data$Adress,  label=~as.factor(routes),icon = urgencyIcons[sampleSDF@data$group])
  numSections=length(unique(routes_final$sectionID))
  pal <- colorNumeric(c("red", "green", "blue"), 1:numRoutes)
  
  for( i in 1:numSections){
    section=unique(routes_final$sectionID)[i]
    data=
    pizzaMap <- addPolylines(pizzaMap, layerId = section, lng=routes_final[routes_final$sectionID==section,]$lon,lat=routes_final[routes_final$sectionID==section,]$lat,data=routes_final[routes_final$sectionID==section,], color=pal(routes_final[routes_final$sectionID==section,]$route[1]))
  }
  #add depot
  print('ADDING DEPOT')
  addMarkers(pizzaMap, lng = 19.1, lat = 47.5)
  addMarkers(pizzaMap, lng = 47.5, lat = 19.1)
  
  pizzaMap<-addMarkers(pizzaMap, layerId =depotAdress,lng = depotCoords[2], lat = depotCoords[1],label='DEPOT')
  return(pizzaMap)
}


mock_pizza=function(){
  print('lol')
#----------------------MOCK DATA GENERATION----------------------
  
  sampleData=mock_pizza_read_data()
#--------------GEOCODING ------------

  sampleSDF=mock_pizza_geocoding(sampleData)
  depotAdress="Budapest IX. kerulet , Viola u. 48"
  depotCoords=rev(geocode_OSM(depotAdress)[[2]])
#now solve the bin packing problem
timeConstraint=60*60
#convert time to km
#v=s/t
avgSpeed=20/3600
#------------------ASSIGNMENT-------------------------

sampleSDF=mock_pizza_assigner(sampleSDF,depotCoords,depotAdress,timeConstraint,avgSpeed)
#---------------------ROUTE CALCULATION---------------------
sleepTime=1
#number of routes
numRoutes=max(sampleSDF@data$routes)
routes_final=mock_pizza_router(sampleSDF,depotAdress)
pal <- colorNumeric(c("red", "green", "blue"), 1:numRoutes)
#----------------------Creating Map -------------------------
pizzaMap=mock_pizza_mapGen(sampleSDF,routes_final)

pizzaMap
return(pizzaMap)
}

#mock_pizza()