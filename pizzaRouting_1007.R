pizzaRouting=function(totalDistMat, depotAdress,depotCoords,avgSpeed){
  route=data.frame(route=rep(0,nrow(totalDistMat)),section=rep(0,nrow(totalDistMat)))
  currentItem=1
  currentRoute=1
  nextItem=1
  while(is.finite(nextItem)){
    currentItem=nextItem
    #delete its distance from others
    totalDistMat[,currentItem]=Inf
    
    route$route[currentItem]=currentRoute
    #select the closest one to it
    #print("CI")
    print(totalDistMat[currentItem,])
    if(is.finite(min(as.numeric(totalDistMat[currentItem,])))){
      print("adding")
      route$route[which(totalDistMat[currentItem,]==min(totalDistMat[currentItem,]))]=currentRoute}
    remain=which(route$route==0)
    print(remain)
    done=which(route$route!=0)
    print(done)
    nextItem=min(remain)
    totalDistMat[,done]=Inf
    currentRoute=currentRoute+1
    if(is.infinite(nextItem)){
      print("lol it ovah m8")
    }
    print("Second")
    print("Currentitem")
    print(currentItem)
    print("Currentroute")
    print(currentRoute)
    print("Route")
    print(route)
    
  }
  #now set the order in which the adresses come!
  for(i in 1:nrow(route)){
    tempTable=table(route$route[1:i])
    route$section[i]=tempTable[names(tempTable)==route$route[i]]
  }
  print("Route with Section!")
  print(route)
  
  return(route)
}
