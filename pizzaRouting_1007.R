pizzaRouting=function(totalDistMat, depotAdress,depotCoords,avgSpeed){
  route=vector(length = nrow(totalDistMat))
  currentItem=1
  currentRoute=1
  nextItem=1
  while(is.finite(nextItem)){
    currentItem=nextItem
    #delete its distance from others
    totalDistMat[,currentItem]=Inf
    
    route[currentItem]=currentRoute
    #select the closest one to it
    print("CI")
    print(totalDistMat[currentItem,])
    if(is.finite(min(as.numeric(totalDistMat[currentItem,])))){
      print("adding")
      route[which(totalDistMat[currentItem,]==min(totalDistMat[currentItem,]))]=currentRoute}
    remain=which(route==0)
    done=which(route!=0)
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
  return(route)
}
