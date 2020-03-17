library(ggmap)
library(dplyr)
library(magrittr)
library(data.table)
library(ggplot2)
library(geosphere)

km<-read.csv("/data/kinmen_randomPoint.csv",stringsAsFactor=F) 

names(km) = c("x","y","id","countyID","townID","villageID","VName","TName","CName","TWD97x","TWD97y")

fun <- function(){ #輸入Google Maps API key (GoogleMapsDirection)
  x <- readline("Pleas input API Key:")  
  
  x <- as.character(x)
  
  out1 <- register_google(key=x)
  
  return(out1)
  
}

fun()

#### driving ####
RandomRoadData1 = list() #開車
RandomRoadData2 = list() #大眾運輸或走路
set.seed(19987)
while (length(RandomRoadData1)<10000) { #nrow(km)
  
  d=0
  while (d<1000) {
    
    target = sample(nrow(km),2,replace = F)

    x1 = km$x[target[1]]
    y1 = km$y[target[1]]
    x2 = km$x[target[2]]
    y2 = km$y[target[2]]

    d = distm(c(x1,y1),c(x2,y2),fun=distHaversine)[1]

  }
  
  t1 = km$TName[target[1]]
  t2 = km$TName[target[2]]
  
  r.model = "driving"
  r.plustime = F
  
  
  
  
  if (any(grepl("烈嶼鄉",t1),grepl("烈嶼鄉",t2))) { #處理烈嶼鄉至其他鄉鎮，避免Google Maps API回應無路徑存在。

    if (t1 == "烈嶼鄉" & t2!="烈嶼鄉") { #起點改成從水頭碼頭出發
      x1 = 118.286501
      y1 = 24.415096
      r.plustime = T
    }

    if (t2 == "烈嶼鄉" & t1 != "烈嶼鄉") { #終點改成到水頭碼頭
      x2 = 118.286501
      y2 = 24.415096
      r.plustime = T
    }
  }
  
  temp1<-route(from=paste(y1,x1,sep=","),
              to=paste(y2,x2,sep=","),
              mode=r.model,structure = "route",output = "all") #,key=""
  Sys.sleep(1)
  
  
  
  temp2 = tryCatch(
    {
      route(from=paste(y1,x1,sep=","),
                  to=paste(y2,x2,sep=","),
                  mode="transit",structure = "route",output = "all")
      }, warning = function(w) {
          return({route(from=paste(y1,x1,sep=","),
                        to=paste(y2,x2,sep=","),
                        mode="walking",structure = "route",output = "all")})
      },error = function(e) {
        return(list(status="NO"))
      }
  )
  
  if (temp1$status=="OK") {
    
    if (r.plustime==T) { #增加烈嶼鄉的船運及陸運時間
      temp1$routes[[1]]$legs[[1]]$duration$value = temp1$routes[[1]]$legs[[1]]$duration$value+1800
    }
    temp1$target = target
    temp1$d=d
    RandomRoadData1[[length(RandomRoadData1)+1]] = temp1
    
  } else {
    Sys.sleep(1)
    next
  }
  
  if (temp2$status=="OK") {
    
    if (r.plustime==T) { #增加烈嶼鄉的船運及陸運時間
      temp2$routes[[1]]$legs[[1]]$duration$value = temp2$routes[[1]]$legs[[1]]$duration$value+1800
    }
    temp2$target = target
    temp2$d = d
    RandomRoadData2[[length(RandomRoadData2)+1]] = temp2
    
  } else {
    Sys.sleep(1)
    next
  }
  
  
  if (length(RandomRoadData1)%%100==0) { #固定存檔，避免中斷造成全部要重來的情況
    print(paste("run at ",length(RandomRoadData1)))
    save(RandomRoadData1,file="/data/RandomRoadData1_drive_population.RData")
    save(RandomRoadData2,file="/data/RandomRoadData2_transit_population.RData")
    #if (routeQueryCheck()<=100) {Sys.sleep(86450)}
   # if (routeQueryCheck()<=100) {break}
  }
  
  wait<-1 
  print(paste(length(RandomRoadData1),"of",10000,"wait",wait,"sec..."))
  Sys.sleep(wait)
  
  rm(temp1)
  rm(temp2)
  rm(target)
  rm(x1)
  rm(x2)
  rm(y1)
  rm(y2)
  rm(d)
  #rm(t1)
  #rm(t2)
  rm(r.model)
  rm(r.plustime)
}
save(RandomRoadData1,file="/data/RandomRoadData1_drive_population.RData")
save(RandomRoadData2,file="/data/RandomRoadData2_transit_population.RData")

Sys.time()
