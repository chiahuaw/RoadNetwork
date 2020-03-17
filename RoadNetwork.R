library(dplyr)
library(ggplot2)

set.seed(19987)

thm <- function() {
  theme_gray(base_family = "STHeiti Light") +
    theme(text=element_text(size=10))} 

load("/data/RandomRoadData1_drive_population.RData")

RandomRoadData = RandomRoadData1 ;rm(RandomRoadData1)

route.net = data.frame(stringsAsFactors = F)

for (i in 1:length(RandomRoadData)) { #取出道路路名
  Temp = RandomRoadData[[i]]$routes[[1]]$legs[[1]]$steps
  
  for (d in 1:length(Temp)) {
    if (grepl("[\u4e00-\u9fa5]",Temp[[d]]$html_instructions)) {
      route.net = rbind(route.net,
                        cbind(i=i,road=Temp[[d]]$html_instructions))
    }
  }
  rm(Temp)
  if (i%%1000==0) {print(paste(i,Sys.time()))}
}

route.net$road = as.character(route.net$road)
route.net$i = as.character(route.net$i)

save(route.net,file = "/data/route_net.RData") #儲存結果
rm(RandomRoadData)

Temp = data.frame(stringsAsFactors = F)
route.uni = unique(route.net$i)

for (d in 1:length(route.uni)) { #以每一次交通模擬，整理通行道路的順序
  
  Temp2 = filter(route.net,i==route.uni[d]) 
  
  if (nrow(Temp2)>0) {
    
    for (da in 1:nrow((Temp2))) {
      
      if (!grepl("<b>+[\u4e00-\u9fa5]+</b>",Temp2$road[da])) {next}
      
      Temp2b = regmatches(Temp2$road[da],gregexpr("<b>+[\u4e00-\u9fa5]+</b>",Temp2$road[da]))[[1]]
      
      for (db in 1:length(Temp2b)) {
        Temp = rbind(Temp,cbind(i=route.uni[d],
                                road = Temp2b[db]))
      }
    }
    
  }
  if (d%%1000==0) {print(paste(d,Sys.time()))}
}



route.net = Temp ; rm(Temp)

route.net$road = gsub("<b>","",route.net$road)
route.net$road = gsub("</b>","",route.net$road)

save(route.net,file = "/data/route_net.RData") #i儲存結果

route.net = unique(route.net) #去除重覆資料。例如從「桃園路到桃園路」的狀況。
route.net2 = data.frame(stringsAsFactors = F)

for (t in 1:length(unique(route.net$i))) { #整理資料為from to格式
  Temp = route.net[route.net$i==unique(route.net$i)[t],]

  if (nrow(Temp)>=2) {
    for (d in 1:nrow(Temp)) {
      if (d<nrow(Temp)) {
        route.net2 = rbind(route.net2,cbind(from=Temp$road[d],to=Temp$road[d+1]))
      }
    }

    #rm(Temp)
    }
  if (nrow(Temp)<=1) {
    rm(Temp)
    next
  }
  
  if (t%%1000==0) {print(paste(t,Sys.time()))}

}

route.net2$from = as.character(route.net2$from)
route.net2$to = as.character(route.net2$to)

for (i in 1:nrow(route.net2)) { #清理同路名所造成的問題。Google Maps 上的路名，「下莊中興路」和「中興路」都是「中興路」。
  if (grepl("中興路",route.net2$from[i])) {
   if (route.net2$to[i]=="黃海路") {route.net2$from[i]="下莊中興路" }
   if (route.net2$to[i]=="光武路") {route.net2$from[i]="下莊中興路" }
   if (route.net2$to[i]=="太湖路三段1巷") {route.net2$from[i]="下莊中興路" }
   if (route.net2$to[i]=="士校路") {route.net2$from[i]="下莊中興路" }
   if (route.net2$to[i]=="太湖路一段") {route.net2$from[i]="下莊中興路" }
   if (route.net2$to[i]=="環島東路二段") {route.net2$from[i]="下莊中興路" }
   if (route.net2$to[i]=="市港路") {route.net2$from[i]="下莊中興路" }
    if (route.net2$to[i]=="復興路一段") {route.net2$from[i]="下莊中興路" }
    if (route.net2$to[i]=="環島南路五段") {route.net2$from[i]="下莊中興路" }
    if (route.net2$to[i]=="自強路") {route.net2$from[i]="下莊中興路" }
    if (route.net2$to[i]=="中正路") {route.net2$from[i]="下莊中興路" }
  }
  
  if (grepl("中興路",route.net2$to[i])) {
    if (route.net2$from[i]=="黃海路") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="光武路") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="太湖路三段1巷") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="士校路") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="太湖路一段") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="環島東路二段") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="市港路") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="復興路一段") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="環島南路五段") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="自強路") {route.net2$to[i]="下莊中興路" }
    if (route.net2$from[i]=="中正路") {route.net2$to[i]="下莊中興路" }
  }
  
}

save(route.net2,file = "/data/route_net2.RData")
