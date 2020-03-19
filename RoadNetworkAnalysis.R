library(dplyr)

load("/data/route_net2.RData")

roadlist = read.csv(file("/data/road_lev_length_utf8.csv",encoding="UTF-8"),stringsAsFactors = F)
roadlist2 = c("環島東路一段","環島東路二段","環島東路三段","環島東路四段","環島東路五段",
              "瓊徑路","環島西路一段","環島西路二段","環島北路一段","環島北路二段",
              "環島北路三段","環島北路四段","伯玉路一段","伯玉路二段","伯玉路三段","伯玉路四段",
              "桃園路","環島南路一段","環島南路二段","環島南路三段","環島南路四段","環島南路五段",
              "頂林路","中山路")

route.net2$lev = ifelse((route.net2$from %in% roadlist$roadname[roadlist$lev=="主要道路"])&(route.net2$to %in% roadlist$roadname[roadlist$lev=="主要道路"]),"主要道路", #集中看涉及主要道路及次要道路部分的資料
                        ifelse((route.net2$from %in% roadlist$roadname[roadlist$lev=="主要道路"])&(route.net2$to %in% roadlist$roadname[roadlist$lev=="次要道路"]),"主要道路",
                               ifelse((route.net2$from %in% roadlist$roadname[roadlist$lev=="次要道路"])&(route.net2$to %in% roadlist$roadname[roadlist$lev=="主要道路"]),"次要道路",
                                      ifelse((route.net2$from %in% roadlist$roadname[roadlist$lev=="次要道路"])&(route.net2$to %in% roadlist$roadname[roadlist$lev=="次要道路"]),"次要道路","道路"))))

#### graph ####
library(igraph)
library(ggraph)
library('RColorBrewer')

nodes1 = route.net2
links1 = mutate(nodes1,from=from,to=to,type="direction",weight=1)

net = graph_from_data_frame(d=links1,directed = F)

g = simplify(net, edge.attr.comb=list(Weight="sum","ignore"))

pal3 <- brewer.pal(3, "Set3")

g.size = log((betweenness(g)*transitivity(g, type="local")))*1.5
g.size = ifelse(is.na(g.size),1,g.size)
g.size = abs(g.size)
g.size = ifelse(is.infinite(g.size),1,g.size)

set_graph_style(
  family = "SourceHanSansTC-Normal")

ggraph(g,layout = 'igraph', algorithm = 'fr') + #graphopt fr kk
  geom_edge_link(colour="gray",alpha = 1) +     # different edge color per group
  geom_node_point(size = g.size, shape = 21, stroke = 1,
                  fill = ifelse(attr(g.size,"names") %in% roadlist$roadname[roadlist$lev=="主要道路"],pal3[1],ifelse(attr(g.size,"names") %in% roadlist$roadname[roadlist$lev=="次要道路"],pal3[2],pal3[3])), color = 'black') + #ifelse(betweenness(g)>quantile(betweenness(g),0.20)[[1]],ifelse(betweenness(g)>=quantile(betweenness(g),0.80)[[1]],pal3[1],pal3[2]),pal3[3])
  geom_node_text(aes(label = ifelse(betweenness(g)>=quantile(betweenness(g),0.80)[[1]],name,"")),size=2) +                   # "name" is automatically generated from the node IDs in the edges
  theme_void()+
  ggtitle("金門縣道路路網關係圖")

#整體描述
length(V(g)) #nodes數量
length(E(g)) #edges數量
graph.density(g) #密度
clusters(g)$no #cluster數（就是大小金門）
transitivity(g,type = "global") #全局係數
diameter(g) #直徑
degree.distribution(g) #nodes的度數 

#兩點間關係
shortest.paths(g) #最短路徑數量表

#單點
degree(g) %>% as.data.frame() %>% data.frame(road=row.names(.),degree=.) %>% arrange((desc(.))) #nodes度數
closeness(g)%>% as.data.frame() %>% data.frame(road=row.names(.),closeness=.) %>% arrange((desc(.))) #nodes最短路徑節點密度
betweenness(g)%>% as.data.frame() %>% data.frame(road=row.names(.),betweenness=.) %>% arrange((desc(.))) #nodes最短路徑的中間性
transitivity(g, type="local")#傳遞度數
evcent(g)$vector%>% as.data.frame() %>% data.frame(road=row.names(.),betweenness=.) %>% arrange((desc(.))) #特徵向量中心點
