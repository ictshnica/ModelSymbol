library(jsonlite)
library(stringr)
library(dplyr)
j = readLines("~/ModelSymbol/models.jsonlines") %>% 
  str_c(collapse = ",") %>%  
  (function(str) str_c("[", str, "]")) %>% 
  fromJSON(simplifyDataFrame = T)
cov = j %>% select(name, covers)
#cov=data_frame(unlist(cov$covers))

cov2 = cov[sapply(1:nrow(cov), function(x){ length(cov$covers[[x]]) != 0 }),]

result = data.frame() 
for (i in 1:nrow(cov2)){ 
  
  a = data.frame(model=cov2$name[i], book2= cov2$covers[[i]]) 
  
  result= plyr::rbind.fill(result, a) 
}
write.csv(result,"models.csv")
result=read.csv("models.csv",header = T)
result$book3=result$book2
library(stringr)
result$book2=str_replace(result$book2,"/magazines/","")
result$book2=sub('\\/.*', '', result$book2)
result$book3=str_replace(result$book3,"/magazines/","")
result$book3=sub('.*\\/covers/', '', result$book3)
result$book3=substr(result$book3,1,nchar(result$book3)-1) 
result$book3=sub('\\/.*', '', result$book3)
result$book3=str_sub(result$book3, start= -4)
write.csv(result,"models-covers-years.csv")
library(ggplot2)
ggplot(result, aes(factor(book3)))+ geom_bar()+ theme(axis.text.x = element_text(angle=60, hjust=1))
mdcy = read.csv("models-covers-years.csv", header = T)
mdcy$X.1=NULL
mdcy$X=NULL
ggplot(mdcy, aes(factor(book3)))+ geom_bar()+ theme(axis.text.x = element_text(angle=60, hjust=1))
library(dplyr)
mags = mdcy %>% group_by(book2) %>% summarize(counts=n())
mdcy=left_join(mdcy,mags)
mdcy$counts=as.numeric(mdcy$counts)
mdcyclean=mdcy[mdcy$counts>15, ]
mods = mdcy %>% group_by(model) %>% summarize(counts=n())
mdcyclean$counts=NULL
mdcyclean=left_join(mdcyclean,mods)
mdcycc2=mdcyclean[mdcyclean$counts>1, ]
write.csv(mdcycc2, "cuttedcovers.csv")
ggplot(mdcycc2, aes(factor(book3)))+ geom_bar()+ theme(axis.text.x = element_text(angle=60, hjust=1))
mdcycc2$counts=NULL
cov7599 = mdcycc2 %>% filter(book3<2000&book3>1975)
cov0005 = mdcycc2 %>% filter(book3<2006&book3>1999)
cov0610 = mdcycc2 %>% filter(book3<2011&book3>2006)
cov1112 = mdcycc2 %>% filter(book3<2013&book3>2010)
cov1314 = mdcycc2 %>% filter(book3<2015&book3>2012)
cov1517 = mdcycc2 %>% filter(book3<2018&book3>2014)
write.csv(cov7599, "cov7599.csv")
write.csv(cov0005, "cov0005.csv")
write.csv(cov0610, "cov0610.csv")
write.csv(cov1112, "cov1112.csv")
write.csv(cov1314, "cov1314.csv")
write.csv(cov1517, "cov1517.csv")
library(igraph)
layout=layout.kamada.kawai(gT1+gT2)
                          +gT3+gT4+gT5+gT6)
cov7599$book2=sub('\\-.*', '', cov7599$book2)
edges1 = data.frame(from=cov7599$model, to=cov7599$book2)
models1 <- data.frame(name = unique(cov7599$model), type=F)
covs1 <- data.frame(name = unique(cov7599$book2), type=T)
nodes1 <- rbind(models1,covs1)
gT1 <- graph_from_data_frame(edges1, directed=FALSE, vertices=nodes1)
V(gT1)$color <- ifelse(V(gT1)$type == TRUE, "lightblue", "red")
pr = bipartite.projection(gT1)
ggT1 <- pr[[-1]]
asinbyrelated <- pr[[1]]
plot(ggT1, vertex.size = 3, vertex.label = NA, layout=layout.kamada.kawai)
q=degree(ggT1)
V()

cov0005$book2=sub('\\-.*', '', cov0005$book2)
edges2 = data.frame(from=cov0005$model, to=cov0005$book2)
models2 <- data.frame(name = unique(cov0005$model), type=F)
covs2 <- data.frame(name = unique(cov0005$book2), type=T)
nodes2 <- rbind(models2,covs2)
gT2 <- graph_from_data_frame(edges2, directed=FALSE, vertices=nodes2)
V(gT2)$color <- ifelse(V(gT2)$type == TRUE, "lightblue", "red")
pr = bipartite.projection(gT2)
ggT2 <- pr[[-1]]
asinbyrelated <- pr[[1]]
plot(ggT2, vertex.size = 3, vertex.label = NA, layout=layout.kamada.kawai)


cov0610$book2=sub('\\-.*', '', cov0610$book2)
edges3 = data.frame(from=cov0610$model, to=cov0610$book2)
models3 <- data.frame(name = unique(cov0610$model), type=F)
covs3 <- data.frame(name = unique(cov0610$book2), type=T)
nodes3 <- rbind(models3,covs3)
gT3 <- graph_from_data_frame(edges3, directed=FALSE, vertices=nodes3)
V(gT3)$color <- ifelse(V(gT3)$type == TRUE, "lightblue", "red")
pr = bipartite.projection(gT3)
ggT3 <- pr[[-1]]
asinbyrelated <- pr[[1]]
plot(ggT3, vertex.size = 3, vertex.label = NA, layout=layout.kamada.kawai)


cov1112$book2=sub('\\-.*', '', cov1112$book2)
edges4 = data.frame(from=cov1112$model, to=cov1112$book2)
models4 <- data.frame(name = unique(cov1112$model), type=F)
covs4 <- data.frame(name = unique(cov1112$book2), type=T)
nodes4 <- rbind(models4,covs4)
gT4 <- graph_from_data_frame(edges4, directed=FALSE, vertices=nodes4)
V(gT4)$color <- ifelse(V(gT4)$type == TRUE, "lightblue", "red")
pr = bipartite.projection(gT4)
ggT4 <- pr[[-1]]
asinbyrelated <- pr[[1]]
plot(ggT4, vertex.size = 3, vertex.label = NA, layout=layout.kamada.kawai)


cov1314$book2=sub('\\-.*', '', cov1314$book2)
edges5 = data.frame(from=cov1314$model, to=cov1314$book2)
models5 <- data.frame(name = unique(cov1314$model), type=F)
covs5 <- data.frame(name = unique(cov1314$book2), type=T)
nodes5 <- rbind(models5,covs5)
gT5 <- graph_from_data_frame(edges5, directed=FALSE, vertices=nodes5)
V(gT5)$color <- ifelse(V(gT5)$type == TRUE, "lightblue", "red")
pr = bipartite.projection(gT5)
ggT5 <- pr[[-1]]
asinbyrelated <- pr[[1]]
plot(ggT5, vertex.size = 3, vertex.label = NA, layout=layout.kamada.kawai)


cov1517$book2=sub('\\-.*', '', cov1517$book2)
edges6 = data.frame(from=cov1517$model, to=cov1517$book2)
models6 <- data.frame(name = unique(cov1517$model), type=F)
covs6 <- data.frame(name = unique(cov1517$book2), type=T)
nodes6 <- rbind(models6,covs6)
gT6 <- graph_from_data_frame(edges6, directed=FALSE, vertices=nodes6)
V(gT6)$color <- ifelse(V(gT6)$type == TRUE, "lightblue", "red")
pr = bipartite.projection(gT6)
ggT6 <- pr[[-1]]
asinbyrelated <- pr[[1]]
plot(ggT6, vertex.size = 3, vertex.label = NA, layout=layout.kamada.kawai)
q

allcov = mdcycc2
allcov$book2=sub('\\-.*', '', allcov$book2)
unmag= unique(allcov$book2)
unmag=as.data.frame(unmag)
write
#Trial version of cutting regions
edges0 = data.frame(from=p$model, to=p$book2)
models0 <- data.frame(name = unique(p$model), type=F)
covs0 <- data.frame(name = unique(p$book2), type=T)
nodes0 <- rbind(models0,covs0)
gT0 <- graph_from_data_frame(edges0, directed=FALSE, vertices=nodes0)
V(gT0)$color <- ifelse(V(gT0)$type == TRUE, "lightblue", "red")
plot(gT0, vertex.size = 3, vertex.label = NA, layout=layout.kamada.kawai)


library(psych)

mag_attr=maattr
magat1=mag_attr
ed1 = data.frame(from=wgr1$V1, to=wgr1$V2)
fg1 <- graph_from_data_frame(ed1, directed=FALSE, vertices=magat1$id)
E(fg1)$weight = wgr1$co2
magat1$degree = degree(fg1)
magat1$closeness = closeness(fg1)
magat1$betweenness = betweenness(fg1)
magat1$live = 1999-magat1$'year start'

fa.parallel(magat1[,3:6], n.obs=106, fa="both", n.iter=100)
fa(magat1[,3:6], nfactors=1, rotate="varimax", fm="ml")
fps=factanal(magat1[,3:6], 1, rotation = "varimax",scores="regression")
print(fps, digits=2, cutoff=.3, sort=TRUE)
factor.plot(fps)
fa.diagram(fps$loadings)
magat1$prestige = (0.8*magat1$circulation+0.7*magat1$countries+0.6*magat1$live+magat1$`price eur`)/4

magat2=mag_attr
ed2 = data.frame(from=wgr2$V1, to=wgr2$V2)
fg2 <- graph_from_data_frame(ed2, directed=FALSE, vertices=magat2$id)
magat2$degree = degree(fg2)
magat2$closeness = closeness(fg2)
magat2$betweenness = betweenness(fg2)
E(fg2)$weight = wgr2$co2
magat2$live = 2005-magat2$'year start'

fa.parallel(magat2[,3:7], n.obs=106, fa="both", n.iter=100)
fa(magat2[,3:7], nfactors=1, rotate="varimax", fm="ml")
fps=factanal(magat2[,3:7], 1, rotation = "varimax",scores="regression")
print(fps, digits=2, cutoff=.3, sort=TRUE)
factor.plot(fps)
fa.diagram(fps$loadings)
magat2$prestige = (0.9*magat2$circulation+0.7*magat2$countries+0.6*magat2$live+0.4*magat2$count+magat1$`price eur`)/5

magat3=mag_attr
ed3 = data.frame(from=wgr3$V1, to=wgr3$V2)
fg3 <- graph_from_data_frame(ed3, directed=FALSE, vertices=magat3$id)
magat3$degree = degree(fg3)
magat3$closeness = closeness(fg3)
magat3$betweenness = betweenness(fg3)
E(fg3)$weight = wgr3$co2
magat3$live = 2010-magat3$'year start'

fa.parallel(magat3[,3:7], n.obs=106, fa="both", n.iter=100)
fa(magat3[,3:7], nfactors=1, rotate="varimax", fm="ml")
fps=factanal(magat3[,3:7], 1, rotation = "varimax",scores="regression")
print(fps, digits=2, cutoff=.3, sort=TRUE)
factor.plot(fps)
fa.diagram(fps$loadings)
magat3$prestige = (0.9*magat3$circulation+0.7*magat3$countries+0.6*magat3$live+0.4*magat3$count+magat4$`price eur`)/5



magat4=mag_attr
ed4 = data.frame(from=wgr4$V1, to=wgr4$V2)
fg4 <- graph_from_data_frame(ed4, directed=FALSE, vertices=magat4$id)
magat4$degree = degree(fg4)
magat4$closeness = closeness(fg4)
magat4$betweenness = betweenness(fg4)
E(fg4)$weight = wgr4$co2
magat4$live = 2012-magat4$'year start'

fa.parallel(magat4[,3:7], n.obs=106, fa="both", n.iter=100)
fa(magat4[,3:7], nfactors=1, rotate="varimax", fm="ml")
fps=factanal(magat4[,3:7], 1, rotation = "varimax",scores="regression")
print(fps, digits=2, cutoff=.3, sort=TRUE)
factor.plot(fps)
fa.diagram(fps$loadings)
magat4$prestige = (0.9*magat4$circulation+0.7*magat4$countries+0.6*magat4$live+0.4*magat4$count+magat4$`price eur`)/5
magat5$prestige = (0.9*magat5$circulation+0.7*magat5$countries+0.6*magat5$live+0.4*magat5$count+magat5$`price eur`)/5
magat6$prestige = (0.9*magat6$circulation+0.7*magat6$countries+0.6*magat6$live+0.4*magat6$count+magat6$`price eur`)/5



magat5=mag_attr
ed5 = data.frame(from=wgr5$V1, to=wgr5$V2)
fg5 <- graph_from_data_frame(ed5, directed=FALSE, vertices=magat5$id)
magat5$degree = degree(fg5)
magat5$closeness = closeness(fg5)
magat5$betweenness = betweenness(fg5)
E(fg5)$weight = wgr5$co2
magat5$live = 2014-magat5$'year start'


magat6=mag_attr
ed6 = data.frame(from=wgr6$V1, to=wgr6$V2)
fg6 <- graph_from_data_frame(ed6, directed=FALSE, vertices=magat6$id)
magat6$degree = degree(fg6)
magat6$closeness = closeness(fg6)
magat6$betweenness 
magat6$live = 2017-magat6$'year start'

gr6$V1=sub('\\-.*', '', gr6$V1)
