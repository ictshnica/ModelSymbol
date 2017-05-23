##############################################
# Example for the SIENA Summer School 2016 at HSE Moscow
# Fitting a SIENA model to one school class of the Mannheim data
#
# Testing structural network effects and
# homophily on 'gender' and 'country of origin'
# in the first class room
#
# 1. read data from files
# 1.5 calculate network descriptives
# 2. create SIENA objects
# 3. specify SIENA model
# 4. create algorithm
# 5. estimate SIENA model
#
# The data used are from the Children of Immigrants Study,
# (c) MZES Mannheim, Manfred Kalter
# Please do not use and distribute outside the course
#
# Script by Christoph Stadtfeld
##############################################

# loading necessary packages
library("RSiena")
library("igraph")

#############################################
# 1. read data from files and prepare raw data

friendship.t1 <- as.matrix(read.table(file = "mannheim_data/friendship.network.t1.dat"))
friendship.t2 <- as.matrix(read.table(file = "mannheim_data/friendship.network.t2.dat"))
gender <- unlist(read.table(file = "mannheim_data/gender.dat"))
coo <- unlist(read.table(file = "mannheim_data/coo.dat"))
nActors <- nrow(friendship.t1)

# define a network cut-off
threshold <- 1
friendship.t1[friendship.t1 >= threshold] <- 1
friendship.t2[friendship.t2 >= threshold] <- 1


################################################
# 1.5 Decribe the data
# Plotting the networks without attributes

graph1 <- graph.adjacency(friendship.t1)
graph2 <- graph.adjacency(friendship.t2)
graph12 <- graph.adjacency(friendship.t1 + friendship.t2)
set.seed(12345)
myLayout <- layout.fruchterman.reingold(graph12)

par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = "darkblue",
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.02,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = "darkblue",
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.02,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")

################################################
# Plotting the networks with gender attribute

par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = ifelse(gender == 1, "pink", "darkblue"),
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.02,
     vertex.size = 10,
     vertex.label = NA,
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = ifelse(gender == 1, "pink", "darkblue"),
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.02,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")

################################################
# Plotting the networks with ethnical homophily
par(mfrow = c(1, 2))
plot(graph1,
     vertex.color = coo,
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.color = "black",
     edge.width = 2,
     edge.arrow.size = 0.02,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 1")
plot(graph2,
     vertex.color = coo,
     vertex.shape = ifelse(gender == 1, "square", "circle"),
     edge.width = 2,
     edge.color = "black",
     edge.arrow.size = 0.02,
     vertex.size = 10,
     vertex.label = "",
     layout = myLayout,
     main = "Network wave 2")


##########################################
# 2. create internal SIENA objects

# create dependent network variable
friendship.array <- array (c(friendship.t1, friendship.t2), dim = c(nActors, nActors, 2) ) 
friendship.dependent <- sienaDependent(
  friendship.array )

# create constant actor covariates
coo.coCovar <- coCovar(coo, centered = F)
gender.coCovar <- coCovar(gender)

mySienaData <- sienaDataCreate(friendship.dependent,
                               coo.coCovar,
                               gender.coCovar)



##############################################
# 3. Specify SIENA model

mySienaEffects <- getEffects(mySienaData)

# print report to check
print01Report( mySienaData,
                       modelname = 'mannheim_network_1' )

# get an overview of available effects
effectsDocumentation(mySienaEffects)

# network effects
mySienaEffects <- includeEffects(mySienaEffects, transTrip)
mySienaEffects <- includeEffects(mySienaEffects, transRecTrip)
mySienaEffects <- includeEffects(mySienaEffects, inPop, inAct)

# homophily effects and ego alter control
mySienaEffects <- includeEffects(mySienaEffects, egoX, altX, 
                                 sameX, interaction1 = "gender.coCovar")
mySienaEffects <- includeEffects(mySienaEffects, sameX, 
                                 interaction1 = "coo.coCovar")

mySienaEffects # check parameters before estimation


##########################################
# 4. Create SIENA algorithm

mySienaAlgorithm <- sienaAlgorithmCreate(projname = "ethnical_segregation",
                                         MaxDegree = c(friendship.dependent = 5))

##########################################
# 5. Estimate

result <- siena07(mySienaAlgorithm,
                  data = mySienaData,
                  effects = mySienaEffects)

result

print(xtable(result, type = "html", file = "results.html"))
print(xtable(result, type = "latex", file = "results.tex"))



jaccardIndex <- function(m1, m2){
  nStableTies <- sum((m1+m2)>1) # in each wave
  nChangingTies <- sum((m1+m2)==1) # in either wave, not both
  return (nStableTies / (nStableTies + nChangingTies))
}

gr1<- as.data.frame(get.edgelist(ggT1))
gr2<- as.data.frame(get.edgelist(ggT2))
gr3<- as.data.frame(get.edgelist(ggT3))
gr4<- as.data.frame(get.edgelist(ggT4))
gr5<- as.data.frame(get.edgelist(ggT5))
gr6<- as.data.frame(get.edgelist(ggT6))
write.csv(gr1,"gr1.csv")
write.csv(gr2,"gr2.csv")
write.csv(gr3,"gr3.csv")
write.csv(gr4,"gr4.csv")
write.csv(gr5,"gr5.csv")
write.csv(gr6,"gr6.csv")
as=get.adjacency(ggT1,sparse=FALSE)
as=as.data.frame(as)
ad<- as.data.frame(get.edgelist(ggT2))
ad=get.adjacency(ggT2,sparse=FALSE)
ad=as.data.frame(ad)
jaccardIndex(as, ad)

wgr1=gr1
library(dplyr)
wgr1=wgr1 %>% group_by(V1,V2) %>% mutate(co = n())
ao1= rbind(gr2,gr3,gr4,gr5,gr6)
ao1$co=0
wgr1=as.data.frame(wgr1)
wgr1=rbind(wgr1,ao1)
hm=as.data.frame(duplicated(wgr1))
wgr1=wgr1 %>% group_by(V1,V2) %>% summarise(co2 = sum(co))
wgr1$V1=sub('\\-.*', '', wgr1$V1)
wgr1$V2=sub('\\-.*', '', wgr1$V2)

wgr2=gr2
wgr2=wgr2 %>% group_by(V1,V2) %>% mutate(co = n())
ao2= rbind(gr1,gr3,gr4,gr5,gr6)
ao2$co=0
wgr2=as.data.frame(wgr2)
wgr2=rbind(wgr2,ao2)
hm=as.data.frame(duplicated(wgr1))
wgr2=wgr2 %>% group_by(V1,V2) %>% summarise(co2 = sum(co))
wgr2$V1=sub('\\-.*', '', wgr2$V1)
wgr2$V2=sub('\\-.*', '', wgr2$V2)


wgr3=gr3
wgr3=wgr3 %>% group_by(V1,V2) %>% mutate(co = n())
ao3= rbind(gr1,gr2,gr4,gr5,gr6)
ao3$co=0
wgr3=as.data.frame(wgr3)
wgr3=rbind(wgr3,ao3)
wgr3=wgr3 %>% group_by(V1,V2) %>% summarise(co2 = sum(co))
wgr3$V1=sub('\\-.*', '', wgr3$V1)
wgr3$V2=sub('\\-.*', '', wgr3$V2)

wgr4=gr4
wgr4=wgr4 %>% group_by(V1,V2) %>% mutate(co = n())
ao4= rbind(gr1,gr2,gr3,gr5,gr6)
ao4$co=0
wgr4=as.data.frame(wgr4)
wgr4=rbind(wgr4,ao4)
wgr4=wgr4 %>% group_by(V1,V2) %>% summarise(co2 = sum(co))
wgr4$V1=sub('\\-.*', '', wgr4$V1)
wgr4$V2=sub('\\-.*', '', wgr4$V2)

wgr5=gr5
wgr5=wgr5 %>% group_by(V1,V2) %>% mutate(co = n())
ao5= rbind(gr1,gr2,gr3,gr4,gr6)
ao5$co=0
wgr5=as.data.frame(wgr5)
wgr5=rbind(wgr5,ao5)
wgr5=wgr5 %>% group_by(V1,V2) %>% summarise(co2 = sum(co))
wgr5$V1=sub('\\-.*', '', wgr5$V1)
wgr5$V2=sub('\\-.*', '', wgr5$V2)


wgr6=gr6
wgr6=wgr6 %>% group_by(V1,V2) %>% mutate(co = n())
ao6= rbind(gr1,gr2,gr3,gr4,gr5)
ao6$co=0
wgr6=as.data.frame(wgr6)
wgr6=rbind(wgr6,ao6)
wgr6=wgr6 %>% group_by(V1,V2) %>% summarise(co2 = sum(co))
wgr6$V1=sub('\\-.*', '', wgr6$V1)
wgr6$V2=sub('\\-.*', '', wgr6$V2)


library(reshape2)
mat1=dcast(wgr1, V1 ~ V2, value.var = "co2",fill = 0)
mat2=dcast(wgr2, V1 ~ V2, value.var = "co2",fill = 0)
mat3=dcast(wgr3, V1 ~ V2, value.var = "co2",fill = 0)
mat4=dcast(wgr4, V1 ~ V2, value.var = "co2",fill = 0)
mat5=dcast(wgr5, V1 ~ V2, value.var = "co2",fill = 0)
mat6=dcast(wgr6, V1 ~ V2, value.var = "co2",fill = 0)

write.csv(wgr1,"wgr1.csv")
write.csv(wgr2,"wgr2.csv")
write.csv(wgr3,"wgr3.csv")
write.csv(wgr4,"wgr4.csv")
write.csv(wgr5,"wgr5.csv")
write.csv(wgr6,"wgr6.csv")

write.csv(mat1,"mat1.csv")
write.csv(mat2,"mat2.csv")
write.csv(mat3,"mat3.csv")
write.csv(mat4,"mat4.csv")
write.csv(mat5,"mat5.csv")
write.csv(mat6,"mat6.csv")

wgr1$V1=as.character(wgr1$V1)
wgr1$V2=as.character(wgr1$V2)
kn = data.frame(unique(wgr1$V1))
kn2 = data.frame(unique(wgr1$V2))
kn$unique.wgr1.V1.=as.character(kn$unique.wgr1.V1.)
kn2$unique.wgr1.V2.=as.character(kn2$unique.wgr1.V2.)
colnames(kn)="brand"
colnames(kn2)="brand"
knn=rbind(kn,kn2)
knn=data.frame(unique(knn))
