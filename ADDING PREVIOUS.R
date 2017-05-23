cov7599$book2=sub('\\-.*', '', cov7599$book2)
cov7599$X1=NULL
cov7599$book3=NULL
cov0005$book2=sub('\\-.*', '', cov0005$book2)
cov0005$X1=NULL
cov0005$book3=NULL
cov0610$book2=sub('\\-.*', '', cov0610$book2)
cov0610$X1=NULL
cov0610$book3=NULL
cov1112$book2=sub('\\-.*', '', cov1112$book2)
cov1112$X1=NULL
cov1112$book3=NULL
cov1314$book2=sub('\\-.*', '', cov1314$book2)
cov1314$X1=NULL
cov1314$book3=NULL
cov1517$book2=sub('\\-.*', '', cov1517$book2)
cov1517$X1=NULL
cov1517$book3=NULL

library(dplyr)
diff2 <- (cov7599$model %in% cov0005$model )
in2 <- cov7599[diff2,]
samej2 = in2 %>% group_by(book2) %>% mutate(count=n())
samej2$model=NULL
samej2=unique(samej2)
magat2$id=as.character(magat2$id)
samej2$book2=as.character(samej2$book2)
magat2= merge(x = magat2, y = samej2, by.x = "id", by.y = "book2", all.x = TRUE)
magat2[is.na(magat2)] <- 0

diff3 <- (cov0005$model %in% cov0610$model )
in3 <- cov0005[diff3,]
samej3 = in3 %>% group_by(book2) %>% mutate(count=n())
samej3$model=NULL
samej3=unique(samej3)
magat3$id=as.character(magat3$id)
samej3$book2=as.character(samej3$book2)
magat3= merge(x = magat3, y = samej3, by.x = "id", by.y = "book2", all.x = TRUE)
magat3[is.na(magat3)] <- 0

diff4 <- (cov0610$model %in% cov1112$model )
in4 <- cov0610[diff4,]
samej4 = in4 %>% group_by(book2) %>% mutate(count=n())
samej4$model=NULL
samej4=unique(samej4)
magat4$id=as.character(magat4$id)
samej4$book2=as.character(samej4$book2)
magat4= merge(x = magat4, y = samej4, by.x = "id", by.y = "book2", all.x = TRUE)
magat4[is.na(magat4)] <- 0

diff5 <- (cov1112$model %in% cov1314$model )
in5 <- cov1112[diff5,]
samej5 = in5 %>% group_by(book2) %>% mutate(count=n())
samej5$model=NULL
samej5=unique(samej5)
magat5$id=as.character(magat5$id)
samej5$book2=as.character(samej5$book2)
magat5= merge(x = magat5, y = samej5, by.x = "id", by.y = "book2", all.x = TRUE)
magat5[is.na(magat5)] <- 0

diff6 <- (cov1314$model %in% cov1517$model )
in6 <- cov1314[diff6,]
samej6 = in5 %>% group_by(book2) %>% mutate(count=n())
samej6$model=NULL
samej6=unique(samej6)
magat6$id=as.character(magat6$id)
samej6$book2=as.character(samej6$book2)
magat6= merge(x = magat6, y = samej6, by.x = "id", by.y = "book2", all.x = TRUE)
magat6[is.na(magat6)] <- 0

write.csv(magat6, "magat6.csv")
write.csv(magat5, "magat5.csv")
write.csv(magat4, "magat4.csv")
write.csv(magat3, "magat3.csv")
write.csv(magat2, "magat2.csv")
write.csv(magat1, "magat1.csv")

maattr= data.frame(maattr$id,maattr$`price eur`)
magat1$`price eur`=NULL
magat2$`price eur`=NULL
magat3$`price eur`=NULL
magat4$`price eur`=NULL
magat5$`price eur`=NULL
magat6$`price eur`=NULL
magat1= merge(x = magat1, y = maattr, by.x = "id", by.y = "maattr.id", all.x = TRUE)
magat2= merge(x = magat2, y = maattr, by.x = "id", by.y = "maattr.id", all.x = TRUE)
magat3= merge(x = magat3, y = maattr, by.x = "id", by.y = "maattr.id", all.x = TRUE)
magat4= merge(x = magat4, y = maattr, by.x = "id", by.y = "maattr.id", all.x = TRUE)
magat5= merge(x = magat5, y = maattr, by.x = "id", by.y = "maattr.id", all.x = TRUE)
magat6= merge(x = magat6, y = maattr, by.x = "id", by.y = "maattr.id", all.x = TRUE)

library(reshape2)
wgr1$X1=NULL
matweig1 = acast(wgr1, V1~V2, value.var="co2")
wgr2$X1=NULL
matweig2 = acast(wgr2, V1~V2, value.var="co2")
wgr3$X1=NULL
matweig3 = acast(wgr3, V1~V2, value.var="co2")
