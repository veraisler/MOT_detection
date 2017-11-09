# Tutorial Intro to statistical learning using R
library(tree)
library(ISLR)

High=ifelse(Speed<=25,"No","Car")
gps_13okt <- data.frame(gps_13okt, High)

tree.gps_13okt=tree(High~Speed,gps_13okt,na.action=na.pass)

summary(tree.gps_13okt)
plot(tree.gps_13okt)
text(tree.gps_13okt,pretty=0)

tree.gps_13okt
set.seed(2)
train= sample(1:nrow(gps_13okt), 200)
gps_13okt.test=gps_13okt[-train,]
High.test=High[-train]
tree.gps_13okt=tree(High~Speed,gps_13okt,subset=train,na.action=na.pass)
tree.pred=predict(tree.gps_13okt,gps_13okt.test,type="class")
table(tree.pred,High.test)
(2829+391)/200

set.seed(3)
cv.gps_13okt=cv.tree(tree.gps_13okt,FUN=prune.misclass)
names(cv.gps_13okt)



#### Carseats ####
attach(Carseats)
High=ifelse(Sales<=8,"No","Yes")
Carseats <- data.frame(Carseats, High)

tree.Carseats=tree(High~.-Sales,Carseats)

summary(tree.Carseats)
plot(tree.Carseats)
text(tree.Carseats,pretty=0)

tree.Carseats
set.seed(2)
train= sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.Carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.Carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200

set.seed(3)
cv.Carseats=cv.tree(tree.Carseats,FUN=prune.misclass)
names(cv.Carseats)
cv.Carseats
par(mfrow=c(1,2))
plot(cv.Carseats$size,cv.Carseats$dev,type="b")
plot(cv.Carseats$k, cv.Carseats$dev,type="b")

prune.Carseats = prune.misclass(tree.Carseats,best=9)
plot(prune.Carseats)
text(prune.Carseats,pretty=0)
tree.pred = predict(prune.Carseats,Carseats.test, type="class")
table(tree.pred, High.test)
(64+90)/200
#classification accuracy: 0.77

prune.Carseats = prune.misclass(tree.Carseats,best=16)
plot(prune.Carseats)
text(prune.Carseats,pretty=0)
tree.pred = predict(prune.Carseats,Carseats.test,type="class")
table(tree.pred, High.test)
(86+62)/200
#classification accuracy: 0.74 -> larger value of best -> larger pruned tree