#Classification

plot(merge_9okt$Speed,merge_9okt$id)
abline(v=7)

# visual speed-threshold at 25km/h -> less = bike, more = car
walk =ifelse(merge_9okt$flag_mode_imu==3,1,0)
merge_9okt <- data.frame(merge_9okt, walk)
#logistic regression
# glm
train = (merge_9okt$ts > "2017-10-09 14:00:00:00")
merge_9okt_100 = merge_9okt[!train,]
dim(merge_9okt_100)
flag_mode_100=merge_9okt$walk[!train]

glm.fits =glm(walk~Speed+acc_tot+dist+bearing, data = merge_9okt, family=binomial, subset=train)
glm.probs=predict(glm.fits,merge_9okt_100,type="response")
glm.pred=rep("no",25201)
glm.pred[glm.probs<0.5]="yes"
table(glm.pred,flag_mode_100)
mean(glm.pred==flag_mode_100) 
mean(glm.pred!=flag_mode_100) # test set error rate

# LDA (linear discriminant analysis)
lda.fit = lda(cycling~Speed, data= merge_9okt,subset=train)
lda.fit
plot(lda.fit)



#smarket data
names(Smarket)
dim(Smarket)
summary(Smarket)
attach(Smarket)
plot(Volume)
glm.fits = glm(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family=binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef[,4]
glm.probs=predict(glm.fits, type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred == Direction)

train = (Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005) #test set error rate


#### functions ####
# glm-classifier

glm_classifier <- function(tracks,var1, var2=None, var3=None){
  train = (tracks$Satellites <11)
  tracks_100 = tracks[!train,]
  dim(tracks_100)
  cycling_100=cycling[!train]
  
  glm.fits =glm(cycling~var1 + var2 + var3,data = tracks, family=binomial, subset=train)
  glm.probs=predict(glm.fits,tracks_100,type="response")
  glm.pred=rep("no",2227)
  glm.pred[glm.probs<.4]="yes"
  table(glm.pred,cycling_100)
  mean = mean(glm.pred==cycling_100) 
  mean2 = mean(glm.pred!=cycling_100) # test set error rate
  return (table,mean, mean2)
}
