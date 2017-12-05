# decision trees (own data)

# data = label_3nov_2

#### Decision Tree ####
label_3nov_2$flag_mode_imu = as.factor(label_3nov_2$flag_mode_imu)
Mode = label_3nov_2$flag_mode_imu
label_3nov_2 <- data.frame(label_3nov_2,Mode)

tree.label_3nov_2 = tree(Mode~Acc_X.mg.+Acc_Y.mg.+Acc_Z.mg.+Mag_X.mgauss.+Mag_Y.mgauss.+Mag_Z.mgauss.+Speed+acc_tot+dist+bearing, label_3nov_2)

summary(tree.label_3nov_2)

plot(tree.label_3nov_2)
text(tree.label_3nov_2,pretty=0)

tree.label_3nov_2

set.seed(2)
train = sample(1:nrow(label_3nov_2),2000)
label_3nov_2.test = label_3nov_2[-train,]
Mode.test = Mode[-train]
tree.label_3nov_2 = tree(Mode~Acc_X.mg.+Acc_Y.mg.+Acc_Z.mg.+Mag_X.mgauss.+Mag_Y.mgauss.+Mag_Z.mgauss.+Speed+acc_tot+dist+bearing,label_3nov_2, subset=train)
tree.pred = predict(tree.label_3nov_2,label_3nov_2.test, type ="class")
table(tree.pred,Mode.test)
# Mode.test
# tree.pred     3     4     9    11
#         3  87724  1255   914     0
#         4   4294 77576  1549   689
#         9    926   835  2692     0
#         11   185  1093     0  2980


(87724+877576+2692+2980)/2000
# [1] 485.486

#### Bagging and RandomForest ####

library (randomForest)

set.seed(1)
bag.label_3nov_2 = randomForest(Mode~Acc_X.mg.+Acc_Y.mg.+Acc_Z.mg.+Mag_X.mgauss.+Mag_Y.mgauss.+Mag_Z.mgauss.+Speed+acc_tot+bearing,
                                subset=train,
                                mtry =9, importance = TRUE)
bag.label_3nov_2


yhat.bag = predict(bag.label_3nov_2,newdata=label_3nov_2[-train,])


set.seed(1)
rf.label_3nov_2 = randomForest(Mode~Acc_X.mg.+Acc_Y.mg.+Acc_Z.mg.+Mag_X.mgauss.+Mag_Y.mgauss.+Mag_Z.mgauss.+Speed+acc_tot+bearing,
                               subset=train,
                               mtry =3, importance = TRUE)
yhat.rf = predict(rf.label_3nov_2,newdata=label_3nov_2[-train,])

importance(rf.label_3nov_2)

