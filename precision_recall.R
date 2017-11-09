# precision and recall

cycling =ifelse(gps_13okt$Speed<30 & gps_13okt$Speed>5,"yes","no")
gps_13okt <- data.frame(gps_13okt, cycling)


predict <- sample(gps_13okt$cycling, 20, replace =T)
true <- sample(gps_13okt$cycling,20, replace =T)

measurePrecisionRecall <- function (predict, actual_labels){
  
  retrieved <- sum(predict)
  xtab <- table(predict, true)
  precision <- sum(predict & true) /retrieved
  recall <- sum(predict & true)/ sum(true)
  Fmeasure <- 2* precision *recall/(precision+recall)
  accuracy <- sum(xtab[1,1],xtab[2,2])/sum(xtab)
  
  cat('precision: ')
  cat(precision *100)
  cat('%')
  cat('\n')
  
  cat('recall:  ')
  cat(recall*100)
  cat('%')
  cat('\n')
  
  cat('f-measure: ')
  cat(Fmeasure*100)
  cat('%')
  cat('\n')
  
  cat('accuracy:  ')
  cat(accuracy*100)
  cat('%')
  cat('\n')
}
