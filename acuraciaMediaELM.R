acuraciaMediaELM<-function(truePositive=0,trueNegative=0,falsePositive=0,falseNegative=0){
  if(is.na(truePositive)){truePositive<-0}
  if(is.na(trueNegative)){trueNegative<-0}
  if(is.na(falsePositive)){falsePositive<-0}
  if(is.na(falseNegative)){falseNegative<-0}
  acur<-(truePositive+trueNegative)/(truePositive+trueNegative+falsePositive+falseNegative)
  return(acur)
}