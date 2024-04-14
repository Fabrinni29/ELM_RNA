#Gerar Resultado com o ELM Treinado
yELM<-function(Xtest,Z,W){
  H<-tanh(Xtest%*%Z)
  H<-cbind(1,H)
  Ytest<-sign(H%*%W)
  return(Ytest)
}