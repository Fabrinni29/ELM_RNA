library(corpcor)
trainELM<-function(Xinput,Yinput,Pneurons){
  
  ###Deve passar Xinput com a coluna de números q
  rows_Z<-ncol(Xinput)
  Z<-matrix(runif(rows_Z*Pneurons,-0.5,0.5),ncol = Pneurons,nrow = rows_Z)
  H<-tanh(Xinput%*%Z)
  H<-cbind(1,H)
  W<-pseudoinverse(H)%*%Yinput
  retlist<-list(Z,W)
  return(retlist)
  
}