
rm(list=ls())
library(mlbench)
source("C:\\Users\\Mediconchip\\Desktop\\Redes Neurais\\ELM\\trainELM.R")
source("C:\\Users\\Mediconchip\\Desktop\\Redes Neurais\\ELM\\yELM.R")
set.seed(123)


###Dados com distribuição Normal
normalDistrib<-mlbench.2dnormals(200)
test_normDistrib<-mlbench.2dnormals(50)
Xin<-as.matrix(normalDistrib$x)
Xin<-cbind(1,Xin)
Yin<-as.matrix(as.numeric(normalDistrib$classes))
#Adequando os labels para rodar no modelo:
class1<-list()
class2<-list()
len<-length(Yin)
for (index in 1:len) {
  
  if(Yin[index]==2)
    {
    Yin[index]<- -1
    class1<-append(class1,list(index))
  }
  if(Yin[index]==1){
    class2<-append(class2,list(index))
  }
  
}
p<-1
result_trainELM<-trainELM(Xin,Yin,p)
Z<-result_trainELM[[1]]
W<-result_trainELM[[2]]

#Gerar o Grid para os Dados
seqx1x2<-seq(-4,5,0.1)
lseq<-length(seqx1x2)
MZ<-matrix(nrow=lseq,ncol=lseq)
cr<-0
for (i in 1:lseq)
{
  for (j in 1:lseq)
  {
    cr<-cr+1
    x1<-seqx1x2[i]
    x2<-seqx1x2[j]
    x<-c(x1,x2)
    x<-c(1,x)
    MZ[i,j]<-yELM(x,Z,W)
  }
}


image(MZ)
contour(seqx1x2,seqx1x2,MZ,nlevels=1,xlim=c(-4,5),ylim = c(-4,5),xlab="X",ylab="Y",main=paste("ELM com dados de Distribuição Normal e p=",p))
par(new=TRUE)
plot(Xin[unlist(class1),2],Xin[unlist(class1),3],col="blue",xlim=c(-4,5),ylim = c(-4,5),xlab="",ylab="")
par(new=TRUE)
plot(Xin[unlist(class2),2],Xin[unlist(class2),3],col="red",xlim=c(-4,5),ylim = c(-4,5),xlab="",ylab="")     