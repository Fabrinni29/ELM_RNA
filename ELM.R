#sAÍDA Y=TGH(HW)
#W=H+Y LINEARIZAR
rm(list=ls())
library(corpcor)
set.seed(12345)
#Gerar dados  - Ou exclusivo
N<-30
#Classe 1
m1<-c(2,2)
m2<-c(4,4)
g1<- matrix(rnorm(N*2,sd=0.6),nrow = N,ncol = 2)+matrix(m1,nrow=N,ncol = 2,byrow = T)
g2<- matrix(rnorm(N*2,sd=0.6),nrow = N,ncol = 2)+matrix(m2,nrow=N,ncol = 2,byrow = T)
xc1<-rbind(g1,g2)
#CLASSE 2
m3<-c(2,4)
m4<-c(4,2)
g3<- matrix(rnorm(N*2,sd=0.6),nrow = N,ncol = 2)+matrix(m3,nrow=N,ncol = 2,byrow = T)
g4<- matrix(rnorm(N*2,sd=0.6),nrow = N,ncol = 2)+matrix(m4,nrow=N,ncol = 2,byrow = T)
xc2<-rbind(g3,g4)

#Visualização dos Dados
plot(xc1[,1],xc1[,2],col='red',xlim = c(0,6),ylim = c(0,6),main = "Dados ou exclusivo")
par(new=TRUE)
plot(xc2[,1],xc2[,2],col='blue',xlim = c(0,6),ylim = c(0,6),xlab="",ylab="")


#preparação dados para treinamento
X<-rbind(xc1,xc2)
Y<-rbind(matrix(-1,ncol = 1,nrow = 2*N),matrix(1,ncol = 1,nrow = 2*N))

#Número de Neurônios e primeira matriz de pesos
p<-5
Z<- matrix(runif(3*p,-0.5,0.5),nrow = 3,ncol = p)

#Transformação de X em H -> tANGENTE hIPERBÓLICA NO PRODUTO X por Z
Xaug<-cbind(1,X)
H1<-cbind(1,xc1)%*%Z
H1<-tanh(H1)
H2<-cbind(1,xc2)%*%Z
H2<-tanh(H2)

H<-tanh(Xaug%*%Z)

#Qualquer método pode ser usado para obter w
#Usar pseudoinversa, pois ELMs foram feitas considerando pseudoinversa
Haug<-cbind(1,H)
w<-pseudoinverse(Haug)%*%Y
#Avaliar o modelo
Yhat_train<-sign(Haug%*%w)
e_train<-sum((Y-Yhat_train)^2)/4
print(e_train)

#Gerar superfície de separação
seqx1x2<-seq(0,6,0.1)
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
    retELM<-tanh(x%*%Z)
    retELM<-c(1,retELM)%*%w
    MZ[i,j]<-sign(retELM)
  }
}
image(MZ)
contour(seqx1x2,seqx1x2,MZ,nlevels=1,xlim=c(0,6),ylim = c(0,6),xlab="",ylab="")
par(new=T)
plot(xc1[,1],xc1[,2],xlim=c(0,6),ylim = c(0,6),col='red',xlab = "x1",ylab = "x2",main ="Separação ELM" )
par(new=T)
plot(xc2[,1],xc2[,2],xlim=c(0,6),ylim = c(0,6),col='blue',xlab="",ylab="")
