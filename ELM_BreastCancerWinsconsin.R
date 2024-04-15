rm(list=ls())
library(mlbench)
source("C:\\Users\\Mediconchip\\Desktop\\Redes Neurais\\ELM\\trainELM.R")
source("C:\\Users\\Mediconchip\\Desktop\\Redes Neurais\\ELM\\yELM.R")
source("C:\\Users\\Mediconchip\\Desktop\\Redes Neurais\\ELM\\acuraciaMediaELM.R")
#set.seed(123)
# Carregar o conjunto de dados diretamente da URL
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
dados <- read.csv(url, header = FALSE)

# Adicionar os nomes das colunas
nomes_colunas <- c("ID", "Clump_Thickness", "Uniformity_of_Cell_Size", "Uniformity_of_Cell_Shape", "Marginal_Adhesion", 
                   "Single_Epithelial_Cell_Size", "Bare_Nuclei", "Bland_Chromatin", "Normal_Nucleoli", "Mitoses", "Class")
colnames(dados) <- nomes_colunas

#Remover NA
dados<-dados[complete.cases(dados),]
lines<- nrow(dados)
#Adequar rotulo das Classes
for (i in 1:lines) {
  if(dados[i,11]==2){
    dados[i,11]<-1
  }
  if(dados[i,11]==4){
    dados[i,11]<- -1
  }
  
}
dados<-as.data.frame(lapply(dados, as.numeric))
#Remover NA
dados<-dados[complete.cases(dados),]
###Separando Treinamento de Teste
acuracia<-list()
p=300
for (i in 1:10) {
  

index<- sample(1:nrow(dados))
trainIndex<-sample(index,length(index)*0.7)
testIndex<-setdiff(index,trainIndex)

###Dados de treino
Xtrain<-as.matrix(dados[trainIndex,2:10])
Ytrain<-as.matrix(dados[trainIndex,11])
###Dados de teste
Xteste<-as.matrix((dados[testIndex,2:10]))
Yteste<-as.matrix((dados[testIndex,11]))
#Treinamento
result_Treino<-trainELM(Xtrain,Ytrain,p)
Z<-as.matrix(result_Treino[[1]])
W<-as.matrix(result_Treino[[2]])
Ymodelo<-yELM(Xteste,Z,W)
confusionMatrix<-table(Ymodelo,Yteste)

acuraciaAtual<-acuraciaMediaELM(confusionMatrix[[1]],confusionMatrix[[4]],confusionMatrix[[2]],confusionMatrix[[3]])
acuracia<-append(acuracia,acuraciaAtual)
}
auraciaMedia<-sum(unlist(acuracia))/length(acuracia)
print(auraciaMedia)
DP_Acur<-sd(unlist(acuracia))
print(DP_Acur)