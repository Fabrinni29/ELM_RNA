rm(list=ls())
library(mlbench)
source("C:\\Users\\Mediconchip\\Desktop\\Redes Neurais\\ELM\\trainELM.R")
source("C:\\Users\\Mediconchip\\Desktop\\Redes Neurais\\ELM\\yELM.R")
source("C:\\Users\\Mediconchip\\Desktop\\Redes Neurais\\ELM\\acuraciaMediaELM.R")
# URL do conjunto de dados
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/heart.dat"

# Importar o conjunto de dados
dados <- read.table(url, header = FALSE, sep = "")

# Definir os nomes das colunas
colnames(dados) <- c("Age", "Sex", "ChestPainType", "RestingBP", "Cholesterol", "FastingBloodSugar", 
                     "RestingECG", "MaxHR", "ExerciseInducedAngina", "Oldpeak", "ST_Slope", 
                     "NumMajorVessels", "Thal", "Class")

#Remover NA
dados<-dados[complete.cases(dados),]

#Adequar rotulo das Classes
lines<- nrow(dados)
for (i in 1:lines) {
  if(dados[i,14]==2){
    dados[i,14]<- -1
  }
}

###Separando Treinamento de Teste
acuracia<-list()
p=300
for (i in 1:10) {
  index<- sample(1:nrow(dados))
  trainIndex<-sample(index,length(index)*0.7)
  testIndex<-setdiff(index,trainIndex)
  
  ###Dados de treino
  Xtrain<-as.matrix(dados[trainIndex,1:13])
  Ytrain<-as.matrix(dados[trainIndex,14])
  ###Dados de teste
  Xteste<-as.matrix((dados[testIndex,1:13]))
  Yteste<-as.matrix((dados[testIndex,14]))
  #Treinamento
  result_Treino<-trainELM(Xtrain,Ytrain,p)
  Z<-as.matrix(result_Treino[[1]])
  W<-as.matrix(result_Treino[[2]])
  Ymodelo<-yELM(Xteste,Z,W)
  confusionMatrix<-table(Ymodelo,Yteste)
  if(is.na(confusionMatrix[1])){confusionMatrix[1]<-0}
  if(is.na(confusionMatrix[2])){confusionMatrix[2]<-0}
  if(is.na(confusionMatrix[3])){confusionMatrix[3]<-0}
  if(is.na(confusionMatrix[4])){confusionMatrix[4]<-0}
  
  
  acuraciaAtual<-acuraciaMediaELM(confusionMatrix[[1]],confusionMatrix[[4]],confusionMatrix[[2]],confusionMatrix[[3]])
  acuracia<-append(acuracia,acuraciaAtual)
}
auraciaMedia<-sum(unlist(acuracia))/length(acuracia)
print(auraciaMedia)
DP_Acur<-sd(unlist(acuracia))
print(DP_Acur)