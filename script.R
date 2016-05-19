library(arules)
library(arulesViz)
library(plyr)
data_periodico=read.csv("periodico.csv")
#data_ejemplo=read.csv("ejemplo.csv")
salida= data.frame(X=data_periodico$X,
                   ID=data_periodico$ID,
                   entry=data_periodico$entry,
                   exit=data_periodico$exit,
                   items=data_periodico$articles)
salida$articles='1'
salida$tiempo='1'

matriz=matrix(data = 0, nrow = nrow(salida), ncol = 81)
matriz=as.data.frame(matriz)
colnames(matriz)= c("deportes/articulo1","deportes/articulo2","deportes/articulo3",
                    "deportes/articulo4","deportes/articulo5","deportes/articulo6",
                    "deportes/articulo7","deportes/articulo8","deportes/articulo9",
                    "politica/articulo1","politica/articulo2","politica/articulo3",
                    "politica/articulo4","politica/articulo5","politica/articulo6",
                    "politica/articulo7","politica/articulo8","politica/articulo9",
                    "variedades/articulo1","variedades/articulo2","variedades/articulo3",
                    "variedades/articulo4","variedades/articulo5","variedades/articulo6",
                    "variedades/articulo7","variedades/articulo8","variedades/articulo9",
                    "internacional/articulo1","internacional/articulo2","internacional/articulo3",
                    "internacional/articulo4","internacional/articulo5","internacional/articulo6",
                    "internacional/articulo7","internacional/articulo8","internacional/articulo9",
                    "nacionales/articulo1","nacionales/articulo2","nacionales/articulo3",
                    "nacionales/articulo4","nacionales/articulo5","nacionales/articulo6",
                    "nacionales/articulo7","nacionales/articulo8","nacionales/articulo9",
                    "sucesos/articulo1","sucesos/articulo2","sucesos/articulo3",
                    "sucesos/articulo4","sucesos/articulo5","sucesos/articulo6",
                    "sucesos/articulo7","sucesos/articulo8","sucesos/articulo9",
                    "comunidad/articulo1","comunidad/articulo2","comunidad/articulo3",
                    "comunidad/articulo4","comunidad/articulo5","comunidad/articulo6",
                    "comunidad/articulo7","comunidad/articulo8","comunidad/articulo9",
                    "negocios/articulo1","negocios/articulo2","negocios/articulo3",
                    "negocios/articulo4","negocios/articulo5","negocios/articulo6",
                    "negocios/articulo7","negocios/articulo8","negocios/articulo9",
                    "opinion/articulo1","opinion/articulo2","opinion/articulo3",
                    "opinion/articulo4","opinion/articulo5","opinion/articulo6",
                    "opinion/articulo7","opinion/articulo8","opinion/articulo9")
bot=c()

for (i in 1:nrow(data_periodico)) {
  #Modificar Items
  item=data_periodico$articles[i]
  item=toString(item)
  num=unique(na.omit(as.numeric(unlist(strsplit(unlist(item), "[^0-9]+")))))
  article=''
  for (k in 1:length(num)) {
    if (num[k]>=1 & num[k]<=9) {
      article=paste(article,"deportes/articulo",num[k],",")
      name=paste("deportes/articulo",num[k])
      name=gsub(" ","",name) 
      index=grep(name, colnames(matriz))
      matriz[i,index]=1
    }else if (num[k]>=10 & num[k]<=18) {
      number=as.numeric(strsplit(as.character(num[k]), "")[[1]])
      number=sum(number)
      if (number>9) {
        number=as.numeric(strsplit(as.character(number), "")[[1]])
        number=sum(number)
      }
      article=paste(article,"politica/articulo",number,",")
      name=paste("politica/articulo",number)
      name=gsub(" ","",name)
      index=grep(name, colnames(matriz))
      matriz[i,index]=1
    }else if (num[k]>=19 & num[k]<=27) {
      number=as.numeric(strsplit(as.character(num[k]), "")[[1]])
      number=sum(number)
      if (number>9) {
        number=as.numeric(strsplit(as.character(number), "")[[1]])
        number=sum(number)
      }
      article=paste(article,"variedades/articulo",number,",")
      name=paste("variedades/articulo",number)
      name=gsub(" ","",name)
      index=grep(name, colnames(matriz))
      matriz[i,index]=1
    }else if (num[k]>=28 & num[k]<=36) {
      number=as.numeric(strsplit(as.character(num[k]), "")[[1]])
      number=sum(number)
      if (number>9) {
        number=as.numeric(strsplit(as.character(number), "")[[1]])
        number=sum(number)
      }
      article=paste(article,"internacional/articulo",number,",")
      name=paste("internacional/articulo",number)
      name=gsub(" ","",name)
      index=grep(name, colnames(matriz))
      matriz[i,index]=1
    }else if (num[k]>=37 & num[k]<=45) {
      number=as.numeric(strsplit(as.character(num[k]), "")[[1]])
      number=sum(number)
      if (number>9) {
        number=as.numeric(strsplit(as.character(number), "")[[1]])
        number=sum(number)
      }
      article=paste(article,"nacionales/articulo",number,",")
      name=paste("nacionales/articulo",number)
      name=gsub(" ","",name)
      index=grep(name, colnames(matriz))
      matriz[i,index]=1
    }else if (num[k]>=46 & num[k]<=54) {
      number=as.numeric(strsplit(as.character(num[k]), "")[[1]])
      number=sum(number)
      if (number>9) {
        number=as.numeric(strsplit(as.character(number), "")[[1]])
        number=sum(number)
      }
      article=paste(article,"sucesos/articulo",number,",")
      name=paste("sucesos/articulo",number)
      name=gsub(" ","",name)
      index=grep(name, colnames(matriz))
      matriz[i,index]=1
    }else if (num[k]>=55 & num[k]<=63) {
      number=as.numeric(strsplit(as.character(num[k]), "")[[1]])
      number=sum(number)
      if (number>9) {
        number=as.numeric(strsplit(as.character(number), "")[[1]])
        number=sum(number)
      }
      article=paste(article,"comunidad/articulo",number,",")
      name=paste("comunidad/articulo",number)
      name=gsub(" ","",name)
      index=grep(name, colnames(matriz))
      matriz[i,index]=1
    }else if (num[k]>=64 & num[k]<=72) {
      number=as.numeric(strsplit(as.character(num[k]), "")[[1]])
      number=sum(number)
      if (number>9) {
        number=as.numeric(strsplit(as.character(number), "")[[1]])
        number=sum(number)
      }
      article=paste(article,"negocios/articulo",number,",")
      name=paste("negocios/articulo",number)
      name=gsub(" ","",name)
      index=grep(name, colnames(matriz))
      matriz[i,index]=1
    }else if (num[k]>=73 & num[k]<=81) {
      number=as.numeric(strsplit(as.character(num[k]), "")[[1]])
      number=sum(number)
      if (number>9) {
        number=as.numeric(strsplit(as.character(number), "")[[1]])
        number=sum(number)
      }
      article=paste(article,"opinion/articulo",number,",")
      name=paste("opinion/articulo",number)
      name=gsub(" ","",name)
      index=grep(name, colnames(matriz))
      matriz[i,index]=1
    }
  }
  article=gsub(" ","",article) 
  article=substr(article,1,nchar(article)-1)
  salida$articles[i]=article
  
  entry = strftime(data_periodico$entry[i], format="%H:%M:%S")
  entry=as.POSIXct(entry,format="%H:%M:%S")
  exit = strftime(data_periodico$exit[i], format="%H:%M:%S")
  exit=as.POSIXct(exit,format="%H:%M:%S")
  resta = difftime(exit,entry,units="secs")
  if (resta<20) {
    bot=c(bot, i)
  }
  salida$tiempo[i]=resta
}

salida=salida[-bot,]
matriz=matriz[-bot,]

newdata = salida
newdata$X=NULL
newdata$entry=NULL
newdata$exit=NULL
newdata$items=NULL
newdata$articles=NULL

newdata$tiempo=as.numeric(newdata$tiempo)

arr=arrange(newdata, tiempo)   
newdata=newdata[ order(newdata$tiempo), ] 

# 10 Visitas con mayor tiempo de estadia
mayor=head(newdata,10)
# 10 Visitas con menor tiempo de estadia
menor=tail(newdata,10)
# 10 transacciones con mayor numero de apariciones en el dataset
tab=table(newdata$ID)
sort=sort(tab,decreasing = T)
apariciones=head(sort,10)
  
matriz=as.matrix(matriz)
matriz=as(matriz, "transactions")

reglas=apriori(matriz,parameter=list(supp= 0.000003,conf=0.98))
summary(reglas)
inspect(reglas)

#Funcion de Recomendar
usuario <- function(arreglo){
  fun <- subset(reglas, subset = lhs %ain% arreglo)
  return(fun)
}

matriz=as(matriz, "matrix")
matriz=as.data.frame(matriz)

kmedias = kmeans(x = matriz,
                   centers = 8)

#Cantidad de Usuarios por cluster
table=table(kmedias$cluster)

# Funcion ROC
generate_ROC = function(scores, real, target){
  sortn=order(scores, decreasing = TRUE)
  real=real[sortn]
  scores=scores[sortn]
  FP=0
  TP=0
  fprev=-Inf
  i=1
  real2=real[real=target]
  P=length(real2)
  N=length(real)-P
  RX=c()
  RY=c()
  k=1
  while (i<=length(scores)) {
    if (scores[i]!=fprev) {
      RX[k]=FP/N
      RY[k]=TP/P
      fprev=scores[i]
      k=k+1
    }
    if(real[i]==target){
      TP=TP+1
    }else{
      FP=FP+1
    }
    i=i+1
  }
  RX[k]=FP/N
  RY[k]=TP/P
  print(RX)
  print(RY)
  # Generar curva
  plot(RX,RY,type="b",xlab = "FP-RATE",ylab = "TP-RATE",main = "ROC Curve")
  lines(RX,RY)
  points(RX,RY,col = 2, pch = 19)
}

real = c(2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1)
scores = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.54, 0.53, 0.52, 0.5, 0.5, 0.5, 0.5, 0.38, 0.37, 0.36, 0.35, 0.34, 0.33, 0.30, 0.1)
target = 2

generate_ROC(scores,real,target)


















