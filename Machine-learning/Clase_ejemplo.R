eduviajes <- read.table("./eduviajes.csv", header=TRUE, sep=";", row.names="ID")

eduviajes$GradoMinimo <- as.factor(eduviajes$GradoMinimo)
eduviajes$GradoMaximo <- as.factor(eduviajes$GradoMaximo)
eduviajes$Estado <- as.factor(eduviajes$Estado)
eduviajes$NoAnual <- as.factor(eduviajes$NoAnual)
eduviajes$Dias <- as.factor(eduviajes$Dias)
eduviajes$Transporte <- as.factor(eduviajes$Transporte)
eduviajes$TipoPago <- as.factor(eduviajes$TipoPago)
eduviajes$LineaPobreza <- as.factor(eduviajes$LineaPobreza)
eduviajes$Region <- as.factor(eduviajes$Region)
eduviajes$SegmentoCRM <- as.factor(eduviajes$SegmentoCRM)
eduviajes$TipoEscuela <- as.factor(eduviajes$TipoEscuela)
eduviajes$ReunionPadres <- as.factor(eduviajes$ReunionPadres)
eduviajes$MenorGradoEscuela <- as.factor(eduviajes$MenorGradoEscuela)
eduviajes$MayorGrado.Escuela <- as.factor(eduviajes$MayorGrado.Escuela)
eduviajes$NivelIngresos <- as.factor(eduviajes$NivelIngresos)
eduviajes$TipoPrograma <- as.factor(eduviajes$TipoPrograma)
eduviajes$NuevoExistente <- as.factor(eduviajes$NuevoExistente)
eduviajes$TipoGradoMinimo<- as.factor(eduviajes$TipoGradoMinimo)
eduviajes$TipoGradoMaximo <- as.factor(eduviajes$TipoGradoMaximo)
eduviajes$MesPartida <- as.factor(eduviajes$MesPartida)
eduviajes$CodigoAgregacionPrograma <- as.factor(eduviajes$CodigoAgregacionPrograma)
eduviajes$Tamano <- as.factor(eduviajes$Tamano)
eduviajes$Reuniones <- as.factor(eduviajes$Reuniones)
eduviajes$MismoGrado <- as.factor(eduviajes$MismoGrado)
eduviajes$CodigoPrograma <- as.factor(eduviajes$CodigoPrograma)
eduviajes$Retenidos2020 <- as.factor(eduviajes$Retenidos2020)

ArreglarNAs<-function(data_frame){
  arreglo_enteros<-0
  arreglo_factores<-"NA_ARREGLADO"
  arreglo_caracteres<-"NA_ARREGLADO"
  arreglo_fechas<-as.Date("1900-01-01")



  # Realice un bucle en las columnas del data frame y de acuerdo al tipo de datos
  # aplique el arreglo determinado y cree una columna sustituta

  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
      data_frame[,paste0(colnames(data_frame)[i],"_indicadorNA")]<- as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
      data_frame[is.na(data_frame[,i]),i]<-arreglo_enteros
    }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
        data_frame[,i]<-as.character(data_frame[,i])
        data_frame[,paste0(colnames(data_frame)[i],"_indicadorNA")]<-as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-arreglo_factores
          data_frame[,i]<-as.factor(data_frame[,i])

      }
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
          data_frame[,paste0(colnames(data_frame)[i],"_indicadorNA")]<- as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-arreglo_caracteres
        }
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_indicadorNA")]<-as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-arreglo_fechas
            }
          }
        }
      }
  }
  return(data_frame)
}

eduviajesNA <-ArreglarNAs(eduviajes)

combinarcategorias<-function(data_frame,conteominimo){
  for (i in 1 : ncol(data_frame)){
  a<-data_frame[,i]
  replace <- names(which(table(a) < conteominimo))
  levels(a)[levels(a)	%in%	replace]	<- paste("Otros",colnames(data_frame)[i],sep=".")



  data_frame[,i]<-a }
  return(data_frame)
  }

combinarcategorias(eduviajesNA, 10)


library(caret)

set.seed(2020)

Particion <- createDataPartition(y = eduviajesNA$Retenidos2020, p=0.8, list = FALSE)
training <- eduviajesNA[Particion,]
testing <- eduviajesNA[-Particion,]


library(partykit)

arbol.ctree <- ctree(training$Retenidos2020 ~.,data=training)

plot(arbol.ctree, gp = gpar(fontsize = 7))

probabilidades.ctree <- predict(arbol.ctree, newdata = testing, type="prob")

mean(as.integer(eduviajes$Retenidos2020))

clasificacion.ctree <- rep("1", 477)
clasificacion.ctree[probabilidades.ctree[,2]<0.607] <- "0"
clasificacion.ctree <- as.factor(clasificacion.ctree)

library(e1071)
confusionMatrix(clasificacion.ctree,testing$Retenidos2020, positive = "1")
# install.packages("ROCR")

library(ROCR)
prediccion.ctree.ROC <- prediction(probabilidades.ctree[,2], testing$Retenidos2020)

ROC.ctree <- performance(prediccion.ctree.ROC, "tpr", "fpr")
plot(ROC.ctree)


AUC.temporal <- performance(prediccion.ctree.ROC, "auc")
AUC.ctree <- as.numeric(AUC.temporal@y.values)
AUC.ctree

library(rpart)
library(rpart.plot)

rpart.cp <- rpart.control(cp=0.0005)

arbol.rpart<-rpart(Retenidos2020 ~.,data=training,	method="class", control=rpart.cp)
rpart.plot(arbol.rpart)


rpart.cp2 = rpart.control(cp = 0.2)
arbol.rpart2<-rpart(Retenidos2020~.,data=training,	method="class", control=rpart.cp2)
rpart.plot(arbol.rpart2)



printcp(arbol.rpart)
plotcp(arbol.rpart)

rpart.plot(arbol.rpart)

arbol.podado <- prune(arbol.rpart, cp = 0.045)
rpart.plot(arbol.podado)

probabilidades.rpart <- predict(arbol.podado,  newdata=testing, type= "prob")

clasificacion.rpart <- rep("1", 477)
clasificacion.rpart[probabilidades.rpart[,2]<0.607] <- "0"
clasificacion.rpart <- as.factor(clasificacion.rpart)
confusionMatrix(clasificacion.rpart, testing$Retenidos2020, positive = "1")

prediccion.rpart.ROC <- prediction(probabilidades.rpart[,2], testing$Retenidos2020)
rpart.ROC <- performance(prediccion.rpart.ROC, "tpr", "fpr")
plot(rpart.ROC)
plot(ROC.ctree, add=TRUE, col = "blue")
legend("right", legend = c("RPART", "CTREE"), col = c("black","blue"), lty = 1:2, cex = 0.8)

AUC.temporal.rpart <- performance(prediccion.rpart.ROC, "auc")
rpart.AUC <- as.numeric(AUC.temporal.rpart@y.values)
rpart.AUC


library(randomForest)

modelo.bosque <- randomForest(Retenidos2020~. -Estado, data=training, importance = TRUE, proximity = TRUE, cutoff = c(0.5, 0.5), type = "classification")
print(modelo.bosque)
plot(modelo.bosque)

importance(modelo.bosque)

varImpPlot(modelo.bosque)

probabilidades.bosque <- predict(modelo.bosque, newdata = testing, type = "prob")

clasificacion.bosques <- rep("1", 477)
clasificacion.bosques[probabilidades.bosque[,2]<0.5] <- "0"
clasificacion.bosques <- as.factor(clasificacion.bosques)

confusionMatrix(clasificacion.bosques, testing$Retenidos2020, positive = "1")

prediccion.bosque.ROC <- prediction(probabilidades.bosque[,2], testing$Retenidos2020)
ROC.bosque <- performance(prediccion.bosque.ROC, "tpr", "fpr")
plot(ROC.bosque)
AUC.temporal <- performance(prediccion.bosque.ROC, "auc")
AUC.bosque <- as.numeric(AUC.temporal@y.values)
AUC.bosque

library(xgboost)

training.x <-model.matrix(Retenidos2020~., data = training)
testing.x <- model.matrix(Retenidos2020~., data = testing)

modelo.xgboost <-xgboost(data = data.matrix(training.x[,-1]), label = as.numeric(as.character(training$Retenidos2020)),
                         eta = 0.1, max_depth = 20, nround = 50,objective ="binary:logistic")

prediccion.xgboost <- predict(modelo.xgboost, newdata = testing.x[,-1],type = "response")

confusionMatrix(as.factor(ifelse(prediccion.xgboost > 0.607, 1, 0)), testing$Retenidos2020, positive = "1")

xgboost.errores <- prediction(prediccion.xgboost, testing$Retenidos2020)
ROC.xgboost <- performance(xgboost.errores, "tpr", "fpr")
plot(ROC.xgboost)
plot(ROC.bosque, add = TRUE, col = "blue")
legend("right", legend = c("xgboost", "randomforest"), col = c("black", "blue"), lty= 1:2, cex = 0.8)

AUC.XGboost.temporal <- performance(xgboost.errores, "auc")
AUC.xgboost <- as.numeric(AUC.XGboost.temporal@y.values)
AUC.xgboost