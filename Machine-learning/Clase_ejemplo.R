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

ArreglarNAs <- function(data_frame) {
  arreglo_enteros <- 0
  arreglo_factores <- "NA_ARREGLADOS"
  arreglo_caracteres <- "NA_ARREGLADO"
  arreglo_fechas <- as.Date("1900-01-01")
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

library(caret)

set.seed(1954)

Particion <- createDataPartition(y=eduviajesNA$Retenidos2020, p=0.8, list = FALSE)
training <- eduviajesNA[Particion,]
testing <- eduviajes[-Particion,]

