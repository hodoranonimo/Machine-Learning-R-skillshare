## Importando datos en R

# Para archivos csv, separados por Coma
# La primera fila contiene los nombres de las variables
# asignar la variable id al nombre
# notar  / en lugar de \ que viene por defecto en windows

eduviajes <- read.table("eduviajes.csv", header=TRUE,
                       sep=";", row.names="ID")

# Al revisar los datos, podemos observar que algunas variables 
# fueron clasificadas como numericas, cuando deberian ser factores, esto debido
# a que como veremos mas adelante, debemos rellenar algunos datos faltantes y esta
# manipulacion dependera del tipo de dato de la variable por lo que requerimos 
# convertir a factor algunas de las variables que contienen numero y sobre todo aquellas
# que son binarias
# de dato.
# Debemos corregir esto

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

# Como podemos observar en la tabla, hay mucho valores faltantes y antes de elaborar
# nuestro arbol de clasificacion, tenemos que lidiar con ellos

# Algunos valores faltantes ya están marcados como "NA" pero
# otros simplemente no tienen ningún valor. Vamos a crear una función que rellene
# los valores faltantes y preserve los NA ya existentes. Llamaremos a nuestra función
# ArreglarNAs


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

# Ahora crearemos otra función para combinar categorías con poca cantidad de registros
# en la categoría "otros" 
# Esta función tendrá dos argumentos: el nombre del dataframe y el número de registros
# para incluirse en "otros." 

combinarcategorias<-function(data_frame,conteominimo){ 
  for (i in 1 : ncol(data_frame)){
  a<-data_frame[,i]
  replace <- names(which(table(a) < conteominimo))
  levels(a)[levels(a)	%in%	replace]	<- paste("Otros",colnames(data_frame)[i],sep=".")
  
  
  
  data_frame[,i]<-a } 
  return(data_frame) 
  }

combinarcategorias(eduviajesNA, 10)

##### Parte 3
# Primero, instalemos el paquete caret y pongamoslo en la librería
# install.packages("caret")
library(caret)

# Luego establezacamos una semilla de generación de números aleatorios para que el 
# el corte se realice siempre en el mismo punto, yo escogeré 2020, podría ser 
# cualquier número pero escoja siempre el mismo para tener resultados consistentes

set.seed(2020) 
# Ahora partimos la data en dos con la función CreateDataPartition. Yo voy a utilizar
# el 80% de los datos para entrenar a mi modelo y el 20% para hacer testing.
# La base está compuesta por 2389 observaciones, estimo el 80% de esto y es 1911.

Particion <- createDataPartition(y = eduviajesNA$Retenidos2020, 
                               p = 0.8, list = FALSE)
training <- eduviajesNA[ Particion,] 
testing <- eduviajesNA[ -Particion,]

## Sesion 4 de Eduviajes
# En esta sesion crearemos el arbol utilizando la funcion CTREE
# Hay dos familias de algoritmos de arboles de clasificacion o regresion, o CART
# por sus siglas en ingles: Classification and Regression Trees.
# en primer lugar tenemos los arboles de inferencia condicional que podemos obtener
# con la funcion de ctree  y en segundo lugar tenemos los arboles
# de particion recursiva que podemos obtener con la funcion rpart 
# . En esta sesion aprenderemos a utilizar el primero.

# CTREE
# Hagamos un arbol con Retenidos2020 como variable a predecir y todas las
# demas variables como predictoras
#install.packages("partykit")
library(partykit)
arbol.ctree = ctree(training$Retenidos2020 ~.,data=training)

# Ahora grafiquemos y ajustemos el tamaño de la letra
plot(arbol.ctree, gp = gpar(fontsize = 7)) 

## Sesion 5 de Eduviajes
# Una vez hemos creado nuestro arbols con ctree, es tiempo de utilizarlo
# para predecir nuestra variable objetivo Retenidos2020 en nuestra base
# testing. Para ello utilizaremos la funcion predict.

# En primer lugar, estimemos la probabilidad de retencion en nuestra base
# testing
probabilidades.ctree<-predict(arbol.ctree,newdata=testing,type="prob") 
# probabilidades.ctree es una tabla con las 477 filas de la base testing y
# dos columnas para las clases 0 y 1. La clase cero contiene la probabilidad
# de no retencion, es decir de que el grupo no adquiera un programa el 
# siguiente año, y la clase uno es la probabilidad de que si lo adquiera

# Con base a las probabilidades de retencion, establezcamos la clasificacion
# para eso, necesitamos la tasa de corte. Generalmente, esta tasa se calcula
# con base a los costos generados por contactar a los clientes de la base.
# Como no los tenemos, estimemos la probabilidad promedio de ser retenido
mean(as.integer(eduviajes$Retenidos2020))
# Observamos que la media es de 1.607 lo que nos debería llamar la atencion
# porque es mayor que 1, revisemos la variable y vemos que R la guarda como
# 1 y 2 en lugar de 0 y 1. Entonces lo unico que tenemos que hacer es restar
# 1, por lo que obtendriamos que la probabilidad promedio de ser retenido
# es de 0.607, que es la que utilizaremos como tasa de corte

# Primero creemos un vector de 477 datos, el tamaño de la base testing
clasificacion.ctree<- rep("1",477)
#Y ahora asignemos cero a aquellos registros con una probabilidad de retencion
# menor a 0.607
clasificacion.ctree[probabilidades.ctree[,2]<0.9]= "0"	

# Ahora convirtamoslo a factor, ya que la funcion que utilizaremos para
# crear la Matriz de Confusion requiere que los datos sean factores con el
# mismo numero de niveles, en nuestro caso dos nivels 0 y 1
clasificacion.ctree<- as.factor(clasificacion.ctree)

#Finalmente observemos la matriz de Confusion de nuestra clasificacion
# para ello utilizaremos la funcion confusionMatrix del paquete e1071
# install.packages("e1071")
library(e1071)
confusionMatrix(clasificacion.ctree,testing$Retenidos2020, positive = "1") 

### Sesion 6
# En esta sesión aprenderemos a graficar la curva ROC
# Una curva ROC se construye trazando la tasa de verdaderos positivos (TPR) 
# contra la tasa de falsos positivos (FPR). La tasa de verdaderos positivos es 
# la proporción de observaciones que se predijo correctamente que serían positivas
# de todas las observaciones positivas (TP / (TP + FN)). De manera similar, 
# la tasa de falsos positivos es la proporción de observaciones que se predice 
# incorrectamente como positivas de todas las observaciones negativas 
# (FP / (TN + FP)). Veamos los resultados de la Matriz de Confusion...
# Que hubiera pasado si en lugar de 0.607 hubieramos ocupado un tasa de corte mas
# alta o mas baja


#### ROC Curve
# install.packages("ROCR")
library(ROCR)
# Calculemos los errores
prediccion.ctree.ROC	<-	prediction(probabilidades.ctree[,2], testing$Retenidos2020) 
# Creemos los datos para la curva ROC con la funcion performance y los parametros
# prediccion.ctree.ROC o el calculo de los errores que acabamos de hacer
# tpr o true positive rate es decir la razón de positivos verdaderos en el
# eje y, y fpr o la razón de positivos falsos en el eje x
ROC.ctree <- performance(prediccion.ctree.ROC,"tpr","fpr") 
# Grafiquemos la curva ROC
plot(ROC.ctree) 
# Como se interpreta esta curva
# La curva ROC muestra la compensación entre sensibilidad (o TPR) y 
# especificidad (1 - FPR). Los clasificadores que dan curvas más cercanas a 
# la esquina superior izquierda indican un mejor desempeño. Como línea de base, 
# se espera que un clasificador aleatorio dé puntos a lo largo de la diagonal 
# (FPR = TPR). Cuanto más se acerque la curva a la diagonal de 45 grados del 
# espacio ROC, menos precisa será la prueba.

## Sesion 7s

#### AUC (area under curve)
# Es el area bajo la curva ROC
# Este índice se puede interpretar como la probabilidad de que un clasificador 
# ordenará o puntuará una instancia positiva elegida aleatoriamente más alta 
# que una negativa.
# Como interpretar esta area bajo la curva
# Para valores mayores 90% - excelente, 
# Entre 80-90% muy buenos
# Entre 70-80% - buenos, 
# Entre 60-70% mas o menos
# Menos de 60% - no es de mucho valor
# La principal utilidad del AUC es comparar la bondad de pronóstico entre dos
# o mas metodos, como por ejemplo nos podria servir para comparar si la Regresion
# Logistica ofrece una mejor predicción que el Arbol de Clasificación para un
# problema determinado.

# Aprendamos a estimar el area bajo la curva con R
# Creemos la data para calcular AUC
AUC.temporal <- performance(prediccion.ctree.ROC,"auc") 
# Extraigamos y convirtamos a variable numerica los valores para calcular AUC
AUC.ctree <- as.numeric(AUC.temporal@y.values) 
AUC.ctree 

#### Eduviajes 8: rpart

# Bienvenidos a la sesion 8 donde aprenderemos a construir un arbol de 
# clasificación con rpart, luego de haberlo aprendido a elaborar con ctree
# Ambos métodos deciden sobre divisiones binarias en cada nodo, 
# pero ctree lo hace basándose en la teoría estadística (pruebas de 
# significancia) mientras que el segundo se basa puramente en la 
# maximización del ajuste (coeficiente de Gini).

# El código R para el árbol rpart es un poco más complejo porque requiere 
# especificar un parámetro de complejidad, cp; un CP más pequeño resultará 
# en un árbol más grande. Hay dos formas de construir los árboles: 
# "creciendo de pequeño a grande" y "podando de grande a pequeño". 

# Dado que nuestro conjunto de de datos no es excesivamente grande, nons
# decantaremos por elaborar un arbol grande y luego podarlo. 

# A modo de comparación, mantengo cp = 0.005 a la izquierda y establezco 
# cp = 0.2 a la derecha, la diferencia es evidente. Un árbol de cp muy 
# bajo es más complejo de lo necesario y claramente está sobreajustado; 
# un árbol de cp muy alto está "inadecuado" y pierde muchas dependencias 
# razonables en los datos.

# install.packages("rpart")
# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)


# Definamos la complejidad del arbol
rpart.cp = rpart.control(cp = 0.0005)
# Creamos el arbol con rpart
arbol.rpart<-rpart(Retenidos2020 ~.,data=training,	method="class", control=rpart.cp) 
rpart.plot(arbol.rpart)

# Ahora creemos un arbol con menor complejidad
rpart.cp2 = rpart.control(cp = 0.2)
arbol.rpart2<-rpart(Retenidos2020~.,data=training,	method="class", control=rpart.cp2) 
rpart.plot(arbol.rpart2)

# En la siguiente sesión aprenderemos una técnica para definir cp

