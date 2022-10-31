##Tarea 9
##COA-501 Herramientas de cómputo para investigadores (R y Python)
## Alumno: Ivan Fermin Quiroz Ibañez


#1.- Con el conjunto de datos de Boston, ejecute los tres modelos lm, rf y nn y rf con un conjunto de datos
#de entrenamiento (train) con 70 % de las observaciones y 30 % para el conjunto de datos de prueba
#(test). Defina cinco funciones para calcular los cinco criterios de desempeño del modelo con base en los
#valores observado y predichos (mencionados en clase).
#Contraste sus resultados de desempeño en un Cuadro con los resultados con los mismos modelos lm, nn
#y rf con 300 y 206. Grafique los valores predichos de los tres modelos versus los valores observados, en
#los conjuntos de prueba. Comente sus resultados.

#lectura de base de detos
boston <- read.csv("Boston_dataset.csv")
boston <- boston[,-1]
head(boston, 10)

#2.- Ejecute el modelo NN para resolver un problema de clasificación, copie las instrucciones a un script
#R, del enlace siguiente: En este ejemplo se clasifican las variedades de vino en función de las
#características enunciadas.
#https://www.r-bloggers.com/2017/02/multilabel-classification-with-neuralnet-package/

#3.- Copie las instrucciones en un script R y ejecute el modelo RF para resolver un problema de
#clasificación descrito en el siguiente enlace: En este ejemplo se utiliza el conjunto de datos (iris dataset)
#para clasificar tres variedades de iris.
#https://www.r-bloggers.com/2021/04/random-forest-in-r/

#4.- Utilice sus scripts de los problemas 2 (clasificador NN) y 3 (clasificador RF) utilice los datos de iris
#(iris dataset) con el clasificador NN; y los datos de vino (wine dataset) con el clasificador RF. Utilice la
#mismas particiones de ambos ejemplos, y compare el desempeño de los modelos NN y RF. Reporte en
#Cuadro la comparación de sus resultados comente sus resultados.


#espacio de trabajo
save.image("T9COA501IFQI.RData")

