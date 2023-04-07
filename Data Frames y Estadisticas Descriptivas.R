# En esta ayudantia trabajaremos con el conjunto de datos
# de Fisher. Aprenderemos a:
# - Usar data frames
# - Obtener estadisticas descriptivas
# - Generar histogramas

# Forma 1 (desde R) -----------------------------------------------------

data("iris") # Cargar la base de datos de R
head(iris, 10) # Vemos las primeras 10 observaciones


# Forma 2 (desde archivo Excel) -----------------------------------------

library(readxl) # Libreria para leer excel
# Leemos excel
IrisData <- read_excel("D:/DEMAT/6to semestre/Ayudantia/Semana 1/FishersIrisData.xls")
View(IrisData) # Para visualizar TODOS los datos
head(IrisData, 10) # Vemos las primeras 10 observaciones


# Extraer una parte del conjunto de datos -------------------------------

# Escoger UNA variable
X <- iris[, 1] #[nada, no. de columna]

# Escoger varias variables
X1 <- iris[, c(1,5)] #[nada, vector de columnas]

# Escoger varias observaciones observaci?n y una variable
X2 <- iris[1:50, 1] #[vector de filas, no. de renglones]

# Escoger varias observaciones observaci?n y varias variables
X3 <- iris[1:50, c(1,5)] #[vector de filas, vector de columnas]

# Obtener un subconjunto
versicolor <- subset(iris, Species == "versicolor")
# subset(data, Nombre de la columna == Valor)


# Estad?sticas descriptivas ---------------------------------------------

n <- length(X) # Tama?o de X
mean(X) # Promedio
var(X)*(n-1)/n # Varianza | Para la desv. estandar sacar raiz
median(X) # Mediana
min(X) # Min
max(X) # Max

summary(X) # Para obtener resumen


# Histograma-------------------------------------------------------------

# Con R
hist(X,  main = "Histograma Longitud Sepalo", 
     xlab = "Longitud", ylab = "Frecuencia",
     col = "purple")

# Con ggplot2
#install.packages(ggplot2)
library(ggplot2)
p <- ggplot(iris, aes(x=Sepal.Length)) + 
  geom_histogram(binwidth = 0.5)
