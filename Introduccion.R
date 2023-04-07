# COSAS BASICAS R

# Declaracion de variables ---------------------------------

n <- 10
m <- 2
a <- 7

# Operaciones baasicas---------------------------------------

10^m # Exponente
a+m # Suma
a-m # Resta
a*n # Multiplicacion
a/n # Division
3%%6 # Modulo
sqrt(a) # Raiz
exp(2) # Esponencial
log(2) # Logaritmo


# Vectores--------------------------------------------------

v1 <- c(1,5,6,7) # Datos especficos
v2 <- 1:10 # Sucesion
v3 <- c(v1, v2) # Unir dos vectores
v4 <- c("le", 0, "puedes", 1, "poner", 3, "lo", TRUE, "que sea?") # Mezcla de datos


# Matrices -------------------------------------------------

A <- matrix(1:12, nrow = 2, ncol = 6)
B <- matrix(1:12, nrow = 6, ncol = 2)

D <- 2*A # multiplica todos los elementos de la matriz
C <- A%*%B # multiplicar dos matrices
Ct <- t(C) # transpone
Cin <- C^(-1) # invierte

# Igual podemos juntar vectores para hacer matrices
v5 <- 1:3
v6 <- 4:6

cbind(v5, v6) # considera los vectores como columnas
rbind(v5, v6) # considera los vectores como renglones


# Generar datos bajo una distribucion ----------------------

# Normal
# rnorm(no. de observaciones, media, varianza)
vec1 <- rnorm(n, 0, 1)

# Chi cuadrada
# rchisq(no. de observaciones, gdl)
vec2 <- rchisq(n, 4)

# Logistica
# rlogis(no. de observaciones, localizacion, escala)
vec2 <- rlogis(n, 0, 1)


# Funciones hermanas de rnorm ------------------------------

# Funcion de densidad
dnorm(0, 0, 1)

# Funcion de distribucion
pnorm(0, 0, 1)

# Funcion de quantil
qnorm(0.5, 0, 1)


# Declarar funciones ---------------------------------------

f <- function(x, m, v){
  return (dnorm(x, m, v))
}


# Verificar tipo de datos ----------------------------------

class("hola")
is.character(7)
typeof(v4)


# For ------------------------------------------------------

# i se va a mover en un vector, asi que hay
# que mandar un vector del tamano deseado
for (i in 1:5) {
  print(i)
}


# Para encontrar mas: https://bookdown.org/jboscomendoza/r-principiantes4/