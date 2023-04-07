# En esta ayudantia graficaremos las densidades normal, gamma
# y exponencial con los parametros obtenidos con los datos mediante
# el metodo de estimadores de momentos


# Datos
X <- c(40, 62, 69, 77, 83, 88, 94, 101, 109, 115,
       123, 125, 128, 136, 137, 152, 152, 153, 160, 165)
n <- length(X)

# 1. Estadisticas descriptivas --------------------------------

min <- min(X)
max <- max(X)
med <- mean(X)
medg <- exp(mean(log(X))) 
medn <- median(X)
var <- (1/n)*(sum((X-mean(X))^2))
des <- sqrt(var)

# 2. Estimadores distribucion normal ----------------------

mu <- med 
sig <- var

# 3. Estimadores distribucion gamma ----------------------

alp <- (med^2)/var # shape
bet <- var/med # scale
mug <- med
  
# 4. Estimadores distribucion exponencial ---------------------

the <- med # 1/rate

# 5. Grafica de distribuciones ------------------------

fnorm <- function(x){
  return(dnorm(x, mu, des))
}

fgamma <- function(x){
  return(dgamma(x, shape = alp, scale = bet))
}


fexp <- function(x){
  return(dexp(x, rate = 1/the))
}

y <- rep(0, 20)

plot(fnorm, # funcion
     ylim = c(0, 0.012), # Eje Y desde 0 hasta 1
     xlim = c(min, max),# Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'blue', #color
     main = 'Densidad normal, gamma y exponencial', # titulo
     ylab = expression(d(x)), # etiqueta eje y
     xlab = expression(x)) # etiqueta eje x
par(new=TRUE)
plot(fgamma, # funcion
     ylim = c(0, 0.012), # Eje Y desde 0 hasta 1
     xlim = c(min, max), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'green', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '',
     axes = FALSE) # etiqueta eje x
par(new=TRUE)
plot(fexp, # funcion
     ylim = c(0, 0.012), # Eje Y desde 0 hasta 1
     xlim = c(min, max), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'red', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '',
     axes = FALSE) # etiqueta eje x
par(new=TRUE)
plot(X, y, # funcion
     ylim = c(0, 0.012), # Eje Y desde 0 hasta 1
     xlim = c(min, max), # Eje X desde -4 hasta 4
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '',
     pch = 4,
     axes = FALSE) # etiqueta eje 
