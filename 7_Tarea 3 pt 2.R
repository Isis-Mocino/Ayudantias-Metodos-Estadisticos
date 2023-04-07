# En esta ayudantia estudiaremos la funcion de 
# verosimilitud correspondiente al considerar
# una distibucion normal sobre los log-datos que
# se muestran a continuacion. De igual manera,
# se presenta la grafica de probabilidad que nos
# permite ver la validez del modelo sobre los datos

# 1. Datos ------------------------------

X <- c(40, 62, 69, 77, 83, 88, 94, 101, 109, 115,
       123, 125, 128, 136, 137, 152, 152, 153, 160, 165)
X <- log(X) # Logdatos
n <- length(X) # Tamano de muestra

# Estadisticas suficientes
t1 <- sum(X) 
t2 <- sum(X^2)

# Estimadores de maxima verosimilitud, normal
muG <- t1/n
sigG <- sqrt((t2 - (t1^2/n))/n)

# 2. Funciones de verosimilitud ------------------------

Ver <- function(mu, sig) {
  a <- sig^{-n}
  b <- exp((-1/(2*(sig^2)))*(t2 - 2*t1*mu + n*(mu^2)))
  return(a*b)
}

VerR <- function(mu, sig){
  return(Ver(mu, sig)/Ver(muG, sigG))
}

# 3. Contornos ----------------------------------

x <- seq(from = muG - 1, to = muG + 1, length.out = 1000)
y <- seq(from = sigG - 1, to = sigG + 1, length.out = 1000)
Z <- matrix(nrow = 1000, ncol = 1000)
  
  
for (i in 1:1000) {
  for (j in 1:1000) {
    Z[i, j] <- VerR(x[i], y[j])
  }
}

contour(x, y, Z, levels = c(0.01,0.05, 0.1),
        ylim = c(0.2,0.7), xlim = c(4.3, 5))
points(muG, sigG, type = "p", pch = 8, col = "red")

# 4. Grafica distribucion estimada ------------------------------

Xsort <- sort(X) # Ordenamos los datos (aunque ya lo estan)

unif <- (1 : n) / (n + 1) 
dist <- pnorm(Xsort, muG, sigG)

# Graficamos
plot(unif, dist, pch = 16,
     ylab = 'Distribucion normal estimada',
     xlab = 'Cuantil uniforme',
     main = 'Grafica PP')
abline(c(0,0), c(1,1), lty = 2, col = 'red') # Anadimos la linea identidad
