# En esta ayudantia aprenderemos a graficar la funcion de 
# densidad y de distribucion, simular muestras y graficar
# la frecuencia relativa. Esto especificamente para la
# distribucion gumbel

# install.packages("dgumbel")
library(dgumbel)

# 1. Graficar densidad ---------------------------------------------------------

# Funciones
f1 <- function(x){return(dgumbel(x, location = 0, scale = 1, log = FALSE))}
f2 <- function(x){return(dgumbel(x, location = 1, scale = 1, log = FALSE))}
f3 <- function(x){return(dgumbel(x, location = -1, scale = 1, log = FALSE))}
f4 <- function(x){return(dgumbel(x, location = 0, scale = 1.5, log = FALSE))}
f5 <- function(x){return(dgumbel(x, location = 0, scale = 0.5, log = FALSE))}

# Gráfica
plot(f1, # funcion
     ylim = c(0, 0.8), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'black', #color
     main = 'Densidades Gumbel', # titulo
     ylab = expression(f(x)), # etiqueta eje y
     xlab = expression(x)) # etiqueta eje x
par(new=TRUE)
plot(f2, # funcion
     ylim = c(0, 0.8), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     lty = 2, # guiones
     col = 'tomato', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '') # etiqueta eje x
par(new=TRUE)
plot(f3, # funcion
     ylim = c(0, 0.8), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     lty = 2, # guiones
     col = 'firebrick1', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '') # etiqueta eje x
par(new=TRUE)
plot(f4, # funcion
     ylim = c(0, 0.8), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     lty = 5, # guiones
     col = 'blue4', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '') # etiqueta eje x
par(new=TRUE)
plot(f5, # funcion
     ylim = c(0, 0.8), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     lty = 5, # guiones
     col = 'dodgerblue3', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '') # etiqueta eje x
legend("topright", legend = c(expression(sigma == 6), "f2", "f3", "f4", "f5"),
       lwd = 2, lty = c(1, 2, 2, 5, 5), cex = 0.5,
       col = c('black', 'tomato','firebrick1','blue4','dodgerblue3'))

# 2. Graficar distribucion------------------------------------------------------

# Funciones
g1 <- function(x){return(pgumbel(x, location = 0, scale = 1, log = FALSE))}
g2 <- function(x){return(pgumbel(x, location = 1, scale = 1, log = FALSE))}
g3 <- function(x){return(pgumbel(x, location = -1, scale = 1, log = FALSE))}
g4 <- function(x){return(pgumbel(x, location = 0, scale = 1.5, log = FALSE))}
g5 <- function(x){return(pgumbel(x, location = 0, scale = 0.5, log = FALSE))}

# Gráfica
plot(g1, # funcion
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'black', #color
     main = 'Distribuciones Gumbel', # titulo
     ylab = expression(g(x)), # etiqueta eje y
     xlab = expression(x)) # etiqueta eje x
par(new=TRUE)
plot(g2, # funcion
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     lty = 2, # guiones
     col = 'tomato', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '') # etiqueta eje x
par(new=TRUE)
plot(g3, # funcion
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     lty = 2, # guiones
     col = 'firebrick1', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '') # etiqueta eje x
par(new=TRUE)
plot(g4, # funcion
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     lty = 5, # guiones
     col = 'blue4', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '') # etiqueta eje x
par(new=TRUE)
plot(g5, # funcion
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4.5), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     lty = 5, # guiones
     col = 'dodgerblue3', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '') # etiqueta eje x

# 3. Simular muestras ----------------------------------------------------------

# Datos

x20 <- rgumbel(20, location = 0, scale = 1)
x50 <- rgumbel(50, location = 0, scale = 1)
x100 <- rgumbel(100, location = 0, scale = 1)

# Histogramas

hist(x20,  main = "Histograma 20 datos", 
     xlab = "Valor", ylab = "Frecuencia",
     breaks = 5, col = "purple")
par(new=TRUE)
plot(f1, # funcion
     from = -2, # limite izquierdo eje x
     to = 3, # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'black', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '',
     axes = FALSE)# etiqueta eje x

hist(x50,  main = "Histograma 50 datos", 
     xlab = "Valor", ylab = "Frecuencia",
     breaks = 10, col = "purple")
par(new=TRUE)
plot(f1, # funcion
     from = -2, # limite izquierdo eje x
     to = 4, # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'black', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '',
     axes = FALSE)# etiqueta eje x

hist(x100,  main = "Histograma 100 datos", 
     xlab = "Valor", ylab = "Frecuencia",
     breaks = 10, col = "purple")
par(new=TRUE)
plot(f1, # funcion
     from = -2, # limite izquierdo eje x
     to = 4, # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'black', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '',
     axes = FALSE)# etiqueta eje x


# 4. Frecuencias relativas -----------------------------------------------------
min(x20)
plot(x = ecdf(x20),
     main = "Frecuencia relativa",
     ylab = "Probabilidad acumulada",
     xlab = "x20")
par(new=TRUE)
plot(g1, # funcion
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(-2, 2.2),
     lwd = 2, # grosor
     col = 'blue', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '', # etiqueta eje x
     axes = FALSE)

plot(x = ecdf(x50),
     from = -2,
     to = 4.5,
     main = "Frecuencia relativa",
     ylab = "Probabilidad acumulada",
     xlab = "x50")
par(new=TRUE)
plot(g1, # funcion
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(-2, 4),
     lwd = 2, # grosor
     col = 'blue', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '', # etiqueta eje x
     axes = FALSE)

plot(x = ecdf(x100),
     from = -2,
     to = 4.5,
     main = "Frecuencia relativa",
     ylab = "Probabilidad acumulada",
     xlab = "x100")
par(new=TRUE)
plot(g1, # funcion
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(-2, 6),
     lwd = 2, # grosor
     col = 'blue', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '', # etiqueta eje x
     axes = FALSE)
