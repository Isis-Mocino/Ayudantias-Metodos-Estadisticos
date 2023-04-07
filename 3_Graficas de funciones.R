# GRAFICAS

f <- function(x){
  return (dnorm(x, 0, 1))
}
dist <- function(x){
  return (pnorm(x, 0, 1))
}
g <- function(x){
  return (dlnorm(x, 0, 1))
}

# Graficas default de R --------------------------------------------

# Op 1
plot(f)

# Op 2
plot(f, # funcion
     from = -4, # limite izquierdo eje x
     to = 4, # limite derecho eje x
     lwd = 1.8, # grosor
     col = 'blue', #color
     main = 'Densidad normal', # titulo
     ylab = expression(d(x)), # etiqueta eje y
     xlab = expression(x)) # etiqueta eje x

# Op 3
plot(f, # funcion
     ylim = c(0, 1), # Eje Y desde 0 hasta 1
     xlim = c(-4, 4), # Eje X desde -4 hasta 4
     lwd = 1.8, # grosor
     col = 'blue', #color
     main = 'Densidad normal', # titulo
     ylab = expression(d(x)), # etiqueta eje y
     xlab = expression(x)) # etiqueta eje x
abline(v = 0, col = "red", lwd = 2, lty = 2)

# Encimar graficas
plot(f, # funcion
     ylim = c(0, 0.8), # Eje Y desde 0 hasta 1
     xlim = c(-4, 4), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'blue', #color
     main = 'Densidad normal Y lognormal', # titulo
     ylab = expression(d(x)), # etiqueta eje y
     xlab = expression(x)) # etiqueta eje x
par(new=TRUE)
plot(g, # funcion
     ylim = c(0, 0.8), # Eje Y desde 0 hasta 1
     xlim = c(-4, 4), # Eje X desde -4 hasta 4
     lwd = 2, # grosor
     col = 'green', #color
     main = '', # titulo
     ylab = '', # etiqueta eje y
     xlab = '') # etiqueta eje x

# Para ver mas, checar en el siguiente link: https://r-coder.com/plot-en-r/


# Graficas ggplot2
#install.packages(ggplot2)
library(ggplot2)


p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("Density") +
  scale_y_continuous(breaks = NULL)
p1

