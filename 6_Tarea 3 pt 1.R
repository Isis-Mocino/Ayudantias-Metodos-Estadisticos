# En esta ayudantia estudiaremos la relacion entre
# la media y varianza en una distribucion Poisson

# Una muestra n = 50, theta = 3 -----------------------

n <- 100 # Tamano de muestra
v1 <- rpois(n,3) # Generamos muestra

theta1 <- mean(v1) # Media
theta2 <- var(v1)*((n-1)/n) # Varianza

# Histograma con media y varianza
hist(v1,
     main = "titulo",
     xlab = "x",
     ylab = "y",
     col = blues9)
abline(v=3, col="black", lwd = 3)
abline(v=theta1, col="blue", lwd = 2, lty = 2)
abline(v=theta2, col="red", lwd = 2, lty = 2)
legend(x = "topright", legend = c(3, "media", "varianza"),
       lwd = c(3,2,2), lty = c(1,2,2),
       col = c("black", "blue", "red"),
       cex = 0.5)

# 500 muestras n = 100, theta = 3 ----------------------

n <- 100 # Tamano de muestra
t1 <- rep(0, 500) # Vector de medias
t2 <- rep(0, 500) # vector de varianzas

# Generamos 500 muestras y guardamos las medias y varianzas
for (i in 1:500) {
  m <- rpois(n, 3)
  t1[i] <- mean(m)
  t2[i] <- var(m)*((n-1)/n)
}

# Graficamos los pares de medias y varianzas obtenidos
plot(t1, t2,
     main = "Media vs Varianza (muestral)",
     xlab = "Media muestral",
     ylab = "Varianza muestral")
abline(v=3, col="red", lwd = 2, lty = 2)
abline(h=3, col="red", lwd = 2, lty = 2)

# Pregunta: ¿que observas?
