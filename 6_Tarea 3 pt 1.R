# Una muestra n = 50, theta = 3 -----------------------

v1 <- rpois(100,3)

theta1 <- mean(v1)
theta2 <- var(v1)*(99/100)

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

n <- 100
t1 <- rep(0, 500)
t2 <- rep(0, 500)

for (i in 1:500) {
  m <- rpois(n, 3)
  t1[i] <- mean(m)
  t2[i] <- var(m)*((n-1)/n)
}

plot(t1, t2,
     main = "Media vs Varianza (muestral)",
     xlab = "Media muestral",
     ylab = "Varianza muestral")
abline(v=3, col="red", lwd = 2, lty = 2)
abline(h=3, col="red", lwd = 2, lty = 2)
