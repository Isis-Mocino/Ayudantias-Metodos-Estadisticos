# INFERENCIAS DE LA GAMMA

# Funciones utiles ---------------------------------------
# Logverosimilitud
lv <- function(alp, mu){
  return(n*alp*log(alp) - n*log(gamma(alp)) - n*alp*log(mu) + alp*t2 - (alp/mu)*t1)
}

# Datos --------------------------------------------------
# (Aqui irian tus datos, yo estoy generando los míos con una gamma)
n <- 20
datos <- rgamma(n, shape = 3, scale = 2)

# Estadisticas suficientes
# Es necesario declararlas al inicio porque se usarán como variables globales
t1 <- sum(datos)
t2 <- sum(log(datos))

# EMV mu --------------------------------------------------
mu_g <- t1/n

# Perfil alpha --------------------------------------------

# Logverosimilitud perfil
lvp_alp <- function(alp){
  return(lv(alp,mu_g))
}

# Derivada logverosimilitud perfil
dlvp_alp <- function(alp){
  return(n*log(alp) - n*digamma(alp) - n*log(t1/n) + t2)
}

# EMV alpha -----------------------------------------------
alp_g <- uniroot(dlvp_alp, c(0.1, 5))$root


# Perfil relativa alpha -----------------------------------
rlvp_alp <- function(alp){
  return(lvp_alp(alp) - lvp_alp(alp_g))
}

Rlvp_alp <- function(alp){
  return(exp(lvp_alp(alp) - lvp_alp(alp_g)))
}

# Niveles de intervalos del 90, 95 y 99
c_90 <- 0.2585 - 1/(0.5171 + 1.6667*n)
c_95 <- 0.1465 - 1/(2.029 + 2.084*n)
c_99 <- 0.0362 - 1/(14.609 + 4.886*n)

# Funcion para obtener intervalos 
intervalo_alp <- function(nivel){
  aux <- function(alp){
    return(rlvp_alp(alp) - log(nivel))
  }
  r <- uniroot(aux, c(alp_g, alp_g*10))$root
  l <- uniroot(aux, c(0.1, alp_g))$root
  inter <- list("l" = l, "r" = r)
  return(inter)
}

# Obtenemos intervalos
int_90_alp <- intervalo_alp(c_90)
int_95_alp <- intervalo_alp(c_95)
int_99_alp <- intervalo_alp(c_99)


# Perfil mu --------------------------------------------
# Logverosimilitud perfil
lvp_mu <- function(mu){
  aux2 <- function(alp){
    return(lv(alp, mu))
  }
  res <- optimize(aux2, c(0.001, alp_g+10), maximum = TRUE)
  return(res$maximum)
}

rlvp_mu <- function(mu){
  return(lvp_mu(mu) - lvp_mu(mu_g))
}
Rlvp_mu <- function(mu){
  return(exp(lvp_mu(mu) - lvp_mu(mu_g)))
}

# Funcion para obtener los intervalos
intervalo_mu <- function(nivel){
  aux3 <- function(mu){
    return(rlvp_mu(mu) - log(nivel))
  }
  r <- uniroot(aux3, c(mu_g-0.5, mu_g + 1000), extendInt = "yes")$root
  l <- uniroot(aux3, c(0.1, mu_g))$root
  inter2 <- list("l" = l, "r" = r)
  return(inter2)
}

# Obtenemos intervalos
int_90_mu <- intervalo_mu(c_90)
int_95_mu <- intervalo_mu(c_95)
int_99_mu <- intervalo_mu(c_99) #dio error porque el intervalo es muy grande
