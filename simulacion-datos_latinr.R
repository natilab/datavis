################################
# SIMULACIÓN DE DATOS PARA DIVULGACIÓN SOBRE VISUALIZACIÓN DE DATOS EN INV BIOLÓGICA
################################
# Autores: Natalia Labadie y Joaquín Ferreyra
################################

# Simulación de datos para comunicar/enseñar buenas y malas prácticas en la visualización de datos 
# en investigación científica, particularmente en ciencias biofísicas y bioquímicas.

# Paquetes

library(truncnorm)
library(dplyr)
library(tidyr)

library(faux)
library(purrr)


#######################
# Funciones para simular cada conjunto de datos
########################

# scaling. Escalar el set de datos para que tenga la media y el desvío estándar deseado
scaling <- function(set, desired.mean, desired.sd) { 
  desired.sd * (set - mean(set)) / sd(set) + desired.mean
}


# Funciones para simular datasets con determinada forma (sin escala particular)

# Normal+outlier
# Genera datos provenientes de una distribución normal estándar y les agrega 1 outlier.
outlier <- function(n) {
  d <- rnorm(n) 
  k <- sample(c(1, -1), 1)*runif(1, 3, 6) 
  d[sample(1:n, 1)] <- 0 + k*1
  
  return(d)
}

# Asymmetric
# Genera un set de datos con distribución asimétrica hacia la derecha, provenientes de una distribución Beta.
rightskewed <- function(n) {
  d <- rbeta(n, 1, 5)
  return(d)
} 

# Genera un set de datos con distribución asimétrica hacia la izquierda, provenientes de una distribución Beta.
leftskewed <- function(n) {
  d <- rbeta(n, 5, 1)
  return(d)
} 

# Genera datos con una distribución bimodal, construida a partir de dos distribuciones normales truncadas.
bimodal <- function(n) {
  c(rtruncnorm(n/2, a=0, b=2, mean=1, sd=.3),
    rtruncnorm(n/2, a=2, b=4, mean=3, sd=.3))
}

#####################################
# Pequeños conjuntos de datos
#####################################

set.seed(2019)
# 3 muestras de n=5, obtenidas de distribución normal. Rango 0-3. Muestra 2 tiene un outlier.

setA <- scaling(rnorm(5), desired.mean = 1.5, desired.sd = 0.5)
setB <- scaling(outlier(5), desired.mean = 1.5, desired.sd = 0.5)
setC <- rnorm(5, mean = 2.1, sd = 0.15)

datos1 <- data.frame(setA, setB, setC) %>% 
  mutate(id = 1:5) %>% 
  pivot_longer(cols = starts_with("set"),
               names_to = "set",
               names_prefix = "set",
               values_to = "fold_change") %>% 
  mutate(set = factor(set, levels = c("A", "B", "C"),
                      labels = c("Dosis 0", "Dosis 1", "Dosis 2")))


#####################################
# Datasets de mayor extensión
#####################################

# 6 conjuntos de datos con misma media y distinta forma, variablilidad y características de distribución

set.seed(2019)

nn = 20
set2A_1 <- scaling(rnorm(nn), desired.mean = 2.5, desired.sd = 0.7) # normal

set2A_2 <- scaling(outlier(nn), desired.mean = 2.5, desired.sd = 0.7) # con outlier

set2A_3 <- scaling(rnorm(nn), desired.mean = 2.5, desired.sd = 0.3) # menor variabilidad

set2A_4 <- scaling(leftskewed(nn), desired.mean = 2.5, desired.sd = 0.7) # asimétrico hacia la izq

set2A_5 <- scaling(bimodal(nn), desired.mean = 2.5, desired.sd = 0.7) # bimodal

# menor extensión muestral n
set2A_6 <- c(scaling(rnorm(6), desired.mean = 2.6, desired.sd = 0.7), rep(NA, nn-6))

datos2A <- data.frame(map(paste0("set2A_", 1:6), get))
colnames(datos2A) <- paste0("set", 1:6)

datos2A <- datos2A %>%
  mutate(id = 1:nn) %>%
  pivot_longer(cols = starts_with("set"),
               names_to = "set",
               names_prefix = "set",
               values_to = "var") %>%
  filter(!is.na(var)) %>%
  filter(set != 4) %>% 
  mutate(set = factor(set, levels = c(1:3, 5:6), labels = LETTERS[1:5])) 


#####################################
# Datos correlacionados
#####################################

# Datos obtenidos a partir de muestras dependientes (correlacionados) con dist normal

set.seed(2020)
datos3 <- rnorm_multi(n = 10, 
                      mu = c(0.6, 0.66),
                      sd = c(0.06, 0.06),
                      r = c(0.95), 
                      varnames = c("A", "B"),
                      empirical = FALSE) %>% 
  mutate(muestra=1:10) %>% 
  pivot_longer(c(A, B), names_to = "tiempo", values_to = "AE") %>% 
  mutate(muestra = factor(muestra),
         tiempo = factor(tiempo, levels = c("A", "B"),
                         labels = c("30 min", "60 min")))

