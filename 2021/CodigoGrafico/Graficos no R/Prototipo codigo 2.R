# Protótipo do código para os gráficos #
#########################################################################################################################################

# Limpa memória 
rm(list = ls())

# Pacotes
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(fields)


# Importando gráfico e manipulando-os

#Veteranos
ex1 <- read_xlsx('Veteranos (final).xlsx',sheet=3)
Veteranos <- as.data.frame(ex1)
rownames(Veteranos) <- Veteranos[1,]
Veteranos <- Veteranos[-1,]
Veteranos <- Veteranos[,-1:-66]

#Bixos
ex2 <- read_xlsx('Bixos (final).xlsx',sheet=3)
Bixos <- as.data.frame(ex2)
rownames(Bixos) <- Bixos[1,]
Bixos <- Bixos[-1,]
Bixos <- Bixos[,-1:-65]

#Professores
ex3 <- read_xlsx('Professores (final).xlsx',sheet=3)
Professores <- as.data.frame(ex3)
rownames(Professores) <- Professores[1,]
Professores <- Professores[-1,]
Professores <- Professores[,-1:-28]

