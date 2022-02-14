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

#Bixos
ex2 <- read_xlsx('Bixos (final).xlsx',sheet=3)
Bixos <- as.data.frame(ex2)
rownames(Bixos) <- Bixos[1,]
Bixos <- Bixos[-1,]

#Professores
ex3 <- read_xlsx('Professores (final).xlsx',sheet=3)
Professores <- as.data.frame(ex3)
rownames(Professores) <- Professores[1,]
Professores <- Professores[-1,]




                            #Infraestrutura - pergunta 1 - MÉDIAS

#Veteranos
Vet_Infraestrutura_1_a<- as.numeric(Veteranos$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...68`)
Vet_Infraestrutura_1_a_media <- mean(Vet_Infraestrutura_1_a,na.rm = TRUE)

Vet_Infraestrutura_1_b<- as.numeric(Veteranos$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...69`)
Vet_Infraestrutura_1_b_media <- mean(Vet_Infraestrutura_1_b,na.rm = TRUE)

Vet_Infraestrutura_1_c<- as.numeric(Veteranos$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...70`)
Vet_Infraestrutura_1_c_media <- mean(Vet_Infraestrutura_1_c,na.rm = TRUE)



#Bixos
Bix_Infraestrutura_1_a<- as.numeric(Bixos$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...67`)
Bix_Infraestrutura_1_a_media <- mean(Bix_Infraestrutura_1_a,na.rm = TRUE)

Bix_Infraestrutura_1_c<- as.numeric(Bixos$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...68`)
Bix_Infraestrutura_1_c_media <- mean(Bix_Infraestrutura_1_c,na.rm = TRUE)



#Professores
Prof_Infraestrutura_1_a<- as.numeric(Professores$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...30`)
Prof_Infraestrutura_1_a_media <- mean(Prof_Infraestrutura_1_a,na.rm = TRUE)

Prof_Infraestrutura_1_b<- as.numeric(Professores$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...31`)
Prof_Infraestrutura_1_b_media <- mean(Prof_Infraestrutura_1_b,na.rm = TRUE)

Prof_Infraestrutura_1_c<- as.numeric(Professores$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...32`)
Prof_Infraestrutura_1_c_media <- mean(Prof_Infraestrutura_1_c,na.rm = TRUE)


                          #Infraestrutura - pergunta 1 - GRÁFICOS

Infraestrutura_1_a <- c(Vet_Infraestrutura_1_a_media, Bix_Infraestrutura_1_a_media, Prof_Infraestrutura_1_a_media)
Infraestrutura_1_b <- c(Vet_Infraestrutura_1_b_media, 0, Prof_Infraestrutura_1_b_media)
Infraestrutura_1_c <- c(Vet_Infraestrutura_1_c_media, Bix_Infraestrutura_1_c_media, Prof_Infraestrutura_1_c_media)

Infraestrutura_1 <- c(Infraestrutura_1_a, Infraestrutura_1_b, Infraestrutura_1_c)


#as.data.frame(Infraestrutura_1)
ggplot(Infraestrutura_1, aes(x = education)) + geom_bar(aes(), position = "dodge")

barplot(height = Infraestrutura_1, names.arg = c("Quanto acha que deveria haver?", "Quanto acha que há?", "Quão importante é para o curso?"),ylab="Média",cex.axis=1, ylim=c(0,5), beside=T, col=c("blue", "green", "yellow"))
legend("topright", pch=15, col=c("#3399FF", "green", "yellow"), legend=c("Veteranos","Bixos","Professores"),bty="n")



