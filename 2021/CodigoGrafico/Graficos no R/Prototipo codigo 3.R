# Protótipo do código para os gráficos #
#########################################################################################################################################

# Limpa variável 
rm(list = ls())

# Pacotes
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(fields)
library(reshape2)



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
Vet_Infraestrutura_1_a_media <- round(mean(Vet_Infraestrutura_1_a,na.rm = TRUE),digits = 2)

Vet_Infraestrutura_1_b<- as.numeric(Veteranos$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...69`)
Vet_Infraestrutura_1_b_media <- round(mean(Vet_Infraestrutura_1_b,na.rm = TRUE),digits =2)

Vet_Infraestrutura_1_c<- as.numeric(Veteranos$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...70`)
Vet_Infraestrutura_1_c_media <- round(mean(Vet_Infraestrutura_1_c,na.rm = TRUE),digits = 2)



#Bixos
Bix_Infraestrutura_1_a<- as.numeric(Bixos$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...67`)
Bix_Infraestrutura_1_a_media <- round(mean(Bix_Infraestrutura_1_a,na.rm = TRUE),digits = 2)

Bix_Infraestrutura_1_c<- as.numeric(Bixos$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...68`)
Bix_Infraestrutura_1_c_media <- round(mean(Bix_Infraestrutura_1_c,na.rm = TRUE),digits = 2)



#Professores
Prof_Infraestrutura_1_a<- as.numeric(Professores$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...30`)
Prof_Infraestrutura_1_a_media <- round(mean(Prof_Infraestrutura_1_a,na.rm = TRUE),digits = 2)

Prof_Infraestrutura_1_b<- as.numeric(Professores$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...31`)
Prof_Infraestrutura_1_b_media <- round(mean(Prof_Infraestrutura_1_b,na.rm = TRUE),digits = 2)

Prof_Infraestrutura_1_c<- as.numeric(Professores$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...32`)
Prof_Infraestrutura_1_c_media <- round(mean(Prof_Infraestrutura_1_c,na.rm = TRUE),digits = 2)


#Infraestrutura - pergunta 1 - GRÁFICOS



#       pergunta1          1) vet         2)Bixos        3)Professores
#            a                 3             3                3
#            b                 6             0                8
#            c                 5             5                4


Vet <- c(Vet_Infraestrutura_1_a_media, Vet_Infraestrutura_1_b_media, Vet_Infraestrutura_1_c_media)
Bixo <- c(Bix_Infraestrutura_1_a_media, 0, Bix_Infraestrutura_1_c_media)
Prof <- c(Prof_Infraestrutura_1_a_media, Prof_Infraestrutura_1_b_media, Prof_Infraestrutura_1_c_media)
Perguntas <- c("Quanto acha que \n deveria haver?", "Quanto acha \n que há?", "Quão importante é \n para o curso?")


df <- data.frame(Perguntas, Bixo, Vet, Prof) 
df.m <- melt(df, id.vars='Perguntas')

# pergunta 1        variable       value
#a              vet               3
#a              bixo              3
#a              prof              x
#b              vet               x
#b              bixo              x
#b              prof              x


names(df.m)[names(df.m) == 'value'] <- 'Média'
names(df.m)[names(df.m) == 'variable'] <- 'Grupos' 


#graf <-
  ggplot(df.m, aes(x = Perguntas, y = Média, fill = Grupos )) + 
  geom_bar(position = position_dodge(0.7), stat="identity",width = 0.7) + 
  geom_text(aes(label=Média), position=position_dodge(width=0.7), vjust = -0.2) +
  ggtitle("1.Com relação à presença de ambientes físicos de estudo \nsatisfatórios fora de aula:")


svg(filename ="saving_plot2.svg")
    plot(graf)
dev.off()


  


