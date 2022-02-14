


library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)

ex <- read_xlsx('Veteranos (final).xlsx',sheet=3)
ex_t <- as.data.frame(ex)

rownames(ex_t) <- ex_t[1,]
ex_t <- ex_t[-1,]
#ex_t <- ex_t[-130,]

Vet_Infraestrutura_1<- as.numeric(ex_t$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...68`)
media <- mean(Vet_Infraestrutura_1,na.rm = TRUE)

cat("Média=",media)

#Prof_Infraestrutura_1<- as.numeric(ex_t$`1. Com relação à presença de ambientes físicos de estudo satisfatórios fora de aula:...68`,na.rm=TRUE)