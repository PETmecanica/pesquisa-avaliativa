# Prototipo do codigo para os graficos #
#########################################################################################################################################

# Limpa variavel 
rm(list = ls())

# Pacotes
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(fields)
library(reshape2)
library(svglite)



# Importando grafico e manipulando-os

#Veteranos
ex1 <- read_xlsx('Veteranos (final).xlsx', sheet=3)
Veteranos <- as.data.frame(ex1)
Veteranos <- Veteranos[-1,]

#Bixos
ex2 <- read_xlsx('Bixos (final).xlsx',sheet=3)
Bixos <- as.data.frame(ex2)
Bixos <- Bixos[-1,]

#Professores
ex3 <- read_xlsx('Professores (final).xlsx',sheet=3)
Professores <- as.data.frame(ex3)
Professores <- Professores[-1,]


get_pergunta_graph <- function(pergunta) {
  get_media_grupo <- function(df, pergunta) {
    colunas <- colnames(df)[startsWith(colnames(df), pergunta)]
    medias <- df[, colunas] %>% sapply(function(x) { x %>% as.numeric() %>% mean(na.rm=TRUE) %>% round(digits = 2)})
    medias
  }
  Veterano <- as.vector(get_media_grupo(Veteranos, pergunta))
  Bixo <- as.vector(get_media_grupo(Bixos, pergunta))
  Bixo <- c(Bixo[1], 0, Bixo[2])
  Professor <- as.vector(get_media_grupo(Professores, pergunta))

  Perguntas <- c("Quanto acha que \n deveria haver?", "Quanto acha \n que h�?", "Qu�o importante � \n para o curso?")

  df <- data.frame(Perguntas, Bixo, Veterano, Professor)
  df.m <- melt(df, id.vars='Perguntas')

  # pergunta 1        variable       value
  #a              vet               3
  #a              bixo              3
  #a              prof              x
  #b              vet               x
  #b              bixo              x
  #b              prof              x


  names(df.m)[names(df.m) == 'value'] <- 'Media'
  names(df.m)[names(df.m) == 'variable'] <- 'Grupos'

  group.colors <- c(Veterano = "#32CD32", Bixo = "#FF6347", Professor ="#4169E1")
  
  return(ggplot(df.m, aes(x = Perguntas, y = Media, fill = Grupos )) +
    geom_bar(position = position_dodge(0.7), stat="identity",width = 0.7) +
    geom_text(aes(label=Media), position=position_dodge(width=0.7), vjust = -0.2) +
    ggtitle(paste(pergunta,':')) + theme(axis.text.x = element_text(size=12)) 
    + scale_fill_manual(values=group.colors)
    )
}

perguntas <- c(
  '1. Com rela��o � presen�a de ambientes f�sicos de estudo satisfat�rios fora de aula',
  '2. Com rela��o � compatibilidade do acervo da biblioteca com as exig�ncias do curso'
)
for(pergunta in perguntas) {
  get_pergunta_graph(pergunta)
  ggsave(paste(pergunta,'.svg'),device = svg,width=10, height=6)
  
}
