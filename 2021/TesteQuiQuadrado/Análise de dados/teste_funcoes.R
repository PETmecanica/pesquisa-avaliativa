# talvez seja necessario instalar algumas dessas bibliotecas para rodar
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(fields)
library(reshape2)

rm(list = ls())

# funcao que retorna a forma na qual a planilha sera usada no codigo
arquivo <- function(nome){
  a <- read_xlsx(nome,sheet=3)
  b <- as.data.frame(a)
  c <- b[-1,]
  return(c)
}

# funcao que retorna o coeficiente de correlacao r
r <- function(x,y){
  z <- cor(cbind(x,y), use = "pairwise.complete.obs")
  return(z[1,2])
}

get_corr <- function(perfil_txt, pergunta) {
  get_correlacao_grupo <- function(df, pergunta) {
    perfil <- df[, perfil_txt]
    colunas <- colnames(df)[startsWith(colnames(df), pergunta)]
    correlacao <- as.data.frame(df[, colunas]) %>% sapply(function(x) { r(perfil, as.numeric(x)) %>% round(digits = 3)})
    correlacao[1:length(correlacao)-1]
  }
  c(
    get_correlacao_grupo(arquivo(planilhas[1]), pergunta),
    get_correlacao_grupo(arquivo(planilhas[2]), pergunta)
  )
}

get_corr('Forma de ingresso:', )
perguntas_fi <- c(
  '5. Com relação à disponibilidade e à qualidade dos laboratórios:',
  '3. Com relação à presença de disciplinas condizentes com o curso:'
)
for (pergunta in perguntas_fi) {
  print(get_corr('Forma de ingresso:', pergunta))
}
# lista com os nomes das planilhas de dados
planilhas <- c("Bixos (final).xlsx","Veteranos (final).xlsx","Professores (final).xlsx")

#colunas <- colnames(df)[startsWith(colnames(df), pergunta)]
get_corr('Forma de ingresso:', '5. Com relação à disponibilidade e à qualidade dos laboratórios:')

FI<- data_imp$`Forma de ingresso:`
I5 <- data_imp$`5. Com relação à disponibilidade e à qualidade dos laboratórios:...75`

## CORRELACOES PARA O PERFIL FORMA DE INGRESSO
# expectativa dos bixos
data_imp <- arquivo(planilhas[1])
FI<- as.numeric(data_imp$`Forma de ingresso:`)
I5 <- as.numeric(data_imp$`5. Com relação à disponibilidade e à qualidade dos laboratórios:...75`)
CG3 <- as.numeric(data_imp$`3. Com relação à presença de disciplinas condizentes com o curso:...84`)
AC2 <- as.numeric(data_imp$`2. Com relação ao acolhimento das alunas e alunos fora de período ideal e de transferência:...110`)
print(r(FI,I5),digits=3)
print(r(FI,CG3),digits=3)
print(r(FI,AC2),digits=3)

#expectativa dos veteranos
data_imp <- arquivo(planilhas[2])
FI <- as.numeric(data_imp$`Forma de ingresso:`)
I5 <- as.numeric(data_imp$`5. Com relação à disponibilidade e à qualidade dos laboratórios:...80`)
CG3 <- as.numeric(data_imp$`3. Com relação à presença de disciplinas condizentes com o curso:...93`)
AC2 <- as.numeric(data_imp$`2. Com relação ao acolhimento das alunas e alunos fora de período ideal e de transferência:...131`)
print(r(FI,I5),digits=3)
print(r(FI,CG3),digits=3)
print(r(FI,AC2),digits=3)

#percepcao dos veteranos
FI <- as.numeric(data_imp$`Forma de ingresso:`)
I5 <- as.numeric(data_imp$`5. Com relação à disponibilidade e à qualidade dos laboratórios:...81`)
CG3 <- as.numeric(data_imp$`3. Com relação à presença de disciplinas condizentes com o curso:...94`)
AC2 <- as.numeric(data_imp$`2. Com relação ao acolhimento das alunas e alunos fora de período ideal e de transferência:...132`)
print(r(FI,I5),digits=3)
print(r(FI,CG3),digits=3)
print(r(FI,AC2),digits=3)

## CORRELACOES PARA O PERFIL RELAÇÃO ATUAL COM O CURSO
# expectativa dos veteranos
data_imp <- arquivo(planilhas[2])
RC <- as.numeric(data_imp$`Relação atual com o curso`)
I5 <- as.numeric(data_imp$`5. Com relação à disponibilidade e à qualidade dos laboratórios:...80`)
CG1 <- as.numeric(data_imp$`1. Com relação à relevância das disciplinas para a capacitação profissional:...87`)
CG3 <- as.numeric(data_imp$`3. Com relação à presença de disciplinas condizentes com o curso:...93`)
CG6 <- as.numeric(data_imp$`6. Com relação à atualidade do conteúdo das disciplinas:...102`)
E2 <- as.numeric(data_imp$`2. Com relação ao comportamento ético dos professores:...112`)
E3 <- as.numeric(data_imp$`3. Com relação ao comportamento ético das alunas e dos alunos:...115`)
E6 <- as.numeric(data_imp$`6. Com relação à presença e à participação das alunas e alunos nas disciplinas:...124`)
AC2 <- as.numeric(data_imp$`2. Com relação ao acolhimento das alunas e alunos fora de período ideal e de transferência:...131`)
print(r(RC,I5),digits=3)
print(r(RC,CG1),digits=3)
print(r(RC,CG3),digits=3)
print(r(RC,CG6),digits=3)
print(r(RC,E2),digits=3)
print(r(RC,E3),digits=3)
print(r(RC,E6),digits=3)
print(r(RC,AC2),digits=3)

#percepcao dos veteranos
RC <- as.numeric(data_imp$`Relação atual com o curso`)
I5 <- as.numeric(data_imp$`5. Com relação à disponibilidade e à qualidade dos laboratórios:...81`)
CG1 <- as.numeric(data_imp$`1. Com relação à relevância das disciplinas para a capacitação profissional:...88`)
CG3 <- as.numeric(data_imp$`3. Com relação à presença de disciplinas condizentes com o curso:...94`)
CG6 <- as.numeric(data_imp$`6. Com relação à atualidade do conteúdo das disciplinas:...103`)
E2 <- as.numeric(data_imp$`2. Com relação ao comportamento ético dos professores:...113`)
E3 <- as.numeric(data_imp$`3. Com relação ao comportamento ético das alunas e dos alunos:...116`)
E6 <- as.numeric(data_imp$`6. Com relação à presença e à participação das alunas e alunos nas disciplinas:...125`)
AC2 <- as.numeric(data_imp$`2. Com relação ao acolhimento das alunas e alunos fora de período ideal e de transferência:...132`)
print(r(RC,I5),digits=3)
print(r(RC,CG1),digits=3)
print(r(RC,CG3),digits=3)
print(r(RC,CG6),digits=3)
print(r(RC,E2),digits=3)
print(r(RC,E3),digits=3)
print(r(RC,E6),digits=3)
print(r(RC,AC2),digits=3)
