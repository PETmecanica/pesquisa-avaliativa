# talvez seja necessario instalar algumas dessas bibliotecas para rodar
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
library(fields)
library(reshape2)

rm(list = ls())

# lista com os nomes das planilhas de dados
planilhas <- c("Bixos (final).xlsx","Veteranos (final).xlsx","Professores (final).xlsx")

# funcao que retorna a forma na qual a planilha sera usada no codigo
arquivo <- function(nome){
  a <- read_xlsx(nome,sheet=3)
  b <- as.data.frame(a)
  c <- b[-1,]
  return(c)
}

# funcao que retorna a tabela de contingencia
tabela <- function(x,y){
  M <- table(x,y)
  return(M)
}

# funcao que retorna o p-value do teste chi-quadrado
r <- function(x,y){
  z <- chisq.test(tabela(x,y))
  return(z$p.value)
}

get_corr <- function(perfil_txt, pergunta) {
  get_correlacao_grupo <- function(df, pergunta) {
    perfil <- df[, perfil_txt]
    colunas <- colnames(df)[startsWith(colnames(df), pergunta)]
    correlacao <- as.data.frame(df[, colunas]) %>% sapply(function(x) { r(perfil, as.numeric(x)) })
    correlacao[1:length(correlacao)-1]
  }
  c(
    get_correlacao_grupo(arquivo(planilhas[1]), pergunta),
    get_correlacao_grupo(arquivo(planilhas[2]), pergunta)
  )
}

## CORRELACOES PARA O PERFIL FORMA DE INGRESSO
perguntas_fi <- c(
  '5. Com relação à disponibilidade e à qualidade dos laboratórios:',
  '3. Com relação à presença de disciplinas condizentes com o curso:',
  '2. Com relação ao acolhimento das alunas e alunos fora de período ideal e de transferência:'
)
print('Forma de ingresso:')
for (pergunta in perguntas_fi) {
  correlacoes <- suppressMessages(get_corr('Forma de ingresso:', pergunta))
  print('Bixos:')
  print(correlacoes[1])
  print('Veteranos - expectativa:')
  print(correlacoes[2])
  print('Veteranos - percepção:')
  print(correlacoes[3])
}

get_corr <- function(perfil_txt, pergunta) {
  get_correlacao_grupo <- function(df, pergunta) {
    perfil <- df[, perfil_txt]
    colunas <- colnames(df)[startsWith(colnames(df), pergunta)]
    correlacao <- as.data.frame(df[, colunas]) %>% sapply(function(x) { r(perfil, as.numeric(x)) })
    correlacao[1:length(correlacao)-1]
  }
  c(
    get_correlacao_grupo(arquivo(planilhas[2]), pergunta)
  )
}

## CORRELACOES PARA O PERFIL RELACAO ATUAL COM O CURSO
perguntas_rc <- c(
  '5. Com relação à disponibilidade e à qualidade dos laboratórios:',
  '1. Com relação à relevância das disciplinas para a capacitação profissional:',
  '3. Com relação à presença de disciplinas condizentes com o curso:',
  '6. Com relação à atualidade do conteúdo das disciplinas:',
  '2. Com relação ao comportamento ético dos professores:',
  '3. Com relação ao comportamento ético das alunas e dos alunos:',
  '6. Com relação à presença e à participação das alunas e alunos nas disciplinas:',
  '2. Com relação ao acolhimento das alunas e alunos fora de período ideal e de transferência:'
)
print('Relação atual com o curso:')
for (pergunta in perguntas_rc) {
  correlacoes <- suppressMessages(get_corr('Relação atual com o curso', pergunta))
  print('Veteranos - expectativa:')
  print(correlacoes[1])
  print('Veteranos - percepção:')
  print(correlacoes[2])
}

get_corr <- function(perfil_txt, pergunta) {
  get_correlacao_grupo <- function(df, pergunta) {
    perfil <- df[, perfil_txt]
    colunas <- colnames(df)[startsWith(colnames(df), pergunta)]
    correlacao <- as.data.frame(df[, colunas]) %>% sapply(function(x) { r(perfil, as.numeric(x)) })
    correlacao[1:length(correlacao)-1]
  }
  c(
    get_correlacao_grupo(arquivo(planilhas[1]), pergunta),
    get_correlacao_grupo(arquivo(planilhas[2]), pergunta)
  )
}

## CORRELACOES PARA O PERFIL GENERO
perguntas_g <- c(
  '1. Com relação à inclusão e à representatividade dentro do curso:',
  '2. Com relação ao comportamento ético dos professores:',
  '3. Com relação ao comportamento ético das alunas e dos alunos:',
  '4. Com relação à colaboração entre alunas e alunos:'
)
print('Gênero:')
for (pergunta in perguntas_g) {
  correlacoes <- suppressMessages(get_corr('Gênero com o qual se identifica:', pergunta))
  print('Bixos:')
  print(correlacoes[1])
  print('Veteranos - expectativa:')
  print(correlacoes[2])
  print('Veteranos - percepção:')
  print(correlacoes[3])
}

get_corr <- function(perfil_txt, pergunta) {
  get_correlacao_grupo <- function(df, pergunta) {
    perfil <- df[, perfil_txt]
    colunas <- colnames(df)[startsWith(colnames(df), pergunta)]
    correlacao <- as.data.frame(df[, colunas]) %>% sapply(function(x) { r(perfil, as.numeric(x)) })
    correlacao[1:length(correlacao)-1]
  }
  c(
    get_correlacao_grupo(arquivo(planilhas[1]), pergunta),
    get_correlacao_grupo(arquivo(planilhas[2]), pergunta),
    get_correlacao_grupo(arquivo(planilhas[3]), pergunta)
  )
}

## CORRELACOES PARA O PERFIL COR/RAÇA
perguntas_cr <- c(
  '1. Com relação à inclusão e à representatividade dentro do curso:',
  '2. Com relação ao comportamento ético dos professores:',
  '3. Com relação ao comportamento ético das alunas e dos alunos:'
)
print('Cor/Raça:')
for (pergunta in perguntas_cr) {
  correlacoes <- suppressMessages(get_corr('Cor/raça:', pergunta))
  print('Bixos:')
  print(correlacoes[1])
  print('Veteranos - expectativa:')
  print(correlacoes[2])
  print('Veteranos - percepção:')
  print(correlacoes[3])
  print('Professores - expectativa:')
  print(correlacoes[4])
  print('Professores - percepção:')
  print(correlacoes[5])
}

get_corr <- function(perfil_txt, pergunta) {
  get_correlacao_grupo <- function(df, pergunta) {
    perfil <- df[, perfil_txt]
    colunas <- colnames(df)[startsWith(colnames(df), pergunta)]
    correlacao <- as.data.frame(df[, colunas]) %>% sapply(function(x) { r(perfil, as.numeric(x)) })
    correlacao[1:length(correlacao)-1]
  }
  c(
    get_correlacao_grupo(arquivo(planilhas[1]), pergunta),
    get_correlacao_grupo(arquivo(planilhas[2]), pergunta)
  )
}

## CORRELACOES PARA O PERFIL ATIVIDADES EXTRACURRICULARES
perguntas_ae <- c(
  '5. Com relação à disponibilidade e à qualidade dos laboratórios:',
  '6. Com relação à qualidade da Infraestrutura e dos recursos voltados para os grupos de extensão:',
  '1. Com relação à relevância das disciplinas para a capacitação profissional:',
  '2. Com relação à valorização institucional dos projetos de extensão:',
  '3. Com relação à presença de disciplinas condizentes com o curso:',
  '6. Com relação à atualidade do conteúdo das disciplinas:',
  '3. Com relação à qualidade dos seus trabalhos realizados e das suas resoluções de provas:',
  '4. Com relação à colaboração entre alunas e alunos:',
  '3. Com relação às avaliações institucionais contínuas do curso de graduação:'
)
print('Atividades extracurriculares:')
for (pergunta in perguntas_ae) {
  correlacoes <- suppressMessages(get_corr('Selecione, abaixo, todas as atividades extracurriculares oferecidas pela Poli que já realizou ou ainda realiza.', pergunta))
  print('Bixos:')
  print(correlacoes[1])
  print('Veteranos - expectativa:')
  print(correlacoes[2])
  print('Veteranos - percepção:')
  print(correlacoes[3])
}

get_corr <- function(perfil_txt, pergunta) {
  get_correlacao_grupo <- function(df, pergunta, titulo_arq) {
    perfil <- colnames(df)[startsWith(colnames(df), perfil_txt)]
    colunas <- colnames(df)[startsWith(colnames(df), pergunta)]
    perfil1 <- as.data.frame(df[, perfil[1]])
    col1 <- as.data.frame(df[, colunas[1]])
    correlacao1 <- r(perfil1[, "df[, perfil[1]]"], col1[, "df[, colunas[1]]"])
    perfil2 <- as.data.frame(df[, perfil[2]])
    col2 <- as.data.frame(df[, colunas[2]])
    correlacao2 <- r(perfil2[, "df[, perfil[2]]"], col2[, "df[, colunas[2]]"])
    if (titulo_arq == 'Bixo') {
      c(correlacao1)
    } else {
      c(correlacao1,correlacao2)
    }
  }
  c(
    get_correlacao_grupo(arquivo(planilhas[1]), pergunta, 'Bixo'),
    get_correlacao_grupo(arquivo(planilhas[2]), pergunta, 'Vet'),
    get_correlacao_grupo(arquivo(planilhas[3]), pergunta, 'Prof')
  )
}

## CORRELACOES PARA O PERFIL A1
perguntas_a1 <- c(
  '1. Com relação à relevância das disciplinas para a capacitação profissional:',
  '3. Com relação ao comportamento ético das alunas e dos alunos:'
)
print('A1')
for (pergunta in perguntas_a1) {
  correlacoes <- suppressMessages(get_corr('1. Com relação ao elevado nível de exigência das disciplinas', pergunta))
  print('Bixos:')
  print(correlacoes[1])
  print('Veteranos - expectativa:')
  print(correlacoes[2])
  print('Veteranos - percepção:')
  print(correlacoes[3])
  print('Professores - expectativa:')
  print(correlacoes[4])
  print('Professores - percepção:')
  print(correlacoes[5])
}