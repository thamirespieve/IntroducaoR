install.packages("ggplot2movies")
install.packages("tidyverse")

library(tidyverse)

dados <- ggplot2movies::movies%>%filter(!is.na(budget),budget>0)%>% 
  select(title,year,budget,rating)%>%arrange(desc(year))
dados

modelo <- lm(rating~budget+year,data=dados)
summary(modelo)

#preve a nota de um filme com base no seu orçamento e ano
funcao_que_preve <- function(orcamento,ano){
  predict(modelo,newdata=data.frame(budget=orcamento, year=ano))
}
#volta uma amostra aleatoria de dez casos da base
solta_10 <- function(){
  dados%>%sample_n(10)
}

##PLUMBER
#criar um arquivo R/nome.R com este conteudo
#* @get /solta
solta_10 <- function(){
  dados%>%sample_n(10)
}

#Apos criar este arquivo rodar os comando abaixo
p <- plumber::plumb('R/nome.R')
p$run(port=8888)#Passar a porta

#POST
#* @post /prever
funcao_que_preve <- function(orcamento,ano){
  d <- data.frame(budget=as.numeric(orcamento),year=as.numeric(ano))
  predict(modelo,newdata=d)
p <- plumber::plumb('R/nome.R')
p$run(port=8888)#Passar a porta 
}
#prever a nota
#curl --data "orcamento=10000&ano=1991" "http://localhost:8888/prever"

#OPEN CPU
#criando o pacote
devtools::create('preditorIMDb')
# @export
solta_10 <- function(){
  dplyr::sample_n(dados,10)
}
funcao_que_preve <- function(orcamento,ano){
  predict(modelo,newdata=data.frame(budget=orcamento,year=ano))
}
#adciona dependencia de pacotes externos
devtools::use_package('tydiverse',pkg='preditorIMDb')
#instalando o pacote na maquina
devtools::install('preditorIMDb')
#Rodar API
opencpu::ocpu_start_app('preditorIMDb')
#Ver a aplicação
#curl http://localhost:5656/ocpu/library/preditorIMDb/R/solta_10/print
#Se a função não tiver parametros rodar
# curl http://localhost:5656/ocpu/library/preditorIMDb/R/solta_10/json -x POST
