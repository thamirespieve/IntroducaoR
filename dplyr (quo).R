require(dplyr)
require(rlang)
#transformando em uma tibble
iris_tibble <- as_tibble(iris)

#fazendo uma função para selecionar uma coluna
seleciona <- function(dados,coluna){
  #Tem como fazer o select com o pull também
  dados%>%select(coluna)
}
#chamando a função e passando o dataset e a colunada desejada como parametros
seleciona(iris_tibble,'Species')

#Para passar o nome da coluna sem aspas e necessario utilizar o quo() ele retorna
#a "expressão" sem fazer qualquer tipo de alterção
seleciona <- function(dados,coluna){
  #quando se utiliza o quo é necessario utilizar a !! para informar que
  #deve se substituir a expressão quo por seu devido valor
  dados %>% select(!!coluna)
}
#Utilizando o quo para especificar a coluna
seleciona(iris_tibble,quo(Species))

#Pasando o quo dentro da função é necessario utilizar o enquo
seleciona <- function(dados,coluna){
  coluna <- enquo(coluna)
  print(coluna)
  dados%>%select(!!coluna)
}

seleciona(iris_tibble, Species)
#Criando uma tibble
(df <- tibble(
  g1= c(1,1,2,2,2),
  g2=c(1,2,1,2,1),
  a=sample(5),
  b=sample(5)
))
my_summarise <- function(df,var_grupo,var_summarise){
    var_grupo <- enquo(var_grupo)
    var_summarise <- enquo(var_summarise)
    df%>%group_by(!!var_grupo)%>%summarise(mean(!!var_summarise))
}
#Calculando a media de a, agrupando pelos valores de g1
my_summarise(df,g1,a)

#MUDAR NOMES DE VARIAVEIS
my_summarise2 <- function(df,var1,var2){
  var1 <- enquo(var1)
  var2 <- enquo(var2)

  mean_name1 <- paste0("media_",quo_name(var1))
  mean_name2 <- paste0("media_",quo_name(var2))
  
  summarise(df,
            !!mean_name1:=mean(!!var1),
            !!mean_name2:=mean(!!var2)
            )
}
my_summarise2(df,a,b)
#Passando o nome da coluna que vai ser criada como parametro
my_summarise3 <- function(df,coluna,nome){
  coluna <- enquo(coluna)
  nome <- enquo(nome)
  
  df%>%summarise(
    !!quo_name(nome):=mean(!!coluna)
  )
}
my_summarise3(df,a,media)

#Passando o argumento da função com aspas utilizar o sym()
my_summarise_string <- function(df,var_group, var_summarise){
  var_group <- sym(var_group)
  var_summarise <- sym(var_summarise)
  
  df%>%group_by(!!var_group)%>%summarise(media=mean(!!var_summarise))
}
my_summarise_string(df,'g1','a')

#Para passar o argumento com ou sem aspas utilizar o try e o if
summarise_by <- function(df,var_grupo,var_soma){
  teste_1 <- try(is.character(var_grupo),silent=T)
  teste_2 <- try(is.character(var_soma),silent=T)
  if(teste_1==T & teste_2==T){
    summ_sym <- sym(var_soma)
    group_syms <- sym(var_grupo)
    
    return(
        df%>% group_by(!!group_syms)%>%
        summarise(soma=sum(!!summ_sym))%>%
        arrange(!!group_syms)
      )
  } else if(teste_1 !=T & teste_2 !=T){
    summ_sym <- enquo(var_soma)
    group_syms <- enquo(var_grupo)
    return(
      df%>% group_by(!!group_syms)%>%
      summarise(soma= sum(!!summ_sym))%>%
      arrange(!!group_syms)
      )
  }
}
summarise_by(df,g1,a)
summarise_by(df,'g1','a')

#CAPTURANDO EXPRESSÔES PARA FILTRAR
meu_filtro <- function(df,filtro){
  filtro <- enquo(filtro)
  df%>%filter(!!filtro)
}
meu_filtro(df,g1==1)

#JOIN
base1 <- tibble(id = sample(1:5, 3), val = sample(1:10, 3))
base1

base2 <- tibble(id = sample(1:5, 3), val = sample(1:10, 3))
base2

meu_full_join<- function(b1,b2,id1,id2){
  id1 <- enquo(id1)
  id2 <- enquo(id2)
#set_names cria um vetor nomeado utilizando o primeiro argumento como valor e o segundo argumento como nome desse vetor
  by <- set_names(quo_name(id2),quo_name(id1))
  b1 %>% 
    full_join(b2, by = by)
  
}
meu_full_join(base1,base2,id,id)

#FUNÇÂO COM MULTIPLAS VARIAVEIS 
conta_casos <- function(df,...){
  var_group <- quos(...)
  if(length(var_group)>0){
    df%>%group_by(!!!var_group)%>%summarise(n=n())
  }
  else{
    warning("Colunas para agrupar não encontradas")
    df%>%summarise(n=n())
  }
}
conta_casos(df,g1,g2)
conta_casos(df)