install.packages("tidyverse")
library(tidyverse)

#Salvar url de download no obj link
link <- "http://transparencia.al.gov.br/media/arquivo/comparativo_despesas-2017.zip"
#Criando uma pasata chamada data para guardar o arquivo
dir.create("data/")
#Realizando o download do arquivo
download.file(link,destfile = "data/desp_al_2017.zip")
#descompactar o arquivo
unzip("data/desp_al_2017.zip",exdir="data/")

# usamos a função read_delim porque o arquivo vem separado por
# barras verticais (|)
# alem disso, precisamos especificar o locale para caracteres especiais
# serem importados corretamente para o R
#no read_delom definimos o nome do arquivo a ser importado, o caracter que separa as colunas,
# e o locale(encoding) necessario ao trabalhar com linux com arquivos brasileiros
df_despesas <- read_delim("data/comparativo_despesas-2017.txt",
                          delim= "|",
                          locale=locale(encoding = "ISO-8859-1"),
                          progress=FALSE)
#Exibir as 6 primeiras linhas do arquivo importado
head(df_despesas)

##### DPLYR
##SELECT
df_despesas
#Buscando dado select()
df_despesas%>%select(ANO=ANO,UG,CODIGO_GESTAO=GESTAO)%>%head()
#Removendo coluna select(-Nome)
df_despesas%>%select(-ANO)%>%head()
#Selecionar todas as colunas numericas
df_despesas%>%select_if(is.numeric)%>%head()
#Selecionar colunas que comecem com determinado nome
df_despesas%>%select(starts_with("VALOR"))%>%head()

## FILTER
df_despesas%>%filter(VALOR_EMPENHADO>100000)%>%head()
#filtrar as linhas onde o MES é igual ao MES atual
df_despesas%>% filter(MES==lubridate::month(lubridate::today()))
#filtrar um mes especifico
df_despesas%>%filter(MES==7)%>%head()
#filtrar as linhas dentro de um intervalo (inclusivo)
df_despesas%>%filter(between(DATA_REGISTRO, as.Date("2017-11-15"),as.Date("2017-12-15")))%>%head()
#filtrar dados exeto
df_despesas%>%filter(ANO!=2017)%>%head()
#filtrar dados baseados em multiplas colunas
df_despesas%>%filter(str_detect(DESCRICAO_UG,'POLICIA'),MES==1,VALOR_EMPENHADO==0)

## MUTATE
#Criam novas colunas 
df_menor <- df_despesas%>%select(DESCRICAO_UG,CODIGO_FAVORECIDO,NOME_FAVORECIDO,DATA_REGISTRO,
                                VALOR_EMPENHADO,VALOR_LIQUIDADO,VALOR_PAGO )%>%head(10)
#Criar novo coluna com o dia do registro
df_menor%>%mutate(DIA_REGISTRO=lubridate::day(DATA_REGISTRO))%>%select(DATA_REGISTRO,DIA_REGISTRO)
#criar varias colunas de resultados de equações simples
df_menor%>%mutate(VALOR_PAGO_RAIZ_2=sqrt(VALOR_PAGO),
                  valor_pago_quadrado=VALOR_PAGO^2,
                  valor_pago_dobro=VALOR_PAGO*2,
                  valor_pago_arredondado=round(VALOR_PAGO),
                  valor_pago_soma=VALOR_PAGO+VALOR_EMPENHADO+VALOR_LIQUIDADO)%>%
  select(contains("valor_pago",ignore.case = TRUE))

#extrair o primeiro nome da coluna NOME_FAVORECIDO
df_menor%>%mutate(primeiro_nome=stringr::word(NOME_FAVORECIDO,1))%>% select(NOME_FAVORECIDO,primeiro_nome)
#Passar o nome para minusculo deixando apenas a primeira letra maiuscula
df_menor%>%mutate(NOME_FAVORECIDO=stringr::str_to_title(NOME_FAVORECIDO))%>%select(NOME_FAVORECIDO)

#ARRANGE
# muda a posição das linhas do dataframe baseado em uma ou mais colunas, em ordem crescente ou decrescente
df_menor%>%arrange(NOME_FAVORECIDO)
#Ordem descrescente
df_menor%>%arrange(desc(NOME_FAVORECIDO))
# mostrar datas onde houve os maiores valores de despesa com folha de pagamento
df_despesas%>%
  filter(NOME_FAVORECIDO=="FOLHA PAGTO. PESSOAL")%>%
  select(DATA_REGISTRO,VALOR_PAGO)%>%
  arrange(desc(VALOR_PAGO))
# classificar dados pelo NOME_FAVORECIDO, mostrando os maiores valores pagos para cada
df_despesas%>%select(DATA_REGISTRO,NOME_FAVORECIDO,VALOR_PAGO)%>%arrange(NOME_FAVORECIDO,desc(VALOR_PAGO))

##group_by() e summarise()
#é excelente para agregar e resumir dados. 
df_despesas%>%group_by(DESCRICAO_UG)%>%summarise(VALOR_PAGO_MEDIO=mean(VALOR_PAGO))
#Calcula a quantida do valor pago, a qtd de registro e de favorecidos classificar em ordem decrescente
df_despesas%>%group_by(DESCRICAO_UG)%>%summarise(VALOR_PAGO=sum(VALOR_PAGO),
                                                 QTD_OBSERVACAO=n(),
                                                 QTD_FAVORECIDO=n_distinct(CODIGO_FAVORECIDO))%>%
  arrange(desc(QTD_FAVORECIDO))
# calcular a soma de todas as variáveis que começam com valor por ano e mês...
# apenas para o UG "ALAGOAS PREVIDENCIA"

df_despesas%>%filter(DESCRICAO_UG=='ALAGOAS PREVIDENCIA')%>%group_by(ANO,MES)%>%
  summarise_at(vars(contains("VALOR_")), sum)
# count() é uma alternativa a group_by() + summarise(n())
df_despesas%>%count(CODIGO_FAVORECIDO,NOME_FAVORECIDO)%>%filter(n>100)%>%arrange(desc(n))

#### JUNTANDO TABELAS
###JOIN

vendedor <- tibble(
  id=c('A12','A13','A14','A15'),
  regiao=c("Sul","Sudeste","Oeste","Norte"),
  experiencia=c(5,2,12,8)
)
vendas=tibble(
  id=c("A13",'A14','A12','A11'),
  vendas=c(1200,2500,350,1000)
)
vendedor
vendas

#LEFT_JOIN(x,y,by)
#retorna todas as observações em x mesmo que não a correspondencia em y de acordo com o by
left_join(vendedor,vendas,by='id')

#INNER_JOIN(x,y,by)
#retorna so as observações correspondentes em x e y
inner_join(vendedor,vendas,by='id')

#FULL_JOIN(x,y,by)
# È mais completo e retorna todas as observações presentes em x e y
full_join(vendedor,vendas,by='id')

##### TIDYR
# É focado no manuseio de dados, transforma datasets no formato tidyr
data("economics")
head(economics)

economics%>% mutate(ano=lubridate::year(date))%>%
  group_by(ano)%>%
  summarise(pce=mean(pce),
            pop=mean(pop),
            psavert=mean(psavert),
            uempmed=mean(uempmed),
            unemploy=mean(unemploy))
#GATHER()
#cria um coluna com indicadores 
economics%>%
  gather(indicador,valor,-date)

data("economics_long")
head(economics_long)
economics_long%>%select(-value01)%>% spread(variable,value,fill=NA)
