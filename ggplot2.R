install.packages('rbcb')
library(tidyverse)
library(janitor)
library(readxl)
#library(rbcb) biblioteca indisponivel
library(ggrepel)

df_feliz <- read_rds("dados_felicidade.rds")
head(df_feliz)

df_feliz%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point(aes(color=continent))+
  geom_smooth(method = 'lm')
df_feliz%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point(aes(color=continent))+
  facet_wrap(~continent)
#DIFERENÇA ENTRE GEOM_BAR E GEOM_COL
#geom_bar pricesa apenas do parametro x, a altura indica o numero de ocorrencia
#quantos países de cada continente estão presentes no nosso dataset
df_feliz%>%ggplot(aes(x=continent))+geom_bar()
#Agora utilizadno o geom_col
df_feliz%>%group_by(continent)%>%summarise(qtd_paises = n())%>%
  ggplot(aes(x=continent, y=qtd_paises))+geom_col()
#aqui o o color apenas muda a cor da borda se quiser mudar o cor da barra utilizar o fill
df_feliz%>%group_by(continent)%>%summarise(qtd_paises=n())%>%ggplot(aes(x=continent,y=qtd_paises))+
  geom_col(aes(color="red"))
df_feliz%>%group_by(continent)%>%summarise(qtd_paises=n())%>%ggplot(aes(x=continent, y = qtd_paises))+
  geom_col(color='red',fill="gray70")

#Selecionar os 10 paises mais felizes e mudar a cor referente a cada continente
df_feliz%>%top_n(10,life_ladder)%>%ggplot(aes(x=country,y=life_ladder))+geom_col(aes(fill=continent))
#Inverter eixos
df_feliz%>%top_n(10,life_ladder)%>%ggplot(aes(x=country, y=life_ladder))+geom_col(aes(fill=continent))+coord_flip()

#Mudando a ordem das barras
#agrupar os dados e contar a quantidade de paises por continente
df_feliz%>%group_by(continent)%>%summarise(qtd_paises=n())%>%mutate(continent=reorder(continent,qtd_paises))%>%
  ggplot(aes(x=continent,y=qtd_paises))+geom_col()
#Para escrever em ordem descrescente basta colocar o sinal de menos antes da variavel continua
df_feliz%>%group_by(continent)%>%summarise(qtd_paises=n())%>%mutate(continent=reorder(continent,-qtd_paises))%>%
  ggplot(aes(x=continent,y=qtd_paises))+geom_col()

#GRAFICOS DE DISTRIBUIÇÕES
#HISTOGRAMA
df_feliz%>%ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_histogram()
#Mudando o intervalo da variavel, utilizando o bins
df_feliz%>%ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_histogram(bins=5)
df_feliz%>%ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_histogram(bins=40)
#Utilizando o binwidth, controla a largura dos intervalos
df_feliz%>%ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_histogram(binwidth = 5)

#Estilizando o histograma
df_feliz%>%ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_histogram(binwidth=5, boundary=5, fill='green',colo='red',alpha=0.74)
#Uma cor para cada continente, pode fazer assim ou colocar o fiil no ggplot
df_feliz%>%ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_histogram(binwidth=5,boundary=5,aes(fill = continent))
#HISTOGRAMA UTILIZANDO FACET
df_feliz%>%ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_histogram(binwidth = 5,boundary=5)+facet_wrap(~continent,ncol=1)

#Grafico de densidade
df_feliz%>%ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_density()
df_feliz%>%ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_density(color='red',fill='green',alpha=0.7)

df_feliz%>%filter(continent !='Oceania')%>% ggplot(aes(x=healthy_life_expectancy_at_birth))+geom_density(aes(fill=continent),alpha=0.7)

#BOXPLOT
df_feliz%>%filter(continent != 'Oceania')%>%ggplot(aes(x=continent,y=healthy_life_expectancy_at_birth))+geom_boxplot()

df_feliz%>%filter(continent !='Oceania')%>% ggplot(aes(x=continent, y=healthy_life_expectancy_at_birth))+geom_boxplot()+coord_flip()

df_feliz%>%filter(continent != 'Oceania')%>%mutate(indice_felicidade= if_else(life_ladder > 6,
                                                                              "acima de 6",
                                                                              "abaixo"))%>%
  ggplot(aes(x=continent, y = healthy_life_expectancy_at_birth))+geom_boxplot(aes(fill=indice_felicidade))

df_feliz %>% filter(continent !='Oceania')%>%ggplot(aes(x=continent, y= healthy_life_expectancy_at_birth))+geom_boxplot()+
  geom_jitter(color='red')

#Importando dados da serie
lista_dataset <- rbcb::get_series(code=c(ipca=443,selic=4390))
str(lista_dataset)

#MAPAS DE CALOR
df_feliz_num <- df_feliz%>% select(life_ladder,log_gdp_per_capita,social_support,healthy_life_expectancy_at_birth,gini,perceptions_of_corruption)%>%
  na.omit()
matriz_correl <- cor(df_feliz_num)
matriz_correl <- as.data.frame(matriz_correl)
matriz_correl <- rownames_to_column(matriz_correl,"var1")
matriz_correl_tidy <- matriz_correl%>%gather(var2,correlacao,-var1)
head(matriz_correl_tidy)

matriz_correl_tidy %>%ggplot(aes(x=var1,y=var2,fill=correlacao))+geom_tile()
#Mudando as posições dos labels para ficar mais legivel
matriz_correl_tidy%>% ggplot(aes(x=var1,y=var2,fill=correlacao))+geom_tile()+theme(axis.text.x=element_text(angle=90))
#Mudando as cores do grafico
matriz_correl_tidy %>% ggplot(aes(x=var1,y=var2,fill=correlacao))+geom_tile()+
  scale_fill_distiller(type="div",palette="RdBu",direction = 1)+
  theme(axis.text.x = element_text(angle=90))

####################### Outro data set
#Cria uma data
ISOdate(year = 2011,month = 11,day = 22)
#importando dataset
data('txhousing')

txhousing %>%mutate(data_mes=as.Date(ISOdate(year=year,month = month,day = 1)))%>%group_by(data_mes)%>%
  summarise(volume_medio=mean(volume,na.rm = TRUE))%>%ggplot(aes(x=data_mes,y=volume_medio))+geom_line()
#Mudando a esca dos eixos
#Pode se utilizar o scale_x_date() ou scale_y_date()
txhousing %>% mutate(data_mes=as.Date(ISOdate(year=year,month=month,day=1)))%>%group_by(data_mes)%>%
  summarise(volume_medio=mean(volume,na.rm=TRUE))%>%ggplot(aes(x=data_mes,y=volume_medio))+geom_line()+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(labels=scales::number_format(big.mark = '.',decimal.mark = ','))+theme(panel.grid.minor.x = element_blank())
#Neste caso tambem pode dividir a scale em y por um milhão e avisar pela label
txhousing%>% mutate(data_mes=as.Date(ISOdate(year=year,month = month,day=1)))%>%group_by(data_mes)%>%
  summarise(volume_medio=mean(volume,na.rm=TRUE))%>%ggplot(aes(x=data_mes,y=volume_medio/1e6))+geom_line()+
  scale_x_date(date_breaks = '1 year',date_labels = '%Y')+labs(x='Ano',y="Volume médio(Milhoes)")
##################### Data set feliz
df_feliz_agg <- df_feliz%>%group_by(continent)%>%summarise(qtd_paises=n())
df_feliz_agg
df_feliz_agg%>%ggplot(aes(x=continent,y=qtd_paises))+geom_col()+geom_text(aes(label=qtd_paises))
df_feliz_agg%>%ggplot(aes(x=continent,y=qtd_paises))+geom_col()+geom_label(aes(label=qtd_paises))
df_feliz_agg%>%ggplot(aes(x=continent,y=qtd_paises))+geom_col()+geom_text(aes(label=qtd_paises),vjust=1,hjus=-1,color="white")
df_feliz%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point()+geom_text(aes(label=country))
america <- df_feliz%>%filter(continent=='Americas')
df_feliz%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point()+geom_text(data=america,aes(label=country))
df_feliz%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point()+geom_text_repel(data=america,aes(label=country))


matriz_correl_tidy%>%ggplot(aes(x=var1,y=var2,fill=correlacao))+geom_tile()+scale_fill_distiller(type='div',palette='RdBu',direction = 1)+
  theme(axis.text.x = element_text(angle=90))+ geom_text(aes(label=round(correlacao,2)))

#Grafico padrao a ser modificado
p <- df_feliz%>% ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point(aes(color=continent))+
  facet_grid(~continent)
#Alterando a tema
p+theme_bw()
p+theme_minimal()
p+theme_classic()
p+theme_dark()
#ALterando a posição da legenda
p+theme(legend.position = 'bottom')
#Remover minior grids do eixo x
theme(panel.grid.minor.y = element_blank())
#Mudando a cor de fundo
p+theme(panel.background = element_rect(fill='azure'))

#Titulos
p+labs(x='Log da PIB per capita',y='Expectativa de vida em anos',subtitle = 'Existe uma correlação positiva entre as duas variaveis',
       color='Continente \n do pais',caption='Autor: Eu')
