install.packages("patchwork")

library(tidyverse)
library(janitor)
library(readxl)
library(countrycode)
library(patchwork)
sessionInfo()

#Baixando o arquivo do endereço

download.file("https://s3.amazonaws.com/happiness-report/2018/WHR2018Chapter2OnlineData.xls",
              destfile = "felicidade.xls")
#importando a planilha
df_pais <- read_excel("felicidade.xls",sheet=1)
#limpando o nome das colunas
df_pais <- janitor::clean_names(df_pais)
#vendo os dados
glimpse(df_pais)

df_continente <- countrycode::codelist %>% 
  # selecionar colunas importantes
  select(country = cow.name,continent)%>%
  # filtrar fora os paises sem continentes
  filter(!is.na(continent))
#criar dataframe com a junção das duas tabelas
df <- left_join(df_continente,df_pais,by='country')
df
df$continent[df$country == "Congo (Brazzaville)"] <- "Africa"
df$continent[df$country == "Congo (Kinshasa)"] <- "Africa"
df$continent[df$country == "Hong Kong S.A.R of China"] <- "Asia"
df$continent[df$country == "Kosovo"] <- "Europe"
df$continent[df$country == "North Cyprus"] <- "Asia"
df$continent[df$country == "Palestinia Territories"] <- "Asia"
df$continent[df$country == "Serbia"] <- "Europe"
df$continent[df$country == "Somililand region"] <- "Africa"
df$continent[df$country == "Taiwan Province of China"] <- "Asia"
df$continent[df$country == "United States"] <- "Americas"

df$year <- as.integer(df$year)
df_2017 <- df%>% filter(year==2017)
#Grafico de Coluna
#group_by agroupou os continentes
#summarise fez a media do life_ladder
#aes definiu o nome dos eixos e o geom_col definiu o tipo grafico
df_2017 %>%
  group_by(continent) %>% 
  summarise(life_ladder = mean(life_ladder)) %>% 
  ggplot(aes(x = continent, y = life_ladder)) +
  geom_col()

#Grafico de pontos
df_2017%>%ggplot(aes(x=log_gdp_per_capita, y= healthy_life_expectancy_at_birth))+geom_point()

#Grafico de linhas
df%>%filter(country=="Brazil",year >= 2007)%>%ggplot(aes(x=year,y=life_ladder))+geom_line()

#Histograma
df_2017%>%ggplot(aes(x=life_ladder))+geom_histogram()

#Inserir texto
#No geom_texte dentro do aes a gente difiniu o label como country, isto é, no grafico de pontos vai aparecer o nome
#de cada país encima de seu determinado ponto
df_2017%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point()+geom_text(aes(label=country))

#Insere linhas verticas ou horizontais no grafico
df_2017%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point()+
  geom_vline(aes(xintercept=mean(df_2017$log_gdp_per_capita,na.rm=TRUE)))+
  geom_hline(aes(yintercept=mean(df_2017$healthy_life_expectancy_at_birth,na.rm = TRUE)))

#AES
#mudando a cor de todos os pontos
df_2017%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point(color='brown')
#definir cores distintas para cada continente
df_2017%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point(aes(color=continent))
df_2017%>%ggplot(aes(x=log_gdp_per_capita, y=healthy_life_expectancy_at_birth,color=continent))+geom_point()

#geom_smooth()
#Mostrando a diferença entre usar o definição de cor dentro do ggplot e do geom_, quando se define
#atributos dentro do geom_ ele é aplicado uma vez referente ao grafico todo, porém quando se utiliza
#dentro da função ggplot ela é aplicado para cada continente (neste exemplo)
df_2017%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point(aes(color=continent))+
  geom_smooth(method = 'lm',se=FALSE)
df_2017%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth,color=continent))+geom_point()+
  geom_smooth(method='lm',se=FALSE)

#MAIS ATRIBUTOS PARA O AES
df_2017%>%ggplot(aes(x=log_gdp_per_capita, y=healthy_life_expectancy_at_birth))+geom_point(aes(color=continent,
                                                                                               size=confidence_in_national_government,
                                                                                               alpha=0.7))
#SEGMENTAÇAÕ DE GRAFICOS UTILIZANDO O FACETS()
# permitem segmentar um gráfico em subgráficos de acordo com uma variável categórica.

#Utilizando o facet_wrap passa continente como atribute ele dividiu o grafico nos 5 continentes
df_2017%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point(aes(color=continent))+facet_wrap(~continent)

#Quandi quizer fazer essa divisão de graficos com mais de uma variavel é necessario utilizar o facet_grid
df%>%filter(between(year,2015,2017))%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point(aes(color=continent))+facet_grid(continent~year)

#Salvando data frame
#retirando as colunas que não quero
df_2017%>%select(-c(democratic_quality,delivery_quality,gini_index_world_bank_estimate,
                    gini_index_world_bank_estimate_average_2000_15,
                    confidence_in_national_government, generosity,
                    standard_deviation_of_ladder_by_country_year,
                    standard_deviation_mean_of_ladder_by_country_year))%>%
  #renomeando uma coluna
  rename(gini=gini_of_household_income_reported_in_gallup_by_wp5_year)%>%
  #Salvando os dados em um arquivo rds
  write_rds("dados_felicidade.rds")

#Salvando um grafico 
p <- df_2017%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point()
ggsave(filename = 'Meu grafico de pontos.png',plot=p)

#Juntando diferente graficos em um só
graf_barras <- df_2017%>%group_by(continent)%>%summarise(life_ladder=mean(life_ladder))%>%
  ggplot(aes(x=continent,y=life_ladder))+geom_col()
graf_pontos <- df_2017%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point()
#Juntando os dois graficos em um só
graf_barras+graf_pontos
#Juntando os dois graficos em apenas um só com uma coluna
graf_barras+graf_pontos+plot_layout(ncol=1)
