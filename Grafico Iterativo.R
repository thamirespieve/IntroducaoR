install.packages("devtools")
devtools::install_github("italocegatta/brmap")
devtools::install_github("r-spatial/sf")
library(tidyverse)
library(plotly)
#library(sf)
library(ggmap)
library(sunburstR)
library(leaflet)
library(treemap)
#library(brmap)
#Imporatando dados
df_feliz <- read_rds('dados_felicidade.rds')

p <- df_feliz%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+geom_point(aes(color=continent))+
  geom_smooth(method='lm')+theme_minimal()
#Transdormando o grafico em iterativo
ggplotly(p)
#cutomizar o dialog que abre quando passa o cursor por um ponto, a principio ele mostra tudo que foi definido no aes
#Mostrando somente as especificações em x
ggplotly(p,tooltip = 'x')

nome <- c('Lucas','Eduardo','Flavio')
sobrenome <- c('Silva','Oliveira','Dias')
paste0(nome,' ', sobrenome)

p <- df_feliz%>%ggplot(aes(x=log_gdp_per_capita,y=healthy_life_expectancy_at_birth))+
  geom_point(aes(color=continent,text=paste0(country,h=healthy_life_expectancy_at_birth,log_gdp_per_capita)))+
  geom_smooth(method='lm')+theme_minimal()

ggplotly(p,tooltip = 'text')
#quebrando uma linha dentro do dialog

p <- df_feliz%>%ggplot(aes(x= healthy_life_expectancy_at_birth,y=log_gdp_per_capita))+
  geom_point(aes(color=continent,text=paste0("Pais: ",country,'\n',
                                            "Expec. vida: ",round(healthy_life_expectancy_at_birth,0),'\n',
                                            "PIB: ",round(log_gdp_per_capita,2))))+
  geom_smooth(method = 'lm')+theme_minimal()

ggplotly(p,tooltip = 'text')

#Salvando um grafico ggplotly
p_interativo <- ggplotly(p,tooltip = 'text')
#Salvando
htmlwidgets::saveWidget(p_interativo,'meu_grafico_interativo.html')

#Como não consegui baixar os pacotes brmap e sf esses comandos não funcionam
#Impartação do dataset(shapefile)
data(brmap_estado)
head(brmap_estado)
#construindo o mapa
ggplot(brmap_estado)+geom_sf
#Customizandoo o grafico utlizando o theme
ggplot(brmap_estado)+
  geom_sf(aes(geometry=geometry, fill=as.character(cod_regiao)))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color='transparent'),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  labs(fill='Região')

link <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_partido_munzona/votacao_partido_munzona_2014.zip"
download.file(link, destfile = "tse.zip")
# descompactar arquivo
unzip("tse.zip")

#definindo um vetor com o nome das colunas
cols <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO", "NUM_TURNO",
          "DESCRICAO_ELEICAO", "SIGLA_UF", "SIGLA_UE", "CODIGO_MUNICIPIO",
          "NOME_MUNICIPIO", "NUMERO_ZONA", "CODIGO_CARGO", "DESCRICAO_CARGO",
          "TIPO_LEGENDA", "NOME_COLIGACAO", "COMPOSICAO_LEGENDA",
          "SIGLA_PARTIDO", "NUMERO_PARTIDO", "NOME_PARTIDO",
          "QTD_VOTOS_NOMINAIS", "QTD_VOTOS_LEGENDA", "TRANSITO"
          )
cols <- str_to_lower(cols)
lcl <- locale(encoding = 'ISO-8859-1')
eleicoes <- read_csv2("/votacao_partido_munzona_2014_BR.csv",
                      col_names = FALSE,
                      locale = lcl,
                      progress = FALSE)%>%seleciona(-22)%>%set_names(cols)
%>%seleciona(-22)%>%set_names(cols)head(eleicoes)

eleicoes_agg <- eleicoes %>% filter(ano_eleicao ==2014,num_turno==2,descricao_cargo=="Presidente")%>%
  group_by(sigla_uf,sigla_partido)%>%summarise(qtd_votos=sum(qtd_votos_nominais))%>%
  spread(sigla_partido,qtd_votos,fill=0)%>%
  mutate(prop_PT=PT/(PT+PSDB))
head(eleicoes_agg)

estado_eleicoes <- left_join(brmap_estado,eleicoes_agg,by=c("estado_sigla"="sigla_uf"))
head(estado_eleicoes)

ggplot(estado_eleicoes)+geom_sf(aes(fill=prop_PT))+scale_fill_distiller(type='seq',
                                                                        pallete='Reds',
                                                                        direction = 1)+
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        panel.grid.major = element_line(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  labs(title='Proporção de votos validos para o PT por estado ',fill=NULL)

################# MAPAS ITERATIVOS
leaflet()%>%addTiles()
#Adiconando pontos no mapa
paleta_cores <- colorFactor(c("blue", "red"),
                            unique(orcamento_sp$pa))
leaflet()%>%addTiles()%>%addCircleMarkers(data=orcamento_sp,
                                          lng=~longitude, lat=~latitude,popup=~ds_fonte,color=~paleta_cores(pa))

#GRAFICO
orcamento_pessoal <- tribble(~nivel1,~nivel2,~nivel3,~despesa,
                             "Moradia", "Aluguel", NA, 1300,
                             "Moradia", "Condominio", NA, 800,
                             "Mercado", "Alimentação", "Carnes", 150,
                             "Mercado", "Alimentação", "Vegetais", 50,
                             "Mercado", "Mat. de limpeza e higiene", NA, 100,
                             "Transp", "Carro", NA, 400,
                             "Transp", "Publico", NA, 150,
                             "Contas", "Energia", NA, 130,
                             "Contas", "Agua", NA, 60,
                             "Contas", "Netflix", NA, 27.90,
                             "Contas", "Internet", NA, 80,
                             "Lazer", "Restaurantes", NA, 350,
                             "Lazer", "Cultura", "Cinema", 100,
                             "Lazer", "Cultura", "Teatro", 50,
                             "Lazer", "Cultura", "Livros", 90)

head(orcamento_pessoal)
treemap(orcamento_pessoal,index = c('nivel1','nivel2','nivel3'),
        vSize = 'despesa',title='Treemap de orcamento pessoal',
        fontface.labels = c("oblique","bold","italic"),
        ymod.labels = c(0.6,0.0))
#GRAFICO DE PIZZA
sequences <- read.csv(system.file("examples/visit-sequences.csv",package='sunburstR'),header = FALSE, stringsAsFactors = FALSE)[1:100,]
head(sequences)
sunburst(sequences)

orcamento_pessoal_sb <- orcamento_pessoal%>%unite(hierarquia, nivel1:nivel3, sep = "-")
sunburst(orcamento_pessoal_sb)
orcamento_pessoal%>%mutate(nivel3=ifelse(is.na(nivel3),"",nivel3))%>%unite(hierarquia,nivel1:nivel3,sep='-')%>%
  sunburst()
