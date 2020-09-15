library(tidyverse)
library(tibble)
library(dplyr)

#Limpando o dataset
mtcars_clean <- mtcars %>% rownames_to_column(var='model')%>%as_tibble()%>%filter(cyl<8)
#Filtrando os dados 
mtcars_clean%>%filter(cyl==4)%>%group_by(cyl)%>%
    summarise(
        mpg = mean(mpg),
        wt = mean(wt)
    )
mtcars_clean%>%filter(cyl==6)%>%group_by(cyl)%>%summarise(drat=mean(drat),
                                                          disp=mean(disp))
########Função

#Limpar dataset
clean <- function(data,cyl_max=8){
    data %>%
        rownames_to_column(var="model")%>%as_tibble()%>%filter(cyl < cyl_max)
}
summarise_cyl <- function(data,cyl_num,...){
    data%>%filter(cyl==cyl_num)%>%group_by(cyl)%>%summarise_at(vars(...),mean)
}
mtcars%>%clean(cyl_max=8)%>%summarise_cyl(cyl_num = 4,mpg,wt)
