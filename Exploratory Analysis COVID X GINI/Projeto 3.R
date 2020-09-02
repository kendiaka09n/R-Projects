install.packages("mosaicCalc")
install.packages("ineq")
install.packages("magrittr")
install.packages("mosaicCalc")

# Carregar pacotes #
library(tidyverse)
library(magrittr)
library(mosaicCalc)
library(ineq)

#Indicar diretorio de trabalho
setwd("C:/Users/kendi/Desktop/R/Atividade1")

# carregar dados gerais dos municipios

dados_municipios_gerais <- read_csv2("dados_municipios_2010.csv")

# dados covid dos municipios

path <- 'https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv'
dados_municipios_covid <- read_csv(path)

dados_municipios_covid %<>% 
  filter( state != 'TOTAL',
          date >= max(date)) %>%
  select(ibgeID, date, state, city, totalCases, deaths)


#análise exploratória
View(dados_municipios_covid)
str(dados_municipios_covid)
summary(dados_municipios_covid)

view(dados_municipios_gerais)
str(dados_municipios_gerais)
summary(dados_municipios_gerais)



# 1) Quantas cidades existem no banco de dados
dados_municipios_covid %>% select() %>% unique() %>% count() %>%  view()
dados_municipios_gerais %>% select(ibgeID) %>% unique() %>% count() %>%  view()

#2 - Como realizar o join desses dataframes? (realizar o join!! Dica, usar a função left_join)

municipios_consolidados <-  tibble(left_join(dados_municipios_covid, 
                                      dados_municipios_gerais,
                                      by ="ibgeID",
                                      na_matches = c( "na", "never")))

#3 - Obtenha o sumário das colunas por UF (state), similar a figura abaixo (apresentar o código):

str(municipios_consolidados)

dados_paises_covid %>% select(country,infected) %>% group_by(country) %>% summarise("Infectados" = sum(infected)) %>% arrange(desc(Infectados)) %>% head(10)

#dataframe consolidado com a somatória de casos (deaths, total cases) e média de indicadores (idhm, rdpc, epvida, etc)
municipios_consolidados_mean <-municipios_consolidados %>% 
  select(state,totalCases, deaths, ESPVIDA, E_ANOSESTUDO, T_ANALF18M, RDPC, IDHM) %>% 
  group_by(state) %>% 
  summarise('TotalCases' = sum(totalCases), 'Deaths' = sum(deaths), 'IDHM' = mean(IDHM, na.rm = TRUE),'RDPC' = mean(RDPC, na.rm = TRUE),'ESPVIDA' = mean(ESPVIDA, na.rm = TRUE),'E_ANOSESTUDO' = mean(E_ANOSESTUDO, na.rm = TRUE),'T_ANALF18M' = mean(T_ANALF18M, na.rm = TRUE)) %>%  view()

#aplicação gini para a coluna AINDA FALTA FAZER

municipios_consolidados_mean %>%select(state,RDPC) %>%  rowwise() %>% mutate(gin =ineq( municipios_consolidados_mean$RDPC, type = "Gini" ) ) %>% view()

####gini_estado_1#  ineq( estados_1$RDPC, type = "Gini" ) # formula que ajuda a vida


#4 - Baseando-se nos resultados da tabela acima obtida, gere os seguintes gráficos:
#a) Gráfico de pontos de totalCases e ESPVIDA.

municipios_consolidados_mean%>%  ggplot(aes(x =ESPVIDA ,y = TotalCases))+ geom_point (stat="identity") + ggtitle("Total de casos X Espectativa de vida")

#b) Gráfico de pontos de totalCases e E_ANOSESTUDO.

municipios_consolidados_mean%>%  ggplot(aes(x =E_ANOSESTUDO ,y = TotalCases))+ 
  geom_point (stat="identity") + 
  ggtitle(" Total de casos X Ano de Estudo")

#c) Gráfico de pontos de totalCases e T_ANALF18M.
municipios_consolidados_mean%>%  ggplot(aes(x =T_ANALF18M ,y = TotalCases))+ 
  geom_point (stat="identity") + 
  ggtitle(" Total de casos X Analfabetismo")

#d) Gráfico de pontos de totalCases e renda.
municipios_consolidados_mean%>%  ggplot(aes(x =RDPC ,y = TotalCases))+ 
  geom_point (stat="identity") + 
  ggtitle(" Total de casos X Renda")

#e) Gráfico de pontos de totalCases e IDHM.
municipios_consolidados_mean%>%  ggplot(aes(x =IDHM ,y = TotalCases))+ 
  geom_point (stat="identity") + 
  ggtitle(" Total de casos X IDHM")

#f) Gráfico de pontos de totalCases e GINI.

#g) Gráfico de pontos de Deaths e ESPVIDA.
municipios_consolidados_mean%>%  ggplot(aes(x =ESPVIDA ,y = Deaths))+ 
  geom_point (stat="identity") + 
  ggtitle(" Mortes X Expectativa de vida")

#h) Gráfico de pontos de Deaths e E_ANOSESTUDO.
municipios_consolidados_mean%>%  ggplot(aes(x =E_ANOSESTUDO ,y = Deaths))+ 
  geom_point (stat="identity") + 
  ggtitle(" Mortes X Anos de Estudo")

#i) Gráfico de pontos de Deaths e T_ANALF18M.
municipios_consolidados_mean%>%  ggplot(aes(x =T_ANALF18M ,y = Deaths))+ 
  geom_point (stat="identity") + 
  ggtitle(" Mortes X Analfabetismo")

#j) Gráfico de pontos de Deaths e renda.
municipios_consolidados_mean%>%  ggplot(aes(x =RDPC ,y = Deaths))+ 
  geom_point (stat="identity") + 
  ggtitle(" Mortes X Renda")

#k) Gráfico de pontos de Deaths e IDHM.
municipios_consolidados_mean%>%  ggplot(aes(x =IDHM ,y = Deaths))+ 
  geom_point (stat="identity") + 
  ggtitle(" Mortes X IDHM")

#l) Gráfico de pontos de Deaths e GINI.


#5 - Por meio das análises dos gráficos pode-se presumir algum resultado interessante? Apresente sua opinião.























