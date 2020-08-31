# Carregar pacotes

library(tidyverse)
library(lubridate)
library(magrittr)

# %<>% é um pipe %>%  que aplica a funçao posterior a ele e dps armazena oq foi executado entro da propria entidade


# Carregar dados
path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
path_dead = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'

dados_paises_covid <- read_csv(path)
dados_paises_covid_dead <- read_csv(path_dead)


# Ajustes

dados_paises_covid %<>% 
  rename(province = 'Province/State',
         country = 'Country/Region')

dados_paises_covid_dead %<>% 
  rename(province = 'Province/State',
         country = 'Country/Region')

colnames(dados_paises_covid)



dados_paises_covid %<>% 
  gather( key = 'date', #as colunas que restaram ele coloca na unica coluna data com as linhas com os valores
          value = 'infected', #os dados que estavam nas linhas antes do gather vai pras colunas infected
          -c(province:Long)) #pega de province até long

dados_paises_covid_dead %<>% 
  gather( key = 'date', 
          value = 'dead', 
          -c(province:Long)) #pega de province até long


dados_paises_covid %<>% 
  mutate( date = ymd(parse_date_time(date, #Troca do padrao americano pro BR (biblioteca lubridate)
                                     orders = c("ymd", "dmy", "mdy"))) )

dados_paises_covid_dead %<>% 
  mutate( date = ymd(parse_date_time(date, #Troca do padrao americano pro BR (biblioteca lubridate)
                                     orders = c("ymd", "dmy", "mdy"))) )


# Combinar os datasets de infectados e mortos

dados_paises_covid %<>% 
  bind_cols(., dados_paises_covid_dead %>% #o "." no primeiro argumento faz com que eu faça um append no fim do dataset dados_paises_covid 
              select(dead) )

#================================ EXERCICIOS ================================================

# 1) Quantos países existem no banco de dados
dados_paises_covid %>% select(country) %>% unique() %>% count() 

# 2) Quantos casos de infectados e mortes Brasil, Itália e E.U.A. possuem até o momento? 
dados_paises_covid %>% filter (country %in% c("Brazil","Italy","US")) %>% select(country,infected,dead) %>% group_by(country) %>% summarise("Infectados" = sum(infected),"Mortos" = sum(dead))

# 3) Quais os 10 países que possuem os maiores valores de infectados? E quais os 10 de maiores mortes? 
dados_paises_covid %>% select(country,infected) %>% group_by(country) %>% summarise("Infectados" = sum(infected)) %>% arrange(desc(Infectados)) %>% head(10)
dados_paises_covid %>% select(country,dead) %>% group_by(country) %>% summarise("Mortos" = sum(dead)) %>% arrange(desc(Mortos)) %>% head(10)

# 4) Criar uma coluna denominada taxa de mortalidade (mortos/infectados). Quais os 10 países que possuem maiores valores desta taxa? 
dados_paises_covid %>% group_by(country) %>% summarise("dead"=sum(dead),"infected"=sum(infected)) %>% mutate(Taxa_de_mortalidade = dead/infected) %>% select(country,Taxa_de_mortalidade) %>% arrange(desc(Taxa_de_mortalidade)) %>% head(10)

# 5) a) Gráfico de linhas contendo a evolução do número de infectados do Brasil e Itália. 
dados_paises_covid %>% select(country,date,infected) %>% filter (country %in% c("Brazil","Italy"))  %>% arrange(date) %>% ggplot(aes(x = date,y = infected,color=country)) + geom_line() + ggtitle(" Evolu��o dos Casos Confirmados de COVID-19 - Brasil e Italia")

# 5) b) Gráfico de linhas contendo a evolução do número de mortos do Brasil e Itália.
dados_paises_covid %>% select(country,date,dead) %>% filter (country %in% c("Brazil","Italy"))  %>% arrange(date) %>% ggplot(aes(x = date,y = dead,color=country)) + geom_line() + ggtitle(" Evolu��o das Mortes Causadas pelo COVID-19 - Brasil e Italia")

# 5) c) Gráfico de barras comparando o número de infectados de Brasil e Itália em 21/abr. 
dados_paises_covid %>%  select(country,date,infected) %>% filter (country %in% c("Brazil","Italy"),date =="2020-04-21") %>%  ggplot(aes(x = country,y = infected))+ geom_bar (stat="identity") + ggtitle(" Total de Infectados pelo COVID-19 - Brasil e Italia - Data: 21/04/2020")                                                                                                                                                                                     

# 5) d) Gráfico de barras comparando o número de mortos de Brasil e Itália em 21/abr. 
dados_paises_covid %>%  select(country,date,dead) %>% filter (country %in% c("Brazil","Italy"),date =="2020-04-21") %>%  ggplot(aes(x = country,y = dead))+ geom_bar (stat="identity") + ggtitle(" Total de Mortos pelo COVID-19 - Brasil e Italia - Data: 21/04/2020")                                                                                                                                                                                    

# 5) e) Gráfico de barras comparando a taxa de mortalidade de Brasil e Itália em 21/abr
dados_paises_covid %>%  select(country,date,infected,dead) %>% filter (country %in% c("Brazil","Italy"),date =="2020-04-21") %>% mutate(Taxa_de_mortalidade = dead/infected) %>%  ggplot(aes(x = country,y = Taxa_de_mortalidade))+ geom_bar (stat="identity") + ggtitle(" Taxa de Mortalidade do COVID-19 - Brasil e Italia - Data: 21/04/2020")