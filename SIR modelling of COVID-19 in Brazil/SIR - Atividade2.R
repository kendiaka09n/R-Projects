# https://analisemacro.com.br/economia/comentario-de-conjuntura/modelando-o-coronavirus-no-brasil-modelo-sir/

### Carregar pacotes ###

library(tidyverse)
library(lubridate)
library(magrittr)
#install.packages("deSolve") # caso nao tenha instalado
library(deSolve)




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
  gather( key = 'date', 
          value = 'infected', 
          -c(province:Long))

dados_paises_covid_dead %<>% 
  gather( key = 'date', 
          value = 'dead', 
          -c(province:Long))


dados_paises_covid %<>% 
  mutate( date = ymd(parse_date_time(date, 
                                     orders = c("ymd", "dmy", "mdy"))) )

dados_paises_covid_dead %<>% 
  mutate( date = ymd(parse_date_time(date, 
                                     orders = c("ymd", "dmy", "mdy"))) )


# Combinar os datasets de infectados e mortos

dados_paises_covid %<>% 
  bind_cols(., dados_paises_covid_dead %>% 
              select(dead) )

# selecionar os paises

dados_brazil <- dados_paises_covid %>% 
  filter(country == 'Brazil') %>% 
  select(country, date, infected, dead)

dados_brazil %<>% 
  na_if(0) %>% 
  drop_na(infected) %>% 
  mutate( dead = ifelse( is.na(dead) == TRUE, 0, dead) )


### Modelo SIR ###

## Simulacao Brasil ##
populacao_brasil <- 211576094 #pesquisar populacao do Brasil#

N <- populacao_brasil

Infectados <- dados_brazil$infected

dias_simulacao <- 1:NROW(dados_brazil)

# equacao SIR (alguns ajustes para acomodar a populacao)
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# estado inicial do SIR
init <- c(S = N-Infectados[1], 
          I = Infectados[1], 
          R = 0)

# minizacao do erro
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  
  out <- ode(y = init, times = dias_simulacao, 
             func = SIR, parms = parameters)
  
  fit <- out[ , 3] # obter coluna de infectados
  
  sum((Infectados - fit)^2)
}

# metodo para encontrar a melhor curva
Opt <- optim(c(0.5, 0.5), RSS,
             method = "L-BFGS-B",
             lower = c(0, 0), upper = c(1, 1))

Opt_par <- setNames(Opt$par, c("beta", "gamma"))

# encontrar a curva
fit <- data.frame(ode(y = init, times = dias_simulacao,
                      func = SIR, parms = Opt_par))

# criar dataset para apresentar as curvas
base_ajustada <- tibble(date =  dados_brazil$date,
                        infectados_reais = dados_brazil$infected,
                        infectados_modelo = round(fit$I, 0))

base_ajustada %>% 
  mutate( erro = infectados_reais - infectados_modelo) %>% 
  ggplot( aes( x = date, y = erro) ) +
  geom_line() +
  geom_point()


# criar dataset de projecao

horizonte_dias <- 1

dias_projecoes <- 1:(NROW(dados_brazil)+horizonte_dias)

projecoes <- data.frame(ode(y = init, 
                            times = dias_projecoes,
                            func = SIR, 
                            parms = Opt_par))

projs_date <- last(dados_brazil$date) + 1:horizonte_dias

dados_projecoes <- tibble(date =  c(dados_brazil$date, projs_date),
                          infectados_reais = c(dados_brazil$infected, rep(NA, horizonte_dias)),
                          infectados_modelo = round(projecoes$I, 0))

dados_projecoes %>%
  gather( key = variaveis, value = valores, -date) %>%
  ggplot( aes( x = date, y = valores, color = variaveis) ) +
  geom_line() +
  geom_point()


#**********EXERCICIOS REESPOSTAS********
dados_projecoes %>%
  View()


#1
dados_brazil %>%
  filter (infected > 0) %>%
  head(1) %>%
  View()

#2
dados_brazil %>%
  filter (dead > 0) %>%
  head(1) %>%
  View()

#3
#Aproximadamente 211576094

#4
projecao_dias_4 <- 1:(NROW(dados_brazil))

projecoes_4 <- data.frame(ode(y = init, 
                            times = projecao_dias_4,
                            func = SIR, 
                            parms = Opt_par))


projecoes_dados_4 <- tibble(date = dados_brazil$date,
                          infectados_reais = dados_brazil$infected,
                          infectados_modelo = round(fit$I, 0))

projecoes_dados_4 %>%
  gather( key = variaveis, value = valores, -date) %>%
  ggplot( aes( x = date, y = valores, color = variaveis) ) +
  geom_line() +
  geom_point()

#5 #puxa a decima linha do dataframe base_ajustada

base_ajustada[10,] %>% 
  View()

#6
dados_projecoes %>%
  tail(1) %>%
  View()

#7
base_ajustada %<>%
  mutate( “erro” = infectados_reais - infectados_modelo)

base_ajustada_erro %>%
  View()

#8
base_ajustada_erro %>%
  gather( key = variaveis, value = valores, -date) %>%
  ggplot( aes( x = date, y = valores, color = variaveis) ) +
  geom_line() +
  geom_point()

#9 Explique o comportamento do gráfico.O modelo desenvolvido é impreciso, com o aumento dos infectados houve um aumento gradual no erro entre o modelo proposto e os casos reais, onde  a quantidade projetada está abaixo da quantidade indicada pelo modelo (erro positivo). O maior erro observado foi no dia 2020-05-15, com a diferença de 132726 casos.
#Ao longo do tempo, é observado que o modelo consegue se aproximar com os números de infectados reais, onde observa-se o ponto de inflexão dos gráficos de modelo e real.
#Entretanto, o ajuste realizado começa a se afastar novamente do modelo real, apresentando maior numero de casos de modelo do que reais, levando a um erro negativo no final de Maio.

#10
#1.0 projeÁ„o para mais 30 dias
mais_dias_10 <- 30

projecao_dias_10 <- 1:(NROW(dados_brazil)+mais_dias_10)

projecoes_10 <- data.frame(ode(y = init, 
                               times = projecao_dias_10,
                               func = SIR, 
                               parms = Opt_par))

data_projecao_10 <- last(dados_brazil$date) + 1:mais_dias_10

projecoes_dados_10 <- tibble(date =  c(dados_brazil$date, data_projecao_10),
                             infectados_reais = c(dados_brazil$infected, rep(NA, mais_dias_10)),
                             infectados_modelo = round(projecoes_10$I, 0))

projecoes_dados_10 %>%
  gather( key = variaveis, value = valores, -date) %>%
  ggplot( aes( x = date, y = valores, color = variaveis) ) +
  geom_line() +
  geom_point()


#2.0 Selecionar data para o pico
dados_projecoes[which.max(dados_projecoes$infectados_modelo),]%>% 
  view()