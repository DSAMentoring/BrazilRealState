#Projeto para analise de dados do banco RealState

#setwd("/Users/tmgoncalves/Documents/Mentoria/estudo_data/")
setwd("/FCD/MENTORIA_DSA_2021/GIT_MAIN_BETH/BrazilRealState-main/script/R")
getwd()


library(data.table)
library(readr)
library(dplyr)
library(datasets)
library(stringr)
library(ggplot2)
library(tidyr)
options(scipen = 999)

# Função utilizada para carregar dataset no Sistema MacOS
#realstate <- fread("properati-BR-2016-11-01-properties-sell.csv")
realstate <- fread("df_nan_597.csv", encoding = "UTF-8")


# características do dataset
names(realstate)
head(realstate)
tail(realstate)
class(realstate)
summary(realstate)
str(realstate)

# view
View(realstate)

# excluindo colunas nao usadas - Windows
# *******************************************************************
#realstate <- subset(realstate, select = -c( geonames_id,
#                                            lat-lon,
#                                            lat,
#                                            lon,
#                                            price_usd_per_m2,
#                                            floor,
#                                            properati_url,
#                                            expenses, 
#                                            description,
#                                            title,
#                                            image_thumbnail))
#
#realstate <- subset(realstate, select = -drops)
# *******************************************************************

# Remoção de todas as colunas insignificantes -> MacOS
#realstate$geonames_id <- NULL
#realstate$`lat-lon` <- NULL
#realstate$lat <- NULL
#realstate$lon <- NULL
#realstate$price_usd_per_m2 <- NULL
#realstate$floor <- NULL
#realstate$properati_url <- NULL
#realstate$expenses <- NULL
#realstate$description <- NULL
#realstate$title <- NULL
#realstate$image_thumbnail <- NULL
#realstate$operation <- NULL

#?separate

# Criando as colunas País, Estado, Cidade e Bairro através da separação
# da variável "place_with_parent_names"
#realstate <- realstate %>%
#  separate(place_with_parent_names, into = c("1", "Pais", "Estado", "Cidade", "Bairro"), "\\|")

# Remoção da Coluna 1
#realstate$`1` <- NULL

# Somatória dos valores missing por colunas
#sapply(realstate, function(x) sum(is.na(x)))

# Porcentagem dos valores missing por colunas
#sapply(realstate, function(x) mean(is.na(x))*100)


# Cálculo price/m2
# *******************************************************************
#realstate$price_calc <- realstate$price / realstate$surface_total_in_m2
#head(realstate)

# O cálculo do preço por metro quadrado realizado acima não faz muito sentido
# uma vez que 75% dos valores da variável "surface_total_in_m2" são NaN.
# O que pode ser realizado é utilizar a variável "surface_covered_in_m2" ao invés
# da variável "surface_total_in_m2".

# Cálculo do preço por metro quadrado utilizando o atributo "surface_covered_in_m2".
realstate$price_calc2 <- realstate$price / realstate$surface_covered_in_m2
head(realstate)
# *******************************************************************


# Ajuste no nome realizado em Sistemas que não reconhecem o formato utf-8;
  
# ************************************************************


# tratando outliers

# ************************************************************
x = realstate$price
q1 = quantile(realstate$price, 0.25, na.rm = TRUE)  # primeiro quartil
q3 = quantile(realstate$price, 0.75, na.rm = TRUE)  # terceiro quartil
distancia = q3 - q1

lim_inf = q1 - 1.5*distancia # limite inferior
lim_inf
#      25%
#-568737.9

lim_sup = q3 + 1.5*distancia # limite superior
lim_sup
#    75% 
#1801003 

typeof(realstate$price)
#double
typeof(lim_sup)
#double

x > lim_sup
x < lim_inf


# retirando outliers
out = (x > lim_sup) | (x < lim_inf)
realstate$price[out] = NA # retirei do conj de dados


boxplot(realstate$price ~ realstate$property_type,xlab ="Tipo" , ylab = "Preço" ) # teste sem outliers

fwrite(realstate,"df_nan_597price_calc.csv")

df<- fread("df_nan_597price_calc.csv", encoding = "UTF-8")
View(df)

# ************************************************************