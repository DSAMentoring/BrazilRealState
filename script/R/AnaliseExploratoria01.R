#Projeto para analise de dados do banco RealState



setwd("C:/Users/Marcia/Documents/cursos/mentoriaDSA")
getwd()



library(readr)
library(dplyr)
library(datasets)
library(stringr)
library(ggplot2)
library(tidyr)
options(scipen = 999)


realstate <- read.csv("properati-BR-2016-11-01-properties-sell.csv", encoding = "utf-8")
names(realstate)
head(realstate)
tail(realstate)
class(realstate)
summary(realstate)



# excluindo colunas nao usadas
# *******************************************************************
realstate <- subset(realstate, select = -c( geonames_id,lat.lon,lat, lon,price_usd_per_m2, floor, properati_url, expenses, description, title, image_thumbnail))
# *******************************************************************


# calculando o campo preco por m2 , mas nao consigo acessar essa coluna
# *******************************************************************
realstate %>% mutate(price_calc = price / surface_total_in_m2)
head(realstate)

# *******************************************************************


# tentei separar o campo place with parent names mas esta dando erro
## entao estou separando com str_split, ele cria um vetor
##amostra %>% separate(amostra$place_with_parent_names, c("pais", "estado", "cidade","bairro"))
##na.action=na.omit

# tratando o campo place_with_parent_names

# ************************************************************


str_split(realstate$place_with_parent_names, fixed('|'))


ajustar_nomes=function(x){
  x%>%
    # stringr::str_trim() %>%                        #Remove espaços em branco sobrando
    # stringr::str_to_lower() %>%                    #Converte todas as strings para minusculo
    # rm_accent() %>%                                #Remove os acentos com a funcao criada acima
    stringr::str_replace_all("Ã£", "a") %>%         #Substitui os caracteres especiais por "_"
    stringr::str_replace_all("Ã§", "c") %>%        #Substitui os caracteres especiais por "_"   
    stringr::str_replace("Ã", "i")  %>%               #Substitui o caracter especiais por "_"
    stringr::str_replace("Ã¡", "a") %>%
    stringr::str_replace("Ã©", "e")   %>%
    stringr::str_replace("Ã³", "o")   %>%
    stringr::str_replace("³", "o")   %>%
    stringr::str_replace("Ãº", "u")   %>%
    stringr::str_replace("Ã­m", "im") 
  
  
}
nomes=ajustar_nomes(realstate$place_with_parent_names)
nomes
# ************************************************************


# tratando outliers

# ************************************************************
x = realstate$price
q1 = quantile(realstate$price, 0.25, na.rm = TRUE)  # primeiro quartil
q3 = quantile(realstate$price, 0.75, na.rm = TRUE)  # terceiro quartil
distancia = q3 - q1
lim_inf = q1 - 1.5*distancia # limite inferior
lim_sup = q3 + 1.5*distancia # limite superior

x > lim_sup
x < lim_inf


# retirando outliers
out = (x > lim_sup) | (x < lim_inf)
realstate$price[out] = NA # retirei do conj de dados


boxplot(realstate$price ~ realstate$property_type,xlab ="Tipo" , ylab = "Preço" ) # teste sem outliers


# ************************************************************














