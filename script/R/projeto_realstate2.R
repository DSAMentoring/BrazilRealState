#Projeto para analise de dados do banco RealState

setwd("/Users/tmgoncalves/Documents/Mentoria/estudo_data/")
getwd()


library(data.table)
library(readr)
library(dplyr)
library(datasets)
library(stringr)
library(ggplot2)
library(tidyr)
options(scipen = 999)

# load data using MacOS
realstate <- fread("properati-BR-2016-11-01-properties-sell.csv")

# data characteristics
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
realstate <- subset(realstate, select = -c( geonames_id,
                                            lat-lon,
                                            lat,
                                            lon,
                                            price_usd_per_m2,
                                            floor,
                                            properati_url,
                                            expenses, 
                                            description,
                                            title,
                                            image_thumbnail))

realstate <- subset(realstate, select = -drops)
# *******************************************************************

# Remove all useless columns -> MacOS
realstate$geonames_id <- NULL
realstate$`lat-lon` <- NULL
realstate$lat <- NULL
realstate$lon <- NULL
realstate$price_usd_per_m2 <- NULL
realstate$floor <- NULL
realstate$properati_url <- NULL
realstate$expenses <- NULL
realstate$description <- NULL
realstate$title <- NULL
realstate$image_thumbnail <- NULL
realstate$operation <- NULL

?separate

#Separate the column place_with_parent_names;
realstate <- realstate %>%
  separate(place_with_parent_names, into = c("1", "Pais", "Estado", "Cidade", "Bairro"), "\\|")

# Remove column 1
realstate$`1` <- NULL

# missing values by columns - total
sapply(realstate, function(x) sum(is.na(x)))

# missing values by columns - percentage
sapply(realstate, function(x) mean(is.na(x))*100)


# calculate price/m2
# *******************************************************************
realstate$price_calc <- realstate$price / realstate$surface_total_in_m2
head(realstate)

# calculate the same price using the attribute surface_covered_in_m2
realstate$price_calc2 <- realstate$price / realstate$surface_covered_in_m2

# *******************************************************************


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
    stringr::str_replace("Ã�m", "im") 
  
  
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