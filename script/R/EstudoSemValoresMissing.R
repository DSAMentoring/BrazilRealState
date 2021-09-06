#Projeto para analise de dados do banco RealState

setwd("/Users/tmgoncalves/Documents/Mentoria/BrazilRealState/script/R/")
getwd()

# Versão dos pacotes/R utilizados neste script
sessionInfo()

#Cenário 1
# A análise desse script é embasado no dataset EstudoSemValoresMissing.csv. 
# O dataset esta com todos os valores missing removidos. 
# Com isso, nenhuma tratativa aos valores missing foram realizadas.

library(data.table)
library(readr)
library(dplyr)
library(datasets)
library(stringr)
library(ggplot2)
library(gridExtra)
library(tidyr)
options(scipen = 999)

# Função utilizada para carregar dataset no Sistema MacOS
df <- fread("../../data/EstudoSemValoresMissing.csv")

########## informações do dataset ##########

# nome das colunas
names(df)

# resumo do dataset
head(df)
tail(df)

# dimensão do dataset
dim(df)

# tipo do dataset
class(df)

# resumo estatístico do dataset (variáveis contínuas)
summary(df)

# tipo das variáveis
str(df)

# view
View(df)


# Transformando o formato da coluna "create_on" em datatime
df$created_on <- as.Date(df$created_on)

####### Análise Gráfica #######

# Cinco maiores estados presentes no dataframe
value_state <- df %>%
  count(Estado, sort = TRUE) %>%
  head(5) %>%
  as.data.frame(value_state)

ggplot(data=value_state, aes(x=reorder(Estado, -n), y=n)) +
  geom_bar(stat="identity", fill="steelblue") +
  ggtitle("Quantidade de Registro por Estado") +
  xlab("") + ylab("Número de Registro") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## O gráfico representa o maior número de estados presentes no dataset. Conforme observado, 
## o estado de São Paulo representa 80% dos dados registrados, nesse dataset. Com isso, 
## o modelo tende a aprender mais com os registros de São Paulo do que com as outras regiões.
## Somente com o gráfico acima pode-se concluir que, dentro desse cenário, compensa elaborar um 
## modelo de regressão linear apenas para o estado de São Paulo
  

# Top 5 Cidades mais caras de São Paulo (na média), segundo o dataset
value_city <- df %>%
  filter(Estado == "São Paulo") %>%
  group_by(Cidade)%>%
  summarise_at(vars(price),
               list(media=mean))%>%
  arrange(desc(media))%>%
  head(5)%>%
  as.data.frame()

value_city$media <- value_city$media/1000000

ggplot(data=value_city, aes(x=reorder(Cidade, -media), y=media)) +
  geom_bar(stat="identity", fill="steelblue") +
  ggtitle("Média dos Valores por Cidade de SP (em Milhões)") +
  xlab("") + ylab("Valores em Milhões") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Top 5 Cidades mais caras de São Paulo (na mediana), segundo o dataset
value_city <- df %>%
  filter(Estado == "São Paulo") %>%
  group_by(Cidade)%>%
  summarise_at(vars(price),
               list(media=median))%>%
  arrange(desc(media))%>%
  head(5)%>%
  as.data.frame()

value_city$media <- value_city$media/1000000

ggplot(data=value_city, aes(x=reorder(Cidade, -media), y=media)) +
  geom_bar(stat="identity", fill="steelblue") +
  ggtitle("Média dos Valores por Cidade de SP (em Milhões)") +
  xlab("") + ylab("Valores em Milhões") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Os valores das médias e medianas de preços do imóveis, das cidades do gráficos acima, 
## são considerando todos os tipos de imóveis. Ou seja, apartment, house, PH e store.

#Preço médio dos imóveis por estado
value_state <- df %>%
  group_by(Estado)%>%
  summarise_at(vars(price),
               list(media=mean))%>%
  arrange(desc(media))%>%
  head(5)%>%
  as.data.frame()

value_state$media <- value_state$media/1000000

ggplot(data=value_state, aes(x=reorder(Estado, -media), y=media)) +
  geom_bar(stat="identity", fill="steelblue") +
  ggtitle("Média dos Valores por Estado (em Milhões)") +
  xlab("") + ylab("Valores em Milhões") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Apesar do estado de São Paulo representar 80% dos registros, presente nesse dataframe, 
## o estado do Rio de Janeiro apresenta a maior média dos imóveis, por estado. 
## São Paulo corresponde como quarto estado com maior média entre os estados brasileiros.
## O gráfico acima não difere os tipos de imóveis, como apartamento ou casa. 
## Essa informação é extraída nos gráficos seguintes.

# Definindo a média dos preços por tipo de imóvel
for (i in unique(df$property_type)){
  print(paste("Médias da variável: ", i))
  df %>%
    filter(property_type == i) %>%
    group_by(Estado)%>%
    summarise_at(vars(price),
                 list(media=mean))%>%
    arrange(desc(media))%>%
    head(5)%>%print(i)
    }

# Colocando as informações em gráficos (por Estados)
for (i in unique(df$property_type)){
  values <- df %>%
    filter(property_type == i) %>%
    group_by(Estado)%>%
    summarise_at(vars(price),
                 list(media=mean))%>%
    arrange(desc(media))%>%
    head(5)%>%
    as.data.frame(values)
  
  # reduzindo escala dos valores para plotar
  values$media <- values$media/1000000
  
  # plotando gráfico
  print(ggplot(data=values, aes(x=reorder(Estado, -media), y=media)) +
    geom_bar(stat="identity", fill="steelblue") +
    ggtitle(paste("Média dos Valores por Estado (em Milhões) -", i)) +
    xlab("") + ylab("Valores em Milhões") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)))
  }

## Os gráficos abaixo mostram que o estado do Rio de Janeiro, na média, possui as casas e apartamentos mais caros. 
## No entanto, nos tipos de imóveis store e PH, o estado do Rio de Janeiro não está presente na média dos estados mais caros.
## Abaixo verifica-se as a média dos preços de cada tipo de imóvel por cidade:

# Colocando as informações em gráficos (por Estados)
for (i in unique(df$property_type)){
  values <- df %>%
    filter(property_type == i) %>%
    group_by(Cidade)%>%
    summarise_at(vars(price),
                 list(media=mean))%>%
    arrange(desc(media))%>%
    head(5)%>%
    as.data.frame(values)
  
  # reduzindo escala dos valores para plotar
  values$media <- values$media/1000000
  
  # plotando gráfico
  print(ggplot(data=values, aes(x=reorder(Cidade, -media), y=media)) +
          geom_bar(stat="identity", fill="steelblue") +
          ggtitle(paste("Média dos Valores por Cidade (em Milhões) -", i)) +
          xlab("") + ylab("Valores em Milhões") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)))
}

## Considerando a média dos preços dos imóveis por estado, encontra-se abaixo o valor médio 
## de cada tipo de imóvel por cidade (os estados utilizados são os que possuem maior média de preços):


# Preço dos tipos de imóveis por cidades, nos estados com maior média de preço
for (i in unique(df$property_type)){
  print(paste("Tipo de imóvel",i))
  values <- df %>%
    filter(property_type == i) %>%
    group_by(Estado)%>%
    summarise_at(vars(price),
                 list(media=mean))%>%
    arrange(desc(media))%>%
    head(5)%>%
    as.data.frame(values)
  
  
  for(k in c(values$Estado)){
    print(paste("Estado:", k))
    valor <- df%>%
      filter(Estado==k)%>%
      group_by(Cidade)%>%
      summarise_at(vars(price),
                   list(med=mean))%>%
      arrange(desc(med))%>%
      head(5)%>%
      as.data.frame(valor)
    print(c(valor$Cidade))
    print("############################################")
  }
}

######### TIPO: APARTMENT ##########
valor <- df%>%
  filter(property_type=="apartment")%>%
  group_by(Estado)%>%
  summarise_at(vars(price),
               list(media=mean))%>%
  arrange(desc(media))%>%
  head(5)%>%
  as.data.frame(valor)

for (i in unique(valor$Estado)){
  values <- df %>%
    filter(Estado == i) %>%
    group_by(Cidade)%>%
    summarise_at(vars(price),
                 list(media=mean))%>%
    arrange(desc(media))%>%
    head(5)%>%
    as.data.frame(values)
  
  # reduzindo escala dos valores para plotar
  values$media <- values$media/1000000
  
  # plotando gráfico
  print(ggplot(data=values, aes(x=reorder(Cidade, -media), y=media)) +
          geom_bar(stat="identity", fill="steelblue") +
          ggtitle(paste("Média dos Valores por Cidade (em Milhões) - apartment -", i)) +
          xlab("") + ylab("Valores em Milhões") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)))
}

######### TIPO: HOUSE ##########
valor <- df%>%
  filter(property_type=="house")%>%
  group_by(Estado)%>%
  summarise_at(vars(price),
               list(media=mean))%>%
  arrange(desc(media))%>%
  head(5)%>%
  as.data.frame(valor)

for (i in unique(valor$Estado)){
  values <- df %>%
    filter(Estado == i) %>%
    group_by(Cidade)%>%
    summarise_at(vars(price),
                 list(media=mean))%>%
    arrange(desc(media))%>%
    head(5)%>%
    as.data.frame(values)
  
  # reduzindo escala dos valores para plotar
  values$media <- values$media/1000000
  
  # plotando gráfico
  print(ggplot(data=values, aes(x=reorder(Cidade, -media), y=media)) +
          geom_bar(stat="identity", fill="steelblue") +
          ggtitle(paste("Média dos Valores por Cidade (em Milhões) - house -", i)) +
          xlab("") + ylab("Valores em Milhões") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)))
}

######### TIPO: PH ##########
valor <- df%>%
  filter(property_type=="PH")%>%
  group_by(Estado)%>%
  summarise_at(vars(price),
               list(media=mean))%>%
  arrange(desc(media))%>%
  head(5)%>%
  as.data.frame(valor)

for (i in unique(valor$Estado)){
  values <- df %>%
    filter(Estado == i) %>%
    group_by(Cidade)%>%
    summarise_at(vars(price),
                 list(media=mean))%>%
    arrange(desc(media))%>%
    head(5)%>%
    as.data.frame(values)
  
  # reduzindo escala dos valores para plotar
  values$media <- values$media/1000000
  
  # plotando gráfico
  print(ggplot(data=values, aes(x=reorder(Cidade, -media), y=media)) +
          geom_bar(stat="identity", fill="steelblue") +
          ggtitle(paste("Média dos Valores por Cidade (em Milhões) - PH -", i)) +
          xlab("") + ylab("Valores em Milhões") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)))
}


######### TIPO: STORE ##########
valor <- df%>%
  filter(property_type=="store")%>%
  group_by(Estado)%>%
  summarise_at(vars(price),
               list(media=mean))%>%
  arrange(desc(media))%>%
  head(5)%>%
  as.data.frame(valor)

for (i in unique(valor$Estado)){
  values <- df %>%
    filter(Estado == i) %>%
    group_by(Cidade)%>%
    summarise_at(vars(price),
                 list(media=mean))%>%
    arrange(desc(media))%>%
    head(5)%>%
    as.data.frame(values)
  
  # reduzindo escala dos valores para plotar
  values$media <- values$media/1000000
  
  # plotando gráfico
  print(ggplot(data=values, aes(x=reorder(Cidade, -media), y=media)) +
          geom_bar(stat="identity", fill="steelblue") +
          ggtitle(paste("Média dos Valores por Cidade (em Milhões) - store -", i)) +
          xlab("") + ylab("Valores em Milhões") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)))
}


######### Análise dos imóveis por ano ######### 

# Para comparar a média dos valores anuais de cada estado,
# necessita criar uma coluna do ano
df <- df%>%
  mutate(date=ymd(created_on))%>%
  mutate_at(vars(created_on), funs(year))

# renomear as novas colunas criadas
names(df)[names(df) == 'created_on'] <- 'Ano'
names(df)[names(df) == 'date'] <- 'created_on'

# definindo média anual dos preços por tipo de imóvel
subset(df, property_type == "apartment")
print("Médias em Milhões")
for (i in unique(df$Ano)){
  print(paste("Médias da variável - apartment -", i))
  df %>%
    filter(Ano == i)%>%
    group_by(Estado)%>%
    summarise_at(vars(price),
                 list(media=mean))%>%
    arrange(desc(media))%>%
    mutate(media = media/1000000) %>%print(i)
}

## Com o cálculo da média anual, de apenas um tipo de imóvel (Apartment), percebe-se 
## que não há presença de todas unidades federativas nos anos de 2013 e 2014. 
## Essa deficiência de dados, presente nos anos 2013 e 2014, pode ter sido consequência da 
## alta remoção de registros para criação deste dataset. Os únicos estados que aparecem em todos os anos, 
## do dataset, são São Paulo, Rio de Janeiro e Paraíba.

# Para plotar um gráfico de linhas dos únicos estados que aparecem em todos os anos, do dataset,
# é necessário pegar a média anual de cada estado.

# [ 'apartment', 'house', 'PH', 'store']
# ['São Paulo', 'Rio de Janeiro', 'Paraíba']

# Filter
df_anual <- subset(x = df,
                   Estado == c("Rio de Janeiro", "São Paulo", "Paraíba") & property_type == "apartment",
                   select = c(Estado, Ano, price))
df_anual <- as.data.frame(aggregate(df_anual[, 3], list(df_anual$Estado, df_anual$Ano), mean))
df_anual$x <- df_anual$x / 1000000
df_anual <- rename(df_anual, Media = x)
df_anual <- rename(df_anual, Estado = Group.1)
df_anual <- rename(df_anual, Ano = Group.2)
# plot
ggplot(df_anual, aes(x = Ano, y = Media, group = Estado)) +
  geom_line(aes(color = Estado), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  ggtitle("Média Anual por Estado - Apartment") +
  xlab("Anos") + ylab("Média dos Preços (em Milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# [ 'apartment', 'house', 'PH', 'store']
# ['São Paulo', 'Rio de Janeiro', 'Paraíba']

# Filter
df_anual <- subset(x = df,
                   Estado == c("Rio de Janeiro", "São Paulo", "Paraíba") & property_type == "house",
                   select = c(Estado, Ano, price))
df_anual <- as.data.frame(aggregate(df_anual[, 3], list(df_anual$Estado, df_anual$Ano), mean))
df_anual$x <- df_anual$x / 1000000
df_anual <- rename(df_anual, Media = x)
df_anual <- rename(df_anual, Estado = Group.1)
df_anual <- rename(df_anual, Ano = Group.2)
# plot
ggplot(df_anual, aes(x = Ano, y = Media, group = Estado)) +
  geom_line(aes(color = Estado), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  ggtitle("Média Anual por Estado - house") +
  xlab("Anos") + ylab("Média dos Preços (em Milhões)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Com o cálculo das médias anuais dos preços dos imóveis, por estado, percebe-se a grande 
## diferença entre os valores por estado. Um exemplo que exemplifica essa suposição encontra-se 
## no gráfico da Média Anual por Estado, para casas. No Rio de Janeiro, no ano de 2013, 
## a média dos valores das casas ultrapassa , frente a média de do Estado de São Paulo e 
## uma média de para o estado da Paraíba. Devido ao alto intervalo entre as médias, abre 
## a hipótese para justificar o desbalanceamento do valores do Histograma.

# Análise do histograma
df_hist <- subset(x = df,
                  select = c(price, price_aprox_local_currency, price_aprox_usd,
                             surface_covered_in_m2, price_usd_per_m2, price_per_m2, rooms))

ggplot(df_hist, aes(x = c(price, price_aprox_local_currency, price_aprox_usd,
                          surface_covered_in_m2, price_usd_per_m2, price_per_m2, rooms))) +
  geom_histogram(fill = "white", colour = "blue")



ggplot(birthwt, aes(x = bwt)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(race ~ ., scales = "free")









myplots <- list()  # new empty list
for(i in 1:ncol(mtcars)){
  col <- names(mtcars)[i]
  ggp <- ggplot(mtcars, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "cornflowerblue", color = "black") +
    geom_vline(xintercept = mean(mtcars[[col]]), col = "red", lwd=1.5) 
  myplots[[i]] <- ggp  # add each plot into plot list
}

multiplot(plotlist = myplots, cols = 3)
