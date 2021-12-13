#####################################Explorando Dados##########################################################

#carregando bilbiotecas
library(ggplot2)
library(tidyverse)
library(readr)

#lendo os dados
dados <- read.csv("~/DataScience-Alura/19. Análise de dados com R/Data Visualization I/Garficos com uma variavel/googleplaystore.csv")

#visualizando
View(dados)

#vendo as 5primeiras
head(dados,5)

#verificando as varaveis chr
str(dados)

#verificando dimensao dos dados
dim(dados)

#Lendo novamente a base, porem passando argumento que as strings n podem ser factors
dados <- read.csv("~/DataScience-Alura/19. Análise de dados com R/Data Visualization I/Garficos com uma variavel/googleplaystore.csv",
                  stringsAsFactors = FALSE)

#executando denovo str
str(dados)

############################Transformando os dados##########################################################

#Plotando gráfico de frequência
hist(dados$Rating)

#Criando tabela de frequencia
table(dados$Rating)

#Primeiro grafico n conseguimos ter bons resultados. Nesse caso add xlim de 1 a 5
hist(dados$Rating, xlim = c(1,5))

#Gerando Grafico
#definindo eixo x de 1 a 5 (bins)
#removendo valores nulos
#utilizando breaks vetor de 1 a 5
ggplot(data = dados) + geom_histogram(mapping = aes(x = Rating), na.rm=TRUE, 
                                      breaks = seq(1,5)) + xlim(c(1,5))


#Istanciando o grafico
ratting.histogram <- ggplot(data = dados) + geom_histogram(mapping = aes(x = Rating), na.rm=TRUE, 
                                                           breaks = seq(1,5)) + xlim(c(1,5))

ratting.histogram


####Grafico de barras#####

#stat ira fazer o count das variaveis
ggplot(dados) + geom_bar(mapping = aes(x = Category), stat = "count")

#Melhorando grafico

#trocando os eixo de posição  
ggplot(dados) + geom_bar(mapping = aes(x = Category), stat = "count") + coord_flip()


############################ Transformando dados ##########################################################

#criando um dataframe somente com a variavel category e instaciando num elemento
Category.Freq <- data.frame(table(dados$Category))

#Plotando com novo df
ggplot(Category.Freq) + geom_bar(mapping = aes(x=Var1, y=Freq), stat= "identity")+
  coord_flip()

#Ordenando na visualização
ggplot(Category.Freq) + geom_bar(mapping = aes(x=reorder(Var1,Freq), y=Freq), stat= "identity")+
  coord_flip()
 
#Ordenando os dados de forma descrescente e salvando no top10
category.top10 <- Category.Freq[(order(-Category.Freq$Freq)), ]

#realizando slice
category.top10 <- category.top10[1:10, ]
category.top10

#reset index
row.names(category.top10) <- NULL
category.top10


#Plotando
freq.category <- ggplot(category.top10) + geom_bar(mapping = aes(x=reorder(Var1,Freq), y=Freq), stat= "identity")+
  coord_flip()


########### Manipulação dos dados básica ##############

#criando subsets dos dados originais
#essa nova base vai conter os valores da categoia diferntes de 1.9
dados_2 <- dados %>%  filter(Category != "1.9")
dados_2

#vendo o tamanho dos dados
dim(dados_2)

#Verificando dados faltantes

min(dados_2$Rating)
max(dados_2$Rating)

#verificando a quantidade de dados na
dados_2 %>% filter(is.na(Rating)) %>% count()

#OU
summary(dados_2$Rating)

#Corringindo os valores NA's com média

#verificando os valores na da variavel rating 
#agrupando por categoria 
#realizando o count desses valores NA's
dados_2 %>% filter(is.na(Rating)) %>% group_by(Category) %>% count()

dados_2 %>% filter(is.na(Rating)) %>% group_by(Category) %>% 
  count() %>%
  summarise(median = mean(Rating))

#Fazendo novo filter de valores que nao seja NA 
#instanciando
mean.Category <- dados_2 %>% filter(!is.na(Rating)) %>% group_by(Category) %>% 
  summarise(mean = mean(Rating))

#Fazendo substituição dos valores NA's pela média feita

#fazendo um loop passsando em cada linha 
#verificando se na coluna rating o valores é na ou nao
#se for, atribuir a media de cada categoria para a propria categoria
#se nao for(else), ira atribuir o valor original das categorias na nova variavel q possui as categ.
#criando subconjuntos com novas colunas para n alterar os originais
for (i in  1:nrow(dados_2)){
  if(is.na(dados_2[i, "Rating"])){
    dados_2[i, "newRating"] <-  mean.Category[mean.Category$Category == dados_2[i, "Category"], "mean"]
  }else{
    dados_2[i, "newRating"] <- dados_2[i, "Rating"]  
  }
}

View(dados_2)

#Verificando se temos valores NA's
summary(dados_2$newRating)

####### Criando classes #########

#newRating < 2 = ruim
#newRating > 4 = bom
#newRating 2.1 e 3.9 = regular

dados_2 <-dados_2 %>%
            mutate(rating_class = if_else(newRating < 2, "ruim",
                                          if_else(newRating > 4, "bom","regular")))
View(dados_2)


#plotando
rating_class <- ggplot(dados_2) + geom_bar(aes(rating_class), stat = "count")


#Analisando variavel Type
type.Freq <- data.frame(table(dados_2$Type))

#verificando a frequencia de pagos e gratis
type.Freq      

#plotando gráfico de pizza
#usando coord polar para alterar o formato da barra de acordo com o raio
type.plot <- ggplot(type.Freq) + geom_bar(aes(x = "",y= Freq,  fill= Var1), stat= "identity", width = 1)+
  coord_polar(theta=  "y", start=0)

type.plot

#vendo o tipo dos dados
str(dados_2)

#Analisando a variavel Size
freq.size <- data.frame(table(dados_2$Size))
freq.size

#Converttendo os valores para mesma escala
teste <- dados_2$Size[1]
teste

#1° Verificadno a ocorrencia de K ou M nos registros
grepl(pattern = "M", x=teste, ignore.case = T)
grepl(pattern = "K", x=teste, ignore.case = T)

#2°Converter de megabits para kilobits.Eliminar a letra M ou K
gsub(pattern = "M", replacement = "--", x=teste)
#Se n encontrar mantem formato original
gsub(pattern = "K", replacement = "--", x=teste) 

#conversão de Mb para Kb
1 *1024


##Aplicando as duas fuções acima com loop sapply e fazendo as transformações
dados_2$kb <- sapply(X= dados_2$Size, FUN = function(x){
  if(grepl("M", x,  ignore.case = T)){ #verificando se tem M, se tiver altera p nada e oq sobrar * 1024
    x <- as.numeric( gsub(pattern = "M", replacement = "", x = x)) *1024 ##alterando
  }else if(grepl("k|\\+", x, ignore.case = T)){
    x <- gsub("k|\\+", replacement = "", x = x) #alterando
  }else{
    x <- "ND"
  }
})

#plotando a nova coluna criada
hist(as.numeric(dados_2$kb))

#tirando os valores em notação cient.
options(scipen = 999)

#Criando novo conjunto de dados para fazer novas transformações
#filtrando todas as variaveis diferente de nd, e convertendo a coluna kb para numeric
size.app <- dados_2 %>% filter(kb != "ND") %>% mutate(kb = as.numeric(kb))
size.app  

#Plotando
size.app.plot <- ggplot(size.app)+geom_histogram(aes(kb))
size.app.plot


#Maioria tem de 0 a 15mil kb 


################### Manipulando Datas e Horas ######################
library(lubridate)

ymd("20211209")
dmy("09122021")

ymd_hms("2021-12-09 12:00:00")  
ymd_hm("2021-12-09 12:00")  
ymd_h("2021-12-09 09")

#fazendo com lubridate
lubridate::hours(12)

#instanciando data_hora com string
data_hora <- '2021-12-10 00:07:00'

#convertendo para datetime
data_hora <- ymd_hms(data_hora)
data_hora


#Extrair apenas o mês
month(data_hora)

#apenas o dia
mday(data_hora)

#ano
year(data_hora)

#Extrair apenas horas
hour(data_hora)

#eXtrair minutos
minute(data_hora)

#segundos
second(data_hora)

#Nome do dia
wday(data_hora, label= T)

#nome do mes
month(data_hora, label = T)

###### Importando novos dados

notas <- read.csv("~/DataScience-Alura/19. Análise de dados com R/Data Visualization I/Garficos com uma variavel/user_reviews.csv")
str(notas)


#Convertendo a variavel data para datetime
#Grafico de linhas

#criando nova coluna
notas$data_2 <- ymd_hms(notas$data)
str(notas)

#plotando de acorod com campo
ggplot(notas) + geom_line(aes(x = data_2, y = Sentiment_Polarity))

#convertendo novamente mas para ano e mes 
notas$data_2 <- parse_date_time(format(notas$data_2, "%Y-%m"), "ym")
notas$data_2[1]

#plotando
ggplot(notas) +geom_line(aes(x = data_2, y = Sentiment_Polarity))


#Calcular a media de cada nota, para cada mes da sentiment_polatiry
media_nota <- notas %>% group_by(data_2) %>% summarise(media = mean(Sentiment_Polarity))

#plotando com nova variavel media_nota
notas_plot <- ggplot(media_nota) +geom_line(aes(x = data_2, y = media))


#### Melhorando graficos

# Histograma
ratting.histogram <- ratting.histogram + ggtitle("Histograma Rating")

#ajustando titulo
ratting.histogram <- ratting.histogram + theme(plot.title = element_text(hjust = 0.5))

#cor do layout  
ratting.histogram <- ratting.histogram + theme_bw()
ratting.histogram


# Barras
freq.category <- freq.category + ggtitle("Quantidade de Apps por Categoria")

#Titulos dos eixos
freq.category <- freq.category + xlab("Categoria") + ylab("Quantidade")

#Alterando as cores das barras por categoria(var1)
freq.category <- freq.category+ geom_bar(aes(Var1, Freq, fill= Var1), stat = "identity")
freq.category

#Outra forma seria escalar
freq.category <- freq.category + geom_bar(aes(Var1, Freq, fill= Freq), stat = "identity")
freq.category

#inserindo uma cor só
freq.category <- freq.category+ geom_bar(aes(Var1, Freq), fill= "darkcyan", stat = "identity")
freq.category

#tema
freq.category <- freq.category + theme_bw()
freq.category



# Pizza
type.plot

#Criando uma variavel para remover alguns elementos do grafico de pizza
blank_theme <- theme_minimal()+
               theme(
                 axis.title.x = element_blank(),
                 axis.title.y = element_blank(),
                 panel.border = element_blank(),
                 panel.grid = element_blank(),
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank()
               )

#aplicando mudançdas
type.plot <- type.plot + blank_theme

#visualizando
type.plot

#importando nova biblitoeca
library(scales)

#inserindo titulos ou rotulos das porcentagens
type.plot <- type.plot + geom_text(aes(x = "", y= Freq/2, label = percent(Freq/sum(Freq))), size = 5)

#legenda
type.plot <- type.plot + scale_fill_discrete(name = "Tipo App")
type.plot

#titulo
type.plot <- type.plot + ggtitle("Tipo de Aplicações") + theme(plot.title = element_text(hjust = 0.5))
type.plot


# Histograma

#Titulo
size.app.plot <- size.app.plot + ggtitle("Histograma do Tamanho de Aplicativos")
size.app.plot

#cores com rainbow(n ficou bom)
#size.app.plot + geom_histogram(aes(kb), fill=rainbow(30))

#Melhor jeito
size.app.plot <- size.app.plot + geom_histogram(aes(kb, fill= ..x..))+
                    scale_fill_gradient(low="blue", high ="yellow") + guides(fill = FALSE)

#Rotulos
size.app.plot <- size.app.plot  + xlab("Tamanho do App (em Kb)")+ ylab("Qantidade de Apps")
size.app.plot

size.app.plot <- size.app.plot + theme_bw()
size.app.plot


# Colunas
rating_class <- rating_class + ggtitle("Categoria de Notas por App") + xlab("Cateogria")+
  ylab("Quantidade")

#Cores
rating_class <- rating_class + geom_bar(aes(rating_class), fill = c("green4", "yellow2", "red"))

rating_class <- rating_class + theme_bw()
rating_class


#Linhas
notas_plot <- notas_plot + ggtitle("Média das Avaliações dos App") + xlab("Data") + ylab("média")

#ajustando titulo
notas_plot <-  notas_plot + theme(plot.title = element_text(hjust = 0.5))


#tema
notas_plot <- notas_plot + theme_bw()
notas_plot

#Graficos em janela
library(gridExtra)

grid.arrange(ratting.histogram, freq.category, type.plot,
             size.app.plot, rating_class, notas_plot, nrow=2, ncol=3)
