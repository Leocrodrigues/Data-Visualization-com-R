############################################## Explorando Dados ###############################################

#carregando bilbiotecas
library(ggplot2)
library(tidyverse)
library(readr)
library(data.table)
library(gridExtra)
library(grid)

#informando o diretorio das bases
getwd()
setwd("C:/Users/leona/OneDrive/Documentos/DataScience-Alura/19. Análise de dados com R/Data Visualization I e II/Graficos com uma variavel/bases")

#Particionando as bases de acordo com ano
enem_2010 <- fread("enem_2010.csv", encoding = "UTF-8")
enem_2011 <- fread("enem_2011.csv", encoding = "UTF-8")
enem_2012 <- fread("enem_2012.csv", encoding = "UTF-8")
enem_2013 <- fread("enem_2013.csv", encoding = "UTF-8")
enem_2014 <- fread("enem_2014.csv", encoding = "UTF-8")
enem_2015 <- fread("enem_2015.csv", encoding = "UTF-8")
enem_2016 <- fread("enem_2016.csv", encoding = "UTF-8")
enem_2017 <- fread("enem_2017.csv", encoding = "UTF-8")
#enem_2018 <- fread("enem_2018.csv", encoding = "UTF-8")

#realizando o merge(junção) das bases em um só df
#informando igual  a true para deixar as colunas que n tem preenchidas com NA
merge_enem <- rbind(enem_2010, enem_2011, enem_2012,enem_2013,enem_2014,enem_2015,enem_2016,enem_2017, fill=T)

dim(merge_enem)

#eliminando as bases, pois agora esta todas em um só df
rm(enem_2010, enem_2011, enem_2012,enem_2013,enem_2014,enem_2015,enem_2016,enem_2017)

#crinado vetor somente com as colunas que queremos para analiasar
colunas <- c("NUMERO_INSCRICAO",             
             "ANO",                       
             "CO_MUNICIPIO_RESIDENCIA",     
             "MUNICIPIO_RESIDENCIA",      
             "UF_RESIDENCIA",               
             "UF_ESCOLA"  ,               
             "IDADE"  ,                     
             "SEXO"   ,                     
             "SITUACAO_CONCLUSAO",          
             "BRAILLE",                     
             "MUNICIPIO_PROVA" ,            
             "UF_PROVA"   ,                 
             "PRESENCA_CIENCIAS_NATUREZA" , 
             "PRESENCA_CIENCIAS_HUMANAS"  , 
             "PRESENCA_LINGUAGENS_CODIGOS" ,
             "PRESENCA_MATEMATICA"    ,     
             "NOTA_CIENCIAS_NATUREZA" ,     
             "NOTA_CIENCIAS_HUMANAS"  ,     
             "NOTA_LINGUAGENS_CODIGOS" ,    
             "NOTA_MATEMATICA"    ,         
             "TIPO_LINGUA"   ,              
             "STATUS_REDACAO"  ,            
             "NOTA_REDACAO" )              
    
#Criando um novo objeto que vai ter somente df com 23 variaveis  
enem <- merge_enem %>% select_(.dots = colunas)

#removendo o merge enem
rm(merge_enem)

#Verificando os tipos e dados das colunas
str(enem)

#Alguns dados estao como char mas são numericos


########################################## Tranformando dados ###################################################

####### Variavel sexo #########
table(enem$SEXO)

#substituindo valores
enem$SEXO <- gsub("1", "FEMININO", enem$SEXO)

#Pegando os valores q se inicia e termina com F e substituindo por FEMININO
enem$SEXO <- gsub("^F$", "FEMININO", enem$SEXO)
  
#Agora os dados masculinos
enem$SEXO <- gsub("0", "MASCULINO", enem$SEXO)

enem$SEXO <- gsub("^M$", "MASCULINO", enem$SEXO)

#Pornto agora só dois valores
table(enem$SEXO)

####### Variavel Tipo de lingua #########
table(enem$TIPO_LINGUA)

#substituindo os valores
enem$TIPO_LINGUA <- gsub("0", "INGLêS", enem$TIPO_LINGUA)
enem$TIPO_LINGUA <- gsub("1", "ESPANHOL", enem$TIPO_LINGUA)

#Verificando se os dados foram normalizados
table(enem$TIPO_LINGUA)

####### Variavel UF de prova #########

#verificando se tem os 27 estados
length(table(enem$UF_PROVA))

#Tem 28, mas n podemos apagar os dados variavel em branco, pois podem interferir em outras varives

####### Variavel situação conclusao #########
table(enem$SITUACAO_CONCLUSAO)

#realizando a substitução
enem$SITUACAO_CONCLUSAO <- gsub("1", "CONCLUÍDO", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("2", "CONCLUIRÁ NO ANO", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("3", "CONCLUIRA POS(ANO)", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("4", "NÃO CONC. NÃO CURSANOD", enem$SITUACAO_CONCLUSAO)

#Verificando
table(enem$SITUACAO_CONCLUSAO)


####### Variaveis NOTA #########
#Verificando dados descritivos
summary(enem$NOTA_CIENCIAS_HUMANAS)

#As variaveis estao como char, e devemos deixar em numeric
enem$NOTA_CIENCIAS_HUMANAS <- as.numeric(enem$NOTA_CIENCIAS_HUMANAS)
enem$NOTA_CIENCIAS_NATUREZA <- as.numeric(enem$NOTA_CIENCIAS_NATUREZA)
enem$NOTA_LINGUAGENS_CODIGOS <- as.numeric(enem$NOTA_LINGUAGENS_CODIGOS)
enem$NOTA_MATEMATICA <- as.numeric(enem$NOTA_MATEMATICA)
enem$NOTA_REDACAO <- as.numeric(enem$NOTA_REDACAO)

#Verficando que agora as colunas notas estao como numeric
str(enem)

############################################### Visualização #####################################################

#Plotando Gráfico sexo e tipo de lingua
ggplot(enem) + geom_bar(aes(x = TIPO_LINGUA), stat = 'count')

#limpando
#criando um objeto somente com tipo de lingua diferente de .
#selecionando smente sexo e tipo ingua
tp_lingua_sexo <- enem %>% 
                    filter(TIPO_LINGUA != '.') %>%
                    select_(.dots = c('SEXO', 'TIPO_LINGUA'))

 
#Plotandoa esse novo objeto
plot_idioma_sexo <- ggplot(tp_lingua_sexo) + geom_bar(aes(x = SEXO, fill = TIPO_LINGUA), 
                                  stat = 'count', position = position_dodge())
  
#INSIGHTS
# ESPANHOL E A LINGUA MAIS ESCOLHIDA
# PESSOAS do sexo FEMININO OPTA mais POR  ESPANHOL
# NA LINGUA INGLESA O FEMINNO TAMBÉM TEM MAIOR NÚMERO COMPARADO A MASCULINO
# NO SEXO MASCULINO MAIOR PARTE OPTAM POR ESPANHOL 


#Melhorando gráfico feito anteriormente
p <- plot_idioma_sexo +
  ggtitle("Idioma por sexo")
  xlab("Sexo") + ylab("Quantidade")

#Mudando fundo
#colocando titulo no meio
p <- p + theme_linedraw()+
  theme(plot.title= element_text(hjust = 0.5))

#substituindo o original pelo que foi alterado
plot_idioma_sexo <- p

#Visualizando
plot_idioma_sexo

#Verificando as provas por estado
ggplot(enem) + geom_bar(aes(x = UF_PROVA), stat='count')

#tirando notação cientitica
options(scipen = 999)


#Limpando 
#filtrando somente os vlaores da UF prova diferente de  vazio
#selecionando soemnte duas colunas
#instanciando em um novo objeto
uf_prova <- enem %>% 
              filter(UF_PROVA != '') %>%
              select_(.dots = c("UF_PROVA", "SITUACAO_CONCLUSAO"))

#Plotando objeto criado
#utilizando divisao das janelas por situacao 
plot_uf_conclusao <- ggplot(uf_prova) + geom_bar(aes(x = UF_PROVA, fill = SITUACAO_CONCLUSAO), position = position_dodge())+
                        facet_grid(SITUACAO_CONCLUSAO~.)


#INSIGHTS
# Rank 3 de estados cujo tem situação escolar concluido É SP, MG E BA
# Rank 4 de estados cujo tem situação escolar não concluido ou não cursando é SP, RS, MG e PR
                        
#Melhorando grafico
#criando um objeto temporario para realizar alterações
p <- plot_uf_conclusao +
  ggtitle("Situação Escolar por Estado") +
  ylab("Quantidade") + xlab("Estado")

#Mudando fundo
#alterando legenda
#colocando titulo no meio
p <- p + theme_linedraw() +
  labs(fill = "Situação") +
  theme(plot.title = element_text(hjust = 0.5))

#Aplicando as mudançãs no original
plot_uf_conclusao <- p
plot_uf_conclusao

################################## Tratando dados faltantes ####################################################

#Analisando Idade
summary(enem$IDADE)

#Existe dados faltantes, nesse preciso trata-los
#Pegando todos os registros que não são NA e atribuindo ao objeto
idade_uf <- enem %>% 
              filter(!is.na(IDADE))

#Analisando após a tratativa
summary(idade_uf$IDADE)

#Agrupando pela uf prova e sexo
#realizar a média das idades
media_idade_sexo <- idade_uf %>%
                      group_by(UF_PROVA, SEXO) %>%
                      summarise(media = mean(IDADE))

#como ten uf em branco vamos tira-lo
#Vamos pegar tudo diferente de vazio e instanciar no proprio objeto
media_idade_sexo <- media_idade_sexo %>%
                              filter(UF_PROVA != "")

#Analisando objeto após tratativa
media_idade_sexo

########################################## Visualizando ####################################################

#Plotando a media de idade do sexo m e f, por estado 
#identity pq ja passamos o valor p eixo y
#rotacionando os eixos com coord
ggplot(media_idade_sexo) + geom_bar(aes(x=UF_PROVA,y = media,  fill = SEXO),
                                    position = position_dodge(), stat = 'identity') +
                           coord_flip()  


#Melhorando a visualização utilizarei grafico de pirâmidade
#atribuindo o objeto
#reordenando  
#Se o sexo for masculino deixaremos a media negativa para ficar do lado esquerdo
#as cores serao diferencias pelo sexo
plot_piramide <- ggplot(media_idade_sexo, aes(x = reorder(UF_PROVA, -media), 
                             y = ifelse(SEXO == "MASCULINO", -media, media), 
                             fill= SEXO)) +
                          geom_bar(stat = "identity") +
                          coord_flip()

#Alterando os valores negativos
plot_piramide <- plot_piramide + scale_y_continuous(labels = abs)

#Add titulos, labels para os eixos
#mudando fundo
p <- plot_piramide +
      ggtitle("Média de Idade por UF e Sexo") +
      ylab("Média de Idade")+
      xlab("Estado") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))

#Mudando as cores do sexo para melhor visualização
p <- p + scale_fill_manual(values = c('hotpink', 'dodgerblue3'))


#Adicionando rotulos para as barras com os valores 
p <- p + geom_text(aes(label = round(media, digits = 2),
                       hjust = 0.5),
                   size = 4.5,
                   colour = 'black',
                   fontface = 'bold')

#Atribuindo as alterações ao original
plot_piramide <- p
plot_piramide

#INSIGHTS
# Média para todos os estados, ambos sexo é a partir de 20 anos 


#Gráfico de pontos

#Selecionando um subconjunto de dados
#valores diferente de na. da nota cienci.human 
#valores diferente de na da idade cujo a idade é maior que 17
notas_ciencias_humanas <- enem %>% filter(!is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(IDADE) & IDADE > 17)

#Calculando a media da nota 
#agrupando pela idade
notas_ciencias_humanas_idade <- notas_ciencias_humanas %>%
                                    group_by(IDADE) %>%
                                    summarise(media_nota_ciencias_humanas = mean(NOTA_CIENCIAS_HUMANAS))


#Plotando objeto criado
ggplot(notas_ciencias_humanas_idade)+
  geom_point(aes(x=IDADE, y = media_nota_ciencias_humanas))

# INSIGHTS
#A media das notas de cien. humanas mantem uma média de 520 até 60 anos

#Fazendo para nota de matematica
notas_mt <- enem %>%
              filter(!is.na(NOTA_MATEMATICA) & !is.na(IDADE) & IDADE  > 17)

#Calculando media da nota de matematica
#agrupando por idade
notas_mt_idade <- notas_mt %>%
                    group_by(IDADE) %>%
                    summarise(media_nota_mt = mean(NOTA_MATEMATICA))

#Plotando
ggplot(notas_mt_idade)+
  geom_point(aes(x = IDADE, y = media_nota_mt))

# INSIGHTS
#A media das notas de matematicas começa com 500 com idades de 20 a 27 anos
#Depois disso começa a cair a média

##################################### Transformando dados ###################################################
#Agora irei juntar os dois objetos criados
#como tem o mesmo campo que é idade, irei fazer o merge
notas_ciencias_humanas_matematica_idade <- merge(notas_ciencias_humanas_idade,
                                                 notas_mt_idade, all = T) 
#analisando
notas_ciencias_humanas_matematica_idade

#converter as colunas para linhas
#Carregando a bilbioteca para efeturar tal processo
library(reshape2)

#função id vars, prioriza qual coluna n vai ser transformada para linha
notas_ciencias_humanas_matematica_idade <- melt(notas_ciencias_humanas_matematica_idade,
                                                id.vars = 'IDADE')
#analisando
notas_ciencias_humanas_matematica_idade

########################################## Visualizando ####################################################

#Plotando gráfico do objetco transformado
ggplot(notas_ciencias_humanas_matematica_idade)+
  geom_point(aes(IDADE, value, color = variable))

# INSIGHTS
#A media das notas de cien. humanas mantem uma média de 520 até 60 anos 
#A media das notas de matematicas começa com 500 com idades de 20 a 27 anos
#Depois disso começa a cair a média, com isso os cursinhos podem focar mais nessa matéria
# e especialmente com as pessoas mais velhas para melhorar os resultados.


#Melhorando gráfico
#Criando um novo objeto
plot_scatter_mt_ch <- ggplot(data = notas_ciencias_humanas_matematica_idade) +
                          geom_point(aes(IDADE, value, color = variable))
p <- plot_scatter_mt_ch +                                    
  ggtitle("Média por idade e  Matéria") +
  xlab("Idade") +
  ylab("Nota(média)")

#alterando layout
p <- p + theme_bw()

#alterando cores 
#rotulos das legenda
p <-  p + scale_color_manual(name = "Matéria", 
                             values = c("cyan", "blue"),
                             labels = c("Ciências\nHumanas", "Matemática"))
p

#atribuindo as alterações ao original
plot_scatter_mt_ch <- p
plot_scatter_mt_ch

################################## Manipulando dados #######################################################

#Verificando desempenho com base nas médias das notas em cada matéria ao longo dos anos
#Se esta piorando ou melhorando

media_anos <- enem %>% filter(!is.na(NOTA_CIENCIAS_NATUREZA) & 
                              !is.na(NOTA_CIENCIAS_HUMANAS) &
                              !is.na(NOTA_LINGUAGENS_CODIGOS) &
                              !is.na(NOTA_MATEMATICA) &
                              !is.na(NOTA_REDACAO)) %>%
                       group_by(ANO) %>%
                      summarise(media_cn = mean(NOTA_CIENCIAS_NATUREZA),
                                media_ch = mean(NOTA_CIENCIAS_HUMANAS),
                                media_lc = mean(NOTA_LINGUAGENS_CODIGOS),
                                media_mat = mean(NOTA_MATEMATICA),
                                media_redac = mean(NOTA_REDACAO))

#analisando
media_anos

#Plotando no gráfico de linhas
ggplot(media_anos) +
  geom_line(aes(x = ANO, y = media_cn), color = 'green')+
  geom_line(aes(x = ANO, y = media_ch), color = 'blue')

#Melhorando gráfico

#criando novo objeto
#transformando colunas em linhas menos a coluna ANO
media_anos_2 <- melt(data = media_anos, id.vars = 'ANO')

media_anos_2
 
#Plotando porém com novo objeto
plot_line_notas <- ggplot(media_anos_2) +
                   geom_line(aes(x = ANO, y = value, color = variable))

plot_line_notas

# INSIGHTS
#Houve uma variação grande nas medias em 2010 a 2017
#Média de ciencias da natureza de 2010 a 2017 houve um crescimento constante
#Ano que se teve maior media em ciencias humanas foi em 2015
#Ano com maior media na redaçao foi em 2010 e 015
#Media de matematia em 2010 era 500 e 505 em 2017 ja aumentou para 520
#media de linguagens de codigo se manteve constante apesar de uma grande variação
#Media em geral de 2010 minimo era 480 e o maximo era acima de 560.
#Em 2017 as médias ficaram bem próximas entre 510 e 530

#Melhorando gráfico
p <- plot_line_notas+
      ggtitle("Média Notas por Matéria") +
      ylab("Média") +
      geom_point(aes(ANO, value, color = variable), size = 3)


#inserindo os valores em cada ponto
p <- p + geom_text(aes(x = ANO, y = value, color= variable, 
                  label = round(value, digits = 2),
                  hjust = -0.15,
                  vjust = 0.2))

#Alterando rotulos da legenda
p <- p + scale_color_discrete(name = 'Matérias', labels = c("Ciên. Natureza", "Ciên. Humanas", 
                                                       "Letras/Códig.", "Matemát.",
                                                       "Redação"))+
  theme_bw()

plot_line_notas <- p


######Graficos de bolhas#######

#fazendo filtro das notas de mate, cien.humanas, redacao
#pegando somente 4 estados
#idade maior que de 17 anos 
#pegando todas informações diferentes de nulos
#agrupando pela idade e uf da prova
#realizando a media dessas tres materias 
notas_mat_reda_ch <- enem %>%
                      filter(!is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(NOTA_MATEMATICA) & !is.na(NOTA_REDACAO) &
                                !is.na(IDADE) & IDADE > 17 & UF_PROVA %in% c('CE', 'DF', 'MG', 'RS')) %>%
                      group_by(IDADE, UF_PROVA) %>%
                      summarise(media_nota_matematica = mean(NOTA_MATEMATICA),
                                media_nota_ch = mean(NOTA_CIENCIAS_HUMANAS),
                                media_nota_redac = mean(NOTA_REDACAO))


#plotando objeto criado coma a media de cada matéria
ggplot(notas_mat_reda_ch) +
  geom_point(aes(x= media_nota_ch, y = media_nota_matematica))

#inserindo color com os 4 estados informado antes
#size ou tamanho  vai de acordo com a media da nota da redação
plot_bolha_uf_notas <- ggplot(notas_mat_reda_ch) +
                       geom_point(aes(x= media_nota_ch, y = media_nota_matematica, color = UF_PROVA,
                                      size = media_nota_redac),
                                  alpha=0.5)

# INSIGHTS
#a media das notas estao entre 500 e 600
#media de matematica do estados esta abaxio de 500
#a media de ciencias humanas nos estados esta acima de 500
#a media mais baixa esta no estado do ceara
#media mais alta nas 3 materias esta em MG


#Melhorando gráfico
p <- plot_bolha_uf_notas+
      ggtitle("Médias Matemát. Ciên.Humanas e Redação")+
      xlab("Média nota Ciên.Human") + 
      ylab("Média nota Matemática")

#Alterando as legendas
p <- p +labs(color = "UF Prova", size = 'Média Nota Redação')

#Alterando fundo e alinhar a legenda
p <- p + theme_bw() + theme(legend.position = 'bottom')
p

#Aplicando as alterações
plot_bolha_uf_notas <- p

plot_bolha_uf_notas

####################################### Manipulação dados #################################################

#Filtrando e removendo somente valores da UF prova diferentes de nulos 
#e nulos da nota redação
#selecionando somente uf prova e nota redacao
notas_redacao_uf <- enem %>%
                      filter(UF_PROVA != "" & !is.na(NOTA_REDACAO)) %>%
                      select_(.dots = c("UF_PROVA", "NOTA_REDACAO"))

notas_redacao_uf

#########Gráfico Boxplot#########
plot_box_uf_redacao <- ggplot(notas_redacao_uf) +
                          geom_boxplot(aes(x = UF_PROVA, y = NOTA_REDACAO))

#passando os dados do plot para objeto dados
dados <- plot_box_uf_redacao$data


#Criando nova coluna na base dados
#coluna vai receber valor t ou f caso uma dessas filiais estiverem 
dados <- dados%>%
  mutate(filial = if_else(UF_PROVA %in% c('CE', 'DF','MG', 'RS'), T, F))

dados

#plotando nova base mas destacando os uf cujo tem filial
#destacando os outlier
p <- ggplot(dados) +
        geom_boxplot(aes(x = UF_PROVA, NOTA_REDACAO, fill = filial),
                     outlier.colour = 'red', outlier.size = 3.5)

#Alterando labels dos eixos
p <- p + xlab("UF Prova") + 
         ylab("Nota Redação") +
         theme_bw()

#Alterando as cores e a legenda
p <- p + scale_fill_manual(name = '', values = c("chocolate3", 'chartreuse3'),
                      labels = c('Sem Filial', 'Com Filial'))
plot_box_uf_redacao <-  p

# INSIGHTS
#SC, AC, E AL TIVERAM nota redadção mil
#barras mais longas representam que as notas variam muito


######################################## Manipulando Dados ################################################

#Filtrando uf prova diferente nulos 
#pegando somente valores diferente de NA da nota redacao
#Calculando a media nacional da redação e atribuindo a uma nova coluna
#agrupando pela uf e media
#fazendo a media de cada uf
media_redacao <- enem %>%
                  filter(UF_PROVA != "" & !is.na(NOTA_REDACAO)) %>%
                  mutate(media_nacional  = mean(NOTA_REDACAO)) %>%
                  group_by(UF_PROVA, media_nacional) %>%
                  summarise(media_uf = mean(NOTA_REDACAO))

#analisando
media_redacao
                
                        
#Graficos de barras
#inserindo a media nacional e a media de cada uf
plot_bar_erro <- ggplot(data = media_redacao, aes(x = reorder(UF_PROVA, media_uf), y=media_uf))+
                  geom_errorbar(aes(ymin = media_nacional/2, ymax=media_nacional), size =1)+
                  geom_bar(stat='identity')+
                  coord_flip() 
plot_bar_erro  


#Melhorando visualização

#instanciando para um novo objeto dados
dados <- plot_bar_erro$data

#Criando nova coluna
#se estiver dentro(%in%) do vetor potenciar retorna true
dados <- dados %>%
            mutate(filial = if_else(UF_PROVA %in% c('CE', 'DF', 'MG', 'RS'), T, F))

#plotando
#removendo legenda com guides
#preenchendo no geom bar fill com as filiais
p <- ggplot(data = dados, aes(x = reorder(UF_PROVA, media_uf), y=media_uf))+
        geom_errorbar(aes(ymin = media_nacional/2, ymax=media_nacional), size =1)+
        geom_bar(aes(fill = filial), stat='identity')+
        coord_flip() +
        guides(fill = FALSE)+
        ggtitle("Média de Nota Redação por UF/Nacional") +
        xlab("UF Prova") + ylab("Média Redação") +
        theme_bw()
plot_bar_erro <- p

# INSIGHTS
#Apenas a filial MG esta acima da média
#as demais estao abaixo da media, nesse caso o cursinho pode verificar as metologias de ensino dos estados
#cujo estao accima e aplicar e fora as filiais, os demais estados que estao abaixo podem identificar 
#e começar a aplicar para haver uma melhora


############################################# data viz ###################################################

#Todos os graficos em uma pagina
grid.arrange(plot_idioma_sexo, plot_uf_conclusao, plot_piramide, plot_scatter_mt_ch,
             plot_line_notas, plot_bolha_uf_notas, plot_box_uf_redacao, plot_bar_erro)


#Criando posições para os graficos
lay <- rbind(c(1,2,3,4),
             c(5,6,7,8))

lay

#Todos os graficos em uma pagina, porém agora organizado
grid.arrange(plot_idioma_sexo, plot_uf_conclusao, plot_piramide, plot_scatter_mt_ch,
             plot_line_notas, plot_bolha_uf_notas, plot_box_uf_redacao, plot_bar_erro,
             layout_matrix = lay)


#criando um novo layout
lay <- rbind(c(1,2),
             c(3,3))
#matriz
lay


grid.arrange( plot_scatter_mt_ch,
              plot_box_uf_redacao, 
              plot_line_notas,
              layout_matrix = lay)


#criando novo layout
lay <- rbind(c(1,2),
             c(3,3), #mesclando
             c(4,5),
             c(6,6))
lay

#aplicando
grid.arrange(plot_box_uf_redacao,
             plot_scatter_mt_ch, 
             plot_line_notas,
             plot_idioma_sexo,
             plot_piramide,
             plot_bar_erro,
             layout_matrix = lay)

lay <- rbind(c(1,1),
             c(2,3))

#aplicando
grid.arrange(plot_line_notas,
             plot_box_uf_redacao,
             plot_bar_erro,
             layout_matrix = lay)
             

lay2 <- rbind(c(1,2),
              c(3,3))

grid.arrange(plot_idioma_sexo, 
             plot_piramide,
             plot_uf_conclusao,
             layout_matrix = lay2)



lay3 <- rbind(c(1,2),
              c(3,3))

grid.arrange(plot_bolha_uf_notas,
             plot_scatter_mt_ch,
             layout_matrix = lay3)
