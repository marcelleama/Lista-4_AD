#LISTA 4 DE ANALISE DE DADOS_

#Exercico numero 1:

#O link do repositório do GitHub:
# https://github.com/marcelleama/Lista-4_AD

#Exercicio numero 2:

library(ffbase)

# definindo o diretório 
getwd()
setwd("C:/Users/celle/Documents/R/dados_encontro_1_ufpe/dados_encontro_1_ufpe")

# checando
getwd()

#Para a resolução do exercício usaremos três base de dados:
# Base 1 (DOCENTES_NORDESTE), Base 2 (MATRICULAS_NORDESTE) e Base 3 (atlas2013_dadosbrutos_pt)

#Tratar as bases para ficar mais fácil de operá-las:

#Tratando as bases:


#BASE 1

# Carregando a base de dados DOCENTES_NORDESTE
docentes_ne <-read.csv2.ffdf(file = "DOCENTES_NORDESTE.csv", sep = "|",
                             first.rows=100000)

# definindo novo diretório
setwd("C:/Users/celle/Documents/R/Lista-4_AD")

#verificando estrutura de base
dim(docentes_ne)

#selecionando PE
docentes_pe <- subset(docentes_ne, CO_UF == 26)
dim (docentes_pe)

#transformando em data.frame
docentes_pe <- as.data.frame(docentes_pe)

getwd()

#salvando arquivo em formato RData
save(docentes_pe, file = "docentes_pe_censo_escolar_2016.RData")



#BASE 2

#acessando o pacote ffbase
library(ffbase)
setwd("C:/Users/celle/Documents/R/Lista-4_AD")

#carregando a base de dados
matricula_ne <- read.csv.ffdf(file = "MATRICULA_NORDESTE.csv", sep = "|", first.rows=100000)

#selecionando PE
matricula_pe <-subset(matricula_ne, CO_UF == 26)
dim (matricula_pe)

#transformando em data.frame
matricula_pe <- as.data.frame(matricula_pe)

#salvando arquivo em formato RData
save(matricula_pe, file = "matricula_pe_censo_escolar_2016.RData")



# BASE 3

#Carregando a base de dados atlas2013_dadosbrutos_pt

install.packages("tidyverse")
library(tidyverse)

#utilizando a função read_xlsx do pacote readxl para carregar os dados do PNUD
install.packages("readxl")
library(readxl)

#checando o diretório e definindo
getwd()
setwd("C:/Users/celle/Documents/R/Lista-4_AD")

PNUD <- read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet=2)

head(PNUD)
unique(PNUD$ANO)

# selecionando dados de 2010 e do Estado de PE

# acessando o pacote para a função filter
library(dplyr)

# criando a nova base
pnud_pe_2010 <- PNUD%>%filter(ANO == 2010 & UF == 26)

# salvando a nova base em RData
save(pnud_pe_2010, file = "pnud_pe_2010.RData")

#Temos 3 novas bases:

# docentes_pe_censo_escolar_2016.RData
# matricula_pe_censo_escolar_2016.RData
# pnud_pe_2010.RData

#removendo do global environment
rm(list = ls())

#Vamos agora atender os outros requisitos da questao 2:

#filtrando:
library (dplyr)

#Não deve haver docente com mais de 70 anos e com menos de 18 anos

load("docentes_pe_censo_escolar_2016.RData")
names(docentes_pe)

# vamos criar uma nova variavel que nos dê o intervalo de idade entre 18 e 70 anos:

#filtrar seguido de nova coluna

docentes_pe_selecao <- docentes_pe %>%
  filter(NU_IDADE >= 18,NU_IDADE <= 70) %>% mutate(FX_IDADE = ifelse(NU_IDADE >=18 & NU_IDADE<=70, 
       "de 18 anos a 70 anos"))

head(docentes_pe_selecao)
summary(factor(docentes_pe_selecao$FX_IDADE))

# criando uma nova base agrupando as duas variáveis de interesse:

# combinando as funcoes group by e summarise:

# A IDADE SELECIONADA E OS MUNICIPIOS
docentes_pe_sel <- docentes_pe %>% group_by(CO_MUNICIPIO) %>% 
    summarise (docentes_pe_selecao = n())

dim(docentes_pe_sel)
length(unique(docentes_pe$CO_MUNICIPIO))
summary (docentes_pe_sel)

#Repetiremos o processo para os alunos na base de dados de matrículas:
#Não deve have aluno com mais de 25 anos ou menos de 1 ano

load("matricula_pe_censo_escolar_2016.RData")

# aplicamos o filtro e incluimos uma nova variavel na base 
# com a condicao da questao: entre 01 e 25 anos

matricula_pe_selecao <- matricula_pe %>% 
  filter(NU_IDADE >= 1, NU_IDADE <= 25) %>% mutate(FX_IDADE = ifelse(NU_IDADE >=1, NU_IDADE <=25, "de 01 a 25 anos"))

head(matricula_pe_selecao)
summary(factor(matricula_pe_selecao$FX_IDADE))

#agrupamos a variável com os municipios, criando o nosso subgrupo de interesse:

matricula_pe_sel <- matricula_pe %>% group_by(CO_MUNICIPIO) %>%
    summarise (matricula_pe_selecao = n())

dim(matricula_pe_sel)
summary(matricula_pe_sel)

#vamos unir as novas 2 bases pela variável comum CO_MUNICIPIO:
#unindo bases 1 e 2

base_1_2 <- matricula_pe_sel %>% full_join(docentes_pe_sel, by = "CO_MUNICIPIO")
names(base_1_2)

#Temos agora uma tabela com 3 colunas
# A tibble: 185 x 3
#CO_MUNICIPIO matricula_pe_selecao docentes_pe_selecao

#salvamos nossa nova tabela em RData
save(base_1_2, file="base_1_2.RData")

#limpando o ambiente
rm(list = ls())


setwd("C:\Users\celle\Documents\R\Lista-4_AD")

#carregando a nova tabela:
load("base_1_2.RData")

#Temos agora uma nova tabela: base_1_2

#vamos dividir o número matriculados(alunos) pelo de docentes
# criando uma 4a coluna em nossa tabela com a proporcao matriculados/docentes

base_1_2$new <- (base_1_2$matricula_pe_selecao/base_1_2$docentes_pe_selecao)
names(base_1_2)

#Apresente estatísticas descritivas do número de alunos por docente nos municípios do Estado
summary(base_1_2$new)

#> summary(base_1_2$new)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#4.651   5.657   6.158   6.293   6.888   9.661

mean (base_1_2$new)
#> mean (base_1_2$new)
#[1] 6.293437

median (base_1_2$new)
#> median (base_1_2$new)
#[1] 6.157609

sample (base_1_2, 1)

#> sample (base_1_2, 1)
# A tibble: 185 x 1
#docentes_pe_selecao
#<int>
1                3996
2                1738
3                1078
4                1214
5                1191
6                1796
7                 527
8                1655
9                 864
10                 781
# ... with 175 more rows


#decis
quantile (base_1_2$new, probs = seq(0,1, .1))

0%      10%      20%      30%      40%      50%      60% 
4.651163 5.240635 5.491350 5.788273 6.037800 6.157609 6.461246 
70%      80%      90%     100% 
  6.676737 6.990493 7.370986 9.660739 

#carregando a Base 3

load("pnud_pe_2010.RData")

pnud_pe_2010_IDHM <- pnud_pe_2010 %>% select (IDHM, Codmun7)

head(pnud_pe_2010_IDHM)

pnud_pe_2010_IDHM <- as.data.frame(pnud_pe_2010_IDHM)

#Vamos juntar as três bases identificando Codmun7 com "CO_MUNICIPIO"
#juntando bases 1_2_ e pnud_pe_2010_IDHM

base_1_2_3_ <- pnud_pe_2010_IDHM %>% full_join(base_1_2, by = c("Codmun7" = "CO_MUNICIPIO"))
                                       

dim(base_1_2_3_)
names(base_1_2_3_)

#salvando nova base 

save (base_1_2_3_, file = "base_1_2_3_censo_pnud_pe.RData")

# continuando o exercício

# apresente o municipio com maior número de alunos por docente

#Municipio - 2615805 - nosso municipio é "TUPANATINGA"

max(base_1_2_3_$new)
#[1] 9.660739

load("base_1_2_3_censo_pnud_pe.RData")

#vamos organizar a nossa base para colocar o valor máximo de número de alunos por docente
# na 1a linha e ter acessá-la para o IDHM 

base_1_2_3_ <- base_1_2_3_ %>% arrange (desc(new))
head (base_1_2_3_)

base_1_2_3_[1,]
#> base_1_2_3_[1,]

# A tibble: 1 x 5
#IDHM Codmun7 matricula_pe_selec~ docentes_pe_selec~   new
#<dbl>   <dbl>               <int>              <int> <dbl>
#  1 0.519 2615805                7062                731  9.66

#Acessando a nossa 1a linha da tabela: 

# Para o municipio com o maior número de alunos por docentes (2615805),
#o IDHM é 0.519

# correlação linear de Pearson

cor(base_1_2_3_$new, base_1_2_3_$IDHM)
#[1] -0.4796604

# o valor de r indica uma correlação fraca e inversa 
# entre as variáveis, há uma tendencia de quanto maior a razão alunos
# por professor, menor o IDHM.

# teste de correlação linear de Pearson
cor.test(base_1_2_3_$new, base_1_2_3_$IDHM)

#Pearson's product-moment correlation

#data:  base_1_2_3_$new and base_1_2_3_$IDHM
#t = -7.3949, df = 183, p-value = 4.917e-12
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
 -0.5835475 -0.3603263
#sample estimates:
       cor 
#-0.4796604

# Caso há hipotese nula seja a de que não há
# nenhuma relação entre a razão aluno/docente
# e o IDHM, o p-valor abaixo de 1% indica que 
# a região crítica para o nível de significância
# é muito pequena, ou que, a chance do pequisador
# incorrer em erro ao descartar a hipótese nula é
# menor que 1%, de modo que, a não relação entre
# as variáveis não procede.
       
       
# seu script deve salvar a base de dados criada para o cálculo em formato .RData

save(base_1_2_3_, file = "base_1_2_3_censo_pnud_pe.RData")

#Exercicio numero 3

#Usando o pacote ggplot2, apresente o gráfico de dispersão entre as duas variáveis 
#(número de alunos por docente e IDHM)

install.packages("ggplot2")
require (ggplot2)

ggplot (data = base_1_2_3_, aes(x = new, y = IDHM)) + 
  geom_point( color = "green", size = 2) + 
  labs (x = "Número de alunos por docentes", y = "IDHM")


