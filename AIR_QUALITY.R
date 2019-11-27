##AUTOR: GUSTAVO VENTURI
##DATA: 27/11/2019
##E-MAIL: GUSTAVO_VENTURI@HOTMAIL.COM
##DESCRIÇÃO: ANÁLISE DE DADOS DE QUALIDADE DO AR DE 12 ESTAÇÕES METEOROLÓGICAS DE PEQUIM/CHINA.
##           TRABALHO ACADÊMICO PARA A MATÉRIA DE ANALISE DE DADOS, PARA O CURSO DE PÓS GRADUAÇÃO DE DATA SCIENCE E BIG DATA PELA UNIVERSIDADE POSITIVO.
##ORIGEM DO DATASET: https://archive.ics.uci.edu/ml/datasets/Beijing+Multi-Site+Air-Quality+Data 
##ORIGEM DADOS OMS: https://www.who.int/news-room/fact-sheets/detail/ambient-(outdoor)-air-quality-and-health

##------------------------------------------------##
##     INSTALAÇÃO DAS ATUALIZAÇÕES DO R           ##
##------------------------------------------------##

#install.packages("installr") #pacote de atualização do R
#library(installr) 
#updateR() #instala os updates do R - versão atual 3.6.1


##------------------------------------------------##
##            TRATAMENTO DO ARQUIVO               ##
##------------------------------------------------##


#install.packages("zip") #pacote para descompactar e compactar arquivos .zip
#library(zip)

getwd() #verifica qual é a pasta de trabalho atual
setwd("C:\\Users\\gustavo13765\\Documents\\AirQuality") #seta a pasta de trabalho desejada
getwd() # verifica a pasta de trabalho.

dados = unzip("C:\\Users\\gustavo13765\\Documents\\PRSA2017_Data_20130301-20170228.zip") #descompacta os dados na pasta informada.

#pacotes abaixo para ler e organizar os arquivos descompactados
install.packages("plyr")
install.packages("readr")
library(plyr)
library(readr)

diretorio = "PRSA_Data_20130301-20170228" # nome da pasta que estão os arquivos CSV
dados = list.files(path=diretorio, pattern="*.csv", full.names=TRUE) #lista os arquivos que possuem extensão .CSV na pasta.
#dados

tabelas = ldply(dados, read_csv) #faz a leitura dos dados das tabelas - todos os dados são agrupados pelo nome da primeira linha do csv.
#tabelas


##------------------------------------------------##
##              ANALISE DOS DADOS                 ##
##------------------------------------------------##

## MÉDIAS GERAIS DOS DADOS DAS COLUNAS

MEDIA_PM25 = round(mean(tabelas$PM2.5, na.rm = T),2)
MEDIA_PM10 = round(mean(tabelas$PM10, na.rm = T),2)
MEDIA_SO2 =  round(mean(tabelas$SO2, na.rm = T),2)
MEDIA_NO2 =  round(mean(tabelas$NO2, na.rm = T),2)
MEDIA_CO =   round(mean(tabelas$CO, na.rm = T),2)
MEDIA_O3 =   round(mean(tabelas$O3, na.rm = T),2)
MEDIA_TEMP = round(mean(tabelas$TEMP, na.rm = T),2)
MEDIA_PRES = round(mean(tabelas$PRES, na.rm = T),2)
MEDIA_DEWP = round(mean(tabelas$DEWP, na.rm = T),2)
MEDIA_RAIN = round(mean(tabelas$RAIN, na.rm = T),2)

## MEDIANAS DOS DADOS DAS COLUNAS

MEDIANA_PM25 = round(median(tabelas$PM2.5, na.rm = T),2)
MEDIANA_PM10 = round(median(tabelas$PM10, na.rm = T),2)
MEDIANA_SO2 =  round(median(tabelas$SO2, na.rm = T),2)
MEDIANA_NO2 =  round(median(tabelas$NO2, na.rm = T),2)
MEDIANA_CO =   round(median(tabelas$CO, na.rm = T),2)
MEDIANA_O3 =   round(median(tabelas$O3, na.rm = T),2)
MEDIANA_TEMP = round(median(tabelas$TEMP, na.rm = T),2)
MEDIANA_PRES = round(median(tabelas$PRES, na.rm = T),2)
MEDIANA_DEWP = round(median(tabelas$DEWP, na.rm = T),2)
MEDIANA_RAIN = round(median(tabelas$RAIN, na.rm = T),2)

## DESVIO PADRÃO

DESVPAD_PM25 = round(sd(tabelas$PM2.5, na.rm = T),2)
DESVPAD_PM10 = round(sd(tabelas$PM10, na.rm = T),2)
DESVPAD_SO2 =  round(sd(tabelas$SO2, na.rm = T),2)
DESVPAD_NO2 =  round(sd(tabelas$NO2, na.rm = T),2)
DESVPAD_CO =   round(sd(tabelas$CO, na.rm = T),2)
DESVPAD_O3 =   round(sd(tabelas$O3, na.rm = T),2)
DESVPAD_TEMP = round(sd(tabelas$TEMP, na.rm = T),2)
DESVPAD_PRES = round(sd(tabelas$PRES, na.rm = T),2)
DESVPAD_DEWP = round(sd(tabelas$DEWP, na.rm = T),2)
DESVPAD_RAIN = round(sd(tabelas$RAIN, na.rm = T),2)

## VARIAÇÃO

VAR_PM25 = round(var(tabelas$PM2.5, na.rm = T),2)
VAR_PM10 = round(var(tabelas$PM10, na.rm = T),2)
VAR_SO2 =  round(var(tabelas$SO2, na.rm = T),2)
VAR_NO2 =  round(var(tabelas$NO2, na.rm = T),2)
VAR_CO =   round(var(tabelas$CO, na.rm = T),2)
VAR_O3 =   round(var(tabelas$O3, na.rm = T),2)
VAR_TEMP = round(var(tabelas$TEMP, na.rm = T),2)
VAR_PRES = round(var(tabelas$PRES, na.rm = T),2)
VAR_DEWP = round(var(tabelas$DEWP, na.rm = T),2)
VAR_RAIN = round(var(tabelas$RAIN, na.rm = T),2)

## MÍNIMO

MINIMO_PM25 = round(min(tabelas$PM2.5, na.rm = T),2)
MINIMO_PM10 = round(min(tabelas$PM10, na.rm = T),2)
MINIMO_SO2 =  round(min(tabelas$SO2, na.rm = T),2)
MINIMO_NO2 =  round(min(tabelas$NO2, na.rm = T),2)
MINIMO_CO =   round(min(tabelas$CO, na.rm = T),2)
MINIMO_O3 =   round(min(tabelas$O3, na.rm = T),2)
MINIMO_TEMP = round(min(tabelas$TEMP, na.rm = T),2)
MINIMO_PRES = round(min(tabelas$PRES, na.rm = T),2)
MINIMO_DEWP = round(min(tabelas$DEWP, na.rm = T),2)
MINIMO_RAIN = round(min(tabelas$RAIN, na.rm = T),2)

## MÁXIMO

MAXIMO_PM25 = round(max(tabelas$PM2.5, na.rm = T),2)
MAXIMO_PM10 = round(max(tabelas$PM10, na.rm = T),2)
MAXIMO_SO2 =  round(max(tabelas$SO2, na.rm = T),2)
MAXIMO_NO2 =  round(max(tabelas$NO2, na.rm = T),2)
MAXIMO_CO =   round(max(tabelas$CO, na.rm = T),2)
MAXIMO_O3 =   round(max(tabelas$O3, na.rm = T),2)
MAXIMO_TEMP = round(max(tabelas$TEMP, na.rm = T),2)
MAXIMO_PRES = round(max(tabelas$PRES, na.rm = T),2)
MAXIMO_DEWP = round(max(tabelas$DEWP, na.rm = T),2)
MAXIMO_RAIN = round(max(tabelas$RAIN, na.rm = T),2)

## APRESENTAR UM RESUMO DOS DADOS
summary(tabelas)

## GUIDELINES OMS (ORGANIZACAO MUNDIAL DA SAUDE) - MÁXIMO ACEITAVEL

OMS_PM25 = 10 #μg/m3 MÉDIA ANUAL
OMS_PM10 = 20 #μg/m3 MÉDIA ANUAL
OMS_SO2 =  20 #μg/m3 MÉDIA EM 24 HORAS
OMS_NO2 =  40 #μg/m3 MÉDIA ANUAL
OMS_O3 =   100 #μg/m3 MÉDIA EM 8 HORAS

## AGREGAÇÕES POR ESTAÇÃO

## MEDIA

st_MEDIA_PM25 = aggregate(tabelas$PM2.5, list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)
st_MEDIA_PM10 = aggregate(tabelas$PM10,  list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)
st_MEDIA_SO2 =  aggregate(tabelas$SO2,   list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)
st_MEDIA_NO2 =  aggregate(tabelas$NO2,   list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)
st_MEDIA_CO =   aggregate(tabelas$CO,    list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)
st_MEDIA_O3 =   aggregate(tabelas$O3,    list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)
st_MEDIA_TEMP = aggregate(tabelas$TEMP,  list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)
st_MEDIA_PRES = aggregate(tabelas$PRES,  list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)
st_MEDIA_DEWP = aggregate(tabelas$DEWP,  list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)
st_MEDIA_RAIN = aggregate(tabelas$RAIN,  list(tabelas$station),FUN = mean, na.rm = TRUE, na.action = NULL)

##MEDIANA

st_MEDIANA_PM25 = aggregate(tabelas$PM2.5, list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)
st_MEDIANA_PM10 = aggregate(tabelas$PM10,  list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)
st_MEDIANA_SO2 =  aggregate(tabelas$SO2,   list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)
st_MEDIANA_NO2 =  aggregate(tabelas$NO2,   list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)
st_MEDIANA_CO =   aggregate(tabelas$CO,    list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)
st_MEDIANA_O3 =   aggregate(tabelas$O3,    list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)
st_MEDIANA_TEMP = aggregate(tabelas$TEMP,  list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)
st_MEDIANA_PRES = aggregate(tabelas$PRES,  list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)
st_MEDIANA_DEWP = aggregate(tabelas$DEWP,  list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)
st_MEDIANA_RAIN = aggregate(tabelas$RAIN,  list(tabelas$station),FUN = median, na.rm = TRUE, na.action = NULL)

##DESVIO PADRAO

st_DESVPAD_PM25 = aggregate(tabelas$PM2.5, list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)
st_DESVPAD_PM10 = aggregate(tabelas$PM10,  list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)
st_DESVPAD_SO2 =  aggregate(tabelas$SO2,   list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)
st_DESVPAD_NO2 =  aggregate(tabelas$NO2,   list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)
st_DESVPAD_CO =   aggregate(tabelas$CO,    list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)
st_DESVPAD_O3 =   aggregate(tabelas$O3,    list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)
st_DESVPAD_TEMP = aggregate(tabelas$TEMP,  list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)
st_DESVPAD_PRES = aggregate(tabelas$PRES,  list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)
st_DESVPAD_DEWP = aggregate(tabelas$DEWP,  list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)
st_DESVPAD_RAIN = aggregate(tabelas$RAIN,  list(tabelas$station),FUN = sd, na.rm = TRUE, na.action = NULL)

##VARIAÇÃO

st_VAR_PM25 = aggregate(tabelas$PM2.5, list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)
st_VAR_PM10 = aggregate(tabelas$PM10,  list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)
st_VAR_SO2 =  aggregate(tabelas$SO2,   list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)
st_VAR_NO2 =  aggregate(tabelas$NO2,   list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)
st_VAR_CO =   aggregate(tabelas$CO,    list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)
st_VAR_O3 =   aggregate(tabelas$O3,    list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)
st_VAR_TEMP = aggregate(tabelas$TEMP,  list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)
st_VAR_PRES = aggregate(tabelas$PRES,  list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)
st_VAR_DEWP = aggregate(tabelas$DEWP,  list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)
st_VAR_RAIN = aggregate(tabelas$RAIN,  list(tabelas$station),FUN = var, na.rm = TRUE, na.action = NULL)

##MÁXIMO

st_MAX_PM25 = aggregate(tabelas$PM2.5, list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)
st_MAX_PM10 = aggregate(tabelas$PM10,  list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)
st_MAX_SO2 =  aggregate(tabelas$SO2,   list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)
st_MAX_NO2 =  aggregate(tabelas$NO2,   list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)
st_MAX_CO =   aggregate(tabelas$CO,    list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)
st_MAX_O3 =   aggregate(tabelas$O3,    list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)
st_MAX_TEMP = aggregate(tabelas$TEMP,  list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)
st_MAX_PRES = aggregate(tabelas$PRES,  list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)
st_MAX_DEWP = aggregate(tabelas$DEWP,  list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)
st_MAX_RAIN = aggregate(tabelas$RAIN,  list(tabelas$station),FUN = max, na.rm = TRUE, na.action = NULL)

##MÍNIMO

st_MIN_PM25 = aggregate(tabelas$PM2.5, list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)
st_MIN_PM10 = aggregate(tabelas$PM10,  list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)
st_MIN_SO2 =  aggregate(tabelas$SO2,   list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)
st_MIN_NO2 =  aggregate(tabelas$NO2,   list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)
st_MIN_CO =   aggregate(tabelas$CO,    list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)
st_MIN_O3 =   aggregate(tabelas$O3,    list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)
st_MIN_TEMP = aggregate(tabelas$TEMP,  list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)
st_MIN_PRES = aggregate(tabelas$PRES,  list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)
st_MIN_DEWP = aggregate(tabelas$DEWP,  list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)
st_MIN_RAIN = aggregate(tabelas$RAIN,  list(tabelas$station),FUN = min, na.rm = TRUE, na.action = NULL)

## PLOTANDO NO GRÁFICO USANDO GGPLOT2

install.packages("ggplot2")
library(ggplot2)

# MÉDIA
ggplot(st_MEDIA_PM25, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("PARTICULAS (PM 2.5)")

ggplot(st_MEDIA_PM10, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("PARTICULAS (PM 10)")

ggplot(st_MEDIA_SO2, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("DIÓXIDO DE ENXÔFRE (SO2)")

ggplot(st_MEDIA_NO2, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("DIÓXIDO DE NITROGÊNIO (NO2)")

ggplot(st_MEDIA_CO, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("MONÓXIDO DE CARBONO (CO)")

ggplot(st_MEDIA_O3, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("OZÔNIO (O3)")

ggplot(st_MEDIA_TEMP, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("TEMPERATURA")

ggplot(st_MEDIA_PRES, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("PRESSÃO ATMOSFÉRICA")

ggplot(st_MEDIA_DEWP, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("TEMPERATURA DO PONTO ORVALHO")

ggplot(st_MEDIA_RAIN, aes(Group.1,x))+
  geom_col(show.legend = FALSE)+
  xlab("Estação Meteorológica")+
  ylab("CHUVA")



  