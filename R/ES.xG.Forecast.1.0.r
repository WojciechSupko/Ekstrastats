rm(list=ls())
########################################
## 0. SETTINGS
########################################
start <- Sys.time()  

# Biblioteki do zainstalowania

# install.packages(c('data.table', 'ggplot2'))

# Libraries

library(data.table)
library(ggplot2)
library(gridExtra)
library(ggforce)
# Database connection

# Working directory (change it accordingly)

# setwd('F:\\EkstraStats\\Ekstraklasa\\Prognoza')
setwd('D:\\prv\\Ekstraklasa\\Prognoza')
# setwd('C:\\Users\\User\\Dropbox\\sezon 2016-17\\Prognoza')

# Colors palette

# colPalette <- c('#1a9641', '#dfc27d', '#e41a1c', '#636363', '#377eb8', '#984ea3')
# names(colPalette) <- c('ProbR', 'ProbExcel', 'Górnik', 'Legia', 'gosp', 'gosc')
# 
# colResult <- c('#984ea3', '#377eb8', 'grey')
# names(colResult) <- c(1, 2, 0)

# Sourcing

# source('F:\\NarysowaneDane\\R\\Plot.Watermark.R')

# Custom functions

# cross-join 

CJ.table <- function(X,Y){
  setkey(X[,c(k=1,.SD)],k)[Y[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]}

# DoDummyVariables

DoDummyVariables <- function(factor){
  final.data.frame <- data.frame(factor)
  for (level in levels(final.data.frame[, 1])){
    final.data.frame[, level] <- as.integer(factor == level)
  }
  return(final.data.frame)
}

########################################
## 1. DATA IMPORT
########################################

# Dane odnosnie strzalow z ExtraStats

Imp.Shots <- fread('./in/Imp.Shots.txt', stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = '')

# Terminarz

Imp.Terminarz <- fread('./in/Imp.Terminarz.txt', stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = '')

# Tabela

Imp.Table <- fread('./in/Imp.Table.txt', stringsAsFactors = FALSE, encoding = 'UTF-8', na.strings = '')

########################################
## 2. DATA CLEAN-UP
########################################

# Przygotowanie danych 0-1


setnames(Imp.Shots, iconv(gsub('\\.','', names(Imp.Shots)), 'UTF-8', 'UTF-8'))
Imp.Shots[,CzęśćCiała := factor(ifelse(CzęśćCiała == 'głowa', 'głowa', 'inne') )]

bStrefa <- data.table(DoDummyVariables(Imp.Shots[,Strefa]))[,factor:=NULL]

bCzescCiala <- data.table(DoDummyVariables(Imp.Shots[,CzęśćCiała]))[,factor:=NULL]

bSFGbezp <- data.table(DoDummyVariables(Imp.Shots[,bezpwolny]))[,factor:=NULL]
names(bSFGbezp)[1] <- 'bezp'
bSFGbezp[is.na(bezp), ':='(bezp = 0)]

bSFG <- data.table(DoDummyVariables(Imp.Shots[,poSFG]))[,factor:=NULL]
bSFG[is.na(aut), ':='(aut = 0, rożny = 0, wolny = 0)]

bGol <- Imp.Shots[, .(gol = ifelse(!is.na(GOL), 1, 0))]
bKP <- Imp.Shots[, .(KP = ifelse(!is.na(KP), 1, 0))]
# Zebranie danych 0-1

Proc.All <- cbind(bGol, bKP, bStrefa, bCzescCiala, bSFGbezp, bSFG)

model <- glm(gol ~., family = binomial(link = 'logit'), data = Proc.All)



modelExpG <- predict(model, newdata = Test.All, type="response")

t <- cbind(Imp.Shots, modelExpG)
