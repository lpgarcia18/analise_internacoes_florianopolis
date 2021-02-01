library(read.dbc)
library(tidyverse)
#devtools::install_github("rfsaldanha/microdatasus")
library(microdatasus)
#Extração das AIHs 
dados <- fetch_datasus(year_start = 2016, month_start = 1, year_end = 2020, month_end = 11, information_system = "SIH-RD")
dados2_fpolis <- subset(dados2, MUNIC_RES == "420540", stringAsFactor = FALSE)
dados <- process_sih(dados2)

## FILTRA FLORIANOPOLIS PELO MUNICIPIO DE RESIDENCIA
dados_fpolis <- subset(dados, MUNIC_RES == "420540", stringAsFactor = FALSE)

## ARRUMA DADOS
#names(tabela_ibge_municipios)[1] <- 'uf'
dados_fpolis$ESPEC <- as.character(dados_fpolis$ESPEC)
dados_fpolis$ESPEC[dados_fpolis$ESPEC=="87"] <- "Saúde Mental (Clínico)"



