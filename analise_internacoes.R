library(tidyverse)
#devtools::install_github("rfsaldanha/microdatasus")
library(microdatasus)
library(readr)
library(reshape2)
library(gghighlight)


#Extração das AIHs 
#dados <- fetch_datasus(year_start = 2016, month_start = 1, year_end = 2020, month_end = 11, information_system = "SIH-RD",uf = "SC")
#write.csv(dados, "bases/dados_internacao_2016_2020_sc.csv", row.names = F)
dados <- read_csv("bases/dados_internacao_2016_2020_sc.csv")

#Dados da população
pop_1992_2019 <- read_delim("bases/sc_1992_2019_tcu.csv", 
    ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), 
    trim_ws = TRUE)

pop_2020 <- read_csv("bases/sc_2020_tcu.csv")


#Trabalhando a população
pop_2017_2019 <- pop_1992_2019 %>% dplyr::select("\"\"\"Município\"\"",  "\"\"2017\"\"", "\"\"2018\"\"","\"\"2019\"\"\"")
names(pop_2017_2019) <- c("MUNICIPIO", 2017:2019)
pop_2017_2019$COD_MUN <- substr(pop_2017_2019$MUNICIPIO, 0, 6)
pop_2017_2019$MUNICIPIO <- substr(pop_2017_2019$MUNICIPIO, 8, 50)

pop_2020$`COD. MUNIC` <- ifelse(nchar(pop_2020$`COD. MUNIC`)==5, substr(pop_2020$`COD. MUNIC`, 1, 4), pop_2020$`COD. MUNIC`)
pop_2020$COD_MUN <- paste0(pop_2020$`COD. UF`, pop_2020$`COD. MUNIC`)
pop_2020$`COD. UF` <- NULL
pop_2020$`COD. MUNIC` <- NULL
pop_2020$`NOME DO MUNICÍPIO` <- NULL
names(pop_2020)[1] <- "2020"
pop <- merge(pop_2017_2019, pop_2020, by = "COD_MUN")
pop_100 <- subset(pop, pop$`2020` >= 100000)


#Juntando internação e população
dados_100 <- merge(dados, pop_100, by.x = "MUNIC_RES", by.y = "COD_MUN")

#Agrupando cids por mês e município
dados_100$INTER <- 1
dados_100$MES_ANO <- paste0(substr(dados_100$DT_INTER, 1,4),"-",substr(dados_100$DT_INTER, 5,6))
dados_100 <- subset(dados_100, dados_100$MES_ANO != "2020-11") #Produção de novembro ainda não foi computada
dados_100$DIAG_PRINC <- substr(dados_100$DIAG_PRINC, 1,1)
# dados_100 <- dados_100 %>%
# 	group_by(MUNICIPIO, MES_ANO, DIAG_PRINC) %>%
# 	summarise("INTER" = sum(INTER, na.rm = T))


#Taxa de internação total
dados_100_grup <- dados_100 %>%
	group_by(MUNIC_RES, MES_ANO) %>%
	summarise("INTER" = sum(INTER, na.rm = T))

dados_100_grup$ANO <- substr(dados_100_grup$MES_ANO,1,4)
pop_100 <- melt(pop_100, id.vars = c("COD_MUN", "MUNICIPIO"))
names(pop_100) <- c("MUNIC_RES", "MUNICIPIO", "ANO", "POP")

dados_100_grup <- merge(dados_100_grup, pop_100, by = c("MUNIC_RES", "ANO"))
dados_100_grup$TX_INTER <- dados_100_grup$INTER / as.numeric(dados_100_grup$POP) * 100000

ggplot(dados_100_grup, aes(MES_ANO, TX_INTER, group = MUNICIPIO, color = MUNICIPIO))+
	#geom_line(size = 2)+
	geom_smooth(se = F)+
	theme(axis.text.x = element_blank(),
	      axis.text.y = element_blank(),
	      axis.ticks = element_blank(),
	      panel.grid.major = element_blank(), 
	      panel.grid.minor = element_blank(),
	      panel.background = element_blank())+
	ylab("Tx Internação Geral por 100.000hab")+
	xlab("Janeiro de 2017 a Outubro de 2020")+
	ggtitle("Municípios de SC com mais de 100.000hab")+
	gghighlight::gghighlight(MUNICIPIO == "Florianópolis", 
				 unhighlighted_params = list(size = 0.5),
				 label_params = list(vjust = 2, hjust = 1.5))+
	geom_segment(aes(x = "2020-05", y = 600, xend = "2020-03", yend = 550),
                  arrow = arrow(length = unit(0.5, "cm")))+
	annotate(geom="text", x="2020-05", y=620, label="COVID-19",
              color="red")

#Taxa de internação em UTI
dados_100$UTI <- ifelse(dados_100$UTI_MES_TO == 0, 0, 1)
dados_100_uti <- dados_100 %>%
	group_by(MUNIC_RES, MES_ANO) %>%
	summarise("UTI" = sum(UTI, na.rm = T))

dados_100_uti$ANO <- substr(dados_100_uti$MES_ANO,1,4)

dados_100_uti <- merge(dados_100_uti, pop_100, by = c("MUNIC_RES", "ANO"))
dados_100_uti$TX_UTI <- dados_100_uti$UTI / as.numeric(dados_100_uti$POP) * 100000

ggplot(dados_100_uti, aes(MES_ANO, TX_UTI, group = MUNICIPIO, color = MUNICIPIO))+
	#geom_line(size = 2)+
	geom_smooth(se = F)+
	theme(axis.text.x = element_blank(),
	      axis.text.y = element_blank(),
	      axis.ticks = element_blank(),
	      panel.grid.major = element_blank(), 
	      panel.grid.minor = element_blank(),
	      panel.background = element_blank())+
	ylab("Tx Internação em UTI por 100.000hab")+
	xlab("Janeiro de 2017 a Outubro de 2020")+
	ggtitle("Municípios de SC com mais de 100.000hab")+
	gghighlight::gghighlight(MUNICIPIO == "Florianópolis", 
				 unhighlighted_params = list(size = 0.5),
				 label_params = list(vjust = 2, hjust = 1.5))+
	geom_segment(aes(x = "2020-05", y = 60, xend = "2020-03", yend = 55),
                  arrow = arrow(length = unit(0.5, "cm")))+
	annotate(geom="text", x="2020-05", y=62, label="COVID-19",
              color="red")


#Análise por CID
CID_10 <- read_csv("bases/CID_10.csv")
dados_100_cid <- dados_100
dados_100_cid$DIAG_PRINC <- substr(dados_100_cid$DIAG_PRINC, 1,1)
dados_100_cid <- merge(dados_100_cid, CID_10, by.x = "DIAG_PRINC", by.y = "LETRA_CID")
dados_100_cid <- subset(dados_100_cid, dados_100_cid$MUNIC_RES == "420540") 

dados_100_cid <- dados_100_cid %>%
	group_by(DESCRICAO_CID, MES_ANO) %>%
	summarise("INTER" = sum(INTER, na.rm = T))

dados_100_cid$ANO <- substr(dados_100_cid$MES_ANO,1,4)
pop_florip <- subset(pop_100, pop_100$MUNICIPIO == "Florianópolis")

dados_100_cid <- merge(dados_100_cid, pop_florip, by = c("ANO"))
dados_100_cid$TX_INTER <- dados_100_cid$INTER / as.numeric(dados_100_cid$POP) * 100000
#unique(dados_100_cid$DESCRICAO_CID)
dados_100_cid <- subset(dados_100_cid, dados_100_cid$DESCRICAO_CID %in% c(
	"Doenças do aparelho respiratório",
	"Doenças do aparelho circulatório",
	"Transtornos mentais e comportamentais",
	"Gravidez parto e puerpério",
	"Doenças do aparelho geniturinário",
	"Doenças do aparelho digestivo",
	"Neoplasias (tumores)",
	"Algumas doenças infecciosas e parasitárias",
	"Códigos para propósitos especiais" 
))

ggplot(dados_100_cid, aes(MES_ANO, TX_INTER, group = DESCRICAO_CID, color = DESCRICAO_CID))+
	geom_line(size = 2)+
	theme(axis.text.x = element_blank(),
	      axis.text.y = element_blank(),
	      axis.ticks = element_blank(),
	      panel.grid.major = element_blank(), 
	      panel.grid.minor = element_blank(),
	      panel.background = element_blank())+
	ylab("Tx Internação por 100.000hab para os Capítulos da CID-10")+
	xlab("Janeiro de 2017 a Outubro de 2020")+
	ggtitle("Principais Causas de Internação em Florianópolis")+
	gghighlight::gghighlight(DESCRICAO_CID == DESCRICAO_CID, 
				 unhighlighted_params = list(size = 0.5),
				 label_params = list(vjust = 2, hjust = 1.5))+
	geom_segment(aes(x = "2020-04", y = 60, xend = "2020-03", yend = 55),
                  arrow = arrow(length = unit(0.5, "cm")), color = "black")+
	annotate(geom="text", x="2020-05", y=64, label="COVID-19")+
  	facet_wrap(~ DESCRICAO_CID)

