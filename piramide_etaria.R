library(readxl)
library(reshape2)
library(tidyverse)
library(plyr)


#Florianópolis
pop_faixa_etaria <- read_excel("bases/pop_faixa_etaria.xlsx")
pop_faixa_etaria$Total <- NULL
pop_faixa_etaria$Feminino <- - pop_faixa_etaria$Feminino
df.melt <- melt(pop_faixa_etaria, id.vars = "Faixa Etária")
names(df.melt) <- c("Faixa Etária", "Sexo", "População")


ggplot(df.melt, aes(x = `Faixa Etária`, y = `População`, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Feminino"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Masculino"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-60000, 60000, 20000), 
                     labels = paste0(as.character(c(seq(60, 0, -20), seq(20, 60, 20))), "mil")) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()

#Brasil
library(readr)
pop_faixa_etaria_brasil <- read_csv("bases/faixa_etaria_brasil_2020.csv", 
    locale = locale(encoding = "WINDOWS-1252"))
pop_faixa_etaria_brasil$Total <- NULL
pop_faixa_etaria_brasil$Feminino <- - pop_faixa_etaria_brasil$Feminino
df.melt_brasil <- melt(pop_faixa_etaria_brasil, id.vars = "Faixa Etária")
names(df.melt_brasil) <- c("Faixa Etária", "Sexo", "População")


ggplot(df.melt_brasil, aes(x = `Faixa Etária`, y = `População`, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Feminino"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Masculino"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-18000000, 18000000, 6000000), 
                     labels = paste0(as.character(c(seq(18, 0, -6), seq(6, 18, 6))), "mil")) + 
  coord_flip() + 
  scale_fill_brewer(palette = "Set1") + 
  theme_bw()



