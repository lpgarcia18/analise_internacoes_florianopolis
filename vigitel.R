library(readxl)
library(reshape2)
library(tidyverse)

vigitel <- read_excel("bases/vigitel_2016_2019.xlsx")
vigitel <- melt(vigitel)
names(vigitel) <- c("Fator de risco", "Ano", "Percentual")

ggplot(vigitel, aes(Ano, Percentual, group = `Fator de risco`, color = `Fator de risco`))+
	geom_line(size = 1)+
	theme_bw()+
	facet_wrap(~`Fator de risco`)
