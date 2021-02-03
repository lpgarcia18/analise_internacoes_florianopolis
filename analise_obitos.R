library(readxl)
library(tidyverse)
library(reshape2)

obitos <- read_excel("bases/obitos_cid_10.xlsx")
obitos <- subset(obitos, obitos$`Capítulo CID-10` %in% c("IX. Doenças do aparelho circulatório",
							 "II. Neoplasias (tumores)",
							 "X. Doenças do aparelho respiratório",
							 "XX. Causas externas de morbidade e mortalidade"))

obitos <- melt(obitos)
names(obitos) <- c("CID-10", "Ano", "Óbitos")

ggplot(obitos, aes(Ano, `Óbitos`, group = `CID-10`, color = `CID-10`))+
	geom_line(size = 1)+
	theme_bw()
