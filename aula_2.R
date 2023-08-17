library(dplyr)

pib_cid <- readRDS(url("https://github.com/mgaldino/book-regression/raw/main/dados/pib_cid.RDS"))

 glimpse(pib_cid)
 

 head(pib_cid)

View(pib_cid)

glimpse(mtcars)

library(dplyr)
library(tidyverse)
library(knitr)

#aula 2 data wrangling

df <- pib_cid %>%
  summarise(pib_medio = mean(pib_total),
            pib_per_capita_medio = mean(pib_per_capita))

kable(df)

pib_cid %>% 
  summarise(maior_pib = max(pib_total))

#qual cidade tem o maior pib?

pib_cid %>% 
  filter(pib_total == max(pib_total))

#apenas municipios de sp

pib_cid_sp <- pib_cid %>% 
  filter(sigla_uf == "SP")

glimpse(pib_cid_sp)


# % do PIB imposto

df <- pib_cid %>% 
  mutate(per_imposto = impostos/pib_total)

View(df)

#% do pib imposto mun sp

df_sp <- df %>% 
  filter(nome_munic == "São Paulo")

View(df_sp)


# groupby pib medio de cada estado

pib_cid %>% 
  group_by(sigla_uf) %>% 
  summarise(pib_medio_uf = mean(pib_total))

df <- pib_cid %>% 
  group_by(sigla_uf) %>% 
  summrarise(soma_pib_munic_uf = sum(pib_total),
             soma_impostos_munic_uf = sum(impostos),
             perc_impostos_uf = soma_impostos_munic_uf/soma_pib_munic_uf) 


#qual é o menor pib e qual o municipio tem o menor pib

menor_pib_mun <- pib_cid %>% 
  filter(pib_total == min(pib_total))

View(menor_pib_mun)
  

View(df)

#selecionando variaveis

#visualização 

library(ggplot2)
pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point()


# gráficos  mais bonitos


pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point() +
  scale_y_continuous(labels = scales::dollar) + theme_light() + theme(text=element_text(size=20)) +
  xlab("impostos municipais") + ggtitle("PIB municipal de 2013 x impostos municipais")
  