# Análises Vozes do Futuro - FAPESP
# Vinicius Tonetti - vrtonetti@ufscar.br

# Carregando pacotes -----------------------------------------------------------
library(tidyverse)

# Limpando ambiente ------------------------------------------------------------
rm(list = ls()) 

# Criando dados ----------------------------------------------------------------

 dados <- "respondentes: 21
 jovens cientistas: 19
 bolsistas de mestrado: 1
 bolsistas de doutorado: 5
 bolsistas de Pós-Doutorado:9
 bolsistas Jovens Pesquisadores: 3
 bolsistas de Jornalismo Científico: 1
 Instituições envolvidas: 6
 participantes INPE: 5
 participantes USP: 8
 participantes UNICAMP: 2
 participantes UNESP: 3
 participantes Cemaden: 1
 representantes da FAPESP: 2
 mulheres (cis e trans): 9
 homens (cis e trans): 12
 pessoas não binárias ou fluídas: 0
 pessoas brancas: 17
 pessoas pardas: 3
 pessoas pretas: 1
 pessoas indígenas: 0"

# Transformando dados em tible
 
df <- tibble(text = str_split(dados, "\n")[[1]]) %>%
   filter(text != "") %>%
   separate(text, into = c("Categoria", "Valor"), sep = ":", extra = "merge") %>%
   mutate(across(everything(), str_trim),
          Valor = as.numeric(Valor))
 
 
 
 
 
 

