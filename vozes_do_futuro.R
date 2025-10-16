# Análises Vozes do Futuro - FAPESP
# Vinicius Tonetti - vrtonetti@ufscar.br

# Carregando pacotes -----------------------------------------------------------
library(tidyverse)

# Limpando ambiente ------------------------------------------------------------
rm(list = ls()) 

# Criando dados ----------------------------------------------------------------

df <- tribble(
   ~Categoria, ~Valor, ~Grupo,
   "jovens cientistas", 19, "Jovens cientistas",
   "Mestrado", 1, "Bolsistas",
   "Doutorado", 5, "Bolsistas",
   "Pós-Doutorado", 9, "Bolsistas",
   "Jovens Pesquisadores", 3, "Bolsistas",
   "Jornalismo Científico", 1, "Bolsistas",
   "INPE", 5, "Instituições",
   "USP", 8, "Instituições",
   "UNICAMP", 2, "Instituições",
   "UNESP", 3, "Instituições",
   "Cemaden", 1, "Instituições",
   "FAPESP", 2, "Instituições",
   "Mulheres (cis e trans)", 9, "Gênero",
   "Homens (cis e trans)", 12, "Gênero",
   "Pessoas não binárias ou fluídas", 0, "Gênero",
   "Pessoas brancas", 17, "Autodeclaração racial",
   "Pessoas pardas", 3, "Autodeclaração racial",
   "Pessoas pretas", 1, "Autodeclaração racial",
   "Pessoas indígenas", 0, "Autodeclaração racial"
 )
 
# Plots ------------------------------------------------------------------------

ggplot(df, aes(x = reorder(Categoria, Valor), y = Valor, fill = Grupo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Grupo, scales = "free_y", ncol = 2) +
  coord_flip() +
  labs(x = "", y = "Número de pessoas") +
  theme_minimal(base_size = 13)

