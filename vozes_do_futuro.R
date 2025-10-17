# Análises Vozes do Futuro - FAPESP
# Vinicius Tonetti - vrtonetti@ufscar.br

# Carregando pacotes -----------------------------------------------------------

library(ggwordcloud)
library(ggplot2)
library(tibble)
library(tidyverse)

# Limpando ambiente ------------------------------------------------------------
rm(list = ls())

output_folder <- "E:/GitHub_Vinicius/analises_vozes_do_futuro/"

# Criando dados ----------------------------------------------------------------

df <- tribble(
   ~Categoria, ~Valor, ~Grupo,
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

(prop_grupos <- ggplot(df, aes(x = reorder(Categoria, Valor), y = Valor)) +
  geom_col(fill = "lightgray", show.legend = FALSE, width = 0.5) +
  facet_wrap(~Grupo, scales = "free_y", ncol = 2) +
  coord_flip() +
  labs(x = "", y = "Número de pessoas") +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  ))


# Salvando o plot
ggsave(paste0("",output_folder,"prop_grupos.jpeg"), prop_grupos, width = 20, height = 15, units = "cm", dpi = 300)

# Nuvem de palavras ------------------------------------------------------------

words <- tribble(
~Palavra, ~Frequencia,
"Adaptação", 12,
"Mitigação", 7,
"Transição Energética", 23,
"Fonte", 6,
"Emissões", 15,
"Biodiversidade", 25,
"Urbano", 12,
"Recursos Financeiros", 14,
"Justiça / Injustiça", 16,
"Desigualdade", 2,
"Ciência", 7,
"Conhecimento", 9,
"População Vulnerável", 4,
"NDCs ou 1,5ºC", 25
)

ggplot(words, aes(label = Palavra, size = Frequencia, color = Frequencia)) +
  geom_text_wordcloud(area_corr = TRUE, family = "sans") +
  scale_size_area(max_size = 20) +
  scale_color_gradient(low = "darkgreen", high = "lightgreen") +
  theme_minimal() +
  theme(legend.position = "none")


