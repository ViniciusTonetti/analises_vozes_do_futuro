# Análises Vozes do Futuro - FAPESP
# Vinicius Tonetti - vrtonetti@ufscar.br

# Carregando pacotes -----------------------------------------------------------


library(ggwordcloud)
library(ggplot2)
library(lemon)
library(tibble)
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(ggalluvial)
library(ggrepel)
library(forcats)


# Limpando ambiente e output folder --------------------------------------------

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
   "Unesp", 3, "Instituições",
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
#ggsave(paste0("",output_folder,"prop_grupos.jpeg"), prop_grupos, width = 20, height = 15, units = "cm", dpi = 300)

# Nuvem de palavras ------------------------------------------------------------

# COP 30 

words_COP30 <- tribble(
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

(
  wc <- ggplot(words_COP30, aes(label = Palavra, size = Frequencia, color = Frequencia)) +
  geom_text_wordcloud(area_corr = TRUE, family = "sans") +
  scale_size_area(max_size = 20) +
  scale_color_viridis_c(option = "turbo") +
  theme_minimal() +
  theme(legend.position = "none")
  )

# Salvando o plot
#ggsave(paste0("",output_folder,"word_cloud COP 30.jpeg"), wc, width = 20, height = 15, units = "cm", dpi = 300)


# Vozes do Futuro

words_vozes <- tribble(
  ~Palavra, ~Frequencia,
  "Adaptação", 16,
  "Mitigação", 10,
  "Transição Energética", 8,
  "Fonte", 0,
  "Emissões", 5,
  "Biodiversidade", 31,
  "Urbano", 10,
  "Recursos Financeiros", 22,
  "Justiça / Injustiça", 16,
  "Desigualdade", 6,
  "Ciência", 27,
  "Conhecimento", 19,
  "População Vulnerável", 11,
  "NDCs ou 1,5ºC", 9
)

(
  wc <- ggplot(words_vozes, aes(label = Palavra, size = Frequencia, color = Frequencia)) +
    geom_text_wordcloud(area_corr = TRUE, family = "sans") +
    scale_size_area(max_size = 20) +
    scale_color_viridis_c(option = "turbo") +
    theme_minimal() +
    theme(legend.position = "none")
)

# Salvando o plot
#ggsave(paste0("",output_folder,"word_cloud_vozes.jpeg"), wc, width = 20, height = 15, units = "cm", dpi = 300)



## Aluvial plot ----------------------------------------------------------------

dados_participacao <- readxl::read_xls("E:/GitHub_Vinicius/analises_vozes_do_futuro/300(inscritos)_lista final sem nome.xls")
dados_participacao <- data.frame(dados_participacao)
colnames(dados_participacao)

dados_participacao <- dados_participacao %>% 
  select(Tipo.de.Instituição.Categoria, Tipo.de.cargo, Tipo.de.partipação)

dados_participacao <- dados_participacao |>
  dplyr::count(Tipo.de.Instituição.Categoria,
               Tipo.de.cargo,
               Tipo.de.partipação,
               name = "Freq")

colnames(dados_participacao)


# Checando se é compatível com o pacote

ggalluvial::is_alluvia_form(dados_participacao, axes = 1:4, silent = TRUE)
is_alluvia_form(dados_participacao, axes = 1:4)

# plot

(plot_participacao <- ggplot(dados_participacao,
       aes(y = Freq,
           axis1 = Tipo.de.Instituição.Categoria,
           axis2 = Tipo.de.cargo,
           axis3 = Tipo.de.partipação)) +
  scale_x_discrete(labels = c("Tipo de Instituição", "Cargo", "Tipo de participação"),
                   expand = c(.05, .05)) +
  geom_alluvium(aes(fill = Tipo.de.Instituição.Categoria), width = 1/20) +
  geom_stratum(width = 1/20, fill = "gray80", color = "gray40") +
    scale_fill_manual(values = c(
      "#66A61E",
      "#E6AB02",
      "#666666",
      "red",
      "#8DA0CB" 
    ))+
  geom_text(stat = "stratum",
           aes(label = after_stat(stratum)),
            #nudge_x = -0.5,
        size = 3) +
  
  theme_minimal() +
  labs(title = "",
       x = "", y = "Frequência",
       fill = ""))

# Salvando o plot
#ggsave(paste0("",output_folder,"relacao_participantes.jpeg"), plot_participacao, width = 20, height = 15, units = "cm", dpi = 300)


# Proporção Instituições -------------------------------------------------------

dados_instituicoes <- readxl::read_xls("E:/GitHub_Vinicius/analises_vozes_do_futuro/300(inscritos)_lista final sem nome.xls")
dados_instituicoes <- data.frame(dados_instituicoes)
colnames(dados_instituicoes)
head(dados_instituicoes)


dados_instituicoes <- dados_instituicoes %>% 
  select(Tipo.de.Instituição.Categoria, Instituição.padronizada.Vinicius)


# calcular contagens

dados_counts <- dados_instituicoes %>%
  count(Tipo.de.Instituição.Categoria, name = "n") %>%
  arrange(n)

# transformar em fator com níveis nessa ordem (menor -> maior),
# assim o maior ficará no topo do eixo y (posições mais altas)

dados_counts$Tipo.de.Instituição.Categoria <- factor(
  dados_counts$Tipo.de.Instituição.Categoria,
  levels = dados_counts$Tipo.de.Instituição.Categoria
)

# plot
(hist_inst <- ggplot(dados_counts, aes(y = Tipo.de.Instituição.Categoria, x = n)) +
    geom_col(fill = "lightgray", width = 0.6, show.legend = FALSE) +
    labs(
      x = "Número de pessoas",
      y = ""
    ) +
    theme_minimal(base_size = 13) +
    theme(
      strip.text = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.title.x = element_text(face = "bold")
    ))

# Salvando o plot
#ggsave(paste0("",output_folder,"hist_instituicoes.jpeg"), hist_inst, width = 20, height = 15, units = "cm", dpi = 300)



# Gráfico de Pizza -------------------------------------------------------------

# Preoarando os dados

dados_instituicoes <- readxl::read_xls("E:/GitHub_Vinicius/analises_vozes_do_futuro/300(inscritos)_lista final sem nome.xls")
dados_instituicoes <- data.frame(dados_instituicoes)
colnames(dados_instituicoes)
head(dados_instituicoes)


dados_instituicoes <- dados_instituicoes %>% 
  select(Tipo.de.Instituição.Categoria, Instituição.padronizada.Vinicius) %>% 
  filter(Tipo.de.Instituição.Categoria == "Universidade") %>% 
  select(Instituição.padronizada.Vinicius)

dados_pizza <- as.data.frame(table(dados_instituicoes)) %>%
  mutate(perc = Freq / sum(Freq) * 100) %>% 
  arrange(desc(perc)) %>% 
  slice_head(n = 10)

# Plotando o gráfico de pizza --------------------------------------------------


# Criar uma coluna com rótulo da legenda: "Universidade (XX%)"

dados_pizza$legend_label <- paste0(
  dados_pizza$Instituição.padronizada.Vinicius, " (", round(dados_pizza$perc, 1), "%)"
)


# Ordenar o fator para que os maiores valores fiquem no topo

dados_pizza$legend_label <- fct_reorder(
  dados_pizza$legend_label, dados_pizza$perc, .desc = TRUE
)


# Gráfico

(grafico_pizza <- ggplot(dados_pizza, aes(x = 2, y = perc, fill = legend_label)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = c(
    "#A8D5BA",
    "#FFD1DC",
    "#FFE4B5",
    "#B0C4DE",
    "#E6E6FA",
    "#F5DEB3",
    "#FFB347",
    "#C1E1C1",
    "#D8BFD8",
    "#F7CAC9" 
  )) +
  labs(fill = "") +
  theme_void(base_size = 13) +
  theme(
    legend.text = element_text(size = 13),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
)


# Salvando o plot
#ggsave(paste0("",output_folder,"donut_universidaes.jpeg"), grafico_pizza, width = 20, height = 15, units = "cm", dpi = 300)







