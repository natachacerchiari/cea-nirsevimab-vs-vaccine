library(ggplot2)
library(dplyr)

# Dados fornecidos
dados_icer <- data.frame(
  Variavel = c("Nirsevimabe effectiveness (upper CI – lower CI)",
               "Dose price (0.75x and 0.25x)",
               "Nirsevimab coverage (50% and 95%)",
               "Incidence RSV-GB-LRTI-2019 (upper CI – lower CI)",
               "Inpatient cost +25%",
               "Outpatient cost +25%",
               "Palivizumab effect excluded"),
  Impacto_Min = c(220425.33, 27791.32, 227700.40, 119368.91, 234911.47, 239481.13, 239569.67),  # ICER mínimo
  Impacto_Max = c(266486.81, 168976.89, 574439.31, 490265.97, 244227.86, 239658.20, 241281.73) # ICER máximo
)

# Valor de centro (ICER base)
centro <- 239569.67

# Ajustar os dados em relação ao centro
dados_ajustados <- dados_icer %>%
  mutate(
    Min_Ajustado = Impacto_Min - centro,  # Desvio abaixo do centro
    Max_Ajustado = Impacto_Max - centro,  # Desvio acima do centro
    # CALCULAR A AMPLITUDE (impacto total)
    Amplitude = abs(Max_Ajustado - Min_Ajustado)
  ) %>%
  # ORDENAR POR AMPLITUDE DECRESCENTE (maior impacto primeiro)
  arrange(Amplitude) %>%
  # MANTER A ORDEM NO GRÁFICO
  mutate(Variavel = factor(Variavel, levels = Variavel))

# Verificar a ordem
print(dados_ajustados[, c("Variavel", "Amplitude")])

# Criar o diagrama de tornado
ggplot(dados_ajustados) +
  # Barra do valor MÍNIMO (vermelha)
  geom_segment(aes(x = Variavel, xend = Variavel, 
                   y = 0, yend = Min_Ajustado),
               color = "darkred", linewidth = 10, alpha = 1) +
  
  # Barra do valor MÁXIMO (azul)
  geom_segment(aes(x = Variavel, xend = Variavel, 
                   y = 0, yend = Max_Ajustado),
               color = "darkblue", linewidth = 10, alpha = 1) +
  
  # Linha de centro (ICER base)
  geom_hline(yintercept = 0, color = "black", 
             linetype = "solid", linewidth = 0.1) +
  
  # Coordenadas e temas
  coord_flip() +
  theme_minimal() +
  
  # Rótulos e título
  labs(
    title = "ICER (Healthcare System Perspective)",
    x = "",
    y = "",
    ) +
  
  # Formatação do eixo Y para mostrar valores em USD
  scale_y_continuous(labels = function(x) {
    paste0("$", format(x + centro, big.mark = ".", decimal.mark = ",", nsmall = 2))
  }) +
  
  # Melhorar a aparência
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray50"),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 10),
    legend.position = "none"
  )