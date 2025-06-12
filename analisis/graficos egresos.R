# Instala los paquetes si no los tienes
# install.packages(c("tidyverse", "readxl"))

library(readxl)
library(dplyr)
library(ggplot2)
library(grid)  # para unit()

# Definición del tema personalizado
tema_personalizado <- function(){
  theme_minimal() + 
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 8, face = "italic", hjust = 0.5),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7), 
      legend.title = element_text(size = 7, face = "italic"),
      legend.text = element_text(size = 5),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_line(size = 0.3, color = "black"), 
      legend.background = element_rect(fill = "white"), 
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.y = unit(0.01, "cm"),
      legend.position = "right",
      plot.margin = margin(20, 20, 20, 20)
    )
}

# 1. Carga de datos
df <- egresos_c_np

# 2. Preparar datos para el gráfico de egresos totales
df_total <- df %>%
  group_by(desc_esp, anio) %>%
  summarise(total_egresos = sum(total_egresos, na.rm = TRUE),
            total_ar = sum(atencion_realizada,na.rm = TRUE), 
            .groups = "drop")

# 3. Gráfico de barras: egresos por especialidad y año
eg <- ggplot(df_total, aes(x = desc_esp, y = total_egresos, fill = factor(anio))) +
  geom_col(position = "dodge") +
  labs(
    x = "Especialidad",
    y = "Total de egresos",
    fill = "Año",
    title = "Egresos por especialidad y año"
  ) +
  scale_fill_brewer(palette = "Purples") +
  tema_personalizado() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = paste0('figs/', "grafico_eg_especialidad_", 'HEGC', ".png"),
  plot = eg,
  dpi = 300,
  width = 12,
  height = 6,
  units = "in"
)

# 2. Pivot a formato largo
df_long <- df_total %>%
  pivot_longer(
    cols = c(total_egresos, total_ar),
    names_to  = "tipo",
    values_to = "valor"
  )


ggplot(df_long, aes(x = desc_esp, y = valor, fill = factor(anio))) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.9) +
  facet_wrap(~ tipo, scales = "free_y", ncol = 1,
             labeller = as_labeller(c(
               total_egresos = "Total Egresos",
               total_ar      = "Total AR"
             ))) +
  labs(
    x = "Especialidad",
    y = "Cantidad",
    fill = "Año",
    title = "Comparación de Egresos y AR por Especialidad y Año"
  ) +
  scale_fill_brewer(palette = "Purples") +
  tema_personalizado() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8, face = "bold")
  )



# 4. Preparar datos para el gráfico de porcentaje NP
df_pct <- df %>%
  group_by(desc_esp, anio) %>%
  summarise(pct_np = mean(porc_np_total, na.rm = TRUE), .groups = "drop")

# 5. Gráfico de barras: porcentaje NP del total de egresos
np <- ggplot(df_pct, aes(x = desc_esp, y = pct_np, fill = factor(anio))) +
  geom_col(position = "dodge") +
  labs(
    x = "Especialidad",
    y = "Porcentaje NP",
    fill = "Año",
    title = "Porcentaje NP de egresos por especialidad y año"
  ) +
  scale_fill_brewer(palette = "Purples") +
  tema_personalizado() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = paste0('figs/', "grafico_np_especialidad_", 'HEGC', ".png"),
  plot = np,
  dpi = 300,
  width = 12,
  height = 6,
  units = "in"
)


df_top5 <- diagnosticos_np %>%
  group_by(desc_esp) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ungroup()

# 3. Crear factor reordenado por porcentaje dentro de cada especialidad
df_top5 <- df_top5 %>%
  group_by(desc_esp) %>%
  mutate(
    diagnostico = fct_reorder(sospecha_diag, n)
  ) %>%
  ungroup()

# 4. Plot
diag <- ggplot(df_top5, aes(x = diagnostico, y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ desc_esp, scales = "free_y") +
  scale_fill_gradient(low = "lightgrey", high = "steelblue") +
  labs(
    x = NULL,
    y = "% NP",
    title = "Top 10 diagnósticos NP por especialidad (2024)",
    subtitle = "Porcentaje de no pertinencia sobre total de egresos"
  ) +
  tema_personalizado() 

ggsave(
  filename = paste0('figs/', "grafico_diag_np_especialidad_", 'HEGC', ".png"),
  plot = diag,
  dpi = 300,
  width = 16,
  height = 6,
  units = "in"
)

