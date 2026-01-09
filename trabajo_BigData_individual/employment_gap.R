library(tidyverse)
library(sf)
library(magrittr)
library(dplyr)
library(dplyr)
library(tidyr)
library(stringr)

#tabla datos de union europea
ruta_union_europea <- "./dataset/employment_gap.csv"
df2 <- rio::import(ruta_union_europea )

# df limpiado
df_work <- df2 %>%
  mutate(
    geo  = as.character(geo),
    year = as.integer(TIME_PERIOD),
    value = as.numeric(OBS_VALUE)
  ) %>%
  select(geo, year, value) %>%
  filter(!is.na(year), !is.na(value))

# variaciones a largo plazo
library(dplyr)
library(DT)

summary_by_geo <- df_work %>%
  group_by(geo) %>%
  summarise(
    year_min = min(year),
    year_max = max(year),
    mean_value = mean(value),
    sd_value   = sd(value),
    min_value  = min(value),
    max_value  = max(value),
    value_first = value[which.min(year)],
    value_last  = value[which.max(year)],
    delta_total = value_last - value_first,
    .groups = "drop"
  ) %>%
  arrange(desc(value_last))

DT::datatable(
  summary_by_geo,
  caption = "Resumen por país: evolución y variación de largo plazo del indicador",
  options = list(
    pageLength = 10,
    autoWidth = TRUE
  )
)
#primer grafico: trend eu
eu_trend <- df_work %>%
  filter(!geo %in% c("EU27_2020","EA20")) %>%
  group_by(year) %>%
  summarise(
    mean_value = mean(value),
    .groups = "drop"
  )

ggplot(eu_trend, aes(x = year, y = mean_value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "tendencia media del indicador en los países europeos",
    x = "Ano",
    y = "valor promedio"
  ) +
  theme_minimal()

#Segundo grafico: nivel actual vs. cambio a lo largo del tiempo
summary_plot <- summary_by_geo %>%
  filter(!geo %in% c("EU27_2020","EA20"))

ggplot(summary_plot, aes(x = delta_total, y = value_last, label = geo)) +
  geom_hline(yintercept = median(summary_plot$value_last), linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 15) +
  labs(
    title = "Nivel actual vs. cambio a largo plazo",
    x = "Delta total (ultimo - primo)",
    y = "Valor ultimo ano disponible"
  ) +
  theme_minimal()

# Distribución durante el último año (gráfico de barras)

last_year <- max(df_work$year)

df_last <- df_work %>%
  filter(year == last_year) %>%
  filter(!geo %in% c("EU27_2020","EA20")) %>%
  arrange(desc(value))

ggplot(df_last, aes(x = reorder(geo, value), y = value)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste(" Distribución durante el último año disponible (", last_year, ")", sep = ""),
    x = "Pais",
    y = "valor"
  ) +
  theme_minimal()
