library("eurostat")
library(tidyverse)
library(sf)
library(magrittr)
library(dplyr)
library(dplyr)
library(tidyr)
library(stringr)

#tabla eurostat : importacion de los datos 
aa <- search_eurostat("all")

# select some data from Eurostat
my_table <- "earn_gr_goeg"

label_eurostat_tables(my_table)     #- gives information about the table

#dowloading the selected data with get_eurostat()
df <- get_eurostat(my_table, time_format = 'raw', keepFlags = TRUE)       #- downloads the table from Eurostat API

#- obtenemos los descriptores de los códigos de las variables
df_names <- names(df)
df <- label_eurostat(df, code = df_names, fix_duplicated = TRUE)
rm(aa,  df_names, my_table)

#clean df con: geo, year, value.
df_work <- df %>%
  mutate(
    year  = as.integer(TIME_PERIOD),
    value = as.numeric(values)
  ) %>%
  select(geo_code, geo, year, value)

# pais con el max gap, el min y la media:

df_2018 <- df_work %>% filter(year == 2018)

gap_summary_ue <- df_2018  %>%
  summarise(
  max_gap   = max(value, na.rm = TRUE),
  min_gap   = min(value, na.rm = TRUE),
  mean_gap  = mean(value, na.rm = TRUE)
)

gap_summary_ue

# primer grafico: gap paises 2018 
library(ggplot2)
library(tidyverse)
library(ggplot2)


g1  <- ggplot(df_2018, aes(
  x = reorder(geo, value),
  y = value
)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Brecha salarial en los piases UE (2018)",
    x = "Pais",
    y = "Gap (%)"
  ) +
  theme_minimal()

print(g1)

# grafico 2: media ue en 2018
mean_ue_2018 <- df_2018 %>%
  summarise(mean_gap = mean(value, na.rm = TRUE)) %>%
  pull(mean_gap)

g2 <- ggplot(df_2018, aes(
  x = reorder(geo, value),
  y = value,
  fill = geo_code %in% c("IT", "ES")
)) +
  geom_col() +
  geom_hline(
    yintercept = mean_ue_2018,
    linetype = "dashed",
    linewidth = 1
  ) +
  coord_flip() +
  labs(
    title = "Brecha salarial en los piases UE UE (2018)",
    subtitle = "Línea discontinua = media de la UE",
    x = "Paese",
    y = "Gap (%)"
  ) +
  theme_minimal() +
  guides(fill = "none")

print(g2)


#ida italia vs spain
df_it_es <- df_work %>%
  filter(geo_code %in% c("IT", "ES"))

# grafico temporale
g_it_es <- ggplot(df_it_es, aes(
  x = year,
  y = value,
  color = geo
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Ida gender pay gap: Italia e Espana",
    subtitle = "valores nel tiempo",
    x = "Ano",
    y = "Gap (%)",
    color = "Pais"
  ) +
  theme_minimal()

print(g_it_es)


