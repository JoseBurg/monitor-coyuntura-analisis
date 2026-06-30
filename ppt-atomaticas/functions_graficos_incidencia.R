library(dplyr)
library(tidyverse)


ipc_articulo <- readxl::read_excel("./empalme_ipc.xlsx", sheet = "index") |> 
  tidyr::pivot_longer(
    cols = -c(articulo:clase),
    names_to = "fecha",
    values_to = "index"
  ) |> 
  mutate(
    fecha = as.Date(as.numeric(fecha), origin = "1899-12-30")
  )




ipc_articulo %>%
  group_by(fecha) %>%
  summarize(sum_pond = sum(pond, na.rm = TRUE)) %>%
  arrange(fecha) %>%
  slice_head(n = 6) -> pond_check

print(pond_check)
# Observa si sum_pond está cercano a 100 (pond en %) o cercano a 1 (pond en fracción)

# --- 2) Crear pond_frac (ponderación como fracción 0-1) por fecha ---
df2 <- ipc_articulo %>%
  group_by(fecha) %>%
  mutate(sum_pond = sum(pond, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    pond_frac = if_else(sum_pond > 2, pond / 100, pond) # si suma >2 → asumimos porcentaje
  ) %>%
  select(-sum_pond)

# --- 3) Calcular variación temporal del índice por artículo ---
# Aquí usamos variación simple R_t / R_{t-1} - 1 (proporción)
df2 <- df2 %>%
  arrange(articulo, fecha) %>%
  group_by(articulo) %>%
  mutate(
    lag_index = lag(index),
    var_prop = (index / lag_index) - 1    # variación como proporción (ej. 0.04 = 4%)
  ) %>%
  ungroup()

# --- 4) Incidencia por artículo (proporción y p.p.) ---
# incidence_prop = pond_frac * var_prop  (ej. 0.01 === 1 punto porcentual)
df2 <- df2 %>%
  mutate(
    incidence_prop = pond_frac * var_prop,      # en proporción (ej. 0.01)
    incidence_pp   = incidence_prop * 100       # en puntos porcentuales (ej. 1.0 p.p.)
  )

# --- 5) Agregados por grupo y fecha ---
group_incidence <- df2 %>%
  group_by(grupo, fecha) %>%
  summarize(
    incidence_pp = sum(incidence_pp, na.rm = TRUE),     # p.p. aporte del grupo
    incidence_prop = sum(incidence_prop, na.rm = TRUE)  # aporte en proporción
  ) %>%
  ungroup()

# --- 6) Comprobar suma total por fecha (debe igualar variación del IPC calculada por ponderación) ---
total_by_date <- df2 %>%
  group_by(fecha) %>%
  summarize(
    total_incidence_pp = sum(incidence_pp, na.rm = TRUE),
    total_incidence_prop = sum(incidence_prop, na.rm = TRUE)
  ) %>%
  ungroup()

# --- 7) (Opcional) Calcular IPC implícito vía ponderado y comparar ---
ipc_weighted <- df2 %>%
  group_by(fecha) %>%
  summarize(weighted_index = sum(pond_frac * index, na.rm = TRUE)) %>%
  arrange(fecha) %>%
  mutate(
    lag_weighted_index = lag(weighted_index),
    ipc_var_prop = (weighted_index / lag_weighted_index) - 1,
    ipc_var_pp = ipc_var_prop * 100
  ) %>%
  ungroup()

# Comparar ipc_var_pp con total_incidence_pp
compare <- left_join(total_by_date, ipc_weighted %>% select(fecha, ipc_var_pp), by = "fecha")

# --- 8) Tablas útiles: top contribuyentes por fecha (ej. top 10) ---
top_contributors <- df2 %>%
  filter(!is.na(incidence_pp)) %>%
  group_by(fecha) %>%
  arrange(fecha, desc(abs(incidence_pp))) %>%  # orden por magnitud del aporte
  slice_head(n = 10) %>%
  select(fecha, articulo, grupo, pond_frac, var_prop, incidence_pp)

# --- 9) Mostrar resultados de diagnóstico/resumen ---
list(
  pond_check = pond_check,
  head_df2 = df2 %>% filter(!is.na(var_prop)) %>% arrange(fecha) %>% slice_head(n = 8),
  group_incidence = head(group_incidence, 10),
  total_by_date = head(total_by_date, 10),
  ipc_weighted = head(ipc_weighted, 10),
  compare = head(compare, 10),
  top_contributors = top_contributors
)  



ggplot(ipc_weighted, aes(x = fecha, y = ipc_var_pp)) +
  geom_line(size = 1) +
  labs(
    title = "Variación mensual del IPC",
    x = "",
    y = "Puntos porcentuales"
  ) +
  theme_minimal()  


ggplot(group_incidence, aes(x = fecha, y = incidence_pp)) +
  geom_line() +
  facet_wrap(~ grupo, scales = "free_y") +
  labs(
    title = "Incidencia mensual por grupo",
    x = "",
    y = "Puntos porcentuales"
  ) +
  theme_minimal()


ggplot(group_incidence, aes(x = fecha, y = incidence_pp, fill = grupo)) +
  geom_col() +
  labs(
    title = "Contribución de los grupos al IPC",
    x = "",
    y = "Puntos porcentuales"
  ) +
  theme_minimal()


ggplot() +
  geom_bar(data = group_incidence,
            aes(x = fecha, y = incidence_pp, fill = grupo)) +
  geom_line(data = ipc_weighted,
            aes(x = fecha, y = ipc_var_pp),
            color = "black", size = 1) +
  labs(
    title = "Contribución al IPC y variación total",
    x = "",
    y = "Puntos porcentuales"
  ) +
  theme_minimal()
