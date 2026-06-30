
# Paquetes ----------------------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
library(databcrd)
library(lubridate)

ipc <- get_ipc_data("general") |>
  filter(year > 2019)

data_tc <- get_tc(entidad = "spot") |>
  mutate(
    year = year(fecha),
    mes  = month(fecha)
  ) |>
  relocate(year, mes, .after = fecha)


tc_eoy <- data_tc |>
  filter(fecha == max(fecha), .by = year) |>
  select(year, venta_eoy = venta)

tc_to_plot <- data_tc |>
  left_join(mutate(tc_eoy, year = year + 1)) |>
  mutate(
    venta_vd = (venta - venta_eoy) / venta_eoy
  ) |>
  mutate(
    day = row_number(),
    year = as.character(year),
    .by = year
  )

tc_before_covid <- tc_to_plot |>
  filter(year > 2010, year < 2020)

plot_tc_vd <- tc_to_plot |>
  filter(year > 2022) |>
  ggplot(aes(x = day, y = venta_vd,  color = year, group = year)) +
  geom_line(data = tc_before_covid, mapping = aes(group = year), color = "gray", alpha = 0.5) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype = 4),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = c("#a8dadc","#457b9d","#1d3557", "#e63946")) +
  labs(
    x = "Día laborable del año",
    y = "Variación porcentual",
    # title = "Evolución anual acumulada del tipo de cambio de venta",
    # subtitle = "En gris opaco, las variaciones de los años 2010-2019",
    color = NULL
  )

plot_tc_margen <- tc_to_plot |>
  mutate(margen = venta - compra) |>
  select(fecha, year, mes, compra, venta, margen) |>
  filter(year > 2010) |>
  summarise(
    year_mes = max(round_date(fecha, unit = "month")),
    venta = mean(venta),
    margen_min = min(margen),
    margen_max = max(margen),
    margen = mean(margen),
    .by = c(year, mes)
  ) |>
  mutate(across(contains("margen"), \(x) x / venta)) |>
  ggplot(aes(x = year_mes, y = margen)) +
  geom_ribbon(
    aes(ymin = margen_min, ymax = margen_max, fill = "Volatilidad mensual")
  ) +
  geom_line(color = "#e63946", size = 1) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) +
  scale_y_continuous(label = scales::percent) +
  scale_fill_manual(values = "gray") +
  labs(
    fill = NULL,
    x = NULL,
    y =  NULL
  )

# Graficos word -----------------------------------------------------------------
library(mschart)
library(officer)

tasas_activas <- get_tasas_activas() |>
  filter(year > 2022)

tasas_pasivas <- get_tasas_pasivas() |>
  filter(year > 2022)

tasas <- tasas_activas |>
  select(fecha, ta_pp) |>
  left_join(select(tasas_pasivas, fecha, tp_pp)) |>
  pivot_longer(-fecha, names_to = "type", values_to = "tasa") |>
  mutate(type = ifelse(type == "tp_pp", "Pasiva pp", "Activa pp"))



chart_style <- mschart_theme(
  legend_position = "n",
  grid_major_line = fp_border(color = "transparent", style = "dashed"),
)


ipc_ms_chart <- ipc |>
  ms_linechart("fecha", "ipc_vi") |>
  chart_data_stroke(values = "#C00000") |>
  chart_data_symbol(values = "none" ) |>
  chart_data_smooth(values = 0) |>
  chart_labels(
    xlab = "",
    ylab = ""
  ) |>
  chart_ax_x(
    minor_tick_mark = "none",
    major_tick_mark = "none",
    num_fmt = "[$-en-US]mmm-yyyy;@"
    # num_fmt = "mmm",
    # rotation = -90,
  ) |>
  chart_ax_y(
    num_fmt = "0.00",
    major_tick_mark = "out",
    limit_min = 2
  ) |>
  mschart::set_theme(chart_style)

ipc2_ms_chart <- ipc |>
  ms_linechart("fecha", "ipc_vm") |>
  chart_data_stroke(values = "midnightblue") |>
  chart_data_symbol(values = "none" ) |>
  chart_data_smooth(values = 0) |>
  chart_labels(
    xlab = "",
    ylab = ""
  ) |>
  chart_ax_x(
    minor_tick_mark = "none",
    major_tick_mark = "none",
    num_fmt = "[$-en-US]mmm-yyyy;@",
    tick_label_pos = "low"
  ) |>
  chart_ax_y(
    num_fmt = "0.00",
    major_tick_mark = "out"
  ) |>
  mschart::set_theme(chart_style)


tasas_ms_chart <- tasas |>
  ms_linechart(x = "fecha", y = "tasa", group = "type") |>
  chart_data_symbol(values = "none" ) |>
  chart_data_smooth(values = 0) |>
  chart_labels(
    xlab = "",
    ylab = ""
  ) |>
  chart_ax_x(
    minor_tick_mark = "none",
    major_tick_mark = "none",
    num_fmt = "[$-en-US]mmm-yyyy;@",
    tick_label_pos = "low"
  ) |>
  chart_ax_y(
    num_fmt = "0.00",
    major_tick_mark = "out"
  ) |>
  mschart::set_theme(chart_style)
