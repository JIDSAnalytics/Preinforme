################################################################################
## JIDS ANALYTICS — Consultoría en Análisis Estadístico de la Sismicidad
## PRE-INFORME DE ASESORÍA 1
## Encargo: Alaska-Aleutianas oriental versus sur de Chile
##          (sismicidad en márgenes de alta latitud)
##
##
## Fuente de datos: catálogo USGS (FDSN), archivos crudos Alaska.csv y Chile.csv.
## base procesada: base_procesada.csv   (en el script es "datos"

## Carpetas de salida ----------------------------------------------------------

dir.create("salidas",        showWarnings = FALSE)
dir.create("salidas/figuras", showWarnings = FALSE)
dir.create("salidas/tablas",  showWarnings = FALSE)

################################################################################


#### 0. PAQUETES ───────────────────────────────────────────────────────────────

library(tidyverse)        # dplyr, ggplot2, tidyr, readr, etc.
library(lubridate)        # manejo de fechas/horas UTC
library(sf)               # objetos espaciales y mapas georreferenciados
library(rnaturalearth)    # mapa base mundial de costas/países
library(scales)           # formato de ejes
library(janitor)          # nombres de columnas limpios (opcional)
library(patchwork)        # combinación de varios gráficos en una sola figura.
library(ineq)             # cálculo de medidas de desigualdad, como el coeficiente de Gini



#### 1. PARÁMETROS DEL ENCARGO (trazabilidad de la descarga) ───────────────────

## Período de referencia y umbral de magnitud.
PERIODO_INI <- as.Date("2000-01-01")
PERIODO_FIN <- as.Date("2025-12-31")
N_ANIOS     <- 26          # años completos del período
N_MESES     <- 312         # 26 * 12 meses
M_MIN       <- 5.0         # umbral de magnitud para comparación internacional

## Cajas de coordenadas asignadas por la contraparte (N, S, O, E) 
zonas_box <- tibble::tribble(
  ~zona,                          ~norte, ~sur,  ~oeste, ~este,
  "A — Alaska-Aleutianas oriental",  62,    51,   -170,   -130,
  "B — Sur de Chile",               -38,   -56,    -77,    -68
)

#### 2. LECTURA DE LAS BASES CRUDAS --------------------------------------------
# Bases originales cuentan ambas con 22 columnas 

ruta_alaska <- "Alaska.csv"   # nombre de archivo en la ruta
ruta_chile  <- "Chile.csv"

raw_alaska <- readr::read_csv(ruta_alaska, show_col_types = FALSE) |>
  mutate(zona = "A — Alaska-Aleutianas oriental")

raw_chile  <- readr::read_csv(ruta_chile,  show_col_types = FALSE) |>
  mutate(zona = "B — Sur de Chile")

## Unión de ambas bases
datos_raw <- bind_rows(raw_alaska, raw_chile)


#### 3. CONTROL DE CALIDAD — diagnóstico sobre la base CRUDA ───────────────────

diagnostico_calidad <- datos_raw |>
  group_by(zona) |>
  summarise(
    registros_crudos      = n(),
    ids_duplicados        = sum(duplicated(id)),
    faltantes_tiempo      = sum(is.na(time)),
    faltantes_magnitud    = sum(is.na(mag)),
    faltantes_profundidad = sum(is.na(depth)),
    faltantes_coordenadas = sum(is.na(latitude) | is.na(longitude)),
    eventos_no_tectonicos = sum(type != "earthquake", na.rm = TRUE),
    fuera_de_periodo      = sum(as.Date(time) < PERIODO_INI |
                                  as.Date(time) > PERIODO_FIN, na.rm = TRUE),
    bajo_umbral_M         = sum(mag < M_MIN, na.rm = TRUE),
    estado_no_reviewed    = sum(status != "reviewed", na.rm = TRUE),
    .groups = "drop"
  )

diagnostico_calidad
readr::write_csv(diagnostico_calidad, "salidas/tablas/diagnostico_calidad.csv")


#### 4. DEPURACIÓN → BASE PROCESADA --------------------------------------------

## Decisiones: se conservan solo eventos tectónicos
## Se documenta el evento de tipo "landslide" detectado en la zona A, que se excluye.

datos <- datos_raw |>
  mutate(
    fecha_hora = ymd_hms(time, tz = "UTC"),
    fecha      = as_date(fecha_hora)
  ) |>
  filter(
    type == "earthquake",                        # excluir no tectónicos (landslide)
    fecha >= PERIODO_INI, fecha <= PERIODO_FIN,  # fecha entre los límites
    mag   >= M_MIN                               # magnitud sobre el umbral  
  ) |>
  # Variables derivadas
  mutate(
    anio = year(fecha_hora),
    mes  = floor_date(fecha_hora, "month"),
    prof_categoria = case_when(           # clasificación operativa 
      depth <  70             ~ "Superficial (0–70)",
      depth >= 70 & depth <= 300 ~ "Intermedio (70–300)",
      depth >  300            ~ "Profundo (>300)",
      TRUE                    ~ NA_character_
    ) |> factor(levels = c("Superficial (0–70)",
                           "Intermedio (70–300)",
                           "Profundo (>300)")),
    fuerte_60 = mag >= 6.0,               # indicadores de eventos fuertes
    fuerte_65 = mag >= 6.5,
    fuerte_70 = mag >= 7.0
  ) |>
  arrange(zona, fecha_hora)  # ordena según zona y luego cronológicamente

## Exportar base procesada (reproducibilidad) 
readr::write_csv(datos, "salidas/base_procesada.csv")


#### 5. TABLAS DE ESTADÍSTICOS DESCRIPTIVOS ------------------------------------

## Frecuencias de categorías de magnitud -----

# table(raw_alaska$magType)
# table(raw_chile$magType)

tabla_magtype <- datos |>
  count(zona, magType, name = "frecuencia") |>
  tidyr::pivot_wider(names_from = zona,
                     values_from = frecuencia,
                     values_fill = 0) |>
  arrange(magType)

print(tabla_magtype)
write_csv(tabla_magtype, "salidas/tablas/tabla_magtype.csv")



## Conteos y tasas de ocurrencia -----
## (tasa anual = n/26; tasa mensual = n/312). 

tabla_conteo_tasas <- datos |>
  group_by(zona) |>
  summarise(
    n_eventos    = n(),
    tasa_anual   = n() / N_ANIOS,
    tasa_mensual = n() / N_MESES,
    .groups = "drop"
  )
print(tabla_conteo_tasas)
write_csv(tabla_conteo_tasas, "salidas/tablas/tabla_conteos_tasas.csv")

## Distribución de magnitud -----

tabla_dist_mag <- datos |>
  group_by(zona) |>
  summarise(
    minimo   = min(mag),
    media    = mean(mag),
    mediana  = median(mag),
    maximo   = max(mag),
    desv_est = sd(mag),
    q25 = quantile(mag, .25),
    q75 = quantile(mag, .75),
    q90 = quantile(mag, .90),
    q95 = quantile(mag, .95),
    .groups = "drop"
  )
print(tabla_dist_mag)
write_csv(tabla_dist_mag, "salidas/tablas/tabla_dist_mag.csv")

## Distribución de profundidad y composición por categoría -----

tabla_dist_depth <- datos |>
  group_by(zona) |>
  summarise(
    min_prof = min(depth), media_prof = mean(depth),
    mediana_prof = median(depth), max_prof = max(depth),
    .groups = "drop"
  )

tabla_depth_cat <- datos |>
  count(zona, prof_categoria) |>
  group_by(zona) |>
  mutate(proporcion = n / sum(n)) |>
  ungroup()

print(tabla_dist_depth); print(tabla_depth_cat)
write_csv(tabla_dist_depth,      "salidas/tablas/tabla_dist_depth.csv")
write_csv(tabla_depth_cat, "salidas/tablas/tabla_depth_cat.csv")

## Eventos fuertes y extremos (n y proporción) -----

tabla_eventos_fuertes <- datos |>
  group_by(zona) |>
  summarise(
    total = n(),
    n_M60 = sum(fuerte_60), p_M60 = mean(fuerte_60),
    n_M65 = sum(fuerte_65), p_M65 = mean(fuerte_65),
    n_M70 = sum(fuerte_70), p_M70 = mean(fuerte_70),
    .groups = "drop"
  )
print(tabla_eventos_fuertes) 
write_csv(tabla_eventos_fuertes, "salidas/tablas/tabla_eventos_fuertes.csv")


## Recurrencia temporal y magnitud máxima anual -----

# Tiempo entre eventos consecutivos (días) dentro de cada zona.
tiempos_entre <- datos |>
  group_by(zona) |>
  mutate(dt_dias = as.numeric(difftime(fecha_hora, lag(fecha_hora),
                                       units = "days"))) |>
  summarise(
    dt_medio   = mean(dt_dias, na.rm = TRUE),
    dt_mediana = median(dt_dias, na.rm = TRUE),
    .groups = "drop"
  )

# Magnitud máxima por año y zona (producto central del encargo).
mag_max_anual <- datos |>
  group_by(zona, anio) |>
  summarise(mag_max = max(mag), .groups = "drop")
print(mag_max_anual) 
write_csv(mag_max_anual,"salidas/tablas/mag_max_anual.csv")

# Tabla resumen
tabla_recurrencia_mag_max <- tiempos_entre |>
  left_join(
    mag_max_anual |> group_by(zona) |>
      summarise(mag_max_anual_media = mean(mag_max),
                mag_max_periodo     = max(mag_max), .groups = "drop"),
    by = "zona"
  )

print(tabla_recurrencia_mag_max)
write_csv(tabla_recurrencia_mag_max,    "salidas/tablas/tabla_recurrencia_mag_max.csv")



#### 6. FIGURAS PRELIMINARES DESCRIPTIVAS  ─────────────────────────────────────

## TEMA GRÁFICO COMÚN ----------------------------------------

tema_jids <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.caption  = element_text(size = 8, color = "grey40"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )
col_zonas <- c("A — Alaska-Aleutianas oriental" = "#1f78b4",
               "B — Sur de Chile"                = "#e31a1c")
## -----------------------------------------------------------


## ST Serie temporal de conteos anuales -----
conteos_anuales <- datos |> count(zona, anio)
fig_conteos_anuales <- ggplot(conteos_anuales, aes(anio, n, color = zona)) +
  geom_line(linewidth = .8) + geom_point(size = 1.6) +
  scale_color_manual(values = col_zonas) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Conteo anual de sismos por zona (M ≥ 5,0)",
       x = "Año", y = "N° de eventos", color = NULL,
       caption = "Fuente: catálogo USGS. Período 2000–2025.") +
  tema_jids
fig_conteos_anuales
ggsave("salidas/figuras/fig_conteos_anuales.png", fig_conteos_anuales,
       width = 9, height = 5, dpi = 300)


## Distribución de magnitud (densidad por zona) -----
fig_dist_mag <- ggplot(datos, aes(mag, fill = zona, color = zona)) +
  geom_density(alpha = .25, linewidth = .7) +
  scale_fill_manual(values = col_zonas) +
  scale_color_manual(values = col_zonas) +
  labs(title = "Distribución de magnitud por zona",
       x = "Magnitud (M)", y = "Densidad", fill = NULL, color = NULL,
       caption = "Fuente: catálogo USGS. Eventos M ≥ 5,0, 2000–2025.") +
  tema_jids
fig_dist_mag
ggsave("salidas/figuras/fig_dist_mag.png", fig_dist_mag,
       width = 9, height = 5, dpi = 300)

## ST Magnitud máxima anual -----
fig_mag_max_anual <- ggplot(mag_max_anual, aes(anio, mag_max, color = zona)) +
  geom_line(linewidth = .8) + geom_point(size = 1.8) +
  scale_color_manual(values = col_zonas) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  labs(title = "Magnitud máxima anual por zona",
       x = "Año", y = "Magnitud máxima (M)", color = NULL,
       caption = "Fuente: catálogo USGS. Un valor por año y zona.") +
  tema_jids
fig_mag_max_anual
ggsave("salidas/figuras/fig_mag_max_anual.png", fig_mag_max_anual,
       width = 9, height = 5, dpi = 300)

## Distribución de profundidad (densidad por zona) ────
fig_dist_depth <- ggplot(datos, aes(depth, fill = zona, color = zona)) +
  geom_density(alpha = .25, linewidth = .7) +
  scale_fill_manual(values = col_zonas) +
  scale_color_manual(values = col_zonas) +
  labs(title = "Distribución de profundidad por zona",
       x = "Profundidad (Km)", y = "Densidad", fill = NULL, color = NULL,
       caption = "Fuente: catálogo USGS. Eventos M ≥ 5,0, 2000–2025.") +
  tema_jids
fig_dist_depth
ggsave("salidas/figuras/fig_dist_depth.png", fig_dist_depth,
       width = 9, height = 5, dpi = 300)



## Gráfico Curva de Lorenz: concentración temporal de la sismicidad ----

## Ajustes previos -----------------------------------------------------

# 1. Conteos mensuales por zona, rellenando con 0 los meses sin sismos 
sismos_ventanas <- datos |>
  count(zona, mes, name = "conteo_sismos") |>
  group_by(zona) |>
  complete(mes = seq(floor_date(PERIODO_INI, "month"),
                     floor_date(PERIODO_FIN, "month"), by = "month"),
           fill = list(conteo_sismos = 0)) |>
  ungroup()

# 2. Función que devuelve coordenadas de Lorenz + Gini
obtener_lorenz <- function(df) {
  conteos <- sort(df$conteo_sismos)
  curva   <- ineq::Lc(conteos)
  data.frame(p_tiempo = curva$p,
             p_sismos = curva$L,
             gini     = ineq::ineq(conteos, type = "Gini"))
}

# 3. Curvas por zona, con el Gini en la etiqueta de la leyenda 
lorenz_alaska <- sismos_ventanas |>
  filter(zona == "A — Alaska-Aleutianas oriental") |>
  obtener_lorenz()
gini_alaska <- unique(lorenz_alaska$gini)
lorenz_alaska <- lorenz_alaska |>
  mutate(etiqueta = paste0("A — Alaska-Aleutianas (Gini = ",
                           round(gini_alaska, 3), ")"),
         zona = "A — Alaska-Aleutianas oriental")

lorenz_chile <- sismos_ventanas |>
  filter(zona == "B — Sur de Chile") |>
  obtener_lorenz()
gini_chile <- unique(lorenz_chile$gini)
lorenz_chile <- lorenz_chile |>
  mutate(etiqueta = paste0("B — Sur de Chile (Gini = ",
                           round(gini_chile, 3), ")"),
         zona = "B — Sur de Chile")

datos_lorenz <- bind_rows(lorenz_alaska, lorenz_chile)

## Mapear cada etiqueta-con-Gini al color de su zona 
etq <- distinct(datos_lorenz, zona, etiqueta)
col_lorenz <- setNames(col_zonas[etq$zona], etq$etiqueta)

## ---------------------------------------------------------------------

## Gráfico -----
fig_lorenz <- ggplot(datos_lorenz, aes(p_tiempo, p_sismos, color = etiqueta)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = "grey40", linewidth = .8) +
  geom_line(linewidth = 1.3) +
  geom_point(data = data.frame(x = c(0, 1), y = c(0, 1)),
             aes(x, y), inherit.aes = FALSE, size = 2.5) +
  scale_color_manual(values = col_lorenz, name = "Zona (índice de Gini)") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
  coord_fixed(ratio = 1) +
  labs(title = "Curva de Lorenz de la concentración sísmica",
       subtitle = "Eventos M ≥ 5,0 agrupados por mes (2000–2025)",
       x = "Proporción acumulada de meses ordenados por actividad",
       y = "Proporción acumulada del total de sismos",
       caption = "Fuente: catálogo USGS. Mayor desviación de la diagonal = más concentración temporal.") +
  tema_jids + theme(legend.position = "bottom")
fig_lorenz
ggsave("salidas/figuras/fig_lorenz.png", fig_lorenz,
       width = 8, height = 8, dpi = 300)


#### 7. MAPAS GEORREFERENCIADOS ────────────────────────────────────────────────

## Código previo para componentes geográficas ----------------------------

## Mapa base mundial (costas/países) en proyección geográfica WGS84.
mundo <- rnaturalearth::ne_countries(scale = "medium",
                                     returnclass = "sf") # objeto tipo simple feature   

## Eventos como objeto espacial, separados por zona. Agrega "geometry" 
datos_sf <- st_as_sf(datos, coords = c("longitude", "latitude"),  
                     crs = 4326, remove = FALSE)   
# crs (coordinate reference system) = 4326. Proyección geográfica WGS84. 
# Sistema geodésico (estandar GPS)

alaska_sf <- filter(datos_sf, zona == "A — Alaska-Aleutianas oriental")
chile_sf  <- filter(datos_sf, zona == "B — Sur de Chile")


## Convertir las cajas de coordenadas a polígonos sf 
caja_a_poligono <- function(norte, sur, oeste, este) {
  st_polygon(list(matrix(c(
    oeste, sur,  este, sur,  este, norte,  oeste, norte,  oeste, sur
  ), ncol = 2, byrow = TRUE)))
}

rect_sf <- zonas_box |>
  rowwise() |>
  mutate(geometry = st_sfc(caja_a_poligono(norte, sur, oeste, este),
                           crs = 4326)) |>
  ungroup() |>
  st_as_sf()


# Límites de placas tectónicas (archivo externo PB2002) 
# Requiere PB2002_boundaries.shp (+ .shx, .dbf, .prj) en el directorio de trabajo.
placas <- st_read("PB2002_boundaries.shp")
st_crs(placas) <- 4326  # Mismo sistema de coordenadas de referencia

# Chile: Sudamericana (SA), Nazca (NZ), Antártica (AN).
# Alaska: Pacífico (PA), Norteamérica (NA).
placas_chile <- placas[placas$PlateA %in% c("SA","NZ","AN") |
                         placas$PlateB %in% c("SA","NZ","AN"), ]
placas_alaska <- placas[placas$PlateA %in% c("PA","NA") |
                          placas$PlateB %in% c("PA","NA"), ]

##-----------------------------------------------------------------------


## Mapa zonas con placas -----
rect_alaska_sf <- filter(rect_sf, zona == "A — Alaska-Aleutianas oriental")
rect_chile_sf  <- filter(rect_sf, zona == "B — Sur de Chile")


mapa_zonas_alaska <- ggplot() +
  geom_sf(data = mundo, fill = "gray95", color = "black") +
  geom_sf(data = placas_alaska, color = "black", linewidth = 1) +
  geom_sf(data = rect_alaska_sf, fill = NA, color = "blue", linewidth = 1.5) +
  annotate("text", x = -150, y = 54, label = "Placa del\nPacífico",
           fontface = "bold", size = 4) +
  annotate("text", x = -155, y = 60, label = "Placa\nNorteamericana",
           fontface = "bold", size = 4) +
  coord_sf(xlim = c(-172, -128), ylim = c(49, 64), expand = FALSE) +
  labs(x = "Longitud (°)", y = "Latitud (°)") +
  theme_bw() + ggtitle("Zona A — Alaska-Aleutianas")

mapa_zonas_chile <- ggplot() +
  geom_sf(data = mundo, fill = "gray95", color = "black") +
  geom_sf(data = placas_chile, color = "black", linewidth = 1) +
  geom_sf(data = rect_chile_sf, fill = NA, color = "red", linewidth = 1.5) +
  annotate("text", x = -78, y = -40, label = "Placa de\nNazca",
           fontface = "bold", size = 4) +
  annotate("text", x = -68, y = -40, label = "Placa\nSudamericana",
           fontface = "bold", size = 4) +
  annotate("text", x = -74, y = -55, label = "Placa\nAntártica",
           fontface = "bold", size = 4) +
  coord_sf(xlim = c(-81, -64), ylim = c(-58, -36), expand = FALSE) +
  labs(x = "Longitud (°)", y = "Latitud (°)") +
  theme_bw() + ggtitle("Zona B — Sur de Chile")

mapa_zonas_placas <- (mapa_zonas_alaska | mapa_zonas_chile) +
  plot_annotation(
    title = "Contexto tectónico y delimitación de las zonas de estudio",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))
ggsave("salidas/figuras/mapa_zonas_placas.png", mapa_zonas_placas,
       width = 13, height = 7, dpi = 300)

## Mapa sismos en Chile con placas -----
mapa_chile <- ggplot() +
  geom_sf(data = mundo, fill = "gray95", color = "black") +
  geom_sf(data = placas_chile, color = "black", linewidth = 1) +
  geom_sf(data = chile_sf, aes(size = mag, color = depth), alpha = 0.7) +
  annotate("text", x = -78, y = -40, label = "Placa de\nNazca",
           size = 4, fontface = "bold") +
  annotate("text", x = -68, y = -40, label = "Placa\nSudamericana",
           size = 4, fontface = "bold") +
  annotate("text", x = -74, y = -55, label = "Placa\nAntártica",
           size = 4, fontface = "bold") +
  coord_sf(xlim = c(-81, -64), ylim = c(-58, -36), expand = FALSE) +
  scale_size_continuous(name = "Magnitud (M)") +
  scale_color_gradient(name = "Profundidad (km)",
                       low = "lightblue", high = "darkblue") +
  labs(x = "Longitud (°)", y = "Latitud (°)") +
  theme_bw() + ggtitle("Zona B — Sur de Chile")


## Mapa sismos en Alaska con placas -----
mapa_alaska <- ggplot() +
  geom_sf(data = mundo, fill = "gray95", color = "black") +
  geom_sf(data = placas_alaska, color = "black", linewidth = 1) +
  geom_sf(data = alaska_sf, aes(size = mag, color = depth), alpha = 0.7) +
  annotate("text", x = -145, y = 54, label = "Placa del\nPacífico",
           size = 4, fontface = "bold") +
  annotate("text", x = -150, y = 64, label = "Placa\nNorteamericana",
           size = 4, fontface = "bold") +
  coord_sf(xlim = c(-172, -128), ylim = c(49, 64), expand = FALSE) +
  scale_size_continuous(name = "Magnitud (M)") +
  scale_color_gradient(name = "Profundidad (km)",
                       low = "lightblue", high = "darkblue") +
  labs(x = "Longitud (°)", y = "Latitud (°)") +
  theme_bw() + ggtitle("Zona A — Alaska-Aleutianas")

# Unión de gráficos
mapa_sismos <- (mapa_alaska | mapa_chile) +
  plot_annotation(
    title = "Distribución espacial de los sismos y principales límites de placas tectónicas",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)))
ggsave("salidas/figuras/mapa_sismos.png", mapa_sismos,
       width = 13, height = 7, dpi = 300)


