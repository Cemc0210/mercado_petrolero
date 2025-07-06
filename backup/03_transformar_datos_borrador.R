#-----------------------------------------------------------------------
# PROYECTO: Dinámica del Mercado Petrolero y Economía de Venezuela
# FASE 3: Transformar los datos
# AUTOR: CEMC
# FECHA: 02-07-2025
#-----------------------------------------------------------------------
# Cargamos la libreria 
library(dplyr)

# Ya con los datos limpios de Venezuela procederemos a crear algunas variables

# 01 "Índice de Exposición a la Renta Petrolera"

# Paso 1: Filtrar y renombrar las columnas
pib <- venezuela_pib_nominal %>%
  filter(anio >= 1980) %>%
  select(anio, pib_nominal = valor)

exportaciones <- venezuela_exportaciones_petroleo %>%
  filter(anio >= 1980) %>%
  select(anio, exportaciones_petroleo = valor)

# Paso 2: Unir por año
indice_exposicion_petrolera <- left_join(pib, exportaciones, by = "anio")

# Paso 3: Calcular el índice
indice_exposicion_petrolera <- indice_exposicion_petrolera %>%
  mutate(indice_exposicion = pib_nominal / exportaciones_petroleo)


# 02 Índice de Eficiencia de la Renta Petrolera"

# Paso 1: Filtrar y renombrar PIB
pib <- venezuela_pib_nominal %>%
  filter(anio >= 1980) %>%
  select(anio, pib_nominal = valor)

# Paso 2: Filtrar producción total y convertir a barriles anuales
produccion <- venezuela_produccion_petroleo %>%
  filter(indicador == "Produccion de Petroleo (1.000 B/D)", anio >= 1980) %>%
  mutate(barriles_anuales = valor * 365) %>%  # Convertir de miles de barriles diarios a anuales
  select(anio, produccion_total_barriles = barriles_anuales)

# Paso 3: Unir y calcular el índice
indice_eficiencia_petrolera <- left_join(produccion, pib, by = "anio") %>%
  mutate(indice_eficiencia = produccion_total_barriles / pib_nominal)
