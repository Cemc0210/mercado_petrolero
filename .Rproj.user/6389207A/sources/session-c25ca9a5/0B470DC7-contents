#-----------------------------------------------------------------------
# PROYECTO: Dinámica del Mercado Petrolero y Economía de Venezuela
# FASE 2: Importar y Ordenar Datos 
# AUTOR: CEMC
# FECHA: 02-07-2025
#-----------------------------------------------------------------------
#Antes de comenzar limpiare el enviroment

rm(list = ls())  # Limpiar todo


# --- PASO 0: PREPARACIÓN Y CARGA DE LIBRERÍAS ---

# Cargar las librerías que usaremos

library(tidyverse)
library(readxl)

# --- PASO 1: Empezaremos a filtrar los datos ---

#Crearemos una funcion

extraer_indicador_pais <- function(ruta_archivo, nombre_pais,
                                   nombre_indicador = NULL,
                                   anio_inicio = 1960,
                                   hoja = 1) {
  datos <- read_excel(ruta_archivo, sheet = hoja)
  columna_paises <- names(datos)[1]
  
  datos_filtrados <- datos %>%
    filter(.data[[columna_paises]] == nombre_pais)
  
  datos_numericos <- datos_filtrados %>%
    mutate(across(-all_of(columna_paises), ~ as.numeric(.)))
  
  datos_largos <- datos_numericos %>%
    pivot_longer(
      cols = -all_of(columna_paises),
      names_to = "columna_original",
      values_to = "valor"
    ) %>%
    mutate(
      anio = seq(from = anio_inicio, length.out = n())
    ) %>%
    select(anio, valor)
  
  if (!is.null(nombre_indicador)) {
    datos_largos <- datos_largos %>%
      mutate(indicador = nombre_indicador) %>%
      select(indicador, everything())
  }
  
  datos_largos
}

#-----------------------------------------------------------
# Ya identificados los documentos importados en el script 01_importar_datos
  #comenzaremos a filtrar los indicadores solo para Venezuela

# 01 PIB Nominal (millones de USD) 
venezuela_pib_nominal <- extraer_indicador_pais(
  ruta_archivo = "~/cursor/mercado_petrolero/proyecto_mercado_petrolero/datos/1_PIB_NOMINAL_OPEP.xlsx",
  nombre_pais = "Venezuela",
  nombre_indicador = "PIB Nominal (millones de USD)",
  anio_inicio = 1960
)

# 02 PIB Real variacion %
venezuela_pib_real <- extraer_indicador_pais(
  ruta_archivo = "~/cursor/mercado_petrolero/proyecto_mercado_petrolero/datos/2_PIB_REAL_VARIACION_OPEP.xlsx",
  nombre_pais = "Venezuela",
  nombre_indicador = "PIB Real variacion %",
  anio_inicio = 1960
)

# 03 Exportaciones de petroleo (millones de USD)
venezuela_exportaciones_petroleo <- extraer_indicador_pais(
  ruta_archivo = "~/cursor/mercado_petrolero/proyecto_mercado_petrolero/datos/3_EXPORTACIONES_PETROLEO_OPEP.xlsx",
  nombre_pais = "Venezuela",
  nombre_indicador = "Exportaciones de petroleo (millones de USD)",
  anio_inicio = 1960
)

# 04 Saldo Corriente (millones de USD)
venezuela_saldo_corriente <- extraer_indicador_pais(
  ruta_archivo = "~/cursor/mercado_petrolero/proyecto_mercado_petrolero/datos/4_SALDOCORRIENTE_OPEP.xlsx",
  nombre_pais = "Venezuela",
  nombre_indicador = "Saldo Corriente (millones de USD)",
  anio_inicio = 1960
)
  
# 05 Produccion Producto Petroliferos Total y Componentes
# Para este caso, en el que tenemos varias filas correspondientes a los distintos
  #componentes de la Produccion petrolera, no vamos a usar la funcion creada anteriormente


# Leer el archivo original
datos_crudos_productos_petroleros_venezuela <- read_excel(
  "~/cursor/mercado_petrolero/proyecto_mercado_petrolero/datos/5_PRODUCCION_PRODUCTOSPETROLIFEROS_OPEP.xlsx",
  sheet = 1,
  col_names = FALSE
)

# Detectar la fila donde comienza Venezuela
fila_inicio_productos_venezuela <- which(datos_crudos_productos_petroleros_venezuela[[1]] == "Venezuela")

# Extraer las 6 filas: total + 5 productos
bloque_productos_petroleros_venezuela <- datos_crudos_productos_petroleros_venezuela[
  fila_inicio_productos_venezuela:(fila_inicio_productos_venezuela + 5), ]

# Asignar nombres a los componentes
componentes_productos_petroleros <- c(
  "Produccion_Total", "Gasolina", "Queroseno", "Destilados", "Residuos", "Otros"
)

bloque_productos_petroleros_venezuela <- bloque_productos_petroleros_venezuela %>%
  mutate(componente = componentes_productos_petroleros)

# Crear secuencia de años
anios_productos_petroleros <- 1980:2024

# Eliminar la primera columna (etiquetas originales)
datos_numericos_productos_petroleros_venezuela <- bloque_productos_petroleros_venezuela %>%
  select(-1)

# Transformar a formato ancho: filas = años, columnas = componentes
venezuela_productos_petro <- datos_numericos_productos_petroleros_venezuela %>%
  column_to_rownames("componente") %>%
  t() %>%
  as.data.frame() %>%
  mutate(anio = anios_productos_petroleros) %>%
  relocate(anio)

# Convertir columnas a numéricas
venezuela_productos_petro <- venezuela_productos_petro %>%
  mutate(across(-anio, as.numeric))




# 06 Produccion de Petroleo (1.000 B/D)
venezuela_produccion_petroleo <- extraer_indicador_pais(
  ruta_archivo = "~/cursor/mercado_petrolero/proyecto_mercado_petrolero/datos/6_PRODUCCION_PETROLEO_OPEP.xlsx",
  nombre_pais = "Venezuela",
  nombre_indicador = "Produccion de Petroleo (1.000 B/D)",
  anio_inicio = 1960
)



# 07 Exportaciones Petroleo Crudo (1.000 B/D)
venezuela_exportaciones_petroleo_bd <- extraer_indicador_pais(
  ruta_archivo = "~/cursor/mercado_petrolero/proyecto_mercado_petrolero/datos/7_EXPORTACIONES_PETROLEOCRUDO_OPEP.xlsx",
  nombre_pais = "Venezuela",
  nombre_indicador = "Exportaciones Petroleo Crudo (1.000 B/D)",
  anio_inicio = 1980
)


# 08 Precio Cesta Nominal y Real ($/b)
# En este caso haremos igual que en el caso 5, no usaremos la funcion sino que lo haremos manualmente

# Leer el archivo
datos_precio_cesta_opep <- read_excel("~/cursor/mercado_petrolero/proyecto_mercado_petrolero/datos/10_PRECIO_NOMINALYREAL_CESTA_OPEP.xlsx", sheet = 1, col_names = FALSE)

# Paso 1: Detectar las filas que contienen los precios
fila_nominal <- which(datos_precio_cesta_opep[[1]] == "Nominal oil price")
fila_real <- which(datos_precio_cesta_opep[[1]] == "Real oil price")

# Paso 2: Extraer los valores de cada fila (excluyendo la primera columna con el texto)
valores_nominal <- as.numeric(datos_precio_cesta_opep[fila_nominal, -1])
valores_real <- as.numeric(datos_precio_cesta_opep[fila_real, -1])

# Paso 3: Crear la secuencia de años
anios <- 1972:2024

# Paso 4: Crear el data frame en formato ancho
precio_cesta_opep <- tibble(
  anio = anios,
  precio_nominal = valores_nominal,
  precio_real = valores_real
)




#-----------------------------------------------------------------
# Para cumplir con la Fase 2 de las pautas, haremos una exploracion de los datos que filtramos

# 01 PIB Nominal (millones de USD) 
str(venezuela_pib_nominal)
summary(venezuela_pib_nominal)
colSums(is.na(venezuela_pib_nominal))

# 02 PIB Real variacion %
str(venezuela_pib_real)
summary(venezuela_pib_real)
colSums(is.na(venezuela_pib_real))

# 03 Exportaciones de petroleo (millones de USD)
str(venezuela_exportaciones_petroleo)
summary(venezuela_exportaciones_petroleo)
colSums(is.na(venezuela_exportaciones_petroleo))

# 04 Saldo Corriente (millones de USD)
str(venezuela_saldo_corriente)
summary(venezuela_saldo_corriente)
colSums(is.na(venezuela_saldo_corriente))

# 05 Produccion Producto Petroliferos Total y Componentes
str(venezuela_productos_petro)
summary(venezuela_productos_petro)
colSums(is.na(venezuela_productos_petro))

# 06 Produccion de Petroleo (1.000 B/D)
str(venezuela_produccion_petroleo)
summary(venezuela_produccion_petroleo)
colSums(is.na(venezuela_produccion_petroleo))

# 07 Exportaciones Petroleo Crudo (1.000 B/D)
str(venezuela_exportaciones_petroleo_bd)
summary(venezuela_exportaciones_petroleo_bd)
colSums(is.na(venezuela_exportaciones_petroleo_bd))

# 08 Precio Cesta Nominal y Real ($/b)
str(precio_cesta_opep)
summary(precio_cesta_opep)
colSums(is.na(precio_cesta_opep))

