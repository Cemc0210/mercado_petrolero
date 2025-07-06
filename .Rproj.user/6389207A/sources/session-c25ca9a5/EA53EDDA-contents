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
library(tidyverse) # Paquete principal para la manipulación de datos
library(readxl)    # Para leer archivos de Excel (.xlsx)


# --- PASO 1: IMPORTAR LOS 10 ARCHIVOS DE EXCEL DESDE LA CARPETA 'datos' ---


# 1. PIB Nominal
pib_nominal_opep <- read_excel("datos/1_PIB_NOMINAL_OPEP.xlsx")

# 2. Crecimiento PIB Real
pib_real_variacion_opep <- read_excel("datos/2_PIB_REAL_VARIACION_OPEP.xlsx")

# 3. Exportaciones de Petróleo (Valor)
export_valor_cruda <- read_excel("datos/3_EXPORTACIONES_PETROLEO_OPEP.xlsx")

# 4. Saldo Cuenta Corriente
saldo_cc_cruda <- read_excel("datos/4_SALDOCORRIENTE_OPEP.xlsx")

# 5. Producción de Productos Petrolíferos (Miembros OPEP)
prod_productos_cruda <- read_excel("datos/5_PRODUCCION_PRODUCTOSPETROLIFEROS_OPEP.xlsx")

# 6. Producción de Petróleo Crudo (Mundial)
produccion_crudo_cruda <- read_excel("datos/6_PRODUCCION_PETROLEO_OPEP.xlsx")

# 7. Exportaciones de Petróleo Crudo
export_crudo_cruda <- read_excel("datos/7_EXPORTACIONES_PETROLEOCRUDO_OPEP.xlsx")

# 10. Precio Nominal y Real Cesta OPEP
precios_nominal_real_cruda <- read_excel("datos/10_PRECIO_NOMINALYREAL_CESTA_OPEP.xlsx")
