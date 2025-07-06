#-----------------------------------------------------------------------
# PROYECTO: Dinámica del Mercado Petrolero y Economía de Venezuela
# FASE 5: Modelado de Datos
# AUTOR: CEMC
# FECHA: 06-07-2025
#-----------------------------------------------------------------------

# --- PASO 0: CARGA DE LIBRERÍAS ---
library(tidyverse) # Para manipulación de datos (dplyr)
library(lmtest)    # Para pruebas estadísticas en modelos lineales (ej. Durbin-Watson)
library(car)       # Para VIF (Factor de Inflación de la Varianza) para multicolinealidad

# --- PASO 1: SELECCIÓN Y APLICACIÓN DE MODELOS ESTADÍSTICOS ---

# Para el análisis de series de tiempo con datos anuales, la regresión lineal
# es un buen punto de partida para identificar relaciones.

# Modelo 1: Impacto de las variables petroleras en el PIB Nominal
# Pregunta: ¿Cómo influyen el precio del petróleo y la producción en el PIB Nominal?
modelo_pib_nominal <- lm(pib_nominal ~ precio_real_cesta + produccion_petroleo_bd, data = datos_venezuela_consolidados)
summary(modelo_pib_nominal)

# Diagnóstico de multicolinealidad (VIF - Factor de Inflación de la Varianza)
# Un VIF > 5-10 sugiere problemas de multicolinealidad.
vif(modelo_pib_nominal)

# Diagnóstico de autocorrelación de los residuos (Durbin-Watson Test)
# Un valor cercano a 2 indica no autocorrelación. Valores cercanos a 0 o 4 indican autocorrelación positiva o negativa.
dwtest(modelo_pib_nominal)

# Modelo 2: Impacto de las exportaciones de petróleo en el Saldo de la Cuenta Corriente
# Pregunta: ¿Cuál es la relación entre las exportaciones de petróleo (valor) y el saldo de la cuenta corriente?
modelo_saldo_corriente <- lm(saldo_corriente_usd ~ exportaciones_petroleo_usd, data = datos_venezuela_consolidados)
summary(modelo_saldo_corriente)
dwtest(modelo_saldo_corriente)
