#-----------------------------------------------------------------------
# PROYECTO: Dinámica del Mercado Petrolero y Economía de Venezuela
# FASE 4: Visualización de Datos
# AUTOR: CEMC
# FECHA: 02-07-2025 (Actualizado a 05-07-2025)
#-----------------------------------------------------------------------


# --- PASO 0: CARGA DE LIBRERÍAS PARA VISUALIZACIÓN ---
library(tidyverse) 
library(plotly)  # Para crear gráficos interactivos
library(DT)      # Para crear tablas interactivas

# --- 1. CREACIÓN DE GRÁFICOS Y TABLAS ---

# Utilizaremos el dataframe 'datos_venezuela_consolidados' para la mayoría de los gráficos.

# --- GRÁFICOS INDIVIDUALES DE TENDENCIAS HISTÓRICAS (Series de Tiempo) ---

# Gráfico 1: Evolución del PIB Nominal de Venezuela
grafico_pib_nominal <- ggplot(datos_venezuela_consolidados, aes(x = anio, y = pib_nominal)) +
  geom_line(color = "darkblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(title = "PIB Nominal de Venezuela (1980-2023)", x = "Año", y = "PIB Nominal (Millones de USD)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
grafico_pib_nominal
ggplotly(grafico_pib_nominal)

# Gráfico 2: Variación Porcentual del PIB Real
grafico_pib_real_variacion <- ggplot(datos_venezuela_consolidados, aes(x = anio, y = pib_real_variacion_porcentual)) +
  geom_line(color = "skyblue", linewidth = 1) +
  geom_point(color = "skyblue", size = 2) +
  labs(title = "Variación Porcentual del PIB Real de Venezuela (1980-2023)", x = "Año", y = "Variación PIB Real (%)") +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") # Línea de referencia en 0
grafico_pib_real_variacion
ggplotly(grafico_pib_real_variacion)

# Gráfico 3: Exportaciones de Petróleo (Valor en USD)
grafico_exportaciones_valor <- ggplot(datos_venezuela_consolidados, aes(x = anio, y = exportaciones_petroleo_usd)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Exportaciones de Petróleo de Venezuela (Valor)", x = "Año", y = "Exportaciones Petróleo (Millones de USD)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
grafico_exportaciones_valor
ggplotly(grafico_exportaciones_valor)

# Gráfico 4: Saldo de la Cuenta Corriente
grafico_saldo_corriente <- ggplot(datos_venezuela_consolidados, aes(x = anio, y = saldo_corriente_usd)) +
  geom_line(color = "coral", linewidth = 1) +
  geom_point(color = "coral", size = 2) +
  labs(title = "Saldo de la Cuenta Corriente de Venezuela (1980-2023)", x = "Año", y = "Saldo Cuenta Corriente (Millones de USD)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50")
grafico_saldo_corriente
ggplotly(grafico_saldo_corriente)

# Gráfico 5: Producción de Petróleo Crudo (1.000 B/D)
grafico_produccion_petroleo_bd <- ggplot(datos_venezuela_consolidados, aes(x = anio, y = produccion_petroleo_bd)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(title = "Producción de Petróleo Crudo de Venezuela (1980-2023)", x = "Año", y = "Producción Petróleo (Miles B/D)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
grafico_produccion_petroleo_bd
ggplotly(grafico_produccion_petroleo_bd)

# Gráfico 6: Exportaciones de Petróleo Crudo (1.000 B/D)
grafico_exportaciones_crudo_bd <- ggplot(datos_venezuela_consolidados, aes(x = anio, y = exportaciones_crudo_bd)) +
  geom_line(color = "brown", linewidth = 1) +
  geom_point(color = "brown", size = 2) +
  labs(title = "Exportaciones de Petróleo Crudo de Venezuela (1980-2023)", x = "Año", y = "Exportaciones Crudo (Miles B/D)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)
grafico_exportaciones_crudo_bd
ggplotly(grafico_exportaciones_crudo_bd)

# Gráfico 7: Precio Cesta OPEP (Nominal y Real)
precios_formato_largo <- datos_venezuela_consolidados %>%
  select(anio, precio_nominal_cesta, precio_real_cesta) %>%
  pivot_longer(cols = starts_with("precio_"), names_to = "tipo_precio", values_to = "valor_precio") %>%
  mutate(tipo_precio = recode(tipo_precio,
                              "precio_nominal_cesta" = "Nominal",
                              "precio_real_cesta" = "Real"))

grafico_precio_cesta_opep <- ggplot(precios_formato_largo, aes(x = anio, y = valor_precio, color = tipo_precio)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Precio Cesta OPEP: Nominal vs. Real (1980-2023)", x = "Año", y = "Precio ($/b)", color = "Tipo de Precio") +
  theme_minimal() +
  scale_color_manual(values = c("Nominal" = "purple", "Real" = "darkorange")) +
  scale_y_continuous(labels = scales::dollar)
grafico_precio_cesta_opep
ggplotly(grafico_precio_cesta_opep)


# Gráfico 8: PRODUCCIÓN DE PRODUCTOS PETROLÍFEROS TOTAL Y COMPONENTES
productos_petroliferos_formato_largo <- productos_petroliferos_preparado %>%
  pivot_longer(cols = -anio, names_to = "componente", values_to = "valor_produccion") %>%
  mutate(componente = factor(componente, levels = c("Produccion_Total", "Gasolina", "Queroseno", "Destilados", "Residuos", "Otros")))

grafico_produccion_productos_petroliferos <- ggplot(productos_petroliferos_formato_largo, aes(x = anio, y = valor_produccion, color = componente)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Producción de Productos Petrolíferos de Venezuela por Componente (1980-2023)",
    x = "Año",
    y = "Producción (Miles de Barriles)",
    color = "Componente Petrolífero" 
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_color_brewer(palette = "Set1")
grafico_produccion_productos_petroliferos
ggplotly(grafico_produccion_productos_petroliferos)


# --- GRÁFICOS PARA LOS ÍNDICES DERIVADOS ---

# Gráfico 9: Evolución del Índice de Exposición a la Renta Petrolera
grafico_indice_exposicion <- ggplot(datos_venezuela_consolidados, aes(x = anio, y = indice_exposicion)) +
  geom_line(color = "orange", linewidth = 1.2) +
  geom_point(color = "orange", size = 2.5) +
  labs(
    title = "Índice de Exposición a la Renta Petrolera en Venezuela (1980-2023)",
    subtitle = "Calculado como: PIB Nominal / Valor de Exportaciones de Petróleo", 
    x = "Año",
    y = "Índice de Exposición (Ratio)" 
  ) +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 9, color = "gray30"))
grafico_indice_exposicion
ggplotly(grafico_indice_exposicion)


# Gráfico 10: Evolución del Índice de Eficiencia Económica de la Producción Petrolera
grafico_indice_eficiencia <- ggplot(datos_venezuela_consolidados, aes(x = anio, y = indice_eficiencia)) +
  geom_line(color = "turquoise4", linewidth = 1.2) +
  geom_point(color = "turquoise4", size = 2.5) +
  labs(
    title = "Índice de Eficiencia Económica de la Producción Petrolera en Venezuela (1980-2023)",
    subtitle = "Calculado como: Producción Total Anual de Barriles / PIB Nominal", 
    x = "Año",
    y = "Índice de Eficiencia (Barriles / Millón USD PIB)" 
  ) +
  theme_minimal() +
  theme(plot.subtitle = element_text(size = 9, color = "gray30"))
grafico_indice_eficiencia
ggplotly(grafico_indice_eficiencia)


# --- GRÁFICOS COMBINADOS ---

# Gráfico: Producción y Exportaciones de Petróleo Crudo (para comparar volumen)
grafico_produccion_exportaciones_crudo <- ggplot(datos_venezuela_consolidados, aes(x = anio)) +
  geom_line(aes(y = produccion_petroleo_bd, color = "Producción Petróleo (Miles B/D)"), linewidth = 1) + 
  geom_line(aes(y = exportaciones_crudo_bd, color = "Exportaciones Crudo (Miles B/D)"), linewidth = 1, linetype = "dotted") + 
  labs(
    title = "Producción vs. Exportaciones de Petróleo Crudo de Venezuela (1980-2023)",
    x = "Año",
    y = "Volumen (Miles de Barriles/Día)",
    color = "Tipo de Flujo"
  ) +
  scale_color_manual(values = c("Producción Petróleo (Miles B/D)" = "darkred", "Exportaciones Crudo (Miles B/D)" = "brown")) +
  theme_minimal() +
  theme(legend.position = "bottom")
grafico_produccion_exportaciones_crudo
ggplotly(grafico_produccion_exportaciones_crudo)


# --- 2. CREACIÓN DE TABLAS INTERACTIVAS ---

# Tabla Interactiva 1: Datos Consolidados Completos
# Permite explorar todos los datos consolidados año por año
tabla_datos_consolidados_interactiva <- DT::datatable(
  datos_venezuela_consolidados,
  options = list(pageLength = 10, scrollX = TRUE), # Mostrar 10 filas por página y permitir scroll horizontal
  rownames = FALSE, # No mostrar los números de fila de R
  caption = "Tabla Interactiva: Datos Consolidados de Venezuela (1980-2023) - Explora todos los indicadores económicos y petroleros consolidados.",
  colnames = c( # Renombrar columnas para la visualización en la tabla
    "Año",
    "PIB Nominal (Millones USD)",
    "Variación PIB Real (%)",
    "Exportaciones Petróleo (Millones USD)",
    "Saldo Cuenta Corriente (Millones USD)",
    "Producción Petróleo (Miles B/D)",
    "Exportaciones Crudo (Miles B/D)",
    "Precio Nominal Cesta OPEP ($/b)",
    "Precio Real Cesta OPEP ($/b)",
    "Índice Exposición Petrolera",
    "Índice Eficiencia Petrolera"
  )
)
tabla_datos_consolidados_interactiva


# Tabla Interactiva 2: Resumen Detallado de los Índices Clave y sus Componentes
# Para comparar año con año los índices y las variables que los componen.
tabla_indices_detallada <- datos_venezuela_consolidados %>%
  mutate(produccion_total_barriles_anuales = produccion_petroleo_bd * 365) %>% # Recalcular para mostrar
  select(
    anio,
    pib_nominal,
    exportaciones_petroleo_usd,
    indice_exposicion,
    produccion_petroleo_bd, # Producción diaria
    produccion_total_barriles_anuales, # Producción anual calculada para el índice
    indice_eficiencia
  ) %>%
  mutate(
    pib_nominal = round(pib_nominal, 2),
    exportaciones_petroleo_usd = round(exportaciones_petroleo_usd, 2),
    indice_exposicion = round(indice_exposicion, 4), # Más decimales para precisión
    produccion_petroleo_bd = round(produccion_petroleo_bd, 2),
    produccion_total_barriles_anuales = round(produccion_total_barriles_anuales, 0), # Redondear a enteros
    indice_eficiencia = round(indice_eficiencia, 8) # Más decimales para precisión
  )

tabla_indices_interactiva <- DT::datatable(
  tabla_indices_detallada,
  options = list(pageLength = 10, scrollX = TRUE),
  rownames = FALSE,
  caption = "Tabla Interactiva: Índices Clave y sus Componentes (1980-2023) - Análisis detallado de los índices de exposición y eficiencia petrolera con sus componentes.",
  colnames = c( # Renombrar columnas para la visualización en la tabla
    "Año",
    "PIB Nominal (Millones USD)",
    "Exportaciones Petróleo (Millones USD)",
    "Índice Exposición Petrolera",
    "Producción Petróleo Diaria (Miles B/D)",
    "Producción Petróleo Anual (Miles Barriles)",
    "Índice Eficiencia Petrolera"
  )
)
tabla_indices_interactiva

