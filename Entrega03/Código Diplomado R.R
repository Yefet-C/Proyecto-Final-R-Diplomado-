 


#Instalación y cargar de librerías necesarias


# Defino un vector con los paquetes que necesito para trabajar
paquetes_necesarios <- c("dplyr", "readr", "lubridate", "ggplot2")

# Reviso si ya están instalados, y si no, los instalo y cargo
for (paquete in paquetes_necesarios) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete, dependencies = TRUE)
    library(paquete, character.only = TRUE)
  }
}

# Por si acaso, vuelvo a cargar las librerías de forma explícita
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)

# Carga de archivos


# Defino la ruta donde están los archivos .csv y la establezco como directorio de trabajo
ruta_carpeta <- "C:/Users/Usuario/Desktop/Entrega03"
setwd(ruta_carpeta)

# Leo los archivos de ventas históricas y stock actual
ventas <- read_csv("ventas_historicas.csv", show_col_types = FALSE)
stock <- read_csv("stock_actual.csv", show_col_types = FALSE)

# Preprocesamiento

# Renombro las columnas de ventas para tener nombres más consistentes
# Además, convierto la columna de fecha a formato tipo Date
ventas <- ventas %>%
  rename(Fecha = FECHA, Tienda = TIENDA, SKU = SKU, Ventas = VENTAS) %>%
  mutate(Fecha = ymd(Fecha))

# Hago lo mismo para el archivo de stock: renombro columnas para que coincidan
stock <- stock %>%
  rename(Tienda = TIENDA, SKU = SKU, Stock_Actual = STOCK_ACTUAL)


# Cálculo de stock crítico y alertas

# Obtengo la fecha más reciente disponible en las ventas
fecha_max <- max(ventas$Fecha, na.rm = TRUE)

# Filtro las ventas de los últimos 7 días (incluyendo la fecha más reciente)
# Agrupo por SKU y Tienda para calcular la venta promedio diaria
ventas_ultimos_7 <- ventas %>%
  filter(Fecha >= fecha_max - days(6)) %>%
  group_by(SKU, Tienda) %>%
  summarise(
    Venta_Promedio_Diaria = mean(Ventas, na.rm = TRUE),
    .groups = "drop"
  )

# Defino un parámetro que representa cuántos días se tarda en reponer stock
dias_reposicion <- 3

# Calculo el stock crítico como el consumo esperado en esos días de reposición
ventas_ultimos_7 <- ventas_ultimos_7 %>%
  mutate(Stock_Critico = round(Venta_Promedio_Diaria * dias_reposicion))

# Uno la tabla de stock con las ventas procesadas
# Luego calculo la cobertura en días y clasifico cada producto según su nivel de alerta
stock_alerta <- stock %>%
  left_join(ventas_ultimos_7, by = c("SKU", "Tienda")) %>%
  mutate(
    Venta_Promedio_Diaria = ifelse(is.na(Venta_Promedio_Diaria), NA, Venta_Promedio_Diaria),
    Dias_Stock = round(Stock_Actual / Venta_Promedio_Diaria, 1),
    Alerta = case_when(
      is.na(Dias_Stock) ~ "Sin datos",  # Si no hay información, lo indico explícitamente
      Dias_Stock <= 3 | Stock_Actual <= Stock_Critico ~ "Alerta",  # Riesgo de quiebre
      TRUE ~ "OK"  # Todo en orden
    )
  )


# Visualización 


# Preparo los datos para el gráfico: ordeno y creo una columna combinada para facilitar la lectura
datos_grafico <- stock_alerta %>%
  filter(!is.na(Dias_Stock)) %>%
  arrange(Dias_Stock) %>%
  mutate(SKU_Tienda = paste0(SKU, " (", Tienda, ")"))

# Construyo un gráfico de barras horizontales mostrando los días de cobertura por SKU/Tienda
grafico <- ggplot(datos_grafico, aes(x = reorder(SKU_Tienda, Dias_Stock), y = Dias_Stock, fill = Alerta)) +
  geom_bar(stat = "identity", width = 0.7, show.legend = TRUE) +
  coord_flip() +  # Giro el gráfico para mejor legibilidad
  geom_hline(yintercept = 3, linetype = "dashed", color = "black", linewidth = 0.5) +  # Línea de referencia
  labs(
    title = "Cobertura de Stock por Producto y Tienda",
    subtitle = "Identificación de productos con riesgo de quiebre en los próximos días",
    x = "Producto (SKU - Tienda)",
    y = "Días estimados de cobertura",
    fill = "Nivel de alerta"
  ) +
  scale_fill_manual(
    values = c("Alerta" = "#D7263D", "OK" = "#1B998B", "Sin datos" = "#A0A0A0"),
    labels = c("Alerta" = "Reposición urgente", "OK" = "Stock suficiente", "Sin datos" = "Sin información")
  ) +
  theme_minimal(base_size = 11) +  # Estética limpia
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    axis.text.y = element_text(size = 7),
    axis.title = element_text(size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Muestro el gráfico por pantalla
print(grafico)

# Guardo el gráfico como archivo PNG con la fecha del día
nombre_imagen <- paste0("grafico_alerta_", format(Sys.Date(), "%Y-%m-%d"), ".png")
ggsave(filename = nombre_imagen,
       plot = grafico, width = 11, height = 8, bg = "white")


# Exportación de datos 

# Creo una columna adicional para facilitar el ordenamiento por nivel de alerta
# Ordeno para que aparezcan primero los productos más críticos
stock_alerta_export <- stock_alerta %>%
  mutate(Alerta_Nivel = case_when(
    Alerta == "Alerta" ~ 1,
    Alerta == "Sin datos" ~ 2,
    TRUE ~ 3
  )) %>%
  arrange(Alerta_Nivel, Dias_Stock) %>%
  select(SKU, Tienda, Stock_Actual, Venta_Promedio_Diaria, Stock_Critico, Dias_Stock, Alerta)

# Defino el nombre del archivo exportado, que incluye la fecha actual
nombre_archivo_mejorado <- paste0("alerta_stock_filtrado_", format(Sys.Date(), "%Y-%m-%d"), ".csv")

# Exporto el archivo a formato CSV
write_csv(stock_alerta_export, nombre_archivo_mejorado)

# Informo en consola que todo terminó correctamente
cat("Archivo exportado correctamente:", nombre_archivo_mejorado, "\n")
cat("Gráfico guardado como:", nombre_imagen, "\n")
