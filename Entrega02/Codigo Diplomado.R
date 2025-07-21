
# Se instalarán las librerias necesarias, para posteriormente llamarlas libremente

paquetes_necesarios <- c("dplyr", "readr", "lubridate")

for (paquete in paquetes_necesarios) {
  if (!require(paquete, character.only = TRUE)) {
    install.packages(paquete, dependencies = TRUE)
    library(paquete, character.only = TRUE)
  }
}

library(dplyr)
library(readr)
library(lubridate)

# esta es la carpeta en la cual se encuentra la base de datos en formato csv "C:/Users/Usuario/Desktop/Entrega02"

ruta_carpeta <- "C:/Users/Usuario/Desktop/Entrega02"
setwd(ruta_carpeta)  # Se establece esta carpeta como la carpeta de trabajo

# se suben y se leen los archivos CSV de ventas históricas y stock actual


ventas <- read_csv("ventas_historicas.csv", show_col_types = FALSE)
stock <- read_csv("stock_actual.csv", show_col_types = FALSE)

# Se renombran las columnas para prevenir errores  
ventas <- ventas %>%
  rename(
    Fecha = FECHA,
    Tienda = TIENDA,
    SKU = SKU,
    Ventas = VENTAS
  )

stock <- stock %>%
  rename(
    Tienda = TIENDA,
    SKU = SKU,
    Stock_Actual = STOCK_ACTUAL
  )

# Se convierte la columna "Fecha" al formato de fecha (año-mes-día)
ventas <- ventas %>% mutate(Fecha = ymd(Fecha))

# Buscamos la fecha más reciente que existe en los datos de ventas
fecha_max <- max(ventas$Fecha, na.rm = TRUE)

# Filtramos las ventas de los últimos 7 días (desde la fecha más reciente)
# Luego agrupamos por SKU y tienda para calcular el promedio diario de ventas
ventas_ultimos_7 <- ventas %>%
  filter(Fecha >= fecha_max - days(6)) %>%  # 6 días antes de la fecha más reciente (7 días en total)
  group_by(SKU, Tienda) %>%
  summarise(
    Venta_Promedio_Diaria = mean(Ventas, na.rm = TRUE),  # Promedio de ventas por día
    .groups = "drop"
  )

# Definimos los días de reposición (tiempo que tarda en llegar un nuevo stock), para probar su funcionamiento se estandarizo
# la cantidad de dias en 4, cabe destacar que puede variar dependiendo del producto
dias_reposicion <- 3

# Calculamos el stock crítico, que es el promedio diario de ventas por los días de reposición
ventas_ultimos_7 <- ventas_ultimos_7 %>%
  mutate(Stock_Critico = round(Venta_Promedio_Diaria * dias_reposicion))

# Unimos los datos de stock con los datos de ventas
stock_alerta <- stock %>%
  left_join(ventas_ultimos_7, by = c("SKU", "Tienda")) %>%
  mutate(
    # Si no hay promedio de ventas, dejamos el valor como NA
    Venta_Promedio_Diaria = ifelse(is.na(Venta_Promedio_Diaria), NA, Venta_Promedio_Diaria),
    
    # Calculamos los días que durará el stock actual (stock dividido por ventas promedio)
    Dias_Stock = round(Stock_Actual / Venta_Promedio_Diaria, 1),
    
    # Creamos una columna que indique si hay alerta de reposición
    Alerta = case_when(
      is.na(Dias_Stock) ~ "Sin datos",  # No hay información suficiente
      Dias_Stock <= 3 | Stock_Actual <= Stock_Critico ~ "Alerta",  # Hay que reponer
      TRUE ~ "OK"  # Todo está bien
    )
  )

# Se creará un nombre para el archivo de salida con la fecha actual
nombre_archivo <- paste0("alerta_stock_", format(Sys.Date(), "%Y-%m-%d"), ".csv")

# Se Guardará el resultado en un nuevo archivo CSV
write_csv(stock_alerta, nombre_archivo)

# Se coloca un aviso para visualizar que funcionó correctamente
cat("✅ Archivo exportado:", nombre_archivo, "\n")


