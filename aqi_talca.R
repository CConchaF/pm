library(httr)
library(jsonlite)
library(dplyr)
library(readr)

token <- Sys.getenv("AQICN_TOKEN")

get_aqi_talca <- function(token) {
  url <- paste0(
    "https://api.waqi.info/feed/chile/universidad-de-talca/?token=",
    token
  )
  
  res <- GET(url)
  txt <- content(res, "text", encoding = "UTF-8")
  aqi_json <- fromJSON(txt)
  
  data.frame(
    ciudad = aqi_json$data$city$name,
    fecha = aqi_json$data$time$s,
    aqi = aqi_json$data$aqi,
    pm25 = aqi_json$data$iaqi$pm25$v,
    pm10 = aqi_json$data$iaqi$pm10$v,
    temp = aqi_json$data$iaqi$t$v,
    viento = aqi_json$data$iaqi$w$v,
    humedad = aqi_json$data$iaqi$h$v,
    fecha_descarga = as.character(Sys.time())
  )
}

archivo <- "data/aqi_talca_log.csv"

df_new <- get_aqi_talca(token)

if (file.exists(archivo)) {
  df_old <- read_csv(archivo, show_col_types = FALSE)
  df_total <- bind_rows(df_old, df_new) %>%
    distinct(ciudad, fecha, .keep_all = TRUE)
} else {
  df_total <- df_new
}

write_csv(df_total, archivo)
