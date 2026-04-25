packages <- c("httr", "jsonlite", "dplyr", "readr", "purrr")

installed <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed)) {
    install.packages(p, repos = "https://cloud.r-project.org")
  }
}

library(httr)
library(jsonlite)
library(dplyr)
library(readr)
library(purrr)

token <- Sys.getenv("AQICN_TOKEN")

estaciones <- data.frame(
  ciudad = c(
    # Talca
    "Talca", "Talca", "Talca",
    
    # Los Ángeles
    "Los Angeles", "Los Angeles",
    
    # Chillán
    "Chillan", "Chillan",
    
    # Rancagua
    "Rancagua", "Rancagua",
    
    # Temuco
    "Temuco", "Temuco", "Temuco",
    
    # Valdivia
    "Valdivia", "Valdivia",
    
    # Osorno
    "Osorno"
  ),
  
  estacion = c(
    # Talca
    "La Florida 2",
    "U.C. Maule",
    "Universidad de Talca",
    
    # Los Ángeles
    "Los Angeles Oriente",
    "21 de Mayo 2",
    
    # Chillán
    "INIA Chillan",
    "Puren",
    
    # Rancagua
    "Rancagua II",
    "Rancagua I",
    
    # Temuco
    "Nielol",
    "Las Encinas",
    "Padre Las Casas II",
    
    # Valdivia
    "Valdivia 2",
    "Valdivia",
    
    # Osorno
    "Osorno"
  ),
  
  api_path = c(
    # Talca
    "chile/la-florida-2",
    "chile/u.c.-maule",
    "chile/universidad-de-talca",
    
    # Los Ángeles
    "chile/los-angeles-oriente",
    "chile/21-de-mayo-2",
    
    # Chillán
    "chile/inia--chillan",
    "chile/puren",
    
    # Rancagua
    "chile/rancagua-ii",
    "chile/rancagua-i",
    
    # Temuco
    "chile/nielol",
    "chile/las-encinas-temuco",
    "chile/padre-las-casas-ii",
    
    # Valdivia
    "chile/valdivia-2",
    "chile/valdivia",
    
    # Osorno
    "chile/osorno"
  ),
  
  stringsAsFactors = FALSE
)

extraer_valor <- function(x, var) {
  if (!is.null(x$data$iaqi[[var]]$v)) {
    return(x$data$iaqi[[var]]$v)
  } else {
    return(NA)
  }
}

get_aqi <- function(ciudad, estacion, api_path, token) {
  
  url <- paste0(
    "https://api.waqi.info/feed/",
    api_path,
    "/?token=",
    token
  )
  
  res <- GET(url)
  txt <- content(res, "text", encoding = "UTF-8")
  aqi_json <- fromJSON(txt)
  
  data.frame(
    ciudad = ciudad,
    estacion = estacion,
    ciudad_api = aqi_json$data$city$name,
    fecha = aqi_json$data$time$s,
    aqi = aqi_json$data$aqi,
    pm25 = extraer_valor(aqi_json, "pm25"),
    pm10 = extraer_valor(aqi_json, "pm10"),
    temp = extraer_valor(aqi_json, "t"),
    viento = extraer_valor(aqi_json, "w"),
    humedad = extraer_valor(aqi_json, "h"),
    fecha_descarga = as.character(Sys.time()),
    stringsAsFactors = FALSE
  )
}

dir.create("data", showWarnings = FALSE)

df_new <- purrr::pmap_dfr(
  estaciones,
  ~ get_aqi(..1, ..2, ..3, token)
)

archivo <- "data/aqi_panel_log.csv"

if (file.exists(archivo)) {
  df_old <- read_csv(
    archivo,
    show_col_types = FALSE,
    col_types = cols(
      fecha = col_character(),
      fecha_descarga = col_character()
    )
  )
  
  df_new <- df_new %>%
    mutate(
      fecha = as.character(fecha),
      fecha_descarga = as.character(fecha_descarga)
    )
  
  df_total <- bind_rows(df_old, df_new) %>%
    distinct(ciudad, estacion, fecha, .keep_all = TRUE)
} else {
  df_total <- df_new %>%
    mutate(
      fecha = as.character(fecha),
      fecha_descarga = as.character(fecha_descarga)
    )
}

write_csv(df_total, archivo)
