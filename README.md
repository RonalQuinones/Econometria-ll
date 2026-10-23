# TRABAJO DE ECONOMETRÍA II
# Pronóstico de la inflación mensual de Colombia mediante un modelo ARIMA
# Datos: enero de 2010 a septiembre de 2026
# Pronóstico: octubre de 2026 a julio de 2027


# 0. INSTALACIÓN Y CARGA DE LIBRERÍAS

paquetes <- c(
  "here", "fs", "readxl", "dplyr", "ggplot2", "seasonal",
  "tseries", "tsibble", "fable", "feasts", "FinTS",
  "lmtest", "knitr", "kableExtra", "tibble", "fabletools"
)

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  install.packages(faltantes)
}

library(here)
library(fs)
library(readxl)
library(dplyr)
library(ggplot2)
library(seasonal)
library(tseries)
library(tsibble)
library(fable)
library(feasts)
library(FinTS)
library(lmtest)
library(knitr)
library(kableExtra)


# 1. RUTAS Y CARPETAS DEL PROYECTO

here::i_am("Codigo.R")

directorio <- here::here()
ruta_inf <- fs::path(directorio, "datos_inflacion_colombia.xlsx")

carpeta_figuras <- fs::path(directorio, "figuras")
carpeta_resultados <- fs::path(directorio, "resultados")

fs::dir_create(carpeta_figuras)
fs::dir_create(carpeta_resultados)

if (!file.exists(ruta_inf)) {
  stop(
    "No se encuentra el archivo: ", ruta_inf,
    "\nVerifica que datos_inflacion_colombia.xlsx esté en la carpeta del proyecto."
  )
}


# 2. IMPORTACIÓN Y ORGANIZACIÓN DE LOS DATOS

datos <- readxl::read_excel(ruta_inf)

if (nrow(datos) == 0 || ncol(datos) == 0) {
  stop("El archivo Excel se importó vacío. Revisa la hoja seleccionada.")
}

# Limpiar espacios en los nombres de columnas
names(datos) <- trimws(names(datos))

# Función para normalizar nombres de columnas
normalizar_nombre <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]", "", x)
  x
}

nombres_originales <- names(datos)
nombres_norm <- vapply(
  nombres_originales,
  normalizar_nombre,
  character(1)
)

# Identificar la columna de fecha
pos_fecha <- which(
  nombres_norm %in% c(
    "fecha", "date", "periodo", "period", "mes", "fechames"
  )
)

# Identificar la columna de inflación
pos_inflacion <- which(
  nombres_norm %in% c(
    "inflacion", "inflacionmensual", "ipc",
    "variacionmensual", "variacionmensualipc",
    "inflacionmensualcolombia"
  )
)

if (length(pos_fecha) == 0) {
  stop(
    "No se identificó la columna de fecha.\n",
    "Nombres encontrados: ",
    paste(nombres_originales, collapse = ", ")
  )
}

if (length(pos_inflacion) == 0) {
  stop(
    "No se identificó la columna de inflación.\n",
    "Nombres encontrados: ",
    paste(nombres_originales, collapse = ", "),
    "\nDebes identificar cuál columna contiene la inflación mensual."
  )
}

# Renombrar las columnas para utilizar nombres uniformes
names(datos)[pos_fecha[1]] <- "Fecha"
names(datos)[pos_inflacion[1]] <- "inflacion"

# Convertir fechas de Excel o texto a Date
if (inherits(datos$Fecha, "POSIXt")) {
  datos$Fecha <- as.Date(datos$Fecha)
} else if (inherits(datos$Fecha, "Date")) {
  datos$Fecha <- as.Date(datos$Fecha)
} else if (is.numeric(datos$Fecha)) {
  datos$Fecha <- as.Date(
    datos$Fecha,
    origin = "1899-12-30"
  )
} else {
  fecha_texto <- trimws(as.character(datos$Fecha))
  
  fecha_convertida <- suppressWarnings(
    as.Date(fecha_texto)
  )
  
  faltan_fechas <- is.na(fecha_convertida)
  
  if (any(faltan_fechas)) {
    formatos <- c("%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d", "%d-%m-%Y")
    
    for (formato in formatos) {
      pendientes <- which(is.na(fecha_convertida))
      
      if (length(pendientes) == 0) break
      
      fecha_convertida[pendientes] <- suppressWarnings(
        as.Date(fecha_texto[pendientes], format = formato)
      )
    }
  }
  
  datos$Fecha <- fecha_convertida
}

# Convertir inflación a numérico
if (is.numeric(datos$inflacion)) {
  datos$inflacion <- as.numeric(datos$inflacion)
} else {
  inflacion_texto <- trimws(as.character(datos$inflacion))
  inflacion_texto <- gsub("%", "", inflacion_texto, fixed = TRUE)
  inflacion_texto <- gsub("\\s", "", inflacion_texto)
  
  # Conversión básica de separadores decimales
  convertir_numero <- function(x) {
    if (is.na(x) || x == "") return(NA_real_)
    
    if (grepl(",", x, fixed = TRUE) &&
        grepl(".", x, fixed = TRUE)) {
      if (regexpr(",", x, fixed = TRUE)[1] >
          regexpr(".", x, fixed = TRUE)[1]) {
        x <- gsub(".", "", x, fixed = TRUE)
        x <- sub(",", ".", x, fixed = TRUE)
      } else {
        x <- gsub(",", "", x, fixed = TRUE)
      }
    } else if (grepl(",", x, fixed = TRUE)) {
      x <- sub(",", ".", x, fixed = TRUE)
    }
    
    suppressWarnings(as.numeric(x))
  }
  
  datos$inflacion <- vapply(
    inflacion_texto,
    convertir_numero,
    numeric(1)
  )
}

# Eliminar filas sin fecha o inflación y ordenar cronológicamente
datos <- datos |>
  dplyr::filter(
    !is.na(Fecha),
    is.finite(inflacion)
  ) |>
  dplyr::arrange(Fecha)

if (nrow(datos) == 0) {
  stop("No quedaron observaciones válidas después de limpiar los datos.")
}

if (anyDuplicated(datos$Fecha)) {
  stop("Hay fechas duplicadas en el archivo Excel. Revisa los datos.")
}

# Comprobar continuidad mensual
meses <- format(datos$Fecha, "%Y-%m")

if (anyDuplicated(meses)) {
  stop("Hay más de una observación en algún mes.")
}

if (nrow(datos) < 30) {
  stop("Hay muy pocas observaciones para este análisis mensual.")
}

print(names(datos))
print(head(datos))
print(tail(datos))
print(dim(datos))
str(datos)

message("Periodo: ", min(datos$Fecha), " a ", max(datos$Fecha))
message("Observaciones válidas: ", nrow(datos))


# 3. CONSTRUCCIÓN Y GRÁFICA DE LA SERIE ORIGINAL

fecha_inicio <- as.Date(format(min(datos$Fecha), "%Y-%m-01"))
fecha_fin <- as.Date(format(max(datos$Fecha), "%Y-%m-01"))

meses_esperados <- seq(
  from = fecha_inicio,
  to = fecha_fin,
  by = "month"
)

if (length(meses_esperados) != nrow(datos)) {
  stop(
    "La serie no contiene todos los meses consecutivos. ",
    "Revisa si faltan meses entre las fechas del Excel."
  )
}

inflacion_ts <- stats::ts(
  datos$inflacion,
  start = c(
    as.integer(format(fecha_inicio, "%Y")),
    as.integer(format(fecha_inicio, "%m"))
  ),
  frequency = 12
)

print(start(inflacion_ts))
print(end(inflacion_ts))
print(frequency(inflacion_ts))
print(length(inflacion_ts))

grafica_inflacion <- ggplot2::ggplot(
  datos,
  ggplot2::aes(x = Fecha, y = inflacion)
) +
  ggplot2::geom_line() +
  ggplot2::labs(
    title = "Inflación mensual de Colombia",
    subtitle = paste(
      format(min(datos$Fecha), "%B de %Y"),
      "a",
      format(max(datos$Fecha), "%B de %Y")
    ),
    x = "Fecha",
    y = "Inflación mensual (%)"
  ) +
  ggplot2::theme_minimal()

print(grafica_inflacion)

ggplot2::ggsave(
  file.path(carpeta_figuras, "inflacion_original.png"),
  plot = grafica_inflacion,
  width = 10,
  height = 6,
  dpi = 300
)


# 4. FAC Y FACP DE LA SERIE ORIGINAL

png(
  filename = file.path(carpeta_figuras, "fac_fap_serie_original.png"),
  width = 2400,
  height = 1200,
  res = 200
)

par(mfrow = c(1, 2))

stats::acf(
  inflacion_ts,
  lag.max = 36,
  ylim = c(-1, 1),
  main = "FAC de la serie original",
  xlab = "Rezago"
)

stats::pacf(
  inflacion_ts,
  lag.max = 36,
  ylim = c(-1, 1),
  main = "FACP de la serie original",
  xlab = "Rezago"
)

par(mfrow = c(1, 1))
dev.off()


# 5. DESESTACIONALIZACIÓN

modelo_estacional <- seasonal::seas(inflacion_ts)
print(summary(modelo_estacional))

inflacion_desest <- seasonal::final(modelo_estacional)

print(head(inflacion_desest))
print(start(inflacion_desest))
print(end(inflacion_desest))
print(length(inflacion_desest))

if (length(inflacion_desest) != nrow(datos)) {
  stop("La serie desestacionalizada no coincide en longitud con los datos.")
}

datos$inflacion_desest <- as.numeric(inflacion_desest)

grafica_comparacion <- ggplot2::ggplot(
  datos,
  ggplot2::aes(x = Fecha)
) +
  ggplot2::geom_line(
    ggplot2::aes(y = inflacion),
    color = "gray65",
    linewidth = 0.7,
    alpha = 0.8
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = inflacion_desest),
    color = "#D62828",
    linewidth = 1
  ) +
  ggplot2::labs(
    title = "Inflación mensual de Colombia",
    subtitle = "Serie original frente a serie desestacionalizada",
    x = NULL,
    y = "Inflación mensual (%)",
    caption = "Fuente: elaboración propia con datos mensuales de Colombia."
  ) +
  ggplot2::scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      size = 17,
      face = "bold",
      color = "#222222"
    ),
    plot.subtitle = ggplot2::element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title.y = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_line(
      color = "gray90",
      linewidth = 0.3
    ),
    plot.caption = ggplot2::element_text(
      color = "gray45",
      hjust = 0
    ),
    plot.margin = ggplot2::margin(15, 15, 15, 15)
  )

print(grafica_comparacion)

ggplot2::ggsave(
  file.path(carpeta_figuras, "serie_original_vs_desestacionalizada.png"),
  plot = grafica_comparacion,
  width = 11,
  height = 6,
  dpi = 300
)


# 6. PRUEBAS DE ESTACIONARIEDAD DE LA SERIE DESESTACIONALIZADA

prueba_adf <- tseries::adf.test(inflacion_desest)
prueba_kpss <- tseries::kpss.test(
  inflacion_desest,
  null = "Level"
)

print(prueba_adf)
print(prueba_kpss)


# 7. PRIMERA DIFERENCIA Y PRUEBAS DE ESTACIONARIEDAD

inflacion_dif <- diff(inflacion_desest)

print(head(inflacion_dif))
message("Observaciones después de diferenciar: ", length(inflacion_dif))

adf_dif <- tseries::adf.test(inflacion_dif)
kpss_dif <- tseries::kpss.test(
  inflacion_dif,
  null = "Level"
)

print(adf_dif)
print(kpss_dif)


# 8. GRÁFICA DE LA PRIMERA DIFERENCIA

datos_dif <- data.frame(
  Fecha = datos$Fecha[-1],
  inflacion_dif = as.numeric(inflacion_dif)
)

grafica_diferencia <- ggplot2::ggplot(
  datos_dif,
  ggplot2::aes(x = Fecha, y = inflacion_dif)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "gray60",
    linetype = "dashed"
  ) +
  ggplot2::geom_line(
    color = "#D62828",
    linewidth = 0.7
  ) +
  ggplot2::labs(
    title = "Primera diferencia de la inflación",
    subtitle = "Serie desestacionalizada",
    x = NULL,
    y = "Cambio mensual (puntos porcentuales)"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 16
    ),
    panel.grid.minor = ggplot2::element_blank()
  )

print(grafica_diferencia)

ggplot2::ggsave(
  file.path(carpeta_figuras, "primera_diferencia.png"),
  plot = grafica_diferencia,
  width = 10,
  height = 6,
  dpi = 300
)


# 9. FAC Y FACP DE LA PRIMERA DIFERENCIA

png(
  filename = file.path(carpeta_figuras, "fac_fap_primera_diferencia.png"),
  width = 2400,
  height = 1200,
  res = 200
)

par(mfrow = c(1, 2))

stats::acf(
  inflacion_dif,
  lag.max = 36,
  ylim = c(-1, 1),
  main = "FAC de la primera diferencia",
  xlab = "Rezago"
)

stats::pacf(
  inflacion_dif,
  lag.max = 36,
  ylim = c(-1, 1),
  main = "FACP de la primera diferencia",
  xlab = "Rezago"
)

par(mfrow = c(1, 1))
dev.off()


# 10. PREPARACIÓN DE LOS DATOS PARA FABLE

fechas <- tsibble::yearmonth(datos$Fecha)

datos_modelo <- tibble::tibble(
  Fecha = fechas,
  inflacion = as.numeric(inflacion_desest)
) |>
  tsibble::as_tsibble(index = Fecha)

print(head(datos_modelo))
print(tail(datos_modelo))
message("Observaciones para los modelos: ", nrow(datos_modelo))


# 11. ESTIMACIÓN Y COMPARACIÓN DE MODELOS ARIMA

modelos <- datos_modelo |>
  fabletools::model(
    arima_110 = fable::ARIMA(inflacion ~ pdq(1, 1, 0)),
    arima_011 = fable::ARIMA(inflacion ~ pdq(0, 1, 1)),
    arima_211 = fable::ARIMA(inflacion ~ pdq(2, 1, 1))
  )

fabletools::report(modelos)

comparacion_modelos <- fabletools::glance(modelos) |>
  dplyr::select(.model, AIC, AICc, BIC) |>
  dplyr::arrange(AICc)

print(comparacion_modelos)

write.csv(
  as.data.frame(comparacion_modelos),
  file.path(carpeta_resultados, "comparacion_modelos_arima.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# 12. SELECCIÓN DEL MODELO FINAL

# Se conserva ARIMA(2,1,1), como en el código original.
# 12. SELECCIÓN DEL MODELO FINAL Y SIGNIFICANCIA
# Modelo seleccionado: ARIMA(2,1,1)

modelo_final <- modelos |>
  dplyr::select(arima_211)

# Mostrar el modelo estimado
print(modelo_final)

# Extraer los coeficientes
coeficientes <- fabletools::tidy(modelo_final)

# Mostrar la tabla de coeficientes
print(coeficientes)

# Guardar los resultados
write.csv(
  as.data.frame(coeficientes),
  file.path(
    carpeta_resultados,
    "coeficientes_arima_211.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# 13. VALIDACIÓN DE SUPUESTOS PARA LOS TRES MODELOS

residuos_modelos <- fabletools::augment(modelos)

validar_residuos <- function(datos, nombre_modelo, p, q) {
  
  datos_validos <- datos |>
    dplyr::filter(
      is.finite(.resid),
      is.finite(.fitted)
    )
  
  residuos <- datos_validos$.resid
  ajustados <- datos_validos$.fitted
  
  if (length(residuos) < 20) {
    stop("No hay suficientes residuos válidos para ", nombre_modelo)
  }
  
  prueba_lb <- stats::Box.test(
    residuos,
    lag = 12,
    type = "Ljung-Box",
    fitdf = p + q
  )
  
  prueba_bp <- lmtest::bptest(
    stats::lm(I(residuos^2) ~ ajustados)
  )
  
  prueba_arch <- FinTS::ArchTest(
    residuos,
    lags = 12
  )
  
  prueba_jb <- tseries::jarque.bera.test(residuos)
  
  data.frame(
    Modelo = nombre_modelo,
    `Ljung-Box p` = prueba_lb$p.value,
    `Breusch-Pagan p` = prueba_bp$p.value,
    `ARCH-LM p` = prueba_arch$p.value,
    `Jarque-Bera p` = prueba_jb$p.value,
    check.names = FALSE
  )
}

tabla_supuestos <- dplyr::bind_rows(
  validar_residuos(
    dplyr::filter(residuos_modelos, .model == "arima_110"),
    "ARIMA(1,1,0)",
    p = 1,
    q = 0
  ),
  validar_residuos(
    dplyr::filter(residuos_modelos, .model == "arima_011"),
    "ARIMA(0,1,1)",
    p = 0,
    q = 1
  ),
  validar_residuos(
    dplyr::filter(residuos_modelos, .model == "arima_211"),
    "ARIMA(2,1,1)",
    p = 2,
    q = 1
  )
)

formatear_p <- function(p) {
  ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
}

tabla_presentacion <- tabla_supuestos |>
  dplyr::mutate(
    dplyr::across(-Modelo, formatear_p)
  )

print(tabla_presentacion)

write.csv(
  tabla_presentacion,
  file.path(carpeta_resultados, "tabla_validacion_supuestos.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

tabla_html <- knitr::kable(
  tabla_presentacion,
  format = "html",
  caption = paste(
    "Validación de supuestos (α = 0.05).",
    "Para cada prueba, p > 0.05 indica que no se rechaza la hipótesis nula."
  ),
  align = "lcccc"
) |>
  kableExtra::kable_styling(
    full_width = FALSE,
    position = "center",
    bootstrap_options = c("striped", "condensed")
  )

print(tabla_html)


# 14. DIAGNÓSTICO DE LOS RESIDUOS DEL MODELO FINAL

residuos_finales <- residuos_modelos |>
  dplyr::filter(
    .model == "arima_211",
    is.finite(.resid)
  ) |>
  dplyr::arrange(Fecha)

valores_residuales <- residuos_finales$.resid

grafica_residuos <- ggplot2::ggplot(
  residuos_finales,
  ggplot2::aes(x = as.Date(Fecha), y = .resid)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "gray50",
    linetype = "dashed"
  ) +
  ggplot2::geom_line(
    color = "#0072B2",
    linewidth = 0.7
  ) +
  ggplot2::labs(
    title = "Residuos del modelo ARIMA(2,1,1)",
    x = "Fecha",
    y = "Residuo"
  ) +
  ggplot2::theme_minimal()

print(grafica_residuos)

ggplot2::ggsave(
  file.path(carpeta_figuras, "residuos_arima_211.png"),
  plot = grafica_residuos,
  width = 10,
  height = 6,
  dpi = 300
)

png(
  filename = file.path(carpeta_figuras, "fac_residuos_arima_211.png"),
  width = 1200,
  height = 900,
  res = 150
)

stats::acf(
  valores_residuales,
  lag.max = 36,
  ylim = c(-1, 1),
  main = "FAC de los residuos del ARIMA(2,1,1)"
)

dev.off()

prueba_ljung_final <- stats::Box.test(
  valores_residuales,
  lag = 24,
  type = "Ljung-Box",
  fitdf = 3
)

prueba_normalidad_final <- tseries::jarque.bera.test(
  valores_residuales
)

prueba_arch_final <- FinTS::ArchTest(
  valores_residuales,
  lags = 12
)

print(prueba_ljung_final)
print(prueba_normalidad_final)
print(prueba_arch_final)


# 15. PRONÓSTICO PARA LOS PRÓXIMOS DIEZ MESES

pronostico <- modelo_final |>
  fabletools::forecast(h = 10)

tabla_pronostico <- pronostico |>
  fabletools::hilo(level = 95) |>
  fabletools::unpack_hilo("95%") |>
  dplyr::select(
    Fecha,
    Pronostico = .mean,
    `Limite inferior 95%` = `95%_lower`,
    `Limite superior 95%` = `95%_upper`
  ) |>
  dplyr::mutate(
    dplyr::across(where(is.numeric), ~ round(.x, 4))
  )

print(tabla_pronostico, n = 10)

write.csv(
  as.data.frame(tabla_pronostico),
  file.path(carpeta_resultados, "pronostico_inflacion_10_meses.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# 16. GRÁFICA DEL PRONÓSTICO CON INTERVALOS

grafica_pronostico_intervalos <- ggplot2::autoplot(
  pronostico,
  datos_modelo
) +
  ggplot2::labs(
    title = "Pronóstico de la inflación mensual de Colombia",
    subtitle = "Modelo ARIMA(2,1,1) sobre la serie desestacionalizada",
    x = "Fecha",
    y = "Inflación mensual (%)"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 16
    ),
    panel.grid.minor = ggplot2::element_blank()
  )

print(grafica_pronostico_intervalos)

ggplot2::ggsave(
  file.path(carpeta_figuras, "pronostico_con_intervalos.png"),
  plot = grafica_pronostico_intervalos,
  width = 11,
  height = 6,
  dpi = 300
)


# 17. GRÁFICA DETALLADA DE LOS DIEZ PRONÓSTICOS

datos_pronostico <- pronostico |>
  dplyr::as_tibble() |>
  dplyr::mutate(mes = as.Date(Fecha)) |>
  dplyr::arrange(mes) |>
  dplyr::mutate(
    variacion = .mean - dplyr::lag(.mean)
  )

ultimo_historico <- as.numeric(tail(inflacion_desest, 1))

datos_pronostico <- datos_pronostico |>
  dplyr::mutate(
    variacion = dplyr::if_else(
      dplyr::row_number() == 1,
      .mean - ultimo_historico,
      variacion
    )
  )

grafica_evolucion_pronostico <- ggplot2::ggplot(
  datos_pronostico,
  ggplot2::aes(x = mes, y = .mean)
) +
  ggplot2::geom_line(
    color = "#0072B2",
    linewidth = 1.3
  ) +
  ggplot2::geom_point(
    color = "#D55E00",
    size = 3
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.3f", .mean)),
    vjust = -1,
    size = 3.5
  ) +
  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b\n%Y"
  ) +
  ggplot2::labs(
    title = "Evolución del pronóstico de la inflación en Colombia",
    subtitle = "Modelo ARIMA(2,1,1) | Octubre de 2026 a julio de 2027",
    x = "Mes",
    y = "Inflación mensual desestacionalizada (%)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 15
    ),
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid.minor = ggplot2::element_blank()
  )

print(grafica_evolucion_pronostico)

ggplot2::ggsave(
  file.path(carpeta_figuras, "evolucion_pronostico_10_meses.png"),
  plot = grafica_evolucion_pronostico,
  width = 11,
  height = 6,
  dpi = 300
)


# 18. SERIE HISTÓRICA Y VARIACIÓN PRONOSTICADA

grafica_superior <- ggplot2::ggplot() +
  ggplot2::geom_line(
    data = datos_modelo,
    ggplot2::aes(x = as.Date(Fecha), y = inflacion),
    color = "gray45",
    linewidth = 0.7
  ) +
  ggplot2::geom_line(
    data = datos_pronostico,
    ggplot2::aes(x = mes, y = .mean),
    color = "#0072B2",
    linewidth = 1.2
  ) +
  ggplot2::geom_point(
    data = datos_pronostico,
    ggplot2::aes(x = mes, y = .mean),
    color = "#D55E00",
    size = 2
  ) +
  ggplot2::geom_vline(
    xintercept = as.numeric(as.Date("2026-10-01")),
    linetype = "dashed",
    color = "gray40"
  ) +
  ggplot2::labs(
    title = "Inflación mensual desestacionalizada de Colombia",
    subtitle = "Serie histórica y pronóstico ARIMA(2,1,1)",
    x = NULL,
    y = "Inflación (%)"
  ) +
  ggplot2::theme_minimal(base_size = 12)

grafica_inferior <- ggplot2::ggplot(
  datos_pronostico,
  ggplot2::aes(x = mes, y = variacion)
) +
  ggplot2::geom_hline(
    yintercept = 0,
    color = "gray50"
  ) +
  ggplot2::geom_col(
    fill = "#56B4E9",
    width = 20
  ) +
  ggplot2::labs(
    title = "Cambio mensual del pronóstico",
    x = "Fecha",
    y = "Cambio en puntos porcentuales"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(grafica_superior)
print(grafica_inferior)

ggplot2::ggsave(
  file.path(carpeta_figuras, "serie_historica_y_pronostico.png"),
  plot = grafica_superior,
  width = 11,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  file.path(carpeta_figuras, "variacion_mensual_pronosticada.png"),
  plot = grafica_inferior,
  width = 11,
  height = 6,
  dpi = 300
)

# 19. SIGNIFICANCIA ESTADISTICA DE LOS COEFICIENTES

tabla_significancia <- coeficientes |>
  dplyr::mutate(
    significancia = dplyr::if_else(
      p.value < 0.05,
      "Significativo",
      "No significativo"
    ),
    nivel_significancia = dplyr::case_when(
      p.value < 0.01 ~ "1%",
      p.value < 0.05 ~ "5%",
      p.value < 0.10 ~ "10%",
      TRUE ~ "No significativo al 10%"
    )
  ) |>
  dplyr::select(
    .model,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    significancia,
    nivel_significancia
  )

print(tabla_significancia)

write.csv(
  as.data.frame(tabla_significancia),
  file.path(carpeta_resultados, "significancia_coeficientes_arima_211.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# DIAGNOSTICO Y COMPARACION DE MODELOS ARIMA
# Pronostico de la inflacion mensual de Colombia


# 1. Estimacion de los modelos candidatos


modelos_comparacion <- datos_modelo |>
  fabletools::model(
    arima_011 = fable::ARIMA(inflacion ~ pdq(0, 1, 1)),
    arima_211 = fable::ARIMA(inflacion ~ pdq(2, 1, 1))
  )

# Mostrar los resultados de los modelos
fabletools::report(
  modelos_comparacion |> dplyr::select(arima_011)
)

fabletools::report(
  modelos_comparacion |> dplyr::select(arima_211)
)


# 2. Extraccion de los residuos


modelo_011 <- modelos_comparacion |>
  dplyr::select(arima_011)

modelo_211 <- modelos_comparacion |>
  dplyr::select(arima_211)

# Residuos del ARIMA(0,1,1)
residuos_011 <- residuals(modelo_011)
residuos_011_df <- as.data.frame(residuos_011)
residuos_011_num <- na.omit(residuos_011_df$.resid)

# Residuos del ARIMA(2,1,1)
residuos_211 <- residuals(modelo_211)
residuos_211_df <- as.data.frame(residuos_211)
residuos_211_num <- na.omit(residuos_211_df$.resid)

# Verificar la cantidad de residuos
length(residuos_011_num)
length(residuos_211_num)


# 3. Prueba de autocorrelacion: Ljung-Box


# ARIMA(0,1,1): 12 y 24 rezagos

ljung_011_12 <- fabletools::augment(modelo_011) |>
  fabletools::features(
    .innov,
    feasts::ljung_box,
    lag = 12,
    dof = 1
  )

ljung_011_24 <- fabletools::augment(modelo_011) |>
  fabletools::features(
    .innov,
    feasts::ljung_box,
    lag = 24,
    dof = 1
  )

ljung_011_12
ljung_011_24


# ARIMA(2,1,1): 12 y 24 rezagos

ljung_211_12 <- fabletools::augment(modelo_211) |>
  fabletools::features(
    .innov,
    feasts::ljung_box,
    lag = 12,
    dof = 3
  )

ljung_211_24 <- fabletools::augment(modelo_211) |>
  fabletools::features(
    .innov,
    feasts::ljung_box,
    lag = 24,
    dof = 3
  )

ljung_211_12
ljung_211_24


# 4. Prueba de normalidad: Jarque-Bera

# ARIMA(0,1,1)
normalidad_011 <- tseries::jarque.bera.test(
  residuos_011_num
)

normalidad_011

# ARIMA(2,1,1)
normalidad_211 <- tseries::jarque.bera.test(
  residuos_211_num
)

normalidad_211


# 5. Prueba de heterocedasticidad: ARCH-LM


# ARIMA(0,1,1): 12 y 24 rezagos

arch_011_12 <- FinTS::ArchTest(
  residuos_011_num,
  lags = 12
)

arch_011_24 <- FinTS::ArchTest(
  residuos_011_num,
  lags = 24
)

arch_011_12
arch_011_24


# ARIMA(2,1,1): 12 y 24 rezagos

arch_211_12 <- FinTS::ArchTest(
  residuos_211_num,
  lags = 12
)

arch_211_24 <- FinTS::ArchTest(
  residuos_211_num,
  lags = 24
)

arch_211_12
arch_211_24


# 6. Comparacion de los criterios de informacion


fabletools::glance(modelos_comparacion)


