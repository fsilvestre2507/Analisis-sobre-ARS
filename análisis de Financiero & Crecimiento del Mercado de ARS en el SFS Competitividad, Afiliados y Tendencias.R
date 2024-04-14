library(xlsx)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shiny)
library(tseries)
library(forecast)

# Lectura de datos desde un archivo Excel
finanzas <- read.xlsx("C:/Users/18293/Desktop/Ambiente de trabajo de R/SFS.EF1_DATOS.xlsx", range = "A1:K17", sheetIndex = 3)

# Muestra las primeras filas del dataframe
head(finanzas)

# Muestra el dataframe completo
print(finanzas)

# Muestra la estructura del dataframe
str(finanzas)

# Resumen estadístico del dataframe
summary(finanzas)

# Acceder a la columna "Ingresos_en_Salud"
finanzas$Ingresos.en.Salud

# Acceder a la tercera columna (Ingresos en Salud)
finanzas[, 3]

# Filtrar datos para ingresos en salud mayores a 1,000,000,000
finanzas[finanzas$Ingresos.en.Salud > 1000000000, ]

# Calcular el promedio de los ingresos en salud
mean(finanzas$Ingresos.en.Salud)

# Calcular la mediana de los gastos en salud
median(finanzas$Ingresos.en.Salud)

# Calcular la desviación estándar de los gastos generales y administrativos
sd(finanzas$Gastos.Generales.y.Administrativos)

# Convertir el año a formato de fecha
finanzas$Año <- as.Date(paste0(finanzas$Año, "-01-01"))

# Gráfico de ingresos y gastos a lo largo del tiempo
ingresos_y_gastos <- ggplot(finanzas, aes(x = Año)) +
  geom_line(aes(y = Ingresos.en.Salud, color = "Ingresos en Salud"), size = 1) +
  geom_line(aes(y = Otros.Ingresos, color = "Otros Ingresos"), size = 1) +
  geom_line(aes(y = Gastos.en.Salud * -1, color = "Gastos en Salud"), size = 1) +
  geom_line(aes(y = Otros.Gastos * -1, color = "Otros Gastos"), size = 1) +
  labs(title = "Evolución de Ingresos y Gastos",
       x = "Año",
       y = "Monto",
       color = "Concepto") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

# Gráfico de beneficio neto a lo largo del tiempo
beneficio_neto <- ggplot(finanzas, aes(x = Año, y = Beneficio.Neto)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Evolución del Beneficio Neto",
       x = "Año",
       y = "Beneficio Neto",
       color = "Beneficio Neto") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

# Gráfico de solvencia a lo largo del tiempo
finanzas$solvencia <- finanzas$Ingresos.en.Salud / finanzas$Gastos.en.Salud

# Visualiza las métricas
solvencia <- ggplot(finanzas, aes(x = Año, y = solvencia)) +
  geom_line(color = "green", size = 1) +
  labs(title = "Evolución del Índice de Solvencia",
       x = "Año",
       y = "Índice de Solvencia") +
  theme_minimal()

# Mostrar los gráficos
ingresos_y_gastos
beneficio_neto
solvencia


mercado <- read.xlsx("C:/Users/18293/Desktop/Ambiente de trabajo de R/SFS.EF1_DATOS.xlsx", range = "A1:C18", sheetIndex = 5)

head(mercado)

print(mercado)

# Participación de mercado de cada ARS (en porcentaje)
participacion <- c(35.41, 25.90, 11.00, 5.78, 5.72, 3.06, 2.23, 2.20, 1.82, 1.82, 1.11, 1.06, 0.88, 0.81, 0.54, 0.49, 0.16)

# Nombres de las ARS
nombres_ars <- c("SENASA", "PRIMERA ARS", "MAPFRE SALUD ARS, S.A.", "ARS UNIVERSAL, S.A.", 
                 "ARS FUTURO", "ARS SEMMA", "LA MONUMENTAL DE SEGUROS", "ARS RENACER", 
                 "ARS DR. YUNEN", "ARS-SIMAG", "ARS APS", "ARS COLEGIO MEDICO DOMINICANO CMD", 
                 "ADMINISTRADORA DE RIESGOS DE SALUD RESERVAS", "ARS META-SALUD SINATRAE", 
                 "GRUPO MEDICO ASOCIADO", "ADM. SERVICIOS MEDICOS AMOR Y PAZ", 
                 "PLAN SALUD DEL BANCO CENTRAL")

# Calcular el cuadrado de la participación de mercado
cuadrado_participacion <- (participacion / 100)^2

# Calcular el Índice de Herfindahl-Hirschman (IHH)
IHH <- sum(cuadrado_participacion) * 10000  # Multiplicado por 10000 para dar un resultado más legible

# Imprimir el resultado
cat("Índice de Herfindahl-Hirschman (IHH):", IHH, "\n")


# Datos de porcentajes y nombres de ARS
porcentajes <- c(35.41, 25.90, 11.00, 5.78, 5.72, 3.06, 2.23, 2.20, 1.82, 1.82, 1.11, 1.06, 0.88, 0.81, 0.54, 0.49, 0.16)
nombres_ARS <- c("SENASA", "PRIMERA ARS", "MAPFRE SALUD ARS, S.A.", "ARS UNIVERSAL, S.A.", "ARS FUTURO", 
                 "ARS SEMMA", "LA MONUMENTAL DE SEGUROS", "ARS RENACER", "ARS DR. YUNEN", "ARS-SIMAG",
                 "ARS APS", "ARS COLEGIO MEDICO DOMINICANO CMD", "ADMINISTRADORA DE RIESGOS DE SALUD RESERVAS",
                 "ARS META-SALUD SINATRAE", "GRUPO MEDICO ASOCIADO", "ADM. SERVICIOS MEDICOS AMOR Y PAZ",
                 "PLAN SALUD DEL BANCO CENTRAL")

# Crear un dataframe con los datos
datos_ARS <- data.frame(ARS = nombres_ARS, Porcentaje = porcentajes)

# Ordenar el dataframe por porcentaje en orden descendente
datos_ARS <- datos_ARS[order(datos_ARS$Porcentaje, decreasing = TRUE), ]

# Crear el gráfico de barras
barplot(height = datos_ARS$Porcentaje, 
        names.arg = datos_ARS$ARS, 
        las = 2, 
        col = "skyblue",
        main = "Distribución de Cuota de Mercado por ARS",
        xlab = "ARS",
        ylab = "Porcentaje",
        ylim = c(0, max(porcentajes) + 5))  # Ajustar límites del eje y para mejor visualización

# Agregar texto con el valor del IHH
text(x = length(porcentajes) + 0.5, 
     y = max(porcentajes) / 2, 
     labels = paste("IHH:", round(2141.95, 2)),  # Cambiar 2141.95 por el valor real del IHH
     col = "red")


# Lectura de datos desde un archivo Excel
finanzas <- read.xlsx("C:/Users/18293/Desktop/Ambiente de trabajo de R/SFS.EF1_DATOS.xlsx", range = "A1:K17", sheetIndex = 3)


# Muestra las primeras filas del dataframe
head(finanzas)

# Resumen estadístico del dataframe
summary(finanzas)

# Muestra la estructura del dataframe
str(finanzas)

# Identifica las columnas a excluir del cálculo de la matriz de correlación
columnas_excluir <- c(1, 10, 11, which(sapply(finanzas, class) == "character"))  

# Calcula la matriz de correlación
correlation_matrix <- cor(finanzas[, -columnas_excluir])
correlation_matrix

# Identifica las columnas no numéricas
non_numeric_cols <- which(sapply(finanzas, class) == "character")

# Crea un diagrama de dispersión para las variables numéricas
pairs(finanzas[, -c(1, 10, 11, non_numeric_cols)])

# Calcula la matriz de correlación excluyendo columnas no numéricas y "Año"
correlation_matrix <- cor(finanzas[, -columnas_excluir])

# Crea un diagrama de calor para la matriz de correlación
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),  # Paleta de colores
        scale = "none",  # No escala los valores
        main = "Matriz de Correlación",  # Título del diagrama
        xlab = "Variables", ylab = "Variables")  # Etiquetas de los ejes x e y

# Ajuste del modelo de regresión lineal
modelo <- lm(Beneficio.Neto ~ Ingresos.en.Salud + Otros.Ingresos + Gastos.en.Salud + Otros.Gastos, data = finanzas)
summary(modelo)

# Análisis de serie temporal de los ingresos en salud
ts_ingresos <- ts(finanzas$Ingresos.en.Salud, start = 2008)
plot(ts_ingresos)



datos <- read.xlsx("C:/Users/18293/Desktop/Ambiente de trabajo de R/SFS.EF1_DATOS.xlsx", range = "A1:J193", sheetIndex = 2)  # Lee los datos de un archivo de Excel

head(datos)  # Muestra las primeras filas de los datos
View(datos)  # Abre una vista de los datos en un visor de datos

print(datos)  # Imprime los datos en la consola

datos$Periodo <- as.Date(paste(datos$Periodo, "01"), format = "%d/%m/%Y")  # Convierte la columna 'Periodo' a formato de fecha

ggplot(datos, aes(Periodo, Ingresos.en.Salud)) + geom_line() + scale_x_date('month')  + ylab("Ingresos en Salud") +
  xlab("Periodo")  # Grafica los ingresos en salud en función del tiempo

count_ts = ts(datos[, c('Ingresos.en.Salud')])  # Crea una serie temporal con los ingresos en salud

datos$clean_Ingresos.en.Salud = tsclean(count_ts)  # Limpia la serie temporal de valores atípicos

ggplot() +
  geom_line(data = datos, aes(x = Periodo, y = clean_Ingresos.en.Salud)) + ylab('Cleaned Ingresos en Salud')  # Grafica los ingresos limpios en salud

datos$Ingresos.en.Salud_ma = ma(datos$clean_Ingresos.en.Salud, order=30)  # Calcula el promedio móvil mensual de los ingresos limpios en salud
datos$Ingresos.en.Salud_ma30 = ma(datos$clean_Ingresos.en.Salud, order=120)  # Calcula el promedio móvil cuatrimestral de los ingresos limpios en salud

ggplot() +
  geom_line(data = datos, aes(x = Periodo, y = clean_Ingresos.en.Salud, colour = "Cuenta")) +
  geom_line(data = datos, aes(x = Periodo, y = Ingresos.en.Salud_ma,   colour = "Promedio móvil mensual"))  +
  geom_line(data = datos, aes(x = Periodo, y = Ingresos.en.Salud_ma30, colour = "Promedio móvil cuatrimestral"))  +
  ylab('Ingresos en Salud')  # Grafica los ingresos en salud y sus promedios móviles

# Convertir la serie temporal a un objeto ts
count_ma <- ts(na.omit(datos$Ingresos.en.Salud_ma), frequency = 30)

# Realizar la descomposición estacional
decomp <- stl(count_ma, s.window = "periodic")

# Obtener la componente estacional ajustada
deseasonal_Ingresos.en.Salud <- seasadj(decomp)
plot(decomp)

adf.test(count_ma, alternative = "stationary")  # Realiza la prueba ADF para verificar estacionalidad

Acf(count_ma, main='')  # Grafica la función de autocorrelación

Pacf(count_ma, main='')  # Grafica la función de autocorrelación parcial

count_d1 = diff(deseasonal_Ingresos.en.Salud, differences = 1)  # Calcula la primera diferencia de los ingresos desestacionalizados
plot(count_d1)

adf.test(count_d1, alternative = "stationary")  # Realiza la prueba ADF para verificar estacionariedad

Acf(count_d1, main='ACF for Differenced Series')  # Grafica la función de autocorrelación para la serie diferenciada

Pacf(count_d1, main='PACF for Differenced Series')  # Grafica la función de autocorrelación parcial para la serie diferenciada

auto.arima(deseasonal_Ingresos.en.Salud, seasonal=FALSE)  # Selecciona automáticamente un modelo ARIMA

modeloarima<-auto.arima(deseasonal_Ingresos.en.Salud, seasonal=FALSE)  # Ajusta un modelo ARIMA automáticamente
modeloarima

tsdisplay(residuals(modeloarima), lag.max=10, main='(3,1,1) Model Residuals')  # Muestra los residuos

prediccion <- forecast(modeloarima, h=30)  # Genera un pronóstico utilizando el modelo ARIMA ajustado para los próximos 30 periodos
plot(prediccion)  # Grafica el pronóstico generado


# Define la interfaz de usuario (UI) para el dashboard de variables financieras
ui <- fluidPage(
  titlePanel("Dashboard de Variables Financieras"),
  
  # Sidebar layout con pestañas
  sidebarLayout(
    sidebarPanel(
      # Aquí puedes agregar controles de selección si deseas filtrar los datos
    ),
    
    # Panel principal con pestañas
    mainPanel(
      tabsetPanel(
        tabPanel("Gráfico de Líneas", plotOutput("line_chart")),  # Pestaña para el gráfico de líneas
        tabPanel("Ajuste del modelo ARIMA", plotOutput("arima_plot")),  # Pestaña para el modelo ARIMA
        tabPanel("Gráfico de Ingresos en Salud", plotOutput("ingresos_salud_plot")),  # Pestaña para el gráfico de ingresos en salud
        tabPanel("Gráfico de Tendencias de Ingresos en Salud", plotOutput("tendencias_ingresos_salud_plot")),  # Pestaña para el gráfico de tendencias de ingresos en salud
        tabPanel("Gráfico ACF", plotOutput("acf_plot")),  # Pestaña para el gráfico ACF
        tabPanel("Gráfico PACF", plotOutput("pacf_plot")),  # Pestaña para el gráfico PACF
        tabPanel("Gráfico ACF para Serie Diferenciada", plotOutput("acf_diff_plot")),  # Pestaña para el gráfico ACF de la serie diferenciada
        tabPanel("Gráfico PACF para Serie Diferenciada", plotOutput("pacf_diff_plot")),  # Pestaña para el gráfico PACF de la serie diferenciada
        tabPanel("Gráfico Pairs", plotOutput("pairs_plot")),  # Pestaña para el gráfico Pairs
        tabPanel("Heatmap de Correlación", plotOutput("heatmap_plot")),  # Pestaña para el heatmap de correlación
        tabPanel("Gráfico de Series Temporales", plotOutput("ts_plot")),  # Pestaña para el gráfico de series temporales
        tabPanel("Gráfico de Cuota de Mercado por ARS", plotOutput("barplot_ARS")),  # Pestaña para el gráfico de cuota de mercado por ARS
        tabPanel("Gráfico de Predicción", plotOutput("prediction_plot")),  # Pestaña para el gráfico de predicción
        tabPanel("Gráfico de Residuos del Modelo ARIMA", plotOutput("residuals_plot")),  # Pestaña para el gráfico de residuos del modelo ARIMA
        tabPanel("Gráfico de Ingresos en Salud Limpio", plotOutput("cleaned_line_plot")),  # Pestaña para el gráfico de ingresos en salud limpio
        tabPanel("Evolución de Ingresos y Gastos", plotOutput("ingresos_gastos_plot")),  # Pestaña para el gráfico de ingresos y gastos
        tabPanel("Evolución del Beneficio Neto", plotOutput("beneficio_neto_plot")),  # Pestaña para el gráfico del beneficio neto
        tabPanel("Evolución del Índice de Solvencia", plotOutput("solvencia_plot"))  # Pestaña para el gráfico del índice de solvencia
      )
    )
  )
)

# Define el servidor
server <- function(input, output) {
  # Función para graficar y mostrar el gráfico de líneas
  output$line_chart <- renderPlot({
    ggplot(data = finanzas, aes(x = Año)) +
      geom_line(aes(y = Ingresos.en.Salud, color = "Ingresos en Salud")) +
      geom_line(aes(y = Otros.Ingresos, color = "Otros Ingresos")) +
      geom_line(aes(y = Gastos.en.Salud, color = "Gastos en Salud")) +
      geom_line(aes(y = Otros.Gastos, color = "Otros Gastos")) +
      geom_line(aes(y = Gastos.Generales.y.Administrativos, color = "Gastos Generales y Administrativos")) +
      geom_line(aes(y = Otros.Ingresos..Gastos., color = "Otros Ingresos (Gastos)")) +
      geom_line(aes(y = Impuestos.Reservas, color = "Impuestos Reservas")) +
      geom_line(aes(y = Beneficio.Neto, color = "Beneficio Neto")) +
      geom_line(aes(y = X..Siniestralidad, color = "Índice de Siniestralidad")) +
      geom_line(aes(y = X..Beneficios, color = "Índice de Beneficios")) +
      labs(x = "Año", y = "Valor", color = "Variables") +
      theme_minimal() +
      ggtitle("Variables Financieras a lo largo del tiempo")
  })
  
  # Función para ajustar y mostrar el modelo ARIMA
  output$arima_plot <- renderPlot({
    # Datos de la serie temporal para "Ingresos en Salud"
    ingresos_salud <- c(12258558889, 15225649669, 20017278082, 22354692159, 25479956719, 27882205477, 31170212067, 34399435636, 39576299818, 43303077249, 50188825577, 54180854258, 60195837902, 64049897469, 76678782618, 82501518087)
    # Convertir los datos a una serie temporal
    ts_ingresos_salud <- ts(ingresos_salud, start = 2008, frequency = 1)
    # Ajustar el modelo ARIMA
    modelo_arima <- arima(ts_ingresos_salud, order = c(1, 2, 0))
    # Graficar los datos originales y el ajuste del modelo
    plot(forecast(modelo_arima), main = "Ajuste del modelo ARIMA para Ingresos en Salud", xlab = "Año", ylab = "Ingresos en Salud")
    lines(ts_ingresos_salud, col = "blue")
    legend("topleft", legend = c("Datos Originales", "Ajuste ARIMA"), col = c("blue", "black"), lty = 1)
  })
  
  # Función para mostrar el gráfico de Ingresos en Salud
  output$ingresos_salud_plot <- renderPlot({
    ggplot(datos, aes(Periodo, Ingresos.en.Salud)) + 
      geom_line() + 
      scale_x_date('month')  + 
      ylab("Ingresos en Salud") +
      xlab("Periodo")
  })
  
  # Función para mostrar el gráfico de tendencias de Ingresos en Salud
  output$tendencias_ingresos_salud_plot <- renderPlot({
    ggplot() +
      geom_line(data = datos, aes(x = Periodo, y = clean_Ingresos.en.Salud, colour = "Cuenta")) +
      geom_line(data = datos, aes(x = Periodo, y = Ingresos.en.Salud_ma,   colour = "Promedio móvil mensual"))  +
      geom_line(data = datos, aes(x = Periodo, y = Ingresos.en.Salud_ma30, colour = "Promedio móvil cuatrimestral"))  +
      ylab('Ingresos en Salud')
  })
  
  # Función para mostrar el gráfico ACF
  output$acf_plot <- renderPlot({
    Acf(count_ma, main='ACF')
  })
  
  # Función para mostrar el gráfico PACF
  output$pacf_plot <- renderPlot({
    Pacf(count_ma, main='PACF')
  })
  
  # Función para mostrar el gráfico ACF para la serie diferenciada
  output$acf_diff_plot <- renderPlot({
    Acf(count_d1, main='ACF for Differenced Series')
  })
  
  # Función para mostrar el gráfico PACF para la serie diferenciada
  output$pacf_diff_plot <- renderPlot({
    Pacf(count_d1, main='PACF for Differenced Series')
  })
  
  # Función para mostrar el gráfico de residuos del modelo ARIMA
  output$residuals_plot <- renderPlot({
    tsdisplay(residuals(modelo_arima), lag.max=10, main='(3,1,1) Model Residuals')
  })
  
  # Función para mostrar el gráfico de predicción
  output$prediction_plot <- renderPlot({
    plot(prediccion)
  })
  
  # Función para mostrar el gráfico Pairs
  output$pairs_plot <- renderPlot({
    pairs(finanzas[, -c(1, 10, 11, non_numeric_cols)])
  })
  
  # Función para mostrar el heatmap de correlación
  output$heatmap_plot <- renderPlot({
    heatmap(correlation_matrix, 
            col = colorRampPalette(c("blue", "white", "red"))(100),  # Especifica la paleta de colores
            scale = "none",  # No escala los valores
            main = "Matriz de Correlación",  # Título del diagrama
            xlab = "Variables", ylab = "Variables")  # Etiquetas de los ejes x e y
  })
  
  # Función para mostrar el gráfico de series temporales
  output$ts_plot <- renderPlot({
    plot(ts_ingresos)
  })
  
  # Calcular el valor del IHH
  ihh_value <- round(2141.95, 2)  # Cambiar 2141.95 por el valor real del IHH
  
  # el gráfico de cuota de mercado por ARS
  output$barplot_ARS <- renderPlot({
    barplot(height = datos_ARS$Porcentaje, 
            names.arg = datos_ARS$ARS, 
            las = 2, 
            col = "skyblue",
            main = "Distribución de Cuota de Mercado por ARS",
            xlab = "ARS",
            ylab = "Porcentaje",
            ylim = c(0, max(porcentajes) + 5))  # Ajustar límites del eje y para mejor visualización
    
    # Agregar texto con el valor del IHH
    text(x = length(porcentajes) + 0.5, 
         y = max(porcentajes) / 2, 
         labels = paste("IHH:", ihh_value),  # Agregar el valor del IHH
         col = "red")
  })
  # el gráfico de predicción
  output$prediction_plot <- renderPlot({
    # Aquí reemplaza "prediccion" con tus datos y métodos adecuados para generar el gráfico de predicción
    plot(prediccion)
  })
  # gráfico de residuos del modelo ARIMA
  output$residuals_plot <- renderPlot({
    # Aquí reemplaza "modeloarima" con tu modelo ARIMA y ajusta la lógica para generar el gráfico de residuos
    tsdisplay(residuals(modeloarima), lag.max = 10, main = "(3,1,1) Model Residuals")
  })
  # Ejemplo de cómo puedes generar el gráfico de ingresos en salud limpio
  output$cleaned_line_plot <- renderPlot({
    ggplot() +
      geom_line(data = datos, aes(x = Periodo, y = clean_Ingresos.en.Salud)) + 
      ylab("Ingresos en Salud") +
      xlab("Periodo") +
      ggtitle("Ingresos en Salud (con limpieza de anomalías)")
  })
  # Gráfico de ingresos y gastos a lo largo del tiempo
  output$ingresos_gastos_plot <- renderPlot({
    ggplot(finanzas, aes(x = Año)) +
      geom_line(aes(y = Ingresos.en.Salud, color = "Ingresos en Salud"), size = 1) +
      geom_line(aes(y = Otros.Ingresos, color = "Otros Ingresos"), size = 1) +
      geom_line(aes(y = Gastos.en.Salud * -1, color = "Gastos en Salud"), size = 1) +
      geom_line(aes(y = Otros.Gastos * -1, color = "Otros Gastos"), size = 1) +
      labs(title = "Evolución de Ingresos y Gastos",
           x = "Año",
           y = "Monto",
           color = "Concepto") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal()
  })
  
  # Gráfico de beneficio neto a lo largo del tiempo
  output$beneficio_neto_plot <- renderPlot({
    ggplot(finanzas, aes(x = Año, y = Beneficio.Neto)) +
      geom_line(color = "blue", size = 1) +
      labs(title = "Evolución del Beneficio Neto",
           x = "Año",
           y = "Beneficio Neto",
           color = "Beneficio Neto") +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal()
  })
  
  # Gráfico de solvencia a lo largo del tiempo
  output$solvencia_plot <- renderPlot({
    ggplot(finanzas, aes(x = Año, y = solvencia)) +
      geom_line(color = "green", size = 1) +
      labs(title = "Evolución del Índice de Solvencia",
           x = "Año",
           y = "Índice de Solvencia") +
      theme_minimal()
  })
}  
# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)