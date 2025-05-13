library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

# Paleta de colores corporativos
eroski_colores <- list(
  rojo_principal = "#E6001F",
  rojo_oscuro = "#CC001C",
  rojo_claro = "#FF3347",
  rojo_intenso = "#B30019",
  rojo_profundo = "#990015",
  azul_corporativo = "#005FAA",
  azul_oscuro = "#004C88",
  azul_medio = "#337FCC",
  azul_pastel = "#669FCC",
  azul_muy_oscuro = "#003366"
)

# Función para gráficos de barras
graficar_barras <- function(data, xvar, yvar, titulo = "", 
                            xlab = NULL, ylab = NULL, color = "#E6001F", flip = TRUE, etiquetas = TRUE) {
  xlab <- xlab %||% xvar
  ylab <- ylab %||% yvar
  
  p <- ggplot(data, aes(x = reorder(.data[[xvar]], .data[[yvar]]), y = .data[[yvar]])) +
    geom_col(fill = color) +
    labs(title = titulo, x = xlab, y = ylab) +
    theme_minimal()
  
  if (etiquetas) {
    p <- p + geom_text(aes(label = .data[[yvar]]), hjust = ifelse(flip, 1.1, 0), color = "black", size = 3)
  }
  
  if (flip) {
    p <- p + coord_flip()
  }
  
  return(p)
}

# Carga de datos
df_ticket <- read.csv("Datos/Transformados/tickets_enc_Bien.csv")
df_maestro <- read.csv("Datos/Transformados/maestroestr.csv")
df <- merge(df_ticket, df_maestro, by = "cod_est")
df$dia <- as.Date(df$dia)
df_cluster<- read.csv("Resultados/clientes_con_cluster.csv")


# Datos preprocesados
ventas_por_mes <- df %>%
  mutate(Mes = month(dia, label = TRUE, abbr = FALSE)) %>%
  group_by(Mes) %>%
  summarise(Total = n())

df$dia_semana <- wday(df$dia, label = TRUE, abbr = FALSE, week_start = 1)
df$dia_semana <- factor(df$dia_semana,
                        levels = c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))
ventas_por_dia <- df %>%
  group_by(dia_semana) %>%
  summarise(cantidad = n())

# UI
ui <- navbarPage(
  "EROSKI",
  
  tabPanel("Análisis exploratorio",
           sidebarLayout(
             sidebarPanel(
               conditionalPanel(
                 condition = "input.subtab == 'productos'",
                 sliderInput("n_productos", "Número de productos a mostrar:",
                             min = 5, max = 30, value = 10)
               ),
               conditionalPanel(
                 condition = "input.subtab == 'producto_mes'",
                 selectInput("producto_seleccionado", "Selecciona un producto:",
                             choices = NULL)
               )
             ),
             mainPanel(
               tabsetPanel(id = "subtab",
                           tabPanel("Productos más vendidos", value = "productos", plotOutput("plot_productos")),
                           tabPanel("Ventas por mes", value = "mes", plotOutput("plot_mes")),
                           tabPanel("Día de la semana", value = "dia", plotOutput("plot_dia")),
                           tabPanel("Producto por mes", value = "producto_mes", plotOutput("plot_producto_mes"))
               )
             )
           )
  ),
  
  tabPanel("Visualización de cada cluster",
           fluidPage(
             h3("Visualización de clusters"),
             p("Aquí podrás mostrar los gráficos de segmentación de clientes o productos según los clusters."),
             p("Por ejemplo: clusters de comportamiento de compra, K-means, dendrogramas, etc."),
             plotOutput("plot_cluster_placeholder")
           )
  ),
  
  tabPanel("Resultado de modelización",
           fluidPage(
             h3("Modelización de comportamiento"),
             p("Aquí podrás incluir resultados de modelos predictivos, clasificación, regresión, etc."),
             p("Por ejemplo: predicción de gasto por cliente, recomendación de productos..."),
             verbatimTextOutput("modelo_placeholder")
           )
  )
)

# SERVER
server <- function(input, output, session) {
  # Actualizar productos disponibles
  observe({
    updateSelectInput(session, "producto_seleccionado",
                      choices = sort(unique(df$descripcion)),
                      selected = unique(df$descripcion)[1])
  })
  
  # Productos más vendidos
  output$plot_productos <- renderPlot({
    top_n <- input$n_productos
    
    productos_top <- df %>%
      group_by(descripcion) %>%
      summarise(frecuencia = n()) %>%
      arrange(desc(frecuencia)) %>%
      slice_head(n = top_n)
    
    graficar_barras(productos_top, "descripcion", "frecuencia",
                    paste("Top", top_n, "productos más vendidos"),
                    "Producto", "Frecuencia",
                    color = eroski_colores$rojo_principal)
  })
  
  # Ventas por mes
  output$plot_mes <- renderPlot({
    graficar_barras(ventas_por_mes, "Mes", "Total",
                    "Total de productos por mes", "Mes", "Cantidad",
                    color = eroski_colores$rojo_claro, flip = FALSE)
  })
  
  # Ventas por día de la semana
  output$plot_dia <- renderPlot({
    graficar_barras(ventas_por_dia, "dia_semana", "cantidad",
                    "Compras por día de la semana", "Día", "Cantidad",
                    color = eroski_colores$rojo_intenso, flip = FALSE)
  })
  
  # Producto seleccionado por mes
  output$plot_producto_mes <- renderPlot({
    req(input$producto_seleccionado)
    
    datos <- df %>%
      filter(descripcion == input$producto_seleccionado) %>%
      mutate(Mes = month(dia, label = TRUE, abbr = FALSE)) %>%
      group_by(Mes) %>%
      summarise(Frecuencia = n()) %>%
      arrange(Mes)
    
    graficar_barras(datos, "Mes", "Frecuencia",
                    paste("Compras por mes del producto:", input$producto_seleccionado),
                    "Mes", "Cantidad",
                    color = eroski_colores$azul_corporativo, flip = FALSE)
  })
  
  # Placeholder para otras pestañas
  output$plot_cluster_placeholder <- renderPlot({
    plot(1, 1, main = "Gráfico de ejemplo para clusters", type = "n")
    text(1, 1, "Aquí irán los gráficos de clustering")
  })
  
  output$modelo_placeholder <- renderPrint({
    cat("Aquí irán los resultados de tu modelo de predicción o clasificación.")
  })
}

# Ejecutar app
shinyApp(ui = ui, server = server)




##
select_var_num = function(df){
  df_num = df %>% select(where(is.numeric))
  return(df_num)
}
df_cluster$Cluster<- as.factor(df_cluster$Cluster)
df_cluster<- df_cluster[,-1]
df_names = colnames(df_cluster)
df_num = select_var_num(df_cluster)
df_cha = df_cluster %>% select(-all_of(colnames(df_num)))
str(df_cluster)


paleta = c("#E6001F", "#CC001C", "#FF3347", "#B30019", "#990015" ,"#005FAA", "#004C88")
paleta3 = c("#FF3347", "#FFFFFF", "#005FAA")

ggplot(df_cluster, aes(x = Cluster, y = total_productos, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Total de productos comprados por Cluster",
       x = "Cluster", 
       y = "Total de productos comprados") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0,325, 25))

ggplot(df_cluster, aes(x = Cluster, y = productos_distintos, fill = Cluster)) +
  geom_violin() +
  labs(title = "Cantidad de productos distintos comprados por Cluster",
       x = "Cluster", 
       y = "Cantidad de productos distintos comprados") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0, 175, 25))

damedia = df_cluster %>% group_by(Cluster) %>% summarise(media = mean(dias_activos))

ggplot(damedia, aes(x = Cluster, y = media, fill = Cluster)) +
  geom_col(color = "#000000") +
  labs(title = "Media de días activos por Cluster",
       x = "Cluster", 
       y = "Media de días activos") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0,3,1))

ggplot(df_cluster, aes(x = Cluster, y = media_compras_por_semana, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Media de productos diferentes comprados por semana por Cluster",
       x = "Cluster", 
       y = "Media de productos diferentes comprados por semana") +
  scale_fill_manual("Cluster", values = paleta3) 

ggplot(df_cluster, aes(x = Cluster, y = compras_entre_semana, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Productos diferentes comprados de lunes a jueves por Cluster",
       x = "Cluster", 
       y = "Productos diferentes comprados de lunes a jueves") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0, 275, 25))

ggplot(df_cluster, aes(x = Cluster, y = compras_fin_de_semana, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Productos diferentes comprados de viernes a domingo por Cluster",
       x = "Cluster", 
       y = "Productos diferentes comprados de viernes a domingo") +
  scale_fill_manual("Cluster", values = paleta3) +
  scale_y_continuous(breaks = seq(0, 75, 25))

