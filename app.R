library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(units)
library(tidygeocoder)
library(ggplot2)
library(tidyr)
library(rsconnect)

library(ggpubr)
library(plotly)
library(shinydashboard)
library(DT)
library(lubridate)
library(chron)

data = read.csv("crimenAOBJ.csv")
initial_df <- data
initial_df = initial_df[,c(2,3,4)]
data$date = as.Date(data$date)
initial_df$Month =month.name[month(as.POSIXlt(data$date, format="%Y/%m/%d"))]
initial_df$Year =as.integer(year(as.POSIXlt(data$date, format="%Y/%m/%d")))
initial_df$hour = as.integer(substr(initial_df$hour, 0, 2))

initial_df <- initial_df %>% select(date, Year, hour, Month, crime)


sf2<- initial_df %>% group_by(date) %>% summarize(count = n())
sf3<- initial_df %>% group_by(Month) %>% summarize(count = n())


sf3 = sf3 %>%
  mutate(
    Month = factor(Month, levels = month.name)
  ) %>%
  arrange(Month)

invincibleicon <- makeIcon(
  iconUrl = "https://p7.hiclipart.com/preview/801/268/966/robert-kirkman-invincible-compendium-head-of-the-class-invincible-vol-24-the-end-of-all-things-part-1-book-thumbnail.jpg",
  iconWidth = 38, iconHeight = 40
)

ui <- dashboardPage(
  dashboardHeader(title = "Crimen en AO & BJ"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tabsetPanel(
      tabPanel("Mapa",
               fluidRow(
                 column(4,
                        sliderInput("rangoAnios",
                                    label = "Que rango de años quieres visualizar",
                                    min = 2003, max = 2023, value = c(2003, 2023), step = 1, sep = "")
                 ),
                 column(4,
                        textInput("address", "Dame tu dirección dentro de AO o BJ:", placeholder = "rio hondo 1, alvaro obregon")
                 ),
                 column(4,
                        sliderInput("polisize", label = "Selecciona el tamaño del círculo
                                        una vez ingresado mueve un poco el ícono (si no ingresaste dirección)", 
                                    min = 1, max = 30, value = 2, step = 1)
                 ),
                 column(4,
                        checkboxInput("showMap", "Seleccionar para ingresar dirección quitar para mover el ícono", TRUE)
                 )
               ),
               conditionalPanel(condition = "input.showMap == true",
                                leafletOutput("map1", height = 400)
               ),
               conditionalPanel(condition = "input.showMap == false",
                                leafletOutput("map2", height = 400)
               )
      ),
      tabPanel("Gráfico de crimen en tu área",
               plotOutput("graph", height = "750px")
      ),
      
      tabPanel("Analizar más los crímenes", 
               fluidRow(
                 
                 box(
                   title = "Tabla: Crímenes Ocurridos en Benito Juárez y Álvaro Obregón", width = 7, solidHeader = TRUE, status = "primary",
                   DTOutput('table')
                 ),
                 box(
                   title = "Número de Incidencias cometidas por hora", width = 5, solidHeader = TRUE, status = "warning",
                   plotlyOutput("fig")),
                 box(
                   title = "Número de Crímenes cometidos por tipo de crimen", width = 5, solidHeader = TRUE, status = "warning",
                   plotlyOutput("fig2"))
                 )
      
      )
    )
  )
)

server <- function(input, output) {
  
  ## estas lineas son para procesar los datos antes de empezar a hacer las visualizaciones
  library(leaflet)
  library(dplyr)
  
  alvobcrime <- read.csv("crimenAOBJ.csv")
  valores_filtrar <- unique(alvobcrime$crime)
  
  dfs_filtrados <- list()
  # esto es importante para poder tener una lista sobre la que se hacen los grupos
  for (valor in valores_filtrar){
    data_filtrado <- alvobcrime[alvobcrime$crime == valor, ]
    dfs_filtrados[[valor]] <- data_filtrado
  }
  
  
  ## construir los reactive values para la app
  poligono <- reactiveVal(NULL)
  new_longitude <- reactiveVal(NULL)
  new_latitude <- reactiveVal(NULL)
  dataforgraph <- reactiveVal(dfs_filtrados)
  
  
  
  output$graph <- renderPlot({
    datareciente <- dataforgraph()
    
    result <- bind_rows(datareciente)
    datareciente <- result
    custom_pastel_colors <- c(
      "#F28D35", "#FBAB57", "#FFCF9E", "#A1D6E2", "#7EC4CF",
      "#F99A8B", "#FBC0A7", "#FFD9C4", "#C6CCB2", "#ffee93",
      "#BBAC93", "#D0C4BA", "#FFC09F", "#AAE0DC", "#9CADCE",
      "#BFA5B3", "#E0C4D9", "#A0CED9", "#B3CEB2", "#adf7b6"
    )
    datareciente$date <- as.Date(datareciente$date)
    
    datareciente$year <- format(datareciente$date, "%Y")
    datareciente$month <- format(datareciente$date, "%m")
    
    
    datareciente <- datareciente %>% 
      group_by(crime, year, month) %>% 
      summarize(count = n())
 
    
    datareciente <- datareciente %>% 
      mutate(date = paste(year, "-", month, sep= ""))
    
    datareciente$date <- paste(datareciente$date, "-01")
    datareciente$date <- gsub(" ", "", datareciente$date)
    datareciente$date <- as.Date(datareciente$date)
    
    
    
    g <- ggplot(datareciente, aes(x=date, y=count, color=crime)) + geom_point()+
      scale_color_manual(values=custom_pastel_colors) +
      geom_smooth(method = "lm", color = "steelblue")  +
      facet_wrap(~crime, scales="free") + 
      #scale_x_date(breaks = values) +
      scale_x_date(date_labels = "%b %Y")+
      theme(strip.text = element_text(size = 6),
            panel.spacing = unit(1.5, "lines"), 
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    g
    
  })
  
  ## mapa que permite ingresar dirección, pero no permite mover al marcador
  
  output$map1 <- renderLeaflet({
    print("Se está ejecutando desde el principio")
    # nos aseguramos que la columna de fechas si este siendo manejada como fechas
    alvobcrime$date <- as.Date(alvobcrime$date)
    
    chosenDate <- input$rangoAnios
    date1 <- paste(chosenDate[1], "01", "01", sep = "-")
    date2 <- paste(chosenDate[2], "12", "31", sep = "-")
    
    alvobcrime <- alvobcrime %>% filter(date >= as.Date(date1), date <= as.Date(date2))
    
    # vamos a dar la posibilidad de escoger los crimenes que se quieran visualizar
    observeEvent(input$polisize, {
      polisize <- input$polisize
    })

    
    
    cdmx <- st_read("map.geojson")
    
    vis <- leaflet(cdmx) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      setView(lng = -99.19877, lat = 19.34286, zoom = 15)
    
    
    
    copiadatos2 <- dfs_filtrados
    
    
    address <- input$address
    if(address != ""){
      
      
      one_address <- tibble::tribble(
        ~addr, 
        paste(input$address, "ciudad de mexico")
      )
      
      latlong <- one_address %>% geocode(addr, method='osm', lat=latitude, long = longitude)
      
      
      ## guardamos los datos en los reactivevalues
      new_latitude(as.double(latlong[1,2]))
      new_longitude(as.double(latlong[1,3]))
      
    

      # Update the `poligono()` reactive val
      poligono(st_buffer(st_point(c(as.double(latlong[1,3]), as.double(latlong[1,2]))), dist = input$polisize * 0.001))
      #print("An address was entered")
      
      
      
    }
    
    
    
    
    if(!is.null(new_latitude()) && !is.null(new_longitude())){
      

      vis <- vis %>%
        setView(lng = new_longitude(), lat = new_latitude(), zoom = 15) %>% 
        addMarkers(lng= new_longitude(), lat= new_latitude(), icon=invincibleicon)
      
      for(val in valores_filtrar){
        ## primero hay que agarrar el dataset a partir de la lista
        dato_filtrado <- dfs_filtrados[[val]]
        ## hay que añadirle unas columnas para no perder los datos
        dato_filtrado$lat1 <- dato_filtrado$lat
        dato_filtrado$lng1 <- dato_filtrado$long
        ## converitmos a un simple feature para poder hacer el join
        datos <- st_as_sf(dato_filtrado, coords = c("long", "lat"), crs = 4326)
        poligono <- poligono()
        filtrofinal <- st_intersects(datos, poligono)
        datos <- datos[lengths(filtrofinal) > 0, ]
        datos <- st_drop_geometry(datos)
        datos$long <- datos$lng1
        datos$lat <- datos$lat1
        datos <- datos %>% filter(date >= as.Date(date1), date <= as.Date(date2))
        copiadatos2[[val]] <- datos
        
      }
      dataforgraph(copiadatos2)
    }else{ ## default values
      one_address <- tibble::tribble(
        ~addr, 
        "Rio Hondo 1"
      )
      
      latlong <- one_address %>% geocode(addr, method='osm', lat=latitude, long = longitude)
      
      vis <- vis %>% addMarkers(lng=as.double(latlong[1,3]), 
                                lat= as.double(latlong[1,2]), 
                                icon = invincibleicon) %>% 
            addPolygons(data = st_buffer(st_point(c(as.double(latlong[1,3]), as.double(latlong[1,2]))), dist = input$polisize * 0.001), color = "lightblue")
        
      
    }
    
    
    vis <- vis %>%
      addLayersControl(overlayGroups = valores_filtrar, options = layersControlOptions(collapsed = TRUE))
    
    
    if (!is.null(poligono())) {
      vis <- vis %>% addPolygons(data = poligono(), color = "lightblue", fillOpacity = 0.2)
    }
    
    for (val in valores_filtrar){
      vis <- vis %>%
        addMarkers(data = copiadatos2[[val]], popup = paste0(
          "<strong> Crimen: </strong>", val, "<br>", 
          "<strong>Hora: </strong>", copiadatos2[[val]]$hour, "<br>",
          "<strong>Fecha: </strong>", copiadatos2[[val]]$date, "<br>"),
          clusterOptions = markerClusterOptions(), group = val
        )
    }
    
    vis %>% hideGroup(valores_filtrar)
  }) 
  

  
  
  ## mapa que te permite mover el marcador pero que no te permite ingresar la dirección
  
  output$map2 <- renderLeaflet({
    print("Se está ejecutando desde el principio")
    # nos aseguramos que la columna de fechas si este siendo manejada como fechas
    alvobcrime$date <- as.Date(alvobcrime$date)
    
    chosenDate <- input$rangoAnios
    date1 <- paste(chosenDate[1], "01", "01", sep = "-")
    date2 <- paste(chosenDate[2], "12", "31", sep = "-")
    
    alvobcrime <- alvobcrime %>% filter(date >= as.Date(date1), date <= as.Date(date2))
    observeEvent(input$polisize, {
      polisize <- input$polisize
    })
    
    # vamos a dar la posibilidad de escoger los crimenes que se quieran visualizar
    
    copiadatos2 <- dfs_filtrados
    
    cdmx <- st_read("map.geojson")
    
    vis <- leaflet(cdmx) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      setView(lng = -99.19877, lat = 19.34286, zoom = 15)
    

    
    ## ERROR:
    # solucionar el problema del orden de ejecucion de los eventos 
    # el error es diferente cuando muevo el observeEvent encima de inputaddress,
    
    observeEvent(input$map2_marker_dragend, {
      new_latitude(input$map2_marker_dragend$lat)
      new_longitude(input$map2_marker_dragend$lng)
      # Update the `poligono()` reactive val
      print("Se movió el marcador a: ")
      print(new_latitude())
      poligono(st_buffer(st_point(c(new_longitude(), new_latitude())), dist = input$polisize * 0.001))
      
      
    })
    
    
    
    if (!is.null(poligono()) && !is.null(new_latitude())) {
      vis <- vis %>% 
        setView(lng = new_longitude(), lat = new_latitude(), zoom = 15) %>% 
        addMarkers(lng = new_longitude(),
                   lat = new_latitude(), 
                   icon = invincibleicon, 
                   options = markerOptions(draggable = TRUE)) %>% 
        addPolygons(data = poligono(), color = "lightblue") %>% 
        addLayersControl(overlayGroups = valores_filtrar, options = layersControlOptions(collapsed = TRUE))
      for(val in valores_filtrar){
        ## primero hay que agarrar el dataset a partir de la lista
        dato_filtrado <- dfs_filtrados[[val]]
        ## hay que añadirle unas columnas para no perder los datos
        dato_filtrado$lat1 <- dato_filtrado$lat
        dato_filtrado$lng1 <- dato_filtrado$long
        ## converitmos a un simple feature para poder hacer el join
        datos <- st_as_sf(dato_filtrado, coords = c("long", "lat"), crs = 4326)
        poligono <- poligono()
        filtrofinal <- st_intersects(datos, poligono)
        datos <- datos[lengths(filtrofinal) > 0, ]
        datos <- st_drop_geometry(datos)
        datos$long <- datos$lng1
        datos$lat <- datos$lat1
        datos <- datos %>% filter(date >= as.Date(date1), date <= as.Date(date2))
        copiadatos2[[val]] <- datos
        
      }
      dataforgraph(copiadatos2)
      
    }else{
      vis <- vis %>%
        addMarkers(lng = -99.19984800000047, 
                   lat = 19.345087323002147, 
                   icon = invincibleicon, 
                   options = markerOptions(draggable = TRUE)) %>% 
        addPolygons(data = st_buffer(st_point(c(-99.19984800000047, 19.345087323002147)), dist = input$polisize * 0.001), color = "lightblue") %>% 
        addLayersControl(overlayGroups = valores_filtrar, options = layersControlOptions(collapsed = TRUE))
      
      poligono(st_buffer(st_point(c(-99.19984800000047, 19.345087323002147)), dist = input$polisize * 0.001))
      
    }
    
    
    
    
    
    
    for (val in valores_filtrar){
      vis <- vis %>%
        addMarkers(data = copiadatos2[[val]], popup = paste0(
          "<strong> Crimen: </strong>", val, "<br>", 
          "<strong>Hora: </strong>", copiadatos2[[val]]$hour, "<br>",
          "<strong>Fecha: </strong>", copiadatos2[[val]]$date, "<br>"),
          clusterOptions = markerClusterOptions(), group = val
        )
    }
    
    vis %>% hideGroup(valores_filtrar)
    
    ## sección de claudio
    
    
  })
  
  in_react_frame<-reactiveVal(initial_df)
  
  filtered_frame <-  reactive({
    frame <- req(in_react_frame())
    indexes <- req(input$table_rows_all)
    
    frame[indexes,]
  })
  
  summarised_frame <- reactive({req(filtered_frame()) %>% group_by(hour) %>% summarize(count = n())})
  summarised_frame2 <- reactive({req(filtered_frame()) %>% group_by(crime) %>% summarize(count = n())})
  
  output$table <- renderDT(in_react_frame(),
                           filter = "top",
                           options = list(
                             pageLength = 25
                           )
  )
  
  output$fig <- renderPlotly({
    fig <- req(summarised_frame()) %>% plot_ly(x = ~hour, y = ~count, type = "bar")
    fig <- fig %>% layout(title = " ",  showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T, title ='Número de Incidencias'),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T, title ='', categoryorder = "total ascending"))
    
  })
  
  output$fig2 <- renderPlotly({
    fig2 <- req(summarised_frame2()) %>% plot_ly(x = ~count, y = ~crime, labels = ~crime, values = ~count, type = "bar", orientation = 'h')
    fig2 <- fig2 %>% layout(title = " ",  showlegend = F,
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T, title ='Número de Incidencias'),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = T, title ='', categoryorder = "total ascending"))
    
  })
}

shinyApp(ui, server)
