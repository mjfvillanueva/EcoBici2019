## shiny dashboard TP FINAL EANT
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("sf")
#install.packages("tidyr")
#install.packages("DT")
#install.packages("shinyWidgets")
#install.packages("ggplot2")
#install.packages("leaflet")
#install.packages("htmltools")
#install.packages("httr")
#install.packages("jsonlite")
#install.packages("tidyverse")
#install.packages("vembedr")
#install.packages("plotly")
#install.packages("plotrix")
#install.packages("lubridate")


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(leaflet)
library(htmltools)
library(httr)
library(jsonlite)
library(tidyr)
library(sf)
library(tidyverse)
library(vembedr)
library(DT)
library(plotly)
library(dplyr)
library(plotrix)
library(lubridate)



#api del clima SMN viene completa
apiclima<-"https://ws.smn.gob.ar/map_items/weather"

requestclima<- GET( url=apiclima)

#requestclima$status_code
responseclima<-content(requestclima, as="text", encoding="UTF-8")

df_clima<-fromJSON(responseclima)%>%data.frame()

# solo tomo la de CABA
actual <-df_clima%>%filter(name=="Capital Federal" & province=="Capital Federal")


#levanto dataset de estaciones de ecobici
df_estaciones <-read.csv("./Estaciones.csv")

#levanto dataset de barrios de capital fed
df_barrios <- st_read("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

#preparo dataset de promedios por barrio
df_prom_barrios <- read.csv("./PromedioBarrios.csv")
df_prom_barrios <- df_barrios%>%inner_join(df_prom_barrios, by=c('barrio'='data.stations.groups'))


# abro dataset validacion
df_validacion <-read.csv("./Validacion.csv",sep=",")

# datasets de fallas
df_tabla_fallas <-read.csv("./TablaFallas.csv")

#df_mapa_fallas <-read.csv("./MapaFallas.csv")

#df_fallas <-read.csv("./Fallas.csv")%>%arrange(desc(diferencia_reportada_int))
#df_fallas$X<-NULL
#df_fallas$diferencia_reportada_int<-NULL
#colnames(df_fallas)=c("Estacion","Desconexion desde","Desconexion hasta","Tiempo sin reportar")

df_grafico <-read.csv("./Grafico.csv", stringsAsFactors = FALSE)
df_grafico$Fecha= as.Date(df_grafico$Fecha)
df_grafico= df_grafico%>%mutate(Horas_falla =  Tiempo_falla/60/60)
# viene en segundos, lo paso su tipo de dato
df_grafico$Tiempo_falla=  as.duration(df_grafico$Tiempo_falla)



## iconos que despues voy a usar en el mapa
icon_bike_available <- makeIcon(
  iconUrl = "./cycling.png",
  iconWidth = 35, iconHeight = 35,
  iconAnchorX = 0, iconAnchorY = 0
)

icon_dock_available <- makeIcon(
  iconUrl = "./bicycle_parking.png",
  iconWidth = 35, iconHeight = 35,
  iconAnchorX = 0, iconAnchorY = 0
)

icon_bike_station <- makeIcon(
  
  iconUrl = "./cycling_station.png",
  iconWidth = 35, iconHeight = 35,
  iconAnchorX = 0, iconAnchorY = 0,
  className = "bike_icon"
)


ui <- dashboardPage(skin="purple",
 
 dashboardHeader(title = "Ecobici 2019"), 
 
  dashboardSidebar(
    sidebarMenu(
      # items del menu principal
      menuItem("Introducción", tabName = "intro", icon = icon("book-reader"),selected=TRUE ),
      menuItem("Análisis Exploratorio", tabName = "datosgenerales", icon = icon("th") ,startExpanded = TRUE ,
               menuSubItem("Datos Generales", tabName = "analisis",icon = icon("bicycle")),
               menuSubItem("Fallas de reporte", tabName = "masdatos", icon = icon("map"))),
      menuItem("Modelos", tabName = "analisis_predictivo", icon = icon("th")  ),
      menuItem("Predicción interactiva", tabName = "predictivo", icon = icon("th")),
      menuItem("Próximos pasos", tabName = "prox",icon = icon("th")),
      menuItem("Más información", tabName = "masinfo",icon = icon("asterisk"))
  
    )
  ),
 
  dashboardBody(
    tabItems(
      
      # Tab de introduccion
      tabItem(tabName = "intro", #h2("Introduccion"), 
              fluidRow(
                    column(12, align="center",              
                    embed_youtube( "dt-5F2nlGSA" ,height=600, width=800))
                ),
              br(),
              fluidRow(
                box(width = 9,
                    
                    leafletOutput("mapaintro", height = 495)
                    
                ),
                infoBox(width = 3, "Mayo", 2019, icon = icon("calendar-alt"), fill = TRUE, color = "purple"),
                infoBox(width = 3, "Barrios de CABA", 32, icon = icon("building"), fill = TRUE, color = "red"),
                infoBox(width = 3, "Estaciones", 399, icon = icon("map-marked"), fill = TRUE, color = "yellow"),
                infoBox(width = 3, "Bicicletas", 4000, icon = icon("bicycle"), fill = TRUE, color = "aqua"),
                infoBox(width = 3, "Usuarios aprox.", 260000, icon = icon("users"), fill = TRUE, color = "orange")
              )
              
              
              ),
     
      # Tab de analisis exploratorio
        tabItem(tabName = "analisis",h2("Promedios de todas las estaciones"),
                fluidRow(
                         valueBox(19, "Capacidad", icon = icon("check-circle"), width = 3, color="purple"),   
                         valueBox(2, "Bicicletas disponibles", icon = icon("bicycle"), width = 3, color="red"),
                         valueBox(14, "Docks disponibles", icon = icon("parking"), width = 3, color="yellow"),
                         valueBox(3, "Bicicletas deshabilitadas", icon = icon("wrench"), width = 3, color="aqua")
                ),
                h2("Promedios por barrio"),
                br(),
                fluidRow(
                         leafletOutput("mapapromedio", height = 500)
                )
        ),
      
      # tab de analisis predictivo
      tabItem(tabName = "analisis_predictivo",
              fluidRow(
                column(12, align="center",              
                       embed_youtube( "J59JGKYvAj4" ,height=600, width=800))

              )),
      
      # tab de prediccion interactiva!
        tabItem("predictivo",h2("Análisis predictivo de la disponibilidad de bicicletas y anclajes") ,
                
                fluidRow(
                  column( 8,
                        #title="Estaciones Ecobici", 
                        leafletOutput("mapapredictivo", height = 550)
                        
                        
                      ),
                  
                  column(width = 4,
                      box(width = 12,
                        tabsetPanel(
                        tabPanel("Opciones", 
                                 fluidRow(
                                   column(width=8,offset=1,
                                 radioButtons(inputId = "dia",label="Seleccione el dia", choices=c("Lunes","Martes","Miercoles","Jueves","Viernes","Sabado","Domingo"), selected = "Jueves"),
                                 selectInput(inputId = "sale",label="Retira la bicicleta en", multiple = FALSE,choices=levels(df_estaciones$data.stations.groups), selected = first(levels(df_barrios$barrio))),
                                 selectInput(inputId = "llega",label="Deja la bicicleta en ", multiple = FALSE,choices=levels(df_estaciones$data.stations.groups), selected = last(levels(df_barrios$barrio))),
                                 numericInput(inputId = "horario", label="Horario",value="9", width = 100)
                                                )
                                          )
                                 ),
                        tabPanel("Clima", 
                                 fluidRow(
                                   column(width=8,offset=1,
                                          br(),
                                          htmlOutput("clima"),
                                          uiOutput("temperatura"),
                                          uiOutput("humedad"),
                                          uiOutput("presion"),
                                          uiOutput("dir_viento"),
                                          uiOutput("vel_viento")
                                          )
                                          )
                                )
                                )),
                      column(width=8,actionButton(inputId = "predict", label="Analizar", width = 100))
                  )
                    ),
                fluidRow(
                  
                  column(12,h3(textOutput("estacion_salida")),
                         dataTableOutput("predict_bici")),
                  br(),
                  column(12,h3(textOutput("estacion_llegada")),
                         dataTableOutput("predict_dock"))
                         
                  
                )
                 ),
      
        tabItem("masdatos",
                h2("last_reported -> fecha y hora de último reporte de la estación"),
                br(),
                fluidRow(
                        valueBox(5, "Semanas", icon = icon("calendar-alt"), width = 2,  color="purple"),   
                        valueBox(590000, "Observaciones", icon = icon("download"), width = 2,  color="red"),
                        valueBox(399, "Estaciones activas", icon = icon("map-marked"), width = 2, color="yellow"),
                        valueBox(140, "Fallas en la conexión", icon = icon("wrench"), width = 2, color="purple"),
                        valueBox(74, "Estaciones con fallas", icon = icon("wrench"), width = 2, color="aqua"),
                        valueBox("60%", "Presentó UNA sola falla", icon = icon("wrench"), width = 2, color="red")
                        ),        
                fluidRow(
                        tabsetPanel(
                          tabPanel("Todas las estaciones", 
                                   
                                   column(4,
                                          plotlyOutput("boxplot1", height = 500)
                                   ),
                                   column(8,
                                          plotlyOutput("graph_fallas_cant_tot", height =250),
                                          plotlyOutput("graph_fallas_tiempo_tot", height = 250)
                                   )
                                   
                          ),
                          tabPanel("Datos por estación", 
                                   column(width=5,
                                          leafletOutput("mapafallas",height = 500)
                                   ),
                                   column(width=7,
                                          h3(textOutput("texto_fallas")),
                                          plotlyOutput("graph_fallas_cant", height =200),
                                          plotlyOutput("graph_fallas_tiempo", height = 200)
                                   )           
                           
                          )
                                  )
                        )
                
                  #box(width=12,
                  #  title = "Listado de estaciones con mas de 2 fallas", solidHeader = TRUE,status = "danger",
                  #  dataTableOutput("tablafallas")
                  #)
              ),
    
      # Tab proximamente
      tabItem("prox",
              column(12 , align="center",
              imageOutput("conclusion", height = "auto")
              )
              #tags$img(src="https://onedrive.live.com/?cid=3BDF574B2871CCEE&id=3BDF574B2871CCEE%21873&parId=3BDF574B2871CCEE%21872&o=OneUp", 
                 #       height=200)

              ),
     
      # Tab mas informacion...
      tabItem("masinfo",
                box(width=4, title = "Autores",solidHeader = TRUE,status = "warning",
                    h5("Betsabe Cohen"),
                    h5("Matias Amodeo"),
                    h5("Nicolas Toranzo"),
                    h5("Maria Jose Ferreyra V.")
                )
              )
          )
      )
)




server <- function(input, output) {
  options(shiny.trace=T)
  
  
  df_barrios_sel <- reactive ({
    
    df_barrios_sel <- (df_barrios) %>% filter( barrio ==input$sale | barrio ==input$llega)
    
    df_barrios_sel
  })
  
  
  df_estaciones_salida <- reactive ({
    
    df_estaciones_salida <- (df_estaciones) %>% filter( data.stations.groups ==input$sale)
    
    df_estaciones_salida
  })
  

  df_estaciones_llegada <- reactive ({
    
    df_estaciones_llegada <- (df_estaciones) %>% filter( data.stations.groups ==input$llega)
    
    df_estaciones_llegada
  })
  
  output$mapaintro <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong><br/>",
      df_estaciones$data.stations.name
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(df_estaciones) %>%
      addTiles()%>%
      addMarkers(lng=~df_estaciones$data.stations.lon, lat=~df_estaciones$data.stations.lat,
                 #popup = ~htmlEscape(df_estaciones$data.stations.name),
                 label=labels,
                 icon = icon_bike_station)
    
  })
  
  output$mapapredictivo <- renderLeaflet({
    
    labels <- sprintf(
      "<strong>%s</strong><br/>",
      df_estaciones_salida()$data.stations.name
    ) %>% lapply(htmltools::HTML)
    
    
    leaflet(df_barrios_sel()) %>%
      addTiles()%>%
      addPolygons(color = "orange", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.3,
                                    popup = ~htmlEscape(df_barrios_sel()$barrio),
                  highlightOptions = highlightOptions(color = "red", weight = 3,
                                                    bringToFront = TRUE))%>%
      addMarkers(lng=~df_estaciones_salida()$data.stations.lon, lat=~df_estaciones_salida()$data.stations.lat,
                 #popup = ~htmlEscape(df_estaciones_salida()$data.stations.name),
                 label=labels,
                 icon = icon_bike_available)%>%
      
      addMarkers(lng=~df_estaciones_llegada()$data.stations.lon, lat=~df_estaciones_llegada()$data.stations.lat,
               #popup = ~htmlEscape(df_estaciones_llegada()$data.stations.name),
               label=labels,
               icon = icon_dock_available)
    
     # addMarkers(lng=~df_salida()$data.stations.lon, lat=~df_salida()$data.stations.lat,
    #             popup = paste(paste0("<strong>",df_salida()$Estacion,"</strong>"),
       #                        paste0("Promedio predicho: ",round(df_salida()[,'Promedio predicho'])),
      #                         paste0("Minimo predicho: ",round(df_salida()[,'Minimo predicho'])),
      #                         sep = "<br/>"),
      #           icon = icon_bike_available)
    
    
    
    
  })
  
  output$mapapromedio <- renderLeaflet({
    
    bins <- c(0,1,2,3,Inf)
    bins2 <- c(0,5,10,15,Inf)
    pal <- colorBin("Greens", domain = floor(df_prom_barrios$promBI), bins = bins)
    pal2 <- colorBin("Blues", domain = floor(df_prom_barrios$promDO), bins = bins2)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Promedio %g bicicleta(s)",
      df_prom_barrios$barrio, floor(df_prom_barrios$promBI)
    ) %>% lapply(htmltools::HTML)
    
    labels2<- sprintf(
      "<strong>%s</strong><br/>Promedio %g anclaje(s)",
      df_prom_barrios$barrio, floor(df_prom_barrios$promDO)
    ) %>% lapply(htmltools::HTML)
    
    mapapromedio <- leaflet(df_prom_barrios) %>%
      addTiles()%>%
      addLayersControl(
        baseGroups = c("Promedio Bicicletas", "Promedio Anclajes"),
        options = layersControlOptions(collapsed = FALSE)
      )%>%
      addPolygons( fillColor = ~pal(floor(promBI)), color = "white",
                   weight = 1, smoothFactor = 0.5,
                              opacity = 1.0, fillOpacity = 0.7,
                   label = labels ,group="Promedio Bicicletas" ) %>%
      addPolygons( fillColor = ~pal2(floor(promDO)), color = "white",
                   weight = 1, smoothFactor = 0.5,
                   opacity = 1.0, fillOpacity = 0.7,
                   label = labels2 ,group="Promedio Anclajes" ) 

  })
  
  observeEvent(input$mapapromedio_groups,{
    
    bins <- c(0,1,2,3,Inf)
    bins2 <- c(0,5,10,15,Inf)
    pal <- colorBin("Greens", domain = floor(df_prom_barrios$promBI), bins = bins)
    pal2 <- colorBin("Blues", domain = floor(df_prom_barrios$promDO), bins = bins2)

    mapapromedio <- leafletProxy("mapapromedio") %>% clearControls()
    if (input$mapapromedio_groups == "Promedio Bicicletas")
    {
      mapapromedio <- mapapromedio %>%addLegend(pal = pal, values = floor(df_prom_barrios$promBI), opacity = 0.7, title = "Promedio de bicicletas",
                                 position = "bottomright",group="Promedio Bicicletas")
    }
    
    else if (input$mapapromedio_groups == "Promedio Anclajes")
    {
      mapapromedio <- mapapromedio %>% addLegend(pal = pal2, values = floor(df_prom_barrios$promDO), opacity = 0.7, title = "Promedio Anclajes",
                                  position = "bottomright",group="Promedio Anclajes") 
    }
    
    
    
  })
  
  output$mapafallas <- renderLeaflet({
    
   # leaflet(df_barrios) %>%
   #   addTiles()%>%
  #    addPolygons(color = "orange", weight = 1, smoothFactor = 0.5,
  #                opacity = 1.0, fillOpacity = 0,
    #                popup = ~htmlEscape(df_barrios$barrio),
    #              highlightOptions = highlightOptions(color = "red", weight = 3,
    #                                                  bringToFront = TRUE))%>%
    #  addMarkers(lng=~df_mapa_fallas$data.stations.lon, lat=~df_mapa_fallas$data.stations.lat,
    #             popup = paste(paste0("<strong>",df_mapa_fallas$data.stations.name,"</strong>"),
    #                                       paste0("Fallas: ",as.character(df_mapa_fallas$Freq)),
    #                                       sep="<br/>"), icon=icon_bike_station
    #                      )  
    #  
    
    df_tempgrafico <- df_grafico%>%filter(Cant_falla>0)%>%
            select(data.stations.name,Cant_falla,Tiempo_falla,data.stations.lat,data.stations.lon)%>%
                  group_by(data.stations.name,data.stations.lat,data.stations.lon)%>%
                        summarise(Cant_falla = sum(Cant_falla),Tiempo_falla= as.character(seconds_to_period(sum(Tiempo_falla))))
                        
    
    #unique(df_tempgrafico$Tiempo_falla)
    #str(df_tempgrafico)
    
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Total de fallas: %g <br/>Total de tiempo: %s",
        df_tempgrafico$data.stations.name, 
        df_tempgrafico$Cant_falla,
        df_tempgrafico$Tiempo_falla
    ) %>% lapply(htmltools::HTML)
    
    getColor <- function(df_tempgrafico) {
      ppcol <-c("lightgreen","green","beige", "orange","lightred","red",
                "white","white","white","white","white","white","white","white",
                "darkred")

      sapply(df_tempgrafico$Cant_falla, function(Cant_falla) {
        ppcol[Cant_falla]
        
      })
    }
    
    icons <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = getColor(df_tempgrafico)
      
    )
    

    leaflet(df_tempgrafico) %>%
      addTiles()%>%
      addAwesomeMarkers(df_tempgrafico$data.stations.lon, df_tempgrafico$data.stations.lat,
                        icon=icons, 
                        label=labels
                        )
    
    
    
    })
  
  
  output$boxplot1 <-renderPlotly({
     plot_ly(y =~df_tabla_fallas$Freq, type = "box")%>%
      layout(title = "",
           xaxis = list(title = "",
                        zeroline = FALSE),
           yaxis = list(title = "Cantidad",
                        zeroline = FALSE))
      #ggplotly(ggplot(data=df_tabla_fallas, aes(y=Freq))+geom_boxplot()+ ylab("Cantidad"))
    })
  
  observeEvent(input$mapafallas_marker_click,{
  
    click <- input$mapafallas_marker_click
    
    if (is.null(click))
        return()
    
    est <- as.data.frame(df_estaciones%>%filter(data.stations.lat==click$lat & data.stations.lon==click$lng))
    
    estacion_nom <- as.character(est$data.stations.name)
    

    output$texto_fallas <- renderText( 
      paste("Estación selecionada ", estacion_nom, sep=" ")
      
    )
    
    output$graph_fallas_cant<-renderPlotly({
      
      plot_ly(df_grafico%>%filter(Nombre==estacion_nom), x = ~Fecha,
              y = ~Cant_falla, type = "bar")%>%
        layout(title = "",
               xaxis = list(title = "5 semanas",
                            zeroline = TRUE),
               yaxis = list(title = "Cantidad de fallas",
                            zeroline = TRUE))
      
      
      #ggplotly(ggplot(df_grafico%>%filter(Nombre==estacion_nom), aes(Fecha, Cant_falla)) + 
      #           geom_bar(stat="identity")+
      #           xlab("5 semanas") + 
      #           ylab("cantidad de fallas"))
      
    })
    
    output$graph_fallas_tiempo<-renderPlotly({
      
      plot_ly(df_grafico%>%filter(Nombre==estacion_nom), x = ~Fecha,
              y = ~Horas_falla, type = "bar")%>%
        layout(title = "",
               xaxis = list(title = "5 semanas",
                            zeroline = TRUE),
               yaxis = list(title = "Horas sin servicio",
                            zeroline = TRUE))

      #ggplotly(ggplot(df_grafico%>%filter(Nombre==estacion_nom), aes(Fecha, Tiempo_falla)) + 
      #           geom_bar(stat="identity")+
      #           xlab("5 semanas") + 
      #           ylab("minutos sin servicio"))
      
    })
               
    print(estacion_nom)
  })
    
  output$graph_fallas_cant_tot<-renderPlotly({
    
    plot_ly(df_grafico, x = ~Fecha,
            y = ~Cant_falla, type = "bar")%>%
      layout(title = "",
             xaxis = list(title = "5 semanas",
                          zeroline = TRUE),
             yaxis = list(title = "Cantidad de fallas",
                          zeroline = TRUE))
    
    
    #ggplotly(ggplot(df_grafico, aes(Fecha, Cant_falla)) + 
    #           geom_bar(stat="identity", aes(fill = Estacion))+
    #           xlab("5 semanas") + 
    #           ylab("cantidad de fallas"))
    
  })
  
  output$graph_fallas_tiempo_tot<-renderPlotly({
    
    #str(duration(num =2100, units = "second"))
    #y = ~Tiempo_falla
    plot_ly(df_grafico, x = ~Fecha,
            y =  ~Horas_falla, type = "bar")%>%
      layout(title = "",
             xaxis = list(title = "5 semanas",
                          zeroline = TRUE),
             yaxis = list(title = "Horas sin servicio",
                          zeroline = TRUE))
    
    
    
    #ggplotly(ggplot(df_grafico, aes(Fecha, Tiempo_falla)) + 
    #           geom_bar(stat="identity", aes(fill = Estacion))+
    #           xlab("5 semanas") + 
    #           ylab("minutos sin servicio"))
    
  })
  
  output$temperatura <- renderUI({
    textInput(inputId = "temperatura", label="Temperatura (ºC)", value=actual$weather['temp'], width = 200)
  })
  
  output$humedad <- renderUI({
    textInput(inputId ="humedad", label="Humedad (%)", value=actual$weather['humidity'], width = 200)
  })
  
  output$presion <- renderUI({
    textInput(inputId ="presion", label="Presion (HPa)", value=actual$weather['pressure'], width = 200)
  })
  output$dir_viento <- renderUI({
    textInput(inputId ="dir_viento", label="Direccion Viento", value=actual$weather['wing_deg'], width = 200)
  })
  output$vel_viento <- renderUI({
    textInput(inputId ="vel_viento", label="Velocidad Viento (km/hr)", value=actual$weather['wind_speed'], width = 200)
  })
  
  
  
  #tomo el valor de los input, pero recien cuando presiona el boton PREDICT, por eso se genera eventreactive
  ndiasemana <- eventReactive(input$predict, {
    tolower(input$dia)
  })
  
  nhora <- eventReactive(input$predict, {
    as.integer(substring(input$horario,1,2))
  })
  
  ndesde <- eventReactive(input$predict, {
    input$sale
  })
  
  nhasta <- eventReactive(input$predict, {
    input$llega
  })
  
  # genero mi dataset cuando presiona el boton
  df_salida_llegada <-eventReactive(input$predict, {
    
    df_salida_llegada <- df_validacion

    df_salida_llegada
    
  })
  
  df_salida <-eventReactive(input$predict, {
    
    df_salida <- df_salida_llegada()%>%filter(data.stations.groups==ndesde() 
                                                    & weekday == ndiasemana()
                                                    & HoraEntera == nhora())%>%
                    select("data.stations.name","mejor_promBi", "promBi","mejor_minBi","minBi","data.stations.lat","data.stations.lon")%>%
                            arrange(data.stations.name)
    colnames(df_salida) =c("Estacion","Promedio predicho","Promedio Real","Minimo predicho", "Minimo Real","data.stations.lat","data.stations.lon")
    
    df_salida
    
  })
  
  
  df_llegada <-eventReactive(input$predict, {
    
    df_llegada <- df_salida_llegada()%>%filter(data.stations.groups==nhasta() 
                                              & weekday == ndiasemana()
                                              & HoraEntera == nhora())%>%
      select("data.stations.name","mejor_promDo", "promBi","mejor_minDo","minBi","data.stations.lat","data.stations.lon")%>%
      arrange(data.stations.name)
    
    colnames(df_llegada) =c("Estacion","Promedio predicho","Promedio Real","Minimo predicho", "Minimo Real","data.stations.lat","data.stations.lon")
    
    df_llegada
    
  })
  

  output$predict_bici <-  renderDataTable(
    
    options=list(columnDefs = list(list(visible=FALSE, targets=c(6,7)))),
    df_salida()
    
    )
  
  output$predict_dock <-  renderDataTable(
    options=list(columnDefs = list(list(visible=FALSE, targets=c(6,7)))),
    df_llegada()
    
  )
  
  
  #output$tablafallas <-  renderDataTable(
  #  options = list(pageLength = 15),df_fallas
  #  )
  
  output$estacion_salida <- renderText({
    paste("Diponibilidad de bicicletas en",ndesde(),sep=" ")
   })
  output$estacion_llegada <- renderText({
    
    paste("Diponibilidad de anclajes en",nhasta(),sep=" ")
  })
  

  output$clima <- renderUI({
    #busca imagen del clima actual y pronostico desde tutiempo.net
    my_test <- tags$div(id="TT_yinkE11kknnBM8IK7ArjjjDjj6lKM4S2bYktEciIK1z", text="El tiempo - Tutiempo.net",
                              tags$script(type="text/javascript" ,src="https://www.tutiempo.net/s-widget/l_yinkE11kknnBM8IK7ArjjjDjj6lKM4S2bYktEciIK1z"))
    print(my_test)
    my_test
  })
  
  
  output$conclusion <- renderImage({
    return(list(src = ".//Conclusiones.gif",contentType = "image/png",alt = "Conclusiones"))
    # ojo sino le agrego lo de abajo, elimina la imagen cada vez que la ves
  }, deleteFile = FALSE)
  
  
}

shinyApp(ui, server)



