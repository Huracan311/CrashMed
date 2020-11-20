
# Load R packages
library(shiny)
library(shinythemes)
library(DT)
library(rgdal)
library(leaflet)
library(mapview)
library(shinycssloaders)
library(shinyWidgets)
library(data.table)
  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
          #setBackgroundColor(
           # color = "Snow",
           # gradient = c("linear", "radial"),
          #  direction = c("bottom", "top", "right", "left"),
           # shinydashboard = FALSE
        #  ),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "Accidentalidad en Medellin",
      tabPanel("Inicio",
           fluidRow(id='header1',
                    column(12,
                           HTML("
                              <div style='text-align: center;margin-top:80px;'>
                                  <img src='car.png' width='200px'/>
                                  <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellin</p></strong>
                              </div>
                              <hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>
                              <strong><h1 style='text-align: center'>Presentación</h1></strong>
                              <p style='font-size: 20px;text-align: justify;'>En esta página web podrás encontrar información consolidada
                                  de la accidentalidad en Medellín en los años 2014-2018, 
                                  además de un agrupamiento categorizado por barrios y 
                                  un modelo predictivo que tiene como objetivo tener un estimado de la accidentalidad 
                                  en la ciudad de Medellín, este sitio web tiene un soporte documentado que se
                                  puede consultar en el siguiente <a href='https://santiagozuluagaa.github.io/informeTAE/'> link </a>
                                  además todo el código de la construcción de esta aplicación web puede ser consultada
                                  en este  <a href='https://github.com/Huracan311/CrashMed'> repositorio </a> .

                              </p>
                              <br>
                              <br>
                              <p style='font-size: 20px;'>Este trabajo fue desarrollado para la asignatura Técnicas de aprendizaje
                                  estadístico, y fue elaborado por:

                                  <br>
                                  <br>
                              </p>
                            "),
                           
                    )
           ), 
           fluidRow(
             column(2,offset = 1,align="center",
                      HTML("
                           <img src='girl.png' width='100%'/>
                           <p style='text-align:center;'><strong>Doris Steffania Obando González</strong>
                            <br>Ingeniería de sistemas
                            <br>Ingeniería de control
                           </p>
                           ")
                      
                    ),
             column(2,align="center",
                    HTML("
                           <img src='boy1.png' width='100%'/>
                           <p style='text-align:center;'><strong>Santiago Zuluaga Ayala</strong>
                            <br>Estadística
                           </p>
                           ")
                    
             ),
             column(2,align="center",
                    HTML("
                           <img src='boy2.png' width='100%'/>
                           <p style='text-align:center;'><strong>Daniel Hoyos González</strong>
                            <br>Ingeniería de sistemas
                           </p>
                           ")
                    
             ),
             column(2,align="center",
                    HTML("
                           <img src='boy3.png' width='100%'/>
                           <p style='text-align:center;'><strong>Daniel Ceballos Monsalve</strong>
                            <br>Estadística
                            <br>Ingeniería de control
                           </p>
                           ")
                    
             ),
             column(2,align="center",
                    HTML("
                           <img src='boy4.png' width='100%'/>
                           <p style='text-align:center;'><strong>Andrés Felipe Mejía</strong>
                            <br>Ingeniería de sistemas
                            <br>Ingeniería de control
                           </p>
                           ")
                    
             ),
            
           ),
           fluidRow(
             column(12,
                    HTML("<strong><h1 style='text-align: center'>Video Promocional</h1></strong>"),
                    HTML("<iframe src='https://drive.google.com/file/d/1pwnpgMpXFBFrOY9O-iirijqRYDyC0N40/preview' height=720 style='margin: auto;width: 100%;border: 0px;padding: 10px;display: block;'></iframe>")
                    )
           ),
               
      ),
      tabPanel("Visualización Datos",
               
               HTML("
                     <div style='text-align: center;margin-top:80px;'>
                        <img src='car.png' width='200px'/>
                        <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellín</p></strong>
                    </div>
                    <hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>
                    <strong><h1>Visualización de datos </h1></strong>
                    <p style='margin-bottom: 30px;font-size: 20px'>En la siguiente tabla podrá visualizar toda la información de la accidentalidad de Medellín desde el 2014 hasta el 2018, puede utilizar el panel lateral para refinar los criterios de búsqueda.
                       En la parte inferior de la página se visualiza en un mapa la ubicación de los accidentes según el criterio de búsqueda seleccionado
                    </p>
                    
                  "),
               sidebarPanel(
                 tags$h3("Intervalo de fecha"),
                 dateInput("fechaInicio","Fecha Inicio",value = "2014-01-01",min="2014-01-01",max = "2018-12-31"),
                 dateInput("fechaFinal","Fecha Final",value = "2014-1-2",min="2014-01-01",max = "2018-12-31"),
                 selectInput("InputClase", "Clase de accidente:", choices=c("Todos",unique(Accidentes$clase))),
                 selectInput("InputGravedad", "Gravedad del accidente:", choices=c("Todos",unique(Accidentes$gravedad)))
                 
               ), # sidebarPanel
               mainPanel(
                 fluidRow(id='mapaData',
                          column(12,
                                 tags$h3("Tabla De Accidentes"),
                                 DTOutput('table1')%>% withSpinner(color="#0dc5c1"),
                                 
                          ),
                          column(12,
                                 tags$h3("Mapa De Ubicación De Accidentes"),
                                 HTML("<p style=font-size:10px;margin-bottom:0px;>Si al cargar el mapa se vizualiza un color gris, recargue la pagina y no cambie de pestaña </p>"),
                                 leafletOutput("mymap",width = "100%")%>% withSpinner(color="#0dc5c1")
                                 )
                          
                 
                          
                     
                 ),
                 
                    
               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      
      
      tabPanel("Agrupamiento", 
               fluidRow(id='header',
                 column(12,
                        HTML("
                          <div style='text-align: center;margin-top:80px;'>
                              <img src='car.png' width='200px'/>
                              <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellin</p></strong>
                          </div>
                          <hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>
                        ")
                        )
               ),
               fluidRow(id='mapa',
                  column(7,
                         style='margin-top:0px;',
                         HTML("<strong><h1 style='text-align:center;'>Mapa Interactivo </h1></strong>"),
                         HTML("<p style=font-size:10px;margin-bottom:0px;>Si al cargar el mapa se vizualiza un color gris, recargue la pagina y no cambie de pestaña </p>"),
                         leafletOutput("mapaBarrios",width = "100%",height = '600')%>% withSpinner(color="#0dc5c1")
                  ),
                 column(5,
                      HTML("
                      <strong><h1 style='text-align:center;'>Agrupamiento </h1></strong>
                      <p style='margin-bottom: 30px;font-size: 20px;text-align: justify;'>
                      En el mapa de la izquierda se puede visualizar la ciudad de Medellín dividida 
                      por cada uno de los barrios que lo componen, cada uno se encuentra sombreado con 
                      un color el cual indica un agrupamiento según el nivel de riesgo respecto a 
                      accidentalidad. El nivel de riesgo se obtiene a través de la tasa de gravedad de 
                      los accidentes (fracción de accidentes con herido o fallecido) y el número total 
                      de accidentes para el periodo entre 2014 y 2017.
                      <br>
                      <br>
                      Se puede visualizar en color verde los de bajo riesgo, 
                      barrios que no necesitan ser intervenidos en temas de accidentalidad; 
                      en color amarillo los de riesgo medio, los cuales deben analizarse en 
                      detalle para determinar una posible intervención; en color rojo los barrios a 
                      los que urge intervenir en temas de accidentalidad y en color gris se encuentran 
                      algunos barrios no clasificados.
                      </p>
                    
                      ")
                  )
                  
                 
               ),
                  
                    
                 
               ),
      tabPanel("Modelo de predicción", 
                 fluidRow(id='header',
                  column(12,
                         HTML("
                    <div style='text-align: center;margin-top:80px;'>
                        <img src='car.png' width='200px'/>
                        <strong><p style='font-size: 40px;margin-top:-5px'>Accidentalidad en Medellin</p></strong>
                    </div>
                    
                  ")
                  )
                 ),
                fluidRow(
                  column(12,HTML("<hr style='border-top: 1px solid rgba(0, 0, 0, 0.4);'/>"))
                ),
               
                fluidRow(
                  column(12,
                         HTML("
                              <strong><h1 style='text-align:center;'>Modelo de predicción interactivo </h1></strong>
                              <p style='margin-bottom: 30px;font-size: 20px;text-align: justify;'>
                                En esta sección podrás visualizar interactivamente las predicciones de un modelo normal 
                                aplicado a la accidentalidad de la ciudad de Medellín
                                para el año 2021, podrás realizar predicciones ya sea por día, semana o mes, 
                                teniendo la posibilidad de refinar la predicción por la clase de accidente.

                              </p>
                              ")
                         )
                ),
                fluidRow(
                  
                  column(3,
                         style = "background-color: #e3e3e3;",
                         selectInput(inputId = "clase", 
                                     label = "Clase", 
                                     choices = c("Atropello", "Caida de ocupante", "Choque",
                                                 "Volcamiento", "Otro"))   
                  ),
                  column(3,
                         style = "background-color: #e3e3e3;",
                         selectInput(inputId = "intervalo", 
                                     label = "Intervalo", 
                                     choices = c("Dia", "Semana", "Mes")),   
                  ),
                  column(3,
                         style = "background-color: #e3e3e3;",
                         dateInput(inputId = "fecha_ini",
                                   label = "Fecha inicial",
                                   value = "2021-01-01",
                                   min = "2021-01-01",
                                   max = "2021-12-31",
                                   format = "yyyy-mm-dd"),  
                  ),
                  column(3,
                         style = "background-color: #e3e3e3;",
                         dateInput(inputId = "fecha_fin",
                                   label = "Fecha final",
                                   value = "2021-12-31",
                                   min = "2021-01-01",
                                   max = "2021-12-31",
                                   format = "yyyy-mm-dd"),
                  ),

                ),
               fluidRow(id='modelos',
                        column(3,dataTableOutput("tablaPred")%>% withSpinner(color="#0dc5c1")),
                        column(9,style = "margin-top:65px;",plotOutput("distPlot")%>% withSpinner(color="#0dc5c1"))
                        
               )
               
        ),
      
      
      position = c("fixed-top")
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    barrios_med=readOGR("Barrios de Medellin/Barrio_Vereda.shp",layer="Barrio_Vereda")
    nombres_barrios=iconv(barrios_med@data$NOMBRE,"UTF-8","ISO_8859-1")
    grupos <- read.csv(file = 'Agrupamiento_barrios_ordenado3.csv' )
    colorMaker <- colorFactor(palette = c("#ABEBC6", "#F5B041", "#EC7063","#922B21"), 
                levels = c("Bajo", "Medio", "Alto","Muy Alta"))
    pal <- colorFactor(
      palette = c("#EC7063","#ABEBC6", "#F5B041"),
      domain = c("Bajo", "Medio", "Alto")
    )
    Accidentes <- data.table(read.csv(file = 'Accidentalidad_Medellin2.csv'))
    AccidentesFilter <- Accidentes[,-c(1,2)]
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })
   
    
    output$table1 <- renderDT({
            if(input$InputClase != "Todos" && input$InputGravedad == "Todos"){
              stateFilter<-     AccidentesFilter[
                                          fecha >= input$fechaInicio & 
                                          fecha <= input$fechaFinal &
                                          clase == input$InputClase, ]
              
            }else if(input$InputGravedad == "Todos" && input$InputGravedad != "Todos"){
              stateFilter<- AccidentesFilter[
                                          fecha >= input$fechaInicio & 
                                          fecha <= input$fechaFinal &
                                          gravedad == input$InputGravedad,]
                                   
            }else if(input$InputGravedad != "Todos" && input$InputGravedad != "Todos"){
              stateFilter<-       AccidentesFilter[
                                           fecha >= input$fechaInicio & 
                                           fecha <= input$fechaFinal &
                                           gravedad == input$InputGravedad &
                                           clase == input$InputClase,]    
              
            }else{
              stateFilter<-       AccidentesFilter[
                                          fecha >= input$fechaInicio & 
                                          fecha <= input$fechaFinal,] 
                                    
              
            }
            
      },options = list(lengthChange = FALSE,
                       scrollX = TRUE
                       )
    )
    
    output$mymap <- renderLeaflet({
      if(input$InputClase != "Todos" && input$InputGravedad == "Todos"){
        stateFilter<-       Accidentes[ 
                                fecha >= input$fechaInicio & 
                                fecha <= input$fechaFinal &
                                clase == input$InputClase,]
        
      }else if(input$InputGravedad == "Todos" && input$InputGravedad != "Todos"){
        stateFilter<- Accidentes[
                             fecha >= input$fechaInicio & 
                             fecha <= input$fechaFinal &
                              gravedad == input$InputGravedad,]
        
      }else if(input$InputGravedad != "Todos" && input$InputGravedad != "Todos"){
        stateFilter<-    Accidentes[ 
                             fecha >= input$fechaInicio & 
                              fecha <= input$fechaFinal &
                              gravedad == input$InputGravedad &
                              clase == input$InputClase,]  
        
      }else{
        stateFilter<-     Accidentes[
                             fecha >= input$fechaInicio & 
                             fecha <= input$fechaFinal,]
                             
        
      }
      
      leaflet() %>%
        addTiles() %>% 
        #addMarkers(data = points())
        addMarkers(data = stateFilter,
                   lng= ~X, 
                   lat=~Y,
                   clusterOptions = markerClusterOptions()
                   )
      
    })
    
    output$mapaBarrios <- renderLeaflet({
        leaflet() %>%
        addPolygons(data=barrios_med,
                    weight = 1,
                    color = "white",
                    fillColor =colorMaker(grupos$riesgo),
                    fillOpacity = 0.7,
                    
                    label=paste0("<p style='font-size:20px'> <strong>Barrio: </strong>",grupos$barrio_mapa,
                                 "<br><strong>Riesgo: </strong>",grupos$riesgo,
                                 "<br><strong>Cantidad de accidentes: </strong>",grupos$accidentes,
                                 "<br><strong>Tasa de graves: </strong>",grupos$tasa_graves,"</p>"
                                 )%>% lapply(htmltools::HTML),
                    
                    
                    )%>%
        addTiles()%>%
        addLegend(position = "bottomright",pal=pal,values = c("Bajo", "Medio", "Alto"))
        
    })
    
    # Codigo Modelo--------------------------------------------------------------------------------------
    ################### FUNCIÃÂÃÂN GEN DATOS #############
    
    ## LibrerÃÂÃÂ­as necesarias para la funciÃÂÃÂ³n ------------
    library(readxl)
    library(lubridate)
    library(dplyr)
    library(ggplot2)
    
    ## Base de datos para la funciÃÂÃÂ³n ------------------
    fest_2021 <- read_xlsx("festivos2021.xlsx")
    fest_2021 <- fest_2021 %>% mutate(Fecha = as.Date(Fecha, format = "yyyy/mm/dd"))
    
    ## FUNCIÃÂÃÂN ----------------------------------------------
    
    ## Entradas
    #fecha_ini <- as.Date("2021/01/01")
    #fecha_fin <- as.Date("2021/03/23")
    
    ## NÃÂÃÂºcleo
    
    gen_datos <- function(fecha_ini,
                          fecha_fin,
                          intervalo)
    {
      # INTERVALO: SEMANA ------------------
      if (intervalo == "Semana"){
        
        fecha_ini <- fecha_ini - yday(fecha_ini) %% 7 + 1
        
        if (yday(fecha_ini) %% 7 != 0){
          fecha_fin <- fecha_fin + (7 - yday(fecha_fin) %% 7)
        }
        
        # Se verifican inconsistencia
        if (fecha_ini < as.Date("2021-01-01")){
          fecha_ini <- as.Date("2021-01-01")
        }
        
        # Se verifican inconsistencia
        if (fecha_fin > as.Date("2021-12-31")){
          fecha_fin <- as.Date("2021-12-31")
        }     
        
        
      }
      
      # INTERVALO: SEMANA ------------------
      if (intervalo == "Mes"){
        fecha_ini <- floor_date(fecha_ini, unit = "month")  # Primer dÃÂÃÂ­a del mes
        fecha_fin <- ceiling_date(fecha_fin, unit = "month") - 1 # ÃÂÃÂltimo dÃÂÃÂ­a del mes
      }
      
      # INTERVALO: DÃÂÃÂA ---------------------
      if (intervalo == "DÃÂÃÂ­a"){
        # No se necesita hacer ninguna conversiÃÂÃÂ³n
        fecha_ini <- fecha_ini
        fecha_fin <- fecha_fin
      }
      
      # Base de datos inicial
      df_fechas <- data.frame(fecha = seq(fecha_ini,fecha_fin, by = '1 day'))
      
      # ObtenciÃÂÃÂ³n de covariables necesarias en el modelo
      df_fechas <- df_fechas %>% mutate(semana = week(fecha),
                                        semana_dia = as.factor(wday(fecha, label = TRUE, abbr = FALSE)),
                                        fes_antes = ifelse(fecha %in% (fest_2021$Fecha[fest_2021$Festivo == 1] + 1), 1, 0),
                                        fes_despues = ifelse(fecha %in% (fest_2021$Fecha[fest_2021$Festivo == 1] - 1), 1, 0)
      )
      
      # Se aÃÂÃÂ±ade la celebraciÃÂÃÂ³n
      for (i in 1:nrow(df_fechas)){
        df_fechas$celebracion[i] <- ifelse(df_fechas$fecha[i] %in% fest_2021$Fecha, fest_2021$Celebracion[fest_2021$Fecha == df_fechas$fecha[i]], "No")
      }
      
      # ConversiÃÂÃÂ³n a factores
      cols <- c("semana_dia",
                "semana",
                "celebracion",
                "fes_antes",
                "fes_despues")
      
      df_fechas[cols] <- lapply(df_fechas[cols], as.factor)
      
      return(df_fechas)
    }
    
    ########### FUNCIÃÂÃÂN PREDICCIÃÂÃÂN ############
    
    ## Objetos requeridos
    load("mod_choque.Rdata")
    load("mod_atropello.Rdata")
    load("mod_caida_ocu.Rdata")
    load("mod_volcamiento.Rdata")
    load("mod_otro.Rdata")
    
    prediccion <- function(clase, 
                           df_fechas,
                           intervalo){
      
      if (clase == "Atropello"){
        prediccion <- predict(mod_atropello, newdata = df_fechas)
      } 
      
      else if (clase == "Caida de ocupante"){
        prediccion <- predict(mod_caida_ocu, newdata = df_fechas)
      }
      
      else if (clase == "Choque"){
        prediccion <- predict(mod_choque, newdata = df_fechas)
      }
      
      else if (clase == "Volcamiento"){
        prediccion <- predict(mod_volcamiento, newdata = df_fechas)
      }
      
      else if (clase == "Otro"){
        prediccion <- round(predict(mod_otro, newdata = df_fechas))
      }
      
      if (intervalo == "Dia"){
        df_prediccion <- data.frame(fecha = df_fechas$fecha, Total = round(prediccion))
      } 
      
      else if (intervalo == "Semana"){
        
        df_prediccion <- data.frame(fecha = df_fechas$fecha, Total = prediccion) %>% 
          mutate(fecha = week(fecha)) %>%    # DANGER: VERIFICAR GRÃÂÃÂFICO Y TRATAR DE CAMBIAR
          group_by(fecha) %>% summarise(Total = round(sum(Total))) # DANGER
        
      }
      
      else if (intervalo == "Mes"){
        
        df_prediccion <- data.frame(fecha = df_fechas$fecha, Total = prediccion) %>%
          mutate(fecha = month(fecha)) %>%  # DANGER: VERIFICAR GRÃÂÃÂFICO Y TRATAR DE CAMBIAR
          group_by(fecha) %>% summarise(Total = round(sum(Total))) # DANGER
        
      }
      
      return(df_prediccion)
    }
    
    ######### FUNCIÃÂÃÂN PARA CAMBIAR EL NOMBRE #####
    
    # Se necesita para que la tabla que se le muestra al usuario sea consistente con el intervalo
    cambio_nombre <- function(df_prediccion, intervalo){
      if(intervalo == "Dia"){
        names(df_prediccion) <- c("Dia", "Total")
      } 
      
      else if (intervalo == "Semana"){
        names(df_prediccion) <- c("Semana", "Total")    
      }
      
      else if (intervalo == "Mes"){
        names(df_prediccion) <- c("Semana", "Total")    
      }
      
      return(df_prediccion)
      
    }
    
    ########### USO DE LAS FUNCIONES ############
    
    
    df_fechas <- reactive({
      gen_datos(input$fecha_ini, input$fecha_fin, input$intervalo)
    })
    
    df_prediccion <- reactive({
      prediccion(input$clase, df_fechas(), input$intervalo)
    })
    
    ############ GRÃÂÃÂFICO ##############
    
    output$distPlot <- renderPlot({
      
      ggplot(df_prediccion()) +
        geom_area(aes(x = fecha, y = Total), color="darkblue", fill="lightblue") +
        geom_point(aes(x = fecha, y = Total), col = "black") +
        theme_minimal(base_size = 14) +
        ggtitle(paste("Predicciones de", paste0(input$clase, "s"), "por", input$intervalo, "en Medellín")) +
        labs(x = input$intervalo, y = paste("Total de", paste0(input$clase, "s"))) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(size=14),
              axis.text.y = element_text(size=14),
              panel.border = element_rect(colour = "black", fill=NA, size=1))
      
      
      
    })
    
    output$tablaPred <- renderDataTable({
      cambio_nombre(df_prediccion(), input$intervalo)
      
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
