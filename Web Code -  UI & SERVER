#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("tidyverse")
#install.packages("DT")
#install.packages("plotly")

# Librerias ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)

# ui ----------------------------------------------------------------------
#View
ui <- dashboardPage(skin = "green",
                    
                    
# Header ------------------------------------------------------------------
dashboardHeader(title = "HIA",
                dropdownMenu(type="tasks",
                             messageItem(from="Conocenos",
                                         message="Asociación Nacional de 
                                         Hospitales Independientes",
                                         icon = icon("people-group"),
                                         href = "https://anhp.mx/",
                                                 ),
                             messageItem(from = "Youtube",
                                         message = "Ve nuestro hospital",
                                         icon = icon("youtube"),
                                         href="https://youtu.be/hXn4vXk3yO0")
                                    )
                    ),
# Sidebar -----------------------------------------------------------------
dashboardSidebar(
  sidebarMenu(
    menuItem("Inicio",  tabName = "sec_01", icon=icon("home")),
    menuItem("Manejo de datos", tabName = "sec_03", icon=icon("chart-simple"), startExpanded = FALSE,
             menuSubItem("Graficas", tabName = "sb_01", icon = icon("chart-gantt")),
             menuSubItem("Descripción General", tabName = "sb_2", icon = icon("magnifying-glass-chart")))
                      )
                    ),
                    
# Body --------------------------------------------------------------------
dashboardBody(
  # Items - Contenido
  tabItems(
    # Primera seccion
    tabItem(tabName = "sec_01",
            fluidRow(
              column(width = 9,
                     box(width = 12 , height = 200, solidHeader = FALSE, background = "purple",
                         h1("Hospital Independiente ARKHAM", align ="Center"),
                         p("Arkham es un hospital moderno diseñado para ofrecer un servicio excepcional y un tratamiento médico de calidad.
                         El hospital cuenta con equipos e instalaciones de última generación, profesionales médicos altamente calificados y un enfoque de atención centrado en el paciente.
                         Los pacientes pueden esperar un ambiente cómodo y acogedor con un personal empático que se compromete a garantizar su bienestar.
                           Ya sea que necesite atención médica de rutina o tratamiento especializado, Arkham se dedica a brindarle la mejor atención posible.", align="Justify")
                                         ),
                     box(width = 12, solidHeader = FALSE, background = "navy",
                         # red, yellow, aqua, blue, light-blue, green, navy, teal,
                         # olive, lime, orange, fuchsia, purple, maroon, black.
                         h1("Objetivo de la aplicación", align ="Center"),
                         p("El objetivo de esta aplicación es ayudar a los administradores del hospital y 
                         a los profesionales sanitarios a visualizar de forma rápida y sencilla datos 
                         importantes sobre el rendimiento de la clínica a través de gráficos y visualizaciones 
                         interactivas, lo que permite tomar decisiones basadas en datos para mejorar la gestión, 
                           los resultados de los pacientes y la eficiencia operativa.", align="Justify")
                                         ),
                     #Carga de archivos
                     box(width=10, h3("Carga de archivos",align="center"), solidHeader = TRUE,
                         fileInput("archivo", "Selecciona tu archivo:", accept = ".csv")
                                         ),
                                  ),
              column(width = 3,
                     box(width = 12,
                         img(src="logo.jpg", height = 165, width= 199)),
                     box(width = 12, solidHeader = FALSE, background = "aqua",
                         h3("¿Quiénes somos?"),
                         p("El Hospital Arkham es un moderno centro médico que ofrece una atención de alta calidad 
                         y un servicio compasivo a los pacientes. Póngase en contacto con nosotros en el 555-88194003 o 
                           visite nuestro sitio web en www.arkhamhospital.com para obtener más información.", align="Justify")
                                         ),
                                  ),
              box(width=12, h3("Datos de la base de datos", align="center"), solidHeader = TRUE,
                  fluidPage(dataTableOutput("table_1")))
                                )
                        ),
    #FIN
    #Tercera seccion
    #sub_1
    tabItem(tabName = "sb_01",
            fluidRow(
              box(width = 9 , height = 120, solidHeader = FALSE, background = "orange",
                  h1("Graficador de puntos", align ="Center"),
                  p("Comparador de status de persona con resultados de examenes", align = "Center"),
                                  ),
              box(width = 1,
                  img(src="logo.jpg", height = 60, width= 50, align = "Left")),  
              box(width = 12,solidHeader = FALSE,background = "orange",
                  box(title = "Grafica",
                      width = 9, status = "primary", solidHeader = TRUE,
                      plotlyOutput("grafica_1", height = 600)
                                      ),
                  box(title = "Selector de variables",background = "green",
                      width = 3, status = "primary", solidHeader = TRUE,
                      selectInput("variable_x", "Selecciona la variable x", 
                                  c("Altura" = "Altura",
                                    "Peso" = "Peso",
                                    "Minutos" = "Minutos")),
                      selectInput("variable_y", "Selecciona la variable y", 
                                  c("Examen 1" = "Examen_1",
                                    "Examen 2" = "Examen_2",
                                    "Examen 3" = "Examen_3"))
                                      )
                                  ),
              box(width = 12 , height = 120, solidHeader = FALSE, background = "blue",
                  h1("Graficador de frecuencias", align ="Center"),
                  p("Comparador de status de persona con resultados de examenes", align= "Center")
                                  ),
              box(width = 12,solidHeader = FALSE,background = "orange",
                  box(title = "Grafica",
                      width = 9, status = "primary", solidHeader = TRUE,
                      plotlyOutput("grafica_2", height = 600)
                                      ),
                  box(title = "Selector de variables",background = "green",
                      width = 3, status = "primary", solidHeader = TRUE,
                      selectInput("variable_x2", "Selecciona la variable x", 
                                  c("Altura" = "Altura",
                                    "Peso" = "Peso",
                                    "Minutos" = "Minutos")),
                      selectInput("variable_y2", "Selecciona la variable y", 
                                  c("Examen 1" = "Examen_1",
                                    "Examen 2" = "Examen_2",
                                    "Examen 3" = "Examen_3"))
                                      )
                                  )
                                )
                        ),
    tabItem(tabName = "sb_2",
            fluidRow(box(width = 9 , height = 120, solidHeader = FALSE, background = "orange",
                         h1("Estatus general del Hospital", align ="Center"),
                         p("Analisis de datos generales del estado del hospital", align = "Center")
                                ),
                     box(width = 1,
                         img(src="logo.jpg", height = 60, width= 50, align = "Left")),
                     box(width = 12,solidHeader = FALSE,background = "orange",
                         box(width = 12, solidHeader = FALSE,background = "blue", 
                             h3("Analísis de densidad de factores generales acerca de la salud", align="Center"),
                             box(title = "Grafica",
                                 width = 9, status = "primary", solidHeader = TRUE,
                                 plotlyOutput("grafica_3", height = 600)
                                        ),
                             box(title = "Descripción",
                                 width = 3, status = "primary", solidHeader = TRUE,background = "green",
                                 p("En esta sección se muestra el comporamiento del estatus medico
                                 de cada factor. El programa toma cada variable, para evaluar
                                   la variedad de datos con las que se cuenta por cada uno de ellas.", align="Justify"))
                                    )
                                    #fin
                                )
                                )

                                #FIN
                                
                        )
                      )
                    )
)
# FIN DE TODO

# server ------------------------------------------------------------------
# code for functions
server <- function(input, output) {
  
  
  # Carga de datos ----------------------------------------------------------
  datos <- reactive({            #acepta archivos
    file <- input$archivo        #revisa si es un archivo
    if (is.null(file)){return()} #revisa si hay un archivo
    read_csv(file$datapath, show_col_types = FALSE) %>% 
      select(Nombre = v2,
             Apellido = v3,
             Genero = v4,
             Peso = v5,
             Altura = v6,
             Minutos = v7,
             Examen_1 = v8,
             Examen_2 = v9,
             Examen_3 = v10) %>% 
      mutate(Peso = Peso + .5) %>% 
      mutate(Altura = Altura * 100) %>% 
      mutate(Genero = case_when(Genero == 1 ~ "Hombre",
                                Genero == 2 ~ "Mujer",
                                Genero == 3 ~ "Otro",)) %>% 
      mutate(Nombre_Completo = paste(Nombre, Apellido)) %>% 
      select(Nombre_Completo,3:9)
    #lee el csv 
  })
  
  output$table_1 <- renderDataTable({
    datatable(datos())
  })
  # Grafica 1-----
  output$grafica_1 <- renderPlotly({
    ggplotly(
      ggplot(datos(), aes(x = datos()[[input$variable_x]], 
                          y = datos()[[input$variable_y]])) + 
        geom_point(color='white', alpha = 0.3) +
        geom_smooth(method ='lm', se = F, color ='green') +
        labs(title = paste("Grafica de", input$variable_x, "vs", input$variable_y),
             x = input$variable_x,
             y = input$variable_y) +
        theme_dark() +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
    )
  })
  
  #Grafica 2----
  output$grafica_2 <- renderPlotly({
    ggplotly(
      ggplot(datos(), aes(x = datos()[[input$variable_x2]], 
                          y = datos()[[input$variable_y2]])) + 
        geom_area(color='white') +
        labs(title = paste("Grafica de", input$variable_x2, "vs", input$variable_y2),
             x = input$variable_x2,
             y = input$variable_y2) +
        theme_dark() +
        theme(plot.title = element_text(hjust = 0.5, size = 20))
    )
  })
  
  #GRAFICA3
  output$grafica_3 <- renderPlotly({
    ggplotly(
      datos() %>% 
        select_if(is.numeric) %>% 
        pivot_longer(everything(), names_to = "variables") %>% 
        select(variables, valores = value) %>% 
        ggplot(aes(x = valores, fill = variables)) +  
        geom_density() + 
        labs(title = "Análisis individual de factores",
             x= "Valores",
             y="Densidad")+
        theme_dark()+
        facet_wrap(~ variables, scales = "free")
    )
  })
  
  #Fin del server
}

# Run the application 
shinyApp(ui = ui, server = server)
