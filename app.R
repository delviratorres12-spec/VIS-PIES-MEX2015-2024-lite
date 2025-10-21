

# app.R
library(shiny)
library(dplyr)

# Cargar datos
# rm(list=ls())
base <- read.csv("data/data.csv")
# base <- read.csv("data.csv")

# Categorías
años <- unique(base$year)
tipos <- c("Gubernatura","Diputados locales","Munícipes")
vars <- c("PEIIndexp", "laws.1", "procedures.1", "boundaries.1", 
          "votereg", "partyreg", "media", "finance", 
          "voting", "count", "results", "EMBs")
vars2 <- c("PEIIndexp_i", "laws.1_i", "procedures.1_i", "boundaries.1_i", 
           "votereg_i", "partyreg_i", "media_i", "finance_i", 
           "voting_i", "count_i", "results_i", "EMBs_i")
entidades <- base$estado

# UI con tabsetPanel (R base)
ui <- fluidPage(
  
  # CSS institucional
  tags$head(
    tags$style(HTML("
      /* Color principal en encabezados y títulos */
      h1, h2, h3, h4 {
        color: #0029a3;
      }

      /* Fondo de las pestañas activas */
      .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:focus, 
      .nav-tabs > li.active > a:hover {
        background-color: #0029a3;
        color: white !important;
      }

      /* Fondo de las pestañas inactivas */
      .nav-tabs > li > a {
        background-color: grey50;
        color: #0029a3;
      }

      /* Botones */
      .btn {
        background-color: #0029a3;
        color: white;
        border: none;
      }

      .btn:hover {
        background-color: grey50;
        color: grey80;
      }

      /* Tablas */
      table {
        border: 1px solid grey50;
      }
      th {
        background-color: #0029a3;
        color: white;
      }
      td {
        border: 1px solid grey50;
      }
    "))
  ),
  
  titlePanel("PIES México 2015-2024"),
  
  tabsetPanel(
    tabPanel("Inicio",
             h3("Acerca del visualizador"),
             h2("Proyecto de integridad electoral subnacional en México 2015-2024"),
             HTML("
             <p><b>Este visualizador interactivo presenta los resultados de la 
             encuesta a expertos del proyecto de 
             integridad electoral subnacional (PIES) México desde su inicio en 
                  2015 hasta 2024 (su última aplicación a la fecha ).</b></p>",
                  
                  "<p>Desde 2012 el Electoral Integrity Project (EIP), 
                                   originalmente encabezado por Pippa Norris, de las universidades de Harvard y Sydney, 
                                   realiza encuestas a expertas y expertos por país para conocer su percepción de la 
                                   integridad de las elecciones en cada nación. En México, en 2015 y 2016, 
                                   investigadores/as de FLACSO México asociados/as al equipo del EIP, han entrevistado 
                                   a expertas/os locales para conocer la integridad electoral de las contiendas en los 
                                   32 estados de la República mexicana. De 2015 a 2024 se ha calibrado la integridad de 
                                   142 procesos electorales locales entrevistando a 1,703 expertas/os con el objetivo 
                                   de es evaluar la integridad de las elecciones 
             subnacionales en México a través de distintos indicadores, 
             permitiendo la comparación entre entidades federativas y 
             tipos de elección.</p>",
                  
                  "<p>En las pestañas del visualizador encontrará:</p>
             
             <ul>
               <li>Un <b>resumen</b> de las elecciones analizadas.</li>
               <li>Un visualizador de la evolución de los <b>indicadores</b> de integridad de las elecciones analizadas.</li>
               <li>Una comparación de la evolución de los indicadores por <b>entidad</b>.</li>
             </ul>
             
             "),
             
             br(),
             HTML("<p><b>Fuente de datos:</b> PIES México 2015-2024. Disponible en <i>Harvard Dataverse</i>:</p>
                  <ul>
             <li><i>Loza, Nicolas; Mendez, Irma; Elvira, Diego, 2025, PIESM 2015-2024 (2025) nivel estado-elección, https://doi.org/10.7910/DVN/7M129L, Harvard Dataverse, V1</i></li>
    <li><i>Loza, Nicolas; Mendez, Irma; Elvira, Diego, 2025, PIESM 2015-2024 (2025) nivel experto(a), https://doi.org/10.7910/DVN/JRGJ3W, Harvard Dataverse, V1</i></li>
                                        </ul>
                  "),
             br(),
             br(),
             p(" - Visualizador elaborado por Diego Elvira - "),
             HTML('
  <a href="https://doi.org/10.5281/zenodo.17241011" target="_blank">
    <img src="https://zenodo.org/badge/DOI/10.5281/zenodo.17241011.svg" alt="DOI">
  </a>
')
    ),
    
    tabPanel("Resumen",
             h3("Elecciones analizadas (2015-2024)"),
             p("Por tipo de elección:"),
             div(style = "display:flex; justify-content:center;",
                 tableOutput("tabla")
             ),
             br(),
             p("Listado de entidades:"),
             selectInput("año", "Selecciona año del proyecto", choices = años,selected = "2024"),
             selectInput("tipo", "Tipo de elección", choices = tipos, selected = "Diputados locales"),
             div(style = "display:flex; justify-content:center;",
                 tableOutput("tabla2")
             ),
    ),
    
    tabPanel("Indicadores",
             
               
                      h3("Evolución de resultados"),
                      selectInput("tipo2", "Tipo de elección", choices = tipos),
                      selectInput("var", "Indicador", choices = vars),
                      div(style = "display:flex; justify-content:center;",
                      plotOutput("graf2", height = "600px",width = "900px"))
               ,
               
                      h3("Evolución de resultados con imputación"),
                      selectInput("tipo3", "Tipo de elección", choices = tipos),
                      selectInput("var2", "Indicador (imputado)", choices = vars2),
                      # tableOutput("tablax"),
               div(style = "display:flex; justify-content:center;",
                      plotOutput("graf3", height = "600px",width = "900px"))
               
             
    ),
    
    tabPanel("Entidades",
             h3("Evolución de indicadores por entidad/elección (con valores imputados)"),
             selectInput("entidad", "Selecciona por entidad", choices = entidades, selected = "Ciudad de México"),
             div(style = "display:flex; justify-content:center;",
                 tableOutput("tabla3")),
             selectInput("tipo4", "Tipo de elección", choices = tipos),
             selectInput("var3", "Indicador (imputado)", choices = vars2),
             div(style = "display:flex; justify-content:center;",
                 plotOutput("graf4", height = "600px",width = "900px")
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Tabla resumen
  output$tabla <- renderTable({
    base %>%
      group_by(year) %>% 
      summarise(`Entidades` = n(),
                `Elecciones de Gubernatura` = sum(`Tipo.de.elección`=="Gubernatura"),
                `Elecciones de Diputados locales` = sum(`Tipo.de.elección`=="Diputados locales"),
                `Elecciones de Munícipes` = sum(`Tipo.de.elección`=="Munícipes")) %>%
      ungroup() %>% 
      rename(Año=year)
  })
  
  # Tabla detalle por tipo
  output$tabla2 <- renderTable({
    base %>%
      filter(year == input$año) %>%
      filter(`Tipo.de.elección` == input$tipo) %>%
      select(estado) %>% 
      rename(`Entidades con elección analizada`=estado)
  })
  
  # output$tablax <- renderTable({
  # z <- base %>%
  #   filter(`Tipo.de.elección` == input$tipo3) %>%
  #   # select(year, !!sym(input$var)) %>% 
  #   rename(value= !!sym(input$var2)) %>% 
  #   arrange(year) %>% 
  #   group_by(year) %>% 
  #   summarise(media=mean(value,na.rm=T)) #%>% 
  #   # filter(!is.na(media))
  # })
  
  
  
  # Gráfico sin imputación
  output$graf2 <- renderPlot({
    z <- base %>%
      filter(`Tipo.de.elección` == input$tipo2) %>%
      # select(year, !!sym(input$var)) %>% 
      rename(value= !!sym(input$var)) %>% 
      arrange(year) %>% 
      group_by(year) %>% 
      summarise(media=mean(value)) %>% 
      filter(!is.na(media))
    
    # valores <- datos[[input$var]]
    # nombres <- datos$estado
    
    # par(mar = c(10, 4, 4, 2) + 0.1)
    
    plot(x = z$year, y = z$media,
         type= "l",
         lwd=5.5,
         lty="dashed",
         pch=15,
         cex=1.5,
         col=adjustcolor("#0029a3", alpha=1),
         ylim=c(10,90),
         yaxt = "n",
         xaxt = "n",
         # main="Personalizado", 
         xlab="", 
         ylab="",
         # font.axis= 3, # font: 1: normal, 2: bold, 3: italic, 4: bold italic
         # col.axis= "steelblue",
         # cex.axis= 1,
         # cex.lab= 3,
         # las=1
    )
    
    # Eje Y manual, de 0 a 40 en 10 en 10
    # axis(side = 2, at = seq(10, 40, by = 10), las = 1)
    axis(side = 1, at = seq(2015, 2024, by = 1), las = 1,font = 3,cex.axis = 1.25)
    
    # Líneas guías horizontales (opcional)
    # abline(h = seq(10, 90, by = 10), col = "grey70", lty = "dotted")
    
    # Etiquetas encima de los puntos
    text(x = z$year, 
         y = z$media + 2.5,   # ligeramente arriba del punto
         labels = round(z$media, 1),
         cex = 1.5, 
         font = 2, 
         col = "grey30")
  })
  
  # Gráfico con imputación
  output$graf3 <- renderPlot({

    
    z <- base %>%
      filter(`Tipo.de.elección` == input$tipo3) %>%
      # select(year, !!sym(input$var)) %>% 
      rename(value= !!sym(input$var2)) %>% 
      arrange(year) %>% 
      group_by(year) %>% 
      summarise(media=mean(value)) %>% 
      filter(!is.na(media))
    
    # valores <- datos[[input$var]]
    # nombres <- datos$estado
    
    # par(mar = c(10, 4, 4, 2) + 0.1)
    
    plot(x = z$year, y = z$media,
         type= "l",
         lwd=5.5,
         lty="dashed",
         pch=15,
         cex=1.5,
         col=adjustcolor("#0029a3", alpha=1),
         ylim=c(10,90),
         yaxt = "n",
         xaxt = "n",
         # main="Personalizado", 
         xlab="", 
         ylab="",
         # font.axis= 3, # font: 1: normal, 2: bold, 3: italic, 4: bold italic
         # col.axis= "steelblue",
         # cex.axis= 1,
         # cex.lab= 3,
         # las=1
    )
    
    # Eje Y manual, de 0 a 40 en 10 en 10
    # axis(side = 2, at = seq(10, 40, by = 10), las = 1)
    axis(side = 1, at = seq(2015, 2024, by = 1), las = 1,font = 3,cex.axis = 1.25)
    
    # Líneas guías horizontales (opcional)
    # abline(h = seq(10, 90, by = 10), col = "grey70", lty = "dotted")
    
    # Etiquetas encima de los puntos
    text(x = z$year, 
         y = z$media + 2.5,   # ligeramente arriba del punto
         labels = round(z$media, 1),
         cex = 1.5, 
         font = 2, 
         col = "grey30")    
  })
  
 
# Tabla de cada entidad
  output$tabla3 <- renderTable({
    base %>%
      filter(estado == input$entidad) %>%
      # filter(`Tipo.de.elección` == input$tipo) %>%
      select(year,`Tipo.de.elección`) %>% 
      rename(Año=year,`Elección analizada`=`Tipo.de.elección`)
  })
  
  
   
  
  # Gráfico por entidades
  output$graf4 <- renderPlot({

    z <- base %>%
      filter(`Tipo.de.elección` == input$tipo4) %>%
      # select(year, !!sym(input$var)) %>% 
      rename(value= !!sym(input$var3)) %>% 
      arrange(year) %>% 
      group_by(year) %>% 
      summarise(media=mean(value)) %>% 
      filter(!is.na(media))   
    
    plot(x = z$year, y = z$media,
         type= "l",
         lwd=5.5,
         # lty="dashed",
         pch=15,
         cex=1.5,
         col=adjustcolor("#0029a3", alpha=1),
         ylim=c(10,90),
         yaxt = "n",
         xaxt = "n",
         # main="Personalizado", 
         xlab="", 
         ylab="",
         # font.axis= 3, # font: 1: normal, 2: bold, 3: italic, 4: bold italic
         # col.axis= "steelblue",
         # cex.axis= 1,
         # cex.lab= 3,
         # las=1
    )
    
    # Eje Y manual, de 0 a 40 en 10 en 10
    # axis(side = 2, at = seq(10, 40, by = 10), las = 1)
    axis(side = 1, at = seq(2015, 2024, by = 1), las = 1,font = 3,cex.axis = 1.25)
    
    # Líneas guías horizontales (opcional)
    # abline(h = seq(10, 90, by = 10), col = "grey70", lty = "dotted")
    
    # Etiquetas encima de los puntos
    text(x = z$year, 
         y = z$media + 2.5,   # ligeramente arriba del punto
         labels = round(z$media, 1),
         cex = 1.5, 
         font = 2, 
         col = "grey30")  
    
  })
  
  
  
}

shinyApp(ui, server)

# library(shinylive)
#
# # Exporta la app a HTML y recursos estáticos
# shinylive::export(appdir = ".", destdir = "docs")

