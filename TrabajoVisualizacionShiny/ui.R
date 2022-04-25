#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("httr")
library("jsonlite")
library(ggplot2)
library(dplyr)


# Datos auxiliares para la visualización

fech_max <- format(as.Date(Sys.Date(), "%Y-%m-%dT%H:%M"), "%Y-%m-%d")

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Visualización avanzada - UNED máster Big Data",
             tabPanel("Descripción del trabajo",
                      mainPanel(
                        h1("Trabajo del modulo de visualización avanzada", align = "center"),
                        h1("UNED - Máster Big Data 2022", align = "center"),
                        h2("Ricardo Hidalgo Aragón", align = "center"),
                        h3("Conjunto de gráficas disponibles sobre datos descargados en tiempo real:", 
                           align = "left"),
                        p(""),
                        h4("* El impacto del precio según las fuentes renovables y no renovables con las que se esta produciendo energía en el sistema eléctrica Español, semanal o mensual."),
                        h4("* La demanda eléctrica histórica desde principios de 2022, permite filtrado por fechas."),
                        h4("* La distribución de las fuentes de producción de energía electrica, según los ultimos datos disponibles y datos del día de ayer"),
                        p(""),
                        h3("Estos graficos son generador a partir de los datos obtenidos mediante API REST de Red Electrica Española, en tiempo real",
                           align = "center"),
                        h3("En ciertas ocasiones se presenta un desfase con los datos del coste de la energía que pueden causar algún fallo puntual, en ese caso se recomienda probar la versión OFFLINE, o esperar unos días.",
                           align = "center"),
                        h3("------------------------", 
                           align = "center"),
                        
                        p(""),
                        h2("IMPORTANTE", align = "center"),
                        h2("Recomendable resolución superior a 1024x768 para visualizar gráficos", 
                           align = "center"))
              ),
             
             tabPanel("Generación Renovable/No VS Precio medio diario",
                      sidebarPanel(
                        radioButtons(inputId = 'periodo', 
                                    'Elige periodo de visualización', 
                                    choices = c("Última semana" = "semana",
                                                "Último mes" = "mes"), 
                                    selected = "mes")
                      ),
                      mainPanel(
                        plotOutput(outputId = 'plotReno_Cost',
                                   height = 800,
                                   width = 900)
                        
                      )
             ),
  
             tabPanel("Demanda Eléctrica",
                      sidebarPanel(
                        dateRangeInput('dateRange',
                                       label = 'Define tu rango de fechas personalizo en formato: yyyy-mm-dd',
                                       start = format(as.Date(Sys.Date(), "%Y-%m-%dT%H:%M")-30, "%Y-%m-%d"), 
                                       end = fech_max,
                                       min = "2022-01-01",
                                       max = fech_max )
                      ),
                      
                      mainPanel(
                        plotOutput(outputId = 'plotDemanda',
                                   height = 800,
                                   width = 900
                        )
                        
                      )
             ),
             
             tabPanel("Generación por tecnologías",
                      sidebarPanel(
                        radioButtons(inputId = 'tecx', 
                                     'Elige periodo de visualización', 
                                     choices = c("Ayer" = "ayer",
                                                 "Últimos datos disponibles" = "ahora"),
                                     selected = "ayer")
                      ),
                      
                      mainPanel(
                        plotOutput(outputId = 'plotTecnologia',
                                   height = 800,
                                   width = 900)
                        
                      )
             )             

    )
)
