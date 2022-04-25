#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

### LOGICA DE ETL

#Generar datos auxiliares
dia_0<-"2022-01-01T00:00"
dia_now<-"2022-04-25T00:00"
dia_ayer<-"2022-04-24T00:00"
dia_menossemana <- "2022-04-17T00:00"
dia_menosmes <- "2022-03-26T00:00"


#Carga del dataset previamente descargado y transformado
#Fuente de datos:=> REE: https://www.ree.es/es/apidatos#request
load("./DatosDemandaEvolucion.rda")
load("./DatosGeneracionRenoYno.rda")
load("./DatosGeneracionXtec.rda")
load("./DatosMercadosPrecio.rda")


### LOGICA DE REPRESENTACIÓN

shinyServer(function(input, output) {

    output$plotReno_Cost <- renderPlot({
      
      if (input$periodo == 'mes'){
        # Ajustamos los datos al periodo temporal - ETL datos para visualización según rango elegido
        # d_dia <- DatosMercadosPrecio[["included"]][["attributes"]][["values"]][[1]] %>% select ("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menosmes),as.Date(dia_ayer)))
        # gen_ren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[1]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menosmes),as.Date(dia_ayer)))
        # gen_NOren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[2]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menosmes),as.Date(dia_ayer)))
        d_dia <- DatosMercadosPrecio[["included"]][["attributes"]][["values"]][[1]] %>% select ("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menosmes),as.Date("2022-04-22")))
        gen_ren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[1]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menosmes),as.Date("2022-04-22")))
        gen_NOren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[2]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menosmes),as.Date("2022-04-22")))        
      }
      if (input$periodo == 'semana'){
        # Ajustamos los datos al periodo temporal - ETL datos para visualización según rango elegido
        # d_dia <- DatosMercadosPrecio[["included"]][["attributes"]][["values"]][[1]] %>% select ("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date(dia_ayer)))
        # gen_ren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[1]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date(dia_ayer)))
        # gen_NOren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[2]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date(dia_ayer)))
        d_dia <- DatosMercadosPrecio[["included"]][["attributes"]][["values"]][[1]] %>% select ("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date("2022-04-22")))
        gen_ren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[1]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date("2022-04-22")))
        gen_NOren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[2]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date("2022-04-22")))        
      }

      #crear df renovable y no renovable frente a media de precios diarios
      
      fecha <- as.vector(format(as.Date(d_dia$datetime, "%Y-%m-%dT%H:%M"), "%Y-%m-%d"))
      d_dia <- cbind(d_dia,fecha)
      d_dia_media <- d_dia %>% select(value, fecha) %>% group_by(fecha) %>% summarise(mediaXdia=mean(value))
      
      fecha <- format(as.Date(DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[1]][["datetime"]], "%Y-%m-%dT%H:%M"), "%Y-%m-%d")
      
      gen_ren <- gen_ren$value/1000
      gen_NOren <- gen_NOren$value/1000
      
      df_preciosXtec <- data.frame(d_dia_media, # Datos Fecha
                                   gen_ren,  # Datos Renovables
                                   gen_NOren)
      #Visualización      
      b <- ggplot(df_preciosXtec, aes(x=fecha)) + 
        geom_line(aes(y=mediaXdia,group=1, color = "Media Precios"), size= 3)  +
        geom_line(aes(y=gen_ren,group=1, color = "Generación renovable"), size= 2)  +
        geom_line(aes(y=gen_NOren,group=1, color = "Generación no renovable"), size= 2)  +
        labs(x = "Fecha",y = "Gigawatio/€", title = "Generación renovable/no renovable frente a precio medio", 
             caption = paste("Ultima actualización:",DatosGeneracionRenoYno[["included"]][["attributes"]][["last-update"]][1])) +
        theme(axis.text.x = element_text(angle = 20, size = 8,hjust = 1, vjust = 1))+
        scale_color_manual(name = "Leyenda", 
                           values = c("Media Precios" = "tomato", "Generación renovable" = "skyblue", "Generación no renovable" = "purple"),
                           labels = c("Media Precios", "Generación renovable", "Generación no renovable" ))    
      print(b)

    })
    
    output$plotDemanda <- renderPlot({
      # ETL datos para visualización según rango elegido
      d_demanda <- DatosDemandaEvolucion[["included"]][["attributes"]][["values"]][[1]] %>% 
        select ("value", "datetime") %>% 
        filter(between(as.Date(datetime), as.Date(input$dateRange[1]),as.Date(input$dateRange[2])))
      d_demanda$datetime <- format(as.Date(d_demanda$datetime), "%Y-%m-%d")
      d_demanda$value <- as.integer(d_demanda$value)/1000

      #Visualización      
      a <- ggplot(d_demanda,
                  aes(datetime, value)) + 
        geom_point(colour = "red", size = 3) +
        labs(x = "Fecha",y = "Gigawatio", title = "Demanda eléctrica en España", 
             caption = paste("Ultima actualización:",DatosDemandaEvolucion[["included"]][["attributes"]][["last-update"]])) +
        theme(axis.text.x = element_text(angle = 80, size = 8,hjust = 1, vjust = 1)) +
        geom_smooth()
      
      print(a)
    })  
    
    output$plotTecnologia <- renderPlot({
      # ETL datos para visualización según rango elegido
      if (input$tecx == 'ayer'){
        # Ajustamos los datos al periodo temporal - ETL datos para visualización según rango elegido
        d_genXtec <- DatosGeneracionXtec[["included"]][["attributes"]] %>% select ("title", "type") %>% filter (title !="Generación total")
        d_genXtecAux <- DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[1]] %>% slice(1) %>% select ("value", "datetime")
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[2]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[3]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[4]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[5]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[6]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[7]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[8]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[9]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[10]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[11]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[12]] %>% slice(1) %>% select ("value", "datetime"))
        d_genXtec <- data.frame(d_genXtec,d_genXtecAux)
      }

      if (input$tecx == 'ahora'){
        # Ajustamos los datos al periodo temporal - ETL datos para visualización según rango elegido
        d_genXtec <- DatosGeneracionXtec[["included"]][["attributes"]] %>% select ("title", "type") %>% filter (title !="Generación total")
        d_genXtecAux <- DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[1]] %>% slice(2) %>% select ("value", "datetime")
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[2]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[3]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[4]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[5]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[6]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[7]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[8]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[9]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[10]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[11]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtecAux <- rbind(d_genXtecAux, DatosGeneracionXtec[["included"]][["attributes"]][["values"]][[12]] %>% slice(2) %>% select ("value", "datetime"))
        d_genXtec <- data.frame(d_genXtec,d_genXtecAux)
      }

       #Visualización
        c <- ggplot(d_genXtec,aes(title, value, fill=title)) +
          geom_col() +
          scale_colour_distiller() +
          labs(x = "Tecnología",y = "Megawatios", title = "Generación por tecnologías",
             caption = paste("Ultima datos disponibles:",DatosGeneracionXtec[["data"]][["attributes"]][["last-update"]]) )+
          theme(axis.text.x = element_text(angle = 80, size = 10,hjust = 1, vjust = 1), legend.position='none')

        print(c)
    })
})
