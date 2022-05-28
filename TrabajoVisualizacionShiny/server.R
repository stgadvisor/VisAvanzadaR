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
dia_now<-as.character(format(Sys.Date(), "%Y-%m-%dT%H:%M")) # Devuelve en formato: aaaa-dd-mmThh (hh/mm son horas/minutos en número decimal)
dia_ayer<-format(as.Date(Sys.Date(), "%Y-%m-%dT%H:%M")-1, "%Y-%m-%dT%H:%M")
dia_menossemana <- format(as.Date(Sys.Date(), "%Y-%m-%dT%H:%M")-8, "%Y-%m-%dT%H:%M")
dia_menosmes <- format(as.Date(Sys.Date(), "%Y-%m-%dT%H:%M")-29, "%Y-%m-%dT%H:%M")



URLBaseDemandaEvolucion <- "https://apidatos.ree.es/es/datos/demanda/evolucion"
URLBaseGeneracionRenoYno <- "https://apidatos.ree.es/es/datos/generacion/evolucion-renovable-no-renovable"
URLBaseMercadosPrecio <- "https://apidatos.ree.es/es/datos/mercados/precios-mercados-tiempo-real"
URLBaseGeneracionXtec <- "https://apidatos.ree.es/es/datos/generacion/estructura-generacion"

#Carga del dataset
#Fuente de datos:=> REE: https://www.ree.es/es/apidatos#request
DatosDemandaEvolucion <- GET(URLBaseDemandaEvolucion, query = list(start_date= dia_0, end_date= dia_now, time_trunc="day", geo_trunc="electric_system", geo_limit="peninsular", geo_ids=8741) )
DatosGeneracionRenoYno <- GET(URLBaseGeneracionRenoYno, query = list(start_date= dia_menosmes, end_date= dia_now, time_trunc="day", geo_trunc="electric_system", geo_limit="peninsular", geo_ids=8741) )
DatosMercadosPrecio <- GET(URLBaseMercadosPrecio, query = list(start_date= dia_menosmes, end_date= dia_now, time_trunc="hour", geo_trunc="electric_system", geo_limit="peninsular", geo_ids=8741) )
DatosGeneracionXtec <- GET(URLBaseGeneracionXtec, query = list(start_date= dia_ayer, end_date= dia_now, time_trunc="day", geo_trunc="electric_system", geo_limit="peninsular", geo_ids=8741) )

#Datos recibidos en JSON, transformamos a lista con df

DatosDemandaEvolucion = fromJSON(rawToChar(DatosDemandaEvolucion$content))
DatosGeneracionRenoYno <- fromJSON(rawToChar(DatosGeneracionRenoYno$content))
DatosMercadosPrecio <- fromJSON(rawToChar(DatosMercadosPrecio$content))
DatosGeneracionXtec <- fromJSON(rawToChar(DatosGeneracionXtec$content))

### LOGICA DE REPRESENTACIÓN

shinyServer(function(input, output) {

    output$plotReno_Cost <- renderPlot({
      
      if (input$periodo == 'mes'){
        # Ajustamos los datos al periodo temporal - ETL datos para visualización según rango elegido
        d_dia <- DatosMercadosPrecio[["included"]][["attributes"]][["values"]][[1]] %>% select ("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menosmes),as.Date(dia_ayer)))
        gen_ren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[1]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menosmes),as.Date(dia_ayer)))
        gen_NOren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[2]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menosmes),as.Date(dia_ayer)))
      }
      if (input$periodo == 'semana'){
        # Ajustamos los datos al periodo temporal - ETL datos para visualización según rango elegido
        d_dia <- DatosMercadosPrecio[["included"]][["attributes"]][["values"]][[1]] %>% select ("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date(dia_ayer)))
        gen_ren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[1]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date(dia_ayer)))
        gen_NOren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[2]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date(dia_ayer)))
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
                  aes(datetime, value, group = 1)) + 
        geom_line() +
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
