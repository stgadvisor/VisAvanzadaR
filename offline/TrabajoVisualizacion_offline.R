#Cargar librerias para lectura de datos mediante APIRest
#install.packages("httr")
#install.packages("jsonlite")

#Activar librerias para lectura de datos mediante APIRest
library("httr")
library("jsonlite")
library(ggplot2)
library(dplyr)
library(ggmosaic)

#Generar datos auxiliares
dia_0<-"2022-01-01T00:00"
dia_now<-as.character(format(Sys.Date(), "%Y-%m-%dT%H:%M")) # Devuelve en formato: aaaa-dd-mmThh (hh/mm son horas/minutos en número decimal)
dia_ayer<-format(as.Date(Sys.Date(), "%Y-%m-%dT%H:%M")-1, "%Y-%m-%dT%H:%M")
dia_menossemana <- format(as.Date(Sys.Date(), "%Y-%m-%dT%H:%M")-8, "%Y-%m-%dT%H:%M")

#Carga del dataset previamente descargado y transformado
#Fuente de datos:=> REE: https://www.ree.es/es/apidatos#request
load("./DatosDemandaEvolucion.rda")
load("./DatosGeneracionRenoYno.rda")
load("./DatosGeneracionXtec.rda")
load("DatosMercadosPrecio.rda")

#Preparar datos para tratamiento
Fechas_demanda <- as.Date(DatosDemandaEvolucion[["included"]][["attributes"]][["values"]][[1]][["datetime"]])
Giga_W <- as.integer(DatosDemandaEvolucion[["included"]][["attributes"]][["values"]][[1]][["value"]])/1000

#crear df renovable y no renovable frente a media de precios diarios

d_dia <- DatosMercadosPrecio[["included"]][["attributes"]][["values"]][[1]] %>% select ("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date(dia_ayer)))
fecha <- as.vector(format(as.Date(d_dia$datetime, "%Y-%m-%dT%H:%M"), "%Y-%m-%d"))
d_dia <- cbind(d_dia,fecha)
d_dia_media <- d_dia %>% select(value, fecha) %>% group_by(fecha) %>% summarise(mediaXdia=mean(value))


fecha <- format(as.Date(DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[1]][["datetime"]], "%Y-%m-%dT%H:%M"), "%Y-%m-%d")

# SELECCIONAR EL MISMO RANGO DE FECHAS que contiene d_dia EXCLUSIVAMENTE en GW es /1000
gen_ren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[1]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date(dia_ayer)))
gen_NOren <- DatosGeneracionRenoYno[["included"]][["attributes"]][["values"]][[2]] %>% select("value", "datetime") %>% filter(between(as.Date(datetime), as.Date(dia_menossemana),as.Date(dia_ayer)))
gen_ren <- gen_ren$value/1000
gen_NOren <- gen_NOren$value/1000

# 24/04/2022 La API de REE no esta respondiendo correctamente, por lo que se cargan los datos offline
# df_preciosXtec <- data.frame(d_dia_media, # Datos Fecha
#                              gen_ren,  # Datos Renovables
#                              gen_NOren # Datos No Renovables
#                              )
load("df_preciosXtec.rda")

### GRAFICO RENOV/NO vs PRECIO
b <- ggplot(df_preciosXtec, aes(x=fecha)) + 
  geom_line(aes(y=mediaXdia,group=1, color = "Media Precios"), size= 1)  +
  geom_line(aes(y=gen_ren,group=1, color = "Generación renovable"), size= 1)  +
  geom_line(aes(y=gen_NOren,group=1, color = "Generación no renovable"), size= 1)  +
  labs(x = "Fecha",y = "Gigawatio/€", title = "Generación renovable/no renovable frente a precio medio", 
       caption = paste("Ultima actualización:",DatosGeneracionRenoYno[["included"]][["attributes"]][["last-update"]][1]),
       )+
  theme(axis.text.x = element_text(angle = 20, size = 8,hjust = 1, vjust = 1))+
  scale_color_manual(name = "Leyenda", 
                     values = c("Media Precios" = "tomato", "Generación renovable" = "skyblue", "Generación no renovable" = "purple"),
                     labels = c("Media Precios", "Generación renovable", "Generación no renovable" ))
b



### GRAFICO DEMANDA
a <- ggplot(DatosDemandaEvolucion[["included"]][["attributes"]][["values"]][[1]],
            aes(Fechas_demanda, Giga_W)) + 
  geom_point(colour = "red", size = 3) +
  labs(x = "Fecha",y = "Gigawatio", title = DatosDemandaEvolucion[["included"]][["attributes"]][["title"]], 
       caption = paste("Ultima actualización:",DatosDemandaEvolucion[["included"]][["attributes"]][["last-update"]]),
  )+
  theme(axis.text.x = element_text(angle = 80, size = 8,hjust = 1, vjust = 1)) +
  geom_smooth()
a

### GRAFICO Por tecnologia
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
d_genXtec <- d_genXtec %>% mutate(ratio=value/sum(value))

c <- ggplot(d_genXtec,aes(title, value, fill=title)) + 
  geom_col() + 
  scale_colour_distiller() +
  labs(x = "Tecnología",y = "Megawatios", title = "Generación por tecnologías", 
       caption = paste("Ultima actualización:",DatosGeneracionXtec[["data"]][["attributes"]][["last-update"]]) )+
  theme(axis.text.x = element_text(angle = 80, size = 8,hjust = 1, vjust = 1), legend.position='none')
c
