# VisAvanzadaR
Se incluyen dos directorios:
  * Aplicación shiny ONLINE que mediante API REST de Red Eléctrica Española realiza el ETL de datos en tiempo real para mostrar gráficas con interactividad.
  * Aplicación shiny OFFLINE que mediante uso de datos previamente descargados por API REST de Red Eléctrica Española realiza el ETL de datos descargados para mostrar gráficas con interactividad.

Los ficheros son todos interpretables desde R Studio, han sido desarrollados con R 4.1.3

Por limitaciones de la API, la versión online puede presentar defectos, pues los datos que entrega Red Eléctrica Española a través de la API no siempre están actualizados, especialmente los lunes se presentan desfases de los fines de semana. En ese caso volver a probar pasados unos días, o utilizar la versión offline que contiene unos datos de prueba.


Enlace a repositorio GitHub: https://github.com/stgadvisor/VisAvanzadaR
Enlace a shinyapp.io: https://stgadvisor.shinyapps.io/trabajovisualizacionshiny/
