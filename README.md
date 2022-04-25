# VisAvanzadaR
Se incluyen dos directorios:
  * Aplicación shiny ONLINE que mediante API REST de Red Electríca Española realiza el ETL de datos en tiempo real para mostrar gráficas con interactividad.
  * Aplicación shiny OFFLINE que mediante uso de datos previamente descargados por API REST de Red Electríca Española realiza el ETL de datos descargados para mostrar gráficas con interactividad.

Los ficheros son todos interpretables desde R Studio, han sido desarrollados con R 4.1.3

Por limitaciones de la API, la versión online puede presentar defectos, pues los datos que entrega Red Electríca Española a traves de la API no siempre estan actualizados, especialemente los lunes se presentan desfases de los fines de semana. En ese caso volver a probar pasados unos días, o utilizar la versión offline que continene unos datos de prueba.
