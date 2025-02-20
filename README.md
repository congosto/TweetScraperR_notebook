# TweetScraperR_notebook

Notebooks para la descarga y visualización con las librerías TweetScraperR

## Introducción

La librería [**TweetScraperR**](https://github.com/agusnieto77/TweetScraperR) es una alternativa para la descarga de datos de Twitter tras el cierra de las APIs de Twitter con acceso gratuito. La librería utiliza técnicas de scraping lo que implica, por un lado, restricciones en algunos tipos de mensajes y por otro una mayor necesidad de recursos de computación con descargas más lentas. No obstante, a pesar de estas dificultades es el único medio actualmente para descargar tweets de forma gratuita.

Con las técnicas de scraping podemos descargar todo lo que podemos acceder por la interfaz Web de Twiter/X. Las limitaciones más importantes afectan al análisis de red, ya que los RTs no aparecerán en una búsqueda y la descarga de seguidores /seguidos está limitada a un número.

Hay un cuaderno para la descarga de tweets **TweetScraperR_notebook.Rmd** y otro para su visualización de los datos descargados. **TweetScraperR_notebook_charts.Rmd**

### TweetScraperR_notebook.Rmd

En este cuaderno se han adaptado las librerías **TweetScraperR** a la forma de trabajar con t_hoarder_R.

1.  **Formato RDS vs. csv**: El formato por defecto de la descarga es RDS, muy útil porque conserva una estructura de datos jerárquica, pero implica usar R para su tratamiento posterior. El formato csv es plano y podemos perder información jerárquica, por ejemplo, la lista de hashtags, la lista de enlaces o la lista de emojis. Sin embargo, este formato es muy fácil de tratar con distintas herramientas. El cuaderno usa el formato **csv**
2.  **Estructura de almacenamiento**: el cuaderno distingue entre dataset que es el directorio donde se almacenan los datos y los ficheros donde se guardan que tendrán un prefijo. En un dataset puede haber ficheros con distintos prefijos, todo depende de cómo se organice el trabajo. Para cada prefijo existen dos ficheros: uno con los datos básicos de los tweets y otro con los metadatos. Estos ficheros se pueden ampliar con capturas posteriores.
3.  **Granularidad de la descarga**: Cuando hacemos búsquedas, el número de mensajes obtenido es inferior a 1000. Por tanto, si la frecuencia de tweets es muy alta hay que descargar por periodos de tiempo más pequeños, ajustándolos para que no sobrepase el límite de tweets de ese intervalo de tiempo. En cada intervalo se guarda la información en el fichero, de forma que si se interrumpe, se podría reanudar en el siguiente.
4.  **Parada y arranque de la descarga**: puede ocurrir cuando la descarga tiene un intervalo de tiempo grande, que se nos colapsen los recursos del ordenador o que tengamos algún fallo. Con este procedimiento podríamos reanudar la descarga en el punto que la dejó.

### Tipos de descargas

1.  **Get Tweets Historical Search**: descarga una consulta en un periodo definido con una frecuencia establecida
2.  **Get Tweets Historical Timeline**: descarga el timeline de una lista de usuarios en un periodo definido con una frecuencia establecida. No se obtienen los RTs porque equivale a una consulta del tipo from:usuario
3.  **Get Tweets images**: obtiene las imágenes de una descarga ya realizada

### TweetScraperR_notebook_charts.Rmd

Genera un conjunto de gráficas parametrizables con los datos descargados con el cuaderno **TweetScraperR_notebook.Rmd**

-   Tweets vs. alcance con influencers (con o sin zoom)
-   Tweets vs. alcance
-   Tweets vs. RTs
-   Palabras más frecuentes (con y sin amplificación)
-   Usuarios citados
-   Emoticonos
-   Acumulado de la aparición de sitios Web (con amplificación)
-   Acumulado de la aparición de topics (con y sin amplificación)
-   Acumulado de la aparición de emojis (con amplificación)
