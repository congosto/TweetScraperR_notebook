---
editor_options: 
  markdown: 
    wrap: 72
---

# TweetScraperR_notebook

Notebooks para la descarga y visualización con la librería
[**TweetScraperR**](https://github.com/agusnieto77/TweetScraperR)

## Instalación

1.  Ejecutar los [requerimientos de la librería
    **TweetScraperR**](https://github.com/agusnieto77/TweetScraperR/blob/main/Requisitos.md).
2.  Descargar los cuadernos
3.  Crear un proyecto en Rstudio

## Visión general de TweetScraperR_notebook

Estos cuadernos utilizan la librería
[**TweetScraperR**](https://github.com/agusnieto77/TweetScraperR) que es
una alternativa para la descarga de datos de Twitter tras el cierra de
las APIs con acceso gratuito. La librería utiliza técnicas de scraping
lo que implica, por un lado, restricciones en algunos tipos de mensajes
y por otro una mayor necesidad de recursos de computación con descargas
más lentas. No obstante, a pesar de estas dificultades es el único medio
actualmente para descargar tweets de forma gratuita.

Con las técnicas de scraping podemos descargar todo lo que podemos
acceder por la interfaz Web de Twiter/X. Las limitaciones más
importantes afectan al análisis de red, ya que los RTs no aparecerán en
una búsqueda y la descarga de seguidores /seguidos está limitada a un
número.

### Estructura de los datos

```         
root ----+----data-+---- dataset_1
         |         |
         |         +---- dataset_n
         |
         +---notebooks-+---- utils
                       |
                       +---- TweetScraperR_notebook.Rmd
                       |
                       +---- TweetScraperR_notebook_charts.Rmd
```

El **dataset** es el directorio donde se almacenan los datos. Las
capturas se distinguen por su prefijo. En un dataset puede haber
ficheros con distintos prefijos, todo depende de cómo se organice el
trabajo. Para cada prefijo se generan dos ficheros: uno con los datos
básicos de los tweets y otro con los metadatos.

### TweetScraperR_notebook.Rmd

En este cuaderno se han adaptado las librerías **TweetScraperR** a la
forma de trabajar con t_hoarder_R.

**Funcionalidades**:

1.  Se especifica el rango de fechas inicial y final de la captura y la
    frecuencia de la descarga. La frecuencia se debe ajustar para que el
    número de tweets que se obtienen en cada petición no supere los 600
    mensajes.
2.  Los datos se almacenan en formato csv
3.  Guarda contexto de la descarga. Si se interrumpe, se reanudará en el
    punto que lo dejó

**Tipos de descargas**:

1.  **Get Tweets Historical Search**: descarga una consulta en un
    periodo definido con una frecuencia establecida
2.  **Get Tweets Historical Timeline**: descarga el timeline de una
    lista de usuarios en un periodo definido con una frecuencia
    establecida. No se obtienen los RTs porque equivale a una consulta
    del tipo from:usuario
3.  **Get Tweets images**: obtiene las imágenes de una descarga ya
    realizada

### TweetScraperR_notebook_charts.Rmd

Genera un conjunto de gráficas parametrizables con los datos descargados
con el cuaderno **TweetScraperR_notebook.Rmd**

-   Tweets vs. alcance con influencers (con o sin zoom)
-   Tweets vs. alcance
-   Tweets vs. RTs
-   Palabras más frecuentes (con y sin amplificación)
-   Usuarios citados
-   Emoticonos
-   Acumulado de la aparición de sitios Web (con amplificación)
-   Acumulado de la aparición de topics (con y sin amplificación)
-   Acumulado de la aparición de emojis (con amplificación)
