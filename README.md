# Instrucciones para leer y reproducir el análisis

**Contexto:** Este repositorio corresponde a una prueba técnica para el proceso de selección de Data Scientist en Scanntech. El objetivo es analizar la elasticidad precio-demanda utilizando herramientas estadísticas y de ciencia de datos en R y Python.

Este repositorio contiene el análisis de elasticidad precio-demanda realizado en R y Python. Para visualizar y reproducir el informe y los resultados, sigue estos pasos:

## 1. Requisitos

- R (>= 4.5.1)
- Paquetes R: `lme4`, `MuMIn`, `dplyr`, `ggplot2`, `knitr`, `magrittr`, `rmarkdown`
- Python (recomendado: usar `poetry` para instalar dependencias)
- GitHub CLI (`gh`) para clonar y gestionar el repositorio

## 2. Clonar el repositorio

```sh
git clone https://github.com/ljofreflor/scannmarket-elasticidad.git
cd scannmarket-elasticidad
```

## 3. Instalar dependencias en R

Abre R y ejecuta:

```r
install.packages(c('lme4', 'MuMIn', 'dplyr', 'ggplot2', 'knitr', 'magrittr', 'rmarkdown'))
```

## 4. Instalar dependencias en Python

```sh
poetry install
```

## 5. Compilar el informe R Markdown

```sh
Rscript -e "rmarkdown::render('elasticidad.Rmd', output_format = 'html_document')"
```
Esto generará el archivo `elasticidad.html` con el informe completo.

Para obtener el informe en formato Markdown:
```sh
Rscript -e "rmarkdown::render('elasticidad.Rmd', output_format = 'md_document')"
```

## 6. Visualizar el análisis

- El archivo `elasticidad.html` contiene el informe profesional con resultados, interpretaciones y recomendaciones.
- El archivo `analisis_prueba_scannmarket.ipynb` contiene el análisis en Python y puede abrirse con Jupyter Notebook.

## 7. Contacto

Para dudas, contactar a Leonardo Jofré (autor).
