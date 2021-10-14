---
title: "Bias-correction"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Objetivo

En este experimento se utilizaron los climáticos utilizados publicados por Karger et al. (2020) [https://www.nature.com/articles/s41597-020-00587-y]. Estos datos corresponden al periodo histórico (aquí consideramos periodo histórico 1950-2005, aunque en la publicación se reportan datos desde 1850) y proyecciones futuras bajo los escenarios RCP4.5 y RCP8.5. Las series publicadas contienen los resultados obtenidos por 4 modelos diferentes (ACCESS1-3, CESM1-GBC, CMCC-CM y MIROC5) a los que se ha aplicado el algoritmo CHELSA para aumentar su resolución (downscaling) hasta los 5km aproximadamente. 

Para evaluar los datos modelados con los datos observados, se han utilizado las estaciones dentro de la zona de estudio. 

La comparación entre los datos modelados y observados resulta en un buen ajuste para la temperatura mínima y máxima Figura_tmin_comparison, Figura_tmax_comparison, mientras que la precipitación tiene un ajuste bastante pobre Figura_prec_comparison.

![Figura_tmin_comparison.](/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/outputs/tasmin_comparison.jpeg)

![Figura_tmax_comparison.](/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/outputs/tasmax_comparison.jpeg)

![Figura_prec_comparison.](/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/outputs/prec_comparison.jpeg)

Ante esta situación analizamos en mayor detalle los datos de precipitación observados y modelados. La Figura_prec_annual_comparison muestra la media mensual para el periodo histórico de los datos modelados (cada uno de los modelos) y los datos de las estaciones de medición.

![Figura_prec_annual_comparison.](/Users/maria.suarez.munoz/Google Drive/proj_LANDIS/experiments/outputs/prec_comparison_monthly_pattern.jpeg)

Procedemos a realizar una correción del sesgo (bias-correction) por meses. Esta correctión se realizará por regiones climáticas (Figura_climate_regions), ya que LANDIS-II utiliza datos agregados de estas regiones y no mapas en las simulaciones. 

![Figura_climate_regions](/Volumes/GoogleDrive/My Drive/proj_LANDIS/experiments/data/climate/climate_regions_map.png)