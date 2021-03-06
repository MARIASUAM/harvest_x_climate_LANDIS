---
title: "Bias-correction"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Objetivo

En este experimento se utilizaron los climáticos utilizados publicados por Karger et al. (2020) [https://www.nature.com/articles/s41597-020-00587-y]. Estos datos corresponden al periodo histórico (aquí consideramos periodo histórico 1950-2005, aunque en la publicación se reportan datos desde 1850) y proyecciones futuras bajo los escenarios RCP4.5 y RCP8.5. Las series publicadas contienen los resultados obtenidos por 4 modelos diferentes (ACCESS1-3, CESM1-GBC, CMCC-CM y MIROC5) a los que se ha aplicado el algoritmo CHELSA para aumentar su resolución (downscaling) hasta los 0.049° (5km aproximadamente). 

Para evaluar los datos modelados, éstos se compararon con datos observados en las estaciones dentro de la zona de estudio para el periodo histórico (1950-2005). La comparación entre los datos modelados y observados resulta en un buen ajuste para la temperatura mínima y máxima (Figura_tmin_comparison, Figura_tmax_comparison), mientras que la precipitación tiene un ajuste bastante pobre (Figura_prec_comparison).

<img src="https://github.com/MARIASUAM/harvest_x_climate_LANDIS/blob/master/images/tasmin_comparison.jpeg" alt="Figura_tmin_comparison" style="zoom:50%;" />

<img src="https://github.com/MARIASUAM/harvest_x_climate_LANDIS/blob/master/images/tasmax_comparison.jpeg" alt="Figura_tmax_comparison" style="zoom:50%;" />

<img src="https://github.com/MARIASUAM/harvest_x_climate_LANDIS/blob/master/images/prec_comparison.jpeg" alt="Figura_prec_comparison" style="zoom:50%;" />

Ante esta situación analizamos en mayor detalle los datos de precipitación observados y modelados. La Figure_prec_precorrection muestra la media mensual para el periodo histórico de los datos modelados frente los datos de las estaciones de medición para cada región climática considerada en la zona de estudio, ya que LANDIS-II utiliza datos agregados para cada región en las simulaciones. La Figura_climate_regions muestra dichas regiones climáticas definidas. 

<img src="https://github.com/MARIASUAM/harvest_x_climate_LANDIS/blob/master/images/prec_pre-correction.jpeg" alt="Figure_prec_precorrection" style="zoom:50%;" />

<img src="https://github.com/MARIASUAM/harvest_x_climate_LANDIS/blob/master/images/climate_regions_map.png" alt="Figura_climate_regions. Climate region 1: red; 2: orange; 3: green; 4: blue." style="zoom:50%;" />

Procedimos a realizar una correción del sesgo (quantile mapping method, Teutschbein et al., 2012, Lucatero et al., 2018). Esta corrección consiste en ajustar los valores modelados de acuerdo a su situación en la distribución mensual, aplicando un coeficiente de corrección diferente a cada valor según cuál sea su frecuencia. Es decir, los valores altos o bajos se corregirán de forma distinta, ya que la frecuencia a la que ocurren es diferente. Estos coeficientes de corrección fueron estimados de forma independiente para cada mes y región climática.

# Pre-tratamiento de los datos

Antes de aplicar la corrección de sesgo, se procedió a preparar y tratar los datos con los que llevarla a cabo. Los datos de precipitación diaria de un total de 98 estaciones fueron extraídos y analizados para asegurar su fiabilidad. Aquellas estaciones para las que existieran datos de meses incompletos fueron rellenados. Este relleno se realizó sólo en el caso de que existieran al menos 5 estaciones en la región climática con datos disponibles para ese día y cuya media fuera igual o menor a 2 mm. De esta manera se asegura el mayor número de observaciones posibles a utilizar en la corrección incluyendo meses que de otra manera serían descartados, pero evitando introducir grandes errores ya que sólo se rellenan días de poca o nula precipitación. 

Una vez completado el relleno de los datos y eliminados posibles duplicados, se calculó la precipitación total mensual para cada estación. Por último, para cada mes se calculó la media de las estaciones dentro de una misma región climática, así como la media modelada para cada modelo climático, que corresponde al valor medio de las celdas donde se encuentra cada estación meteorológica.

# Bias-correction: trend-preserving quantile mapping 

Los coeficientes de corrección fueron calculados...

# References

Teutschbein, C.; Seibert, J. Bias correction of regional climate model simulations for hydrological
climate-change impact studies: Review and evaluation of different methods. J. Hydrol. Reg. Stud.
2012, 456, 12–29. 

Lucatero, D.; Madsen, H.; Refsgaard, J.C.; Kidmose, J.; Jensen, K.H. On the skill of raw and post-processed
ensemble seasonal meteorological forecasts in Denmark. Hydrol. Earth Syst. Sci. 2018, 22, 6591–6609.
