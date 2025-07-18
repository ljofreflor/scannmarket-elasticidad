# Introducción

Este informe analiza la **elasticidad precio-demanda** de los productos,
utilizando modelos estadísticos avanzados para obtener estimaciones
robustas y útiles para la toma de decisiones comerciales.

En el análisis de elasticidad precio-demanda, es fundamental entender
cómo el precio afecta las ventas y qué otros factores pueden influir en
la relación. Por eso, se parte de un **modelo base** de regresión lineal
simple, que estima el efecto del precio sobre la cantidad vendida sin
considerar otras fuentes de variabilidad.

Sin embargo, en mercados reales existen múltiples fuentes de
heterogeneidad: diferencias entre productos, puntos de venta,
estacionalidad, promociones, etc. Para capturar esta complejidad y
obtener estimaciones más robustas y realistas, se avanza a un **modelo
mixto** que incorpora efectos aleatorios y fijos adicionales. Este
enfoque permite:

- Controlar por la variabilidad entre tiendas y productos.
- Ajustar por estacionalidad y otros factores estructurales.
- Obtener una elasticidad más precisa y útil para decisiones
  comerciales.

La comparación entre ambos modelos permite cuantificar cuánto aporta el
precio por sí solo y cuánto mejora la explicación de las ventas al
considerar la estructura completa del mercado.

------------------------------------------------------------------------

# Datos y Preprocesamiento

    library(dplyr)
    library(lme4)
    # Cargar datos
    raw <- read.csv('datos/ventas_procesadas.csv')
    # Resumen inicial
    glimpse(raw)

    ## Rows: 242,246
    ## Columns: 12
    ## $ fecha_comercial <chr> "2020-06-02", "2020-06-17", "2020-06-11", "2020-06-02"…
    ## $ pdv_codigo      <int> 514, 8155, 7604, 3207, 8008, 1454, 210, 7748, 7426, 43…
    ## $ codigo_barras   <dbl> 7.730400e+12, 7.734284e+12, 7.730400e+12, 7.734284e+12…
    ## $ imp_vta         <dbl> 69.00, 70.00, 77.00, 70.00, 84.00, 70.00, 77.00, 77.00…
    ## $ cant_vta        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 6, 3, 1, 1, 1, 1, 2, 1, 3, …
    ## $ month_year      <chr> "2020-06", "2020-06", "2020-06", "2020-06", "2020-06",…
    ## $ mes             <int> 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, …
    ## $ semana_mes      <int> 1, 3, 2, 1, 1, 2, 1, 4, 4, 4, 1, 3, 2, 3, 1, 2, 1, 4, …
    ## $ precio          <dbl> 69.00000, 70.00000, 77.00000, 70.00000, 84.00000, 70.0…
    ## $ log_precio      <dbl> 4.234107, 4.248495, 4.343805, 4.248495, 4.430817, 4.24…
    ## $ log_cant_vta    <dbl> 0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.0000000,…
    ## $ dia_semana      <chr> "Tuesday", "Wednesday", "Thursday", "Tuesday", "Sunday…

    # Limpieza y preparación
    data <- raw %>%
      filter(!is.na(log_cant_vta), !is.na(log_precio)) %>%
      filter(is.finite(log_cant_vta), is.finite(log_precio)) %>%
      mutate(
        mes = as.factor(mes),
        pdv_codigo = as.factor(pdv_codigo),
        codigo_barras = as.factor(codigo_barras)
      )

------------------------------------------------------------------------

# Modelo Base: Regresión Lineal Simple

    modelo_base <- lm(log_cant_vta ~ log_precio, data = data)
    base_summary <- summary(modelo_base)
    kable(base_summary$coefficients, caption = 'Coeficientes modelo base', digits = 4)

<table>
<caption>Coeficientes modelo base</caption>
<thead>
<tr>
<th style="text-align: left;"></th>
<th style="text-align: right;">Estimate</th>
<th style="text-align: right;">Std. Error</th>
<th style="text-align: right;">t value</th>
<th style="text-align: right;">Pr(&gt;|t|)</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">(Intercept)</td>
<td style="text-align: right;">1.1336</td>
<td style="text-align: right;">0.0262</td>
<td style="text-align: right;">43.2779</td>
<td style="text-align: right;">0</td>
</tr>
<tr>
<td style="text-align: left;">log_precio</td>
<td style="text-align: right;">-0.1952</td>
<td style="text-align: right;">0.0059</td>
<td style="text-align: right;">-32.9608</td>
<td style="text-align: right;">0</td>
</tr>
</tbody>
</table>

    cat('R²:', round(base_summary$r.squared, 4), '\n')

    ## R²: 0.0045

> **Interpretación:** El modelo base estima la elasticidad sin controlar
> por estacionalidad ni heterogeneidad. Su poder explicativo es
> limitado.

------------------------------------------------------------------------

# Modelo Mixto: Efectos Aleatorios y Estacionales

    modelo_mixto <- lmer(log_cant_vta ~ log_precio + mes + (1 | pdv_codigo) + (1 | codigo_barras), data = data)
    mixto_summary <- summary(modelo_mixto)
    kable(mixto_summary$coefficients, caption = 'Coeficientes modelo mixto', digits = 4)

<table>
<caption>Coeficientes modelo mixto</caption>
<thead>
<tr>
<th style="text-align: left;"></th>
<th style="text-align: right;">Estimate</th>
<th style="text-align: right;">Std. Error</th>
<th style="text-align: right;">t value</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">(Intercept)</td>
<td style="text-align: right;">4.3201</td>
<td style="text-align: right;">0.0754</td>
<td style="text-align: right;">57.2912</td>
</tr>
<tr>
<td style="text-align: left;">log_precio</td>
<td style="text-align: right;">-0.9520</td>
<td style="text-align: right;">0.0158</td>
<td style="text-align: right;">-60.2264</td>
</tr>
<tr>
<td style="text-align: left;">mes7</td>
<td style="text-align: right;">0.0013</td>
<td style="text-align: right;">0.0042</td>
<td style="text-align: right;">0.2988</td>
</tr>
<tr>
<td style="text-align: left;">mes8</td>
<td style="text-align: right;">0.0544</td>
<td style="text-align: right;">0.0038</td>
<td style="text-align: right;">14.1441</td>
</tr>
<tr>
<td style="text-align: left;">mes9</td>
<td style="text-align: right;">0.0870</td>
<td style="text-align: right;">0.0039</td>
<td style="text-align: right;">22.5302</td>
</tr>
<tr>
<td style="text-align: left;">mes10</td>
<td style="text-align: right;">0.0841</td>
<td style="text-align: right;">0.0039</td>
<td style="text-align: right;">21.7342</td>
</tr>
<tr>
<td style="text-align: left;">mes11</td>
<td style="text-align: right;">0.1028</td>
<td style="text-align: right;">0.0039</td>
<td style="text-align: right;">26.6273</td>
</tr>
</tbody>
</table>

    print(paste('AIC:', round(AIC(modelo_mixto), 2), '| BIC:', round(BIC(modelo_mixto), 2)))

    ## [1] "AIC: 267001.76 | BIC: 267105.74"

    # Mostrar el summary completo
    print(mixto_summary)

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## log_cant_vta ~ log_precio + mes + (1 | pdv_codigo) + (1 | codigo_barras)
    ##    Data: data
    ## 
    ## REML criterion at convergence: 266981.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -4.7418 -0.6564 -0.4022  0.7520  7.4413 
    ## 
    ## Random effects:
    ##  Groups        Name        Variance Std.Dev.
    ##  pdv_codigo    (Intercept) 0.01894  0.1376  
    ##  codigo_barras (Intercept) 0.02013  0.1419  
    ##  Residual                  0.17491  0.4182  
    ## Number of obs: 242246, groups:  pdv_codigo, 470; codigo_barras, 22
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept)  4.320098   0.075406  57.291
    ## log_precio  -0.952039   0.015808 -60.226
    ## mes7         0.001269   0.004248   0.299
    ## mes8         0.054414   0.003847  14.144
    ## mes9         0.087037   0.003863  22.530
    ## mes10        0.084136   0.003871  21.734
    ## mes11        0.102758   0.003859  26.627
    ## 
    ## Correlation of Fixed Effects:
    ##            (Intr) lg_prc mes7   mes8   mes9   mes10 
    ## log_precio -0.911                                   
    ## mes7       -0.025 -0.005                            
    ## mes8        0.129 -0.182  0.580                     
    ## mes9        0.148 -0.202  0.577  0.745              
    ## mes10       0.203 -0.263  0.576  0.752  0.758       
    ## mes11       0.219 -0.281  0.576  0.754  0.759  0.783

    # Analizar varianzas de los efectos aleatorios
    var_pdvs <- as.numeric(mixto_summary$varcor$pdv_codigo)
    var_skus <- as.numeric(mixto_summary$varcor$codigo_barras)
    var_residual <- attr(mixto_summary$varcor, 'sc')^2
    print(paste('Varianza PDV:', round(var_pdvs, 4)))

    ## [1] "Varianza PDV: 0.0189"

    print(paste('Varianza SKU:', round(var_skus, 4)))

    ## [1] "Varianza SKU: 0.0201"

    print(paste('Varianza residual:', round(var_residual, 4)))

    ## [1] "Varianza residual: 0.1749"

### Análisis de varianzas y conclusiones

- **Varianza de PDV (punto de venta):** Indica la dispersión de ventas
  entre tiendas. Valores altos sugieren que el comportamiento de ventas
  varía mucho entre locales.
- **Varianza de SKU (código de barras):** Mide la heterogeneidad entre
  productos. Si es alta, los productos tienen patrones de venta muy
  distintos.
- **Varianza residual:** Es la parte no explicada por el modelo. Un
  valor bajo indica que el modelo captura bien la variabilidad de las
  ventas.
- **Conclusión:** Si las varianzas de los efectos aleatorios son altas,
  hay mucha heterogeneidad entre tiendas y productos. Si la varianza
  residual es baja, el modelo explica bien la variabilidad y es adecuado
  para estimar elasticidad y tomar decisiones de pricing.

### Análisis de varianza de los efectos aleatorios

El modelo mixto estima las siguientes varianzas:

- **Varianza PDV (punto de venta):** 0.0189
- **Varianza SKU (producto):** 0.0201
- **Varianza residual:** 0.1749

**Interpretación detallada:**

- La **varianza de PDV** (0.0189) indica que existen diferencias
  moderadas en el nivel base de ventas entre los distintos puntos de
  venta. Esto sugiere que el canal o local donde se comercializa el
  producto tiene un impacto relevante, pero no extremo, en la cantidad
  vendida.
- La **varianza de SKU** (0.0201) muestra que los productos (códigos de
  barras) también presentan heterogeneidad en sus ventas. Es decir, hay
  diferencias entre productos, pero la magnitud es similar a la de los
  PDV, lo que implica que tanto el tipo de producto como el punto de
  venta contribuyen de forma comparable a la variabilidad total.
- La **varianza residual** (0.1749) es considerablemente mayor que las
  varianzas de los efectos aleatorios. Esto significa que, aunque el
  modelo captura parte de la heterogeneidad entre tiendas y productos,
  aún existe una proporción importante de variabilidad en las ventas que
  no es explicada por los factores incluidos (precio, mes, PDV, SKU).

**Conclusión práctica:**

- El modelo mixto logra controlar parcialmente la heterogeneidad
  estructural del mercado, pero la mayor parte de la variabilidad de las
  ventas se debe a factores no modelados (por ejemplo, promociones,
  competencia, estacionalidad fina, quiebres de stock, etc.).
- Para mejorar la capacidad explicativa del modelo, sería recomendable
  incorporar más covariables relevantes o información adicional sobre el
  contexto comercial.
- La elasticidad estimada bajo este modelo es robusta y confiable para
  decisiones de pricing, pero se debe considerar que existen fuentes de
  variabilidad no capturadas que pueden afectar la precisión de las
  predicciones en escenarios específicos.

------------------------------------------------------------------------

# Visualización: Efecto del Precio

    library(ggplot2)
    ggplot(data, aes(x = log_precio, y = log_cant_vta)) +
      geom_point(alpha = 0.2) +
      geom_smooth(method = 'lm', color = 'blue', se = FALSE) +
      labs(title = 'Relación log(precio) vs log(cantidad vendida)', x = 'log(precio)', y = 'log(cant_vta)') +
      theme_minimal()

![](elasticidad_files/figure-markdown_strict/unnamed-chunk-4-1.png)

------------------------------------------------------------------------

# Resultados y Conclusiones

    elasticidad <- fixef(modelo_mixto)["log_precio"]
    cat('Elasticidad estimada (modelo mixto):', round(elasticidad, 4), '\n')

    ## Elasticidad estimada (modelo mixto): -0.952

    if (elasticidad < -1) {
      cat('Demanda elástica: consumidores muy sensibles al precio\n')
    } else if (elasticidad > -1 && elasticidad < 0) {
      cat('Demanda inelástica: consumidores poco sensibles al precio\n')
    } else {
      cat('Elasticidad inusual: revisar modelo o datos\n')
    }

    ## Demanda inelástica: consumidores poco sensibles al precio

### Interpretación del coeficiente de log(precio) en el modelo mixto

- El coeficiente estimado para **log(precio)** en el modelo mixto
  representa la elasticidad precio-demanda controlando por
  estacionalidad y heterogeneidad entre tiendas y productos.
- Por ejemplo, si el coeficiente es **–0.95**, esto significa que un
  aumento del 1% en el precio genera una disminución aproximada del
  0.95% en la cantidad vendida, manteniendo constantes los demás
  factores.
- Este valor indica una **demanda casi unitaria**: los consumidores
  responden de forma proporcional a los cambios de precio, lo que
  implica que la política de precios tiene un impacto directo y
  relevante en el volumen de ventas.

### Acciones comerciales recomendadas

- **Ajuste de precios:** Dado que la demanda es sensible al precio, se
  recomienda analizar cuidadosamente cualquier incremento, ya que puede
  traducirse en una caída proporcional de las ventas. Si el objetivo es
  maximizar ingresos, se puede calcular el punto óptimo de precio
  considerando la elasticidad estimada.

- **Segmentación de productos y tiendas:** Las varianzas encontradas
  sugieren que existen diferencias entre productos y puntos de venta. Se
  recomienda personalizar estrategias de pricing y promociones según el
  tipo de producto y el canal.

- **Promociones focalizadas:** Para productos o tiendas con mayor
  sensibilidad al precio, implementar descuentos temporales puede
  generar incrementos significativos en el volumen vendido.

- **Monitoreo continuo:** Dado que existe variabilidad residual no
  explicada, es importante monitorear el mercado y ajustar el modelo con
  nuevas variables relevantes (promociones, competencia, estacionalidad
  fina, etc.) para mejorar la precisión de las predicciones.

- La elasticidad estimada es **robusta (estadísticamente significativa)
  y útil** para decisiones de pricing.

------------------------------------------------------------------------

# Apéndice: Comparación de Modelos

    # Comparación de varianza explicada por log_precio
    # Modelo base: R² ya representa la varianza explicada total por log_precio
    var_exp_base <- base_summary$r.squared

    # Modelo mixto: calcular el marginal R² (efectos fijos) y el condicional R² (efectos fijos + aleatorios)
    library(MuMIn)
    r2_mixto <- r.squaredGLMM(modelo_mixto)
    var_exp_mixto_marginal <- r2_mixto[1] # Solo efectos fijos (incluye log_precio)
    var_exp_mixto_condicional <- r2_mixto[2] # Efectos fijos + aleatorios

    comp <- data.frame(
      Modelo = c('Base', 'Mixto'),
      Elasticidad = c(coef(modelo_base)["log_precio"], fixef(modelo_mixto)["log_precio"]),
      R2_total = c(var_exp_base, var_exp_mixto_condicional),
      R2_log_precio = c(var_exp_base, var_exp_mixto_marginal),
      AIC = c(AIC(modelo_base), AIC(modelo_mixto)),
      BIC = c(BIC(modelo_base), BIC(modelo_mixto))
    )
    kable(comp, caption = 'Comparación de modelos y varianza explicada', digits = 4)

<table>
<caption>Comparación de modelos y varianza explicada</caption>
<thead>
<tr>
<th style="text-align: left;">Modelo</th>
<th style="text-align: right;">Elasticidad</th>
<th style="text-align: right;">R2_total</th>
<th style="text-align: right;">R2_log_precio</th>
<th style="text-align: right;">AIC</th>
<th style="text-align: right;">BIC</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Base</td>
<td style="text-align: right;">-0.1952</td>
<td style="text-align: right;">0.0045</td>
<td style="text-align: right;">0.0045</td>
<td style="text-align: right;">290433.5</td>
<td style="text-align: right;">290464.6</td>
</tr>
<tr>
<td style="text-align: left;">Mixto</td>
<td style="text-align: right;">-0.9520</td>
<td style="text-align: right;">0.2535</td>
<td style="text-align: right;">0.0868</td>
<td style="text-align: right;">267001.8</td>
<td style="text-align: right;">267105.7</td>
</tr>
</tbody>
</table>

### Interpretación de la varianza explicada

- En el **modelo base**, el R² representa la proporción de la varianza
  total de las ventas explicada únicamente por el precio. Si el valor es
  bajo, el precio por sí solo no explica gran parte de la variabilidad.
- En el **modelo mixto**, el R² marginal indica la varianza explicada
  por los efectos fijos (incluyendo log\_precio y mes), mientras que el
  R² condicional incluye también los efectos aleatorios (PDV y SKU).
  Comparar ambos valores permite ver cuánto aporta el precio y los
  efectos fijos respecto a la estructura completa del modelo.
- Si el R² marginal del modelo mixto es mucho mayor que el R² del modelo
  base, significa que el precio y los efectos fijos explican mejor la
  variabilidad cuando se controla por heterogeneidad. Si el R²
  condicional es mucho mayor, los efectos aleatorios (diferencias entre
  tiendas y productos) son relevantes para explicar las ventas.

------------------------------------------------------------------------

# Referencias

- Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting
  Linear Mixed-Effects Models Using lme4. *Journal of Statistical
  Software*, 67(1), 1–48.
- R Core Team (2025). *R: A language and environment for statistical
  computing*. R Foundation for Statistical Computing, Vienna, Austria.
