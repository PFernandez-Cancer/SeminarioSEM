# Instrucciones:
# Lynam et al. (1993) midieron el estatus socioeconomico familiar (SES), el IQ verbal, la motivacion durante el test de IQ (test effort), el logro escolar y la delincuencia en muestras de adolescentes Negros (n=214) y Blancos (n=181) de entre 12 y 13 años. La hipotesis era que permanecer en el colegio es un factor protector de la delincuencia mas fuerte para adolescentes Negros que para Blancos, especialmente en las areas urbanas de bajo SES con hogares monoparentales.
# Ejecuta el código e interpreta los resultados



# Leer los datos y cargar la librería de lavaan
data <- read.csv("lynam_data.csv")
# install.packages("lavaan")
library(lavaan)

# Observar los primeros 10 casos de la base de datos y los nombres de las variables
head(data, 10)


# Construir el modelo en lenguaje de lavaan y guardarlo en "model"
model <- "
 # Efectos de regresion
 achieve ~ ses + effort + viq
 delinq ~ achieve + ses + effort + viq
 
 # Varianzas endogenas
 achieve ~~ achieve
 delinq ~~ delinq
 
 # Varianzas y covarianzas exogenas
 ses ~~ ses + effort + viq
 effort ~~ effort + viq
 viq ~~ viq
 
 # Medias e intersecciones
 achieve ~ 1
 delinq ~ 1
 ses ~ 1
 effort ~ 1
 viq ~ 1
"


# MODELO 1 ----
### Ajuste de modelo 1. Todos los efectos directos iguales entre grupos
fit1 <- sem(model = model,
            data = data, 
            group = "race",
            group.equal = c("regressions"))

### Estimaciones de los parámetros del modelo 1:
summary(fit1, standardized = TRUE, fit.measures = TRUE)

### Residuos estandarizados (calculados como en mplus) y correlaciones residuales, para cada uno de los dos grupos:
residuals(fit1, type = "standardized.mplus")
residuals(fit1, type = "cor.bollen")



# MODELO 2 ----
### Ajuste de modelo 2. Todos los efectos directos iguales, excepto achieve -> delinq
fit2 <- sem(model = model,
            data = data,
            group = "race",
            group.equal = c("regressions"),
            group.partial = c("delinq ~ achieve"))

### Estimaciones de los parámetros del modelo 2:
summary(fit2, standardized = TRUE, fit.measures = TRUE)

### Residuos estandarizados (calculados como en mplus) y correlaciones residuales, para cada uno de los dos grupos:
residuals(fit2, type = "standardized.mplus")
residuals(fit2, type = "cor.bollen")



# MODELO 3 ----
### Ajuste de modelo 3. Todos los efectos directos e intersecciones iguales, excepto por el efecto directo achieve -> delinq y todas las intersecciones menos la de achievement. Dicho de otro modo, al modelo anterior le añadimos la restriccion de que la interseccion de achievement sea igual en los dos grupos.
fit3 <- sem(model = model,
            data = data,
            group = "race",
            group.equal = c("regressions", "intercepts"),
            group.partial = c("delinq ~ achieve", "delinq ~ 1", "ses ~ 1", "effort ~ 1", "viq ~ 1"))

### Estimaciones de los parámetros del modelo 3:
summary(fit3, standardized = TRUE, fit.measures = TRUE)

### Residuos estandarizados (calculados como en mplus) y correlaciones residuales, para cada uno de los dos grupos:
residuals(fit3, type = "standardized.mplus")
residuals(fit3, type = "cor.bollen")



# MODELO 4 ----
### Objetivo: Partiendo del modelo 3, añadir la restricción de que la intersección de delinquency sea igual para los dos grupos.
fit4 <- sem(model = model,
            data = data,
            group = "race",
            group.equal = c("regressions", "intercepts"),
            group.partial = c("delinq ~ achieve", "ses ~ 1", "effort ~ 1", "viq ~ 1"))

### Estimaciones de los parámetros del modelo 4:
summary(fit4, standardized = TRUE, fit.measures = TRUE)

### Residuos estandarizados (calculados como en mplus) y correlaciones residuales, para cada uno de los dos grupos:
residuals(fit4, type = "standardized.mplus")
residuals(fit4, type = "cor.bollen")



# Podemos especificar que índices queremos y guardarlos en ind.ajuste (o cualquier otro nombre)
ind.ajuste <- c("chisq", "df", "pvalue", 
              "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr", "cfi", "tli")



# Con la función fitMeasures, le decimos que, de los distintos modelos ajustados, nos dé solo los índices de ajuste que hemos guardado en el objeto ind.ajuste.
fitMeasures(fit1, ind.ajuste)
fitMeasures(fit2, ind.ajuste)
fitMeasures(fit3, ind.ajuste)
fitMeasures(fit4, ind.ajuste)

# Con la función anova, comparamos los modelos a nivel de chi-cuadrado y RMSEA. El valor p nos indica si el ajuste del modelo es significativamente distinto entre un modelo y otro.
anova(fit1, fit2)
anova(fit2, fit3)
anova(fit3, fit4)





