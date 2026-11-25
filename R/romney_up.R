
# Instrucciones:
# Romney et al. (1992) plantearon la hipótesis de que el estado de ánimo del paciente transmite los efectos de la disfunción neurológica y el estatus socioeconómico (SES) disminuido sobre los síntomas físicos y las relaciones sociales. El modelo médico convencional, representado a la derecha, propone un patrón diferente de relaciones causales entre las mismas variables.

# Ejecuta el código e interpreta los resultados


# Leer los datos y carga la librería de lavaan
data <- read.csv("romney_data.csv")
# install.packages (lavaan)
library(lavaan)


# Observa las primeras 10 filas de los datos y los nombres de las variables
head(data, n = 10)



# Construir los modelos en lenguaje de lavaan
psychosomatic.model <- '
# Efectos directos
 morale ~ ses + dysfunction
 relations ~ morale
 symptoms ~ morale
 
# Varianzas
 morale ~~ morale
 ses ~~ ses
 dysfunction ~~ dysfunction
 symptoms ~~ symptoms
 relations ~~ relations
 
# Covarianzas (indicamos que la relación entre sintomas y relaciones es 0 en el modelo)
 ses ~~ dysfunction
 symptoms ~~ 0*relations
'

medical.model <- '
# Efectos directos
 ses ~ symptoms + dysfunction
 morale ~ symptoms + ses
 relations ~ dysfunction + morale
 
# Varianzas
 morale ~~ morale
 ses ~~ ses
 dysfunction ~~ dysfunction
 symptoms ~~ symptoms
 relations ~~ relations
 
# Covarianzas
 symptoms ~~ dysfunction
'


# Ajustar modelos a los datos
psychosomatic.fit <- sem(model = psychosomatic.model, data = data)
medical.fit <- sem(model = medical.model, data = data)


# Resumen de los resultados del modelo psicosomatico. Pedimos los índices de ajuste y los R^2. Debajo pedimos también los residuos estandarizados y correlaciones residuales.
summary(psychosomatic.fit, fit.measures = TRUE, rsquare = TRUE)

residuals(psychosomatic.fit, type = "standardized.mplus")
residuals(psychosomatic.fit, type = "cor.bollen")


# Repetimos lo mismo para el modelo médico convencional
summary(medical.fit, fit.measures = TRUE, rsquare = TRUE)

residuals(medical.fit, type = "standardized.mplus")
residuals(medical.fit, type = "cor.bollen")


# Aquí podemos ver los AIC y BIC de cada modelo, aunque solo los muestra y no hace un contraste de hipótesis para compararlos.
AIC(psychosomatic.fit, medical.fit)
BIC(psychosomatic.fit, medical.fit)

# Si queremos un contraste formal, el paquete nonnest2 aplica la prueba de Vuong para modelos no anidados y da intervalos de confianza para la diferencia en AIC y BIC
library(nonnest2)
vuongtest(psychosomatic.fit, medical.fit)
icci(psychosomatic.fit, medical.fit)

# Importante: preferir un modelo a otro por AIC o BIC no prueba que su estructural causal sea la correcta, porque puede haber modelos equivalentes que ajustan igual de bien.


