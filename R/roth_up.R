
# Instrucciones:
# Roth et al. (1989) administraron medidas de ejercicio (exercise), resiliencia (hardiness), condición física (fitness), estrés (stress) y nivel de enfermedad (illness). Propusieron un modelo en el que representaron la hipótesis de que los efectos del ejercicio y de la resiliencia en la enfermedad son puramente indirectos a través de un único intermediario: la condición física para el ejercicio y el estrés para la resiliencia.

# Ejecuta el código e interpreta los resultados


# Leer los datos y carga la librería de lavaan
data <- read.csv("roth_data.csv")
# install.packages (lavaan)
library(lavaan)

###########
# PARTE 1 #
###########

# Construir el modelo en lenguaje de lavaan y guardarlo en "model"
model <- '
# Efectos directos. Para ponerle un nombre/etiqueta a los efectos, premultiplicamos la variable predictora por la etiqueta deseada.
 fitness ~ a*exercise
 stress ~ c*hardy
 illness ~ b*fitness + d*stress
 
# Varianzas
 exercise ~~ exercise
 fitness ~~ fitness
 hardy ~~ hardy
 stress ~~ stress
 illness ~~ illness
 
# Covarianzas
 exercise ~~ hardy

# Vamos a decirle a lavaan que nos de también los efectos indirectos de exercise sobre illness (que se obtiene multiplicando a*b) y de hardiness sobre illness (que se obtiene multiplicando c*d)
 ab := a*b
 cd := c*d 
'

# Ajustar el modelo a los datos
fit <- sem(model = model, data = data)


# Pedir un resumen con indices de ajuste, estimaciones estandarizadas y R^2 de los ítems (varianza de cada ítem explicada por el factor)
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Examinar los residuos estandarizados y correlaciones residuales
residuals(fit, type = "standardized.mplus")
residuals(fit, type = "cor.bollen")



###########
# PARTE 2 #
###########

# Los residuos estandarizados y correlaciones residuales nos indican que la relación entre fitness y stress no está correctamente reproducida por el modelo. Vamos a incluir, por motivos didácticos, un efecto directo de stress a fitness para ilustrar la comparación de grupos.

# Construimos el modelo y lo guardamos en model2
model2 <- '
# Efectos directos. Para ponerle un nombre/etiqueta a los efectos, premultiplicamos la variable predictora por la etiqueta deseada.
 fitness ~ a*exercise
 stress ~ c*hardy
 illness ~ b*fitness + d*stress
 fitness ~ e*stress
 
# Varianzas
 exercise ~~ exercise
 fitness ~~ fitness
 hardy ~~ hardy
 stress ~~ stress
 illness ~~ illness
 
# Covarianzas
 exercise ~~ hardy

# Vamos a decirle a lavaan que nos de también los efectos indirectos de exercise sobre illness (que se obtiene multiplicando a*b) y de hardiness sobre illness (que se obtiene multiplicando c*d)
 ab := a*b
 cd := c*d 
'

# Vamos a ajustar el modelo anterior y el nuevo:
fit1 <- sem(model = model, data = data)
fit2 <- sem(model = model2, data = data)


# Aqui podemos ver un resumen de los resultados de cada modelo. Le pedimos los indices de ajuste, las estimaciones estandarizadas y los R^2.
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Utilizamos la funcion anova para comparar los dos modelos
anova(fit1, fit2)


