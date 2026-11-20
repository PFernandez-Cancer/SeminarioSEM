
# Instrucciones:
# La primera edición de la Kaufman Assessment Battery for Children (KABC-I) es un test de habilidad cognitiva para niños de edades de entre los 2.5 y los 12.5 años. Los autores afirmaron que los 8 subtests representados en el siguiente diagrama miden dos factores. Las tres tareas que se piensa que reflejan procesamiento secuencial requieren todas recuperar correctamente un estímulo auditivo (Number Recall, Word Order) o visual (Hand Movements) en un orden particular. Las otras cinco se supone que miden un procesamiento simultáneo más holístico y un razonamiento menos dependiente del orden.
# Ejecuta el código e interpreta los resultados


# Leer los datos y carga la librería de lavaan
data <- read.csv("kabc_data.csv")
library(lavaan) # Si da error, utiliza install.packages("lavaan") y vuelve a ejecutarlo


###########
# PARTE 1 #
###########

# Construir el modelo de 1 factor en lenguaje de lavaan y guardarlo en "model"
model1 <- '
# Efectos directos del factor sobre los items (lavaan fija por defecto el primer peso factorial a 1; para anular esto multiplicamos el primer item por NA)
General =~ NA*hm + nr + wo + gc + tr + sm + ma + ps 

# Varianza del factor (fijada a 1) y de los items
General ~~ 1*General
hm ~~ hm
nr ~~ nr
wo ~~ wo
gc ~~ gc
tr ~~ tr
sm ~~ sm
ma ~~ ma
ps ~~ ps
'

# Ajustar el modelo a los datos
fit1 <- sem(model = model1, data = data)


# Pedir un resumen con índices de ajuste, estimaciones estandarizadas y R^2 de los ítems (varianza de cada ítem explicada por el factor)
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Examinar los residuos estandarizados y correlaciones residuales
residuals(fit1, type = "standardized.mplus")
residuals(fit1, type = "cor.bollen")



###########
# PARTE 2 #
###########

# Como el modelo de un factor no ajusta bien a los datos, vamos a probar el modelo de dos factores (factor secuencial y factor simultaneo)
model2 <- '
# Efectos directos del factor sobre los items (lavaan fija por defecto el primer peso factorial a 1; para anular esto multiplicamos el primer item por NA)
Sequent =~ NA*hm + nr + wo
Simultan =~ NA*gc + tr + sm + ma + ps

# Varianza del factor (fijada a 1) y de los items
Sequent ~~ 1*Sequent
Simultan ~~ 1*Simultan
hm ~~ hm
nr ~~ nr
wo ~~ wo
gc ~~ gc
tr ~~ tr
sm ~~ sm
ma ~~ ma
ps ~~ ps

# Covarianza entre los factores
Sequent ~~ Simultan
'

# Ajustar el modelo a los datos
fit2 <- sem(model = model2, data = data)


# Pedir un resumen con índices de ajuste, estimaciones estandarizadas y R^2 de los ítems (varianza de cada ítem explicada por el factor)
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Examinar los residuos estandarizados y correlaciones residuales
residuals(fit2, type = "standardized.mplus")
residuals(fit2, type = "cor.bollen")



###########
# PARTE 3 #
###########

# Hemos encontrado unas correlaciones residuales demasiado altas entre el item Hand Movements (hm) y los ítems del factor de Procesamiento Simultaneo. Vamos a probar a incluir en el modelo un efecto directo del factor de Procesamiento Simultaneo en el item Hand Movements:
model3 <- '
# Efectos directos del factor sobre los items (lavaan fija por defecto el primer peso factorial a 1; para anular esto multiplicamos el primer item por NA)
Sequent =~ NA*hm + nr + wo
Simultan =~ NA*hm + gc + tr + sm + ma + ps

# Varianza del factor (fijada a 1) y de los items
Sequent ~~ 1*Sequent
Simultan ~~ 1*Simultan
hm ~~ hm
nr ~~ nr
wo ~~ wo
gc ~~ gc
tr ~~ tr
sm ~~ sm
ma ~~ ma
ps ~~ ps

# Covarianza entre los factores
Sequent ~~ Simultan
'

# Ajustar el modelo a los datos
fit3 <- sem(model = model3, data = data)


# Pedir un resumen con índices de ajuste, estimaciones estandarizadas y R^2 de los ítems (varianza de cada ítem explicada por el factor)
summary(fit3, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Examinar los residuos estandarizados y correlaciones residuales
residuals(fit3, type = "standardized.mplus")
residuals(fit3, type = "cor.bollen")
