
# Instrucciones
# Worland et al. (1984) administraron medidas cognitivas (verbal, visual y memory) y de logro escolar (Reading, math  y spelling) a una muestra de 158 adolescentes. También recogieron información sobre el ajuste de la clase (motivation, harmony y emotional stability) y midieron el nivel socioeconómico de la familia (SES) y el nivel de sintomatología psiquiátrica de los padres.
# Ejecuta el código e interpreta los resultados


# Leer los datos y cargar la librería de lavaan
data <- read.csv("worland_data.csv")
# install.packages("lavaan")
library(lavaan)



###########
# STEP 1 #
###########

# Construir el modelo en lenguaje de lavaan y guardarlo en "model"
cfa.model <- '
# Efectos directos de los factores en los indicadores
 Cognitive =~ verbal + visual + memory
 Achieve =~ read + math + spell
 Adjust =~ motive + harmony + stable
 Risk =~ parent + ses 

# Varianzas de los indicadores
verbal ~~ verbal
visual ~~ visual
memory ~~ memory
read ~~ read
math ~~ math
spell ~~ spell
parent ~~ parent
ses ~~ ses
motive ~~ motive
harmony ~~ harmony 
stable ~~ stable

# Varianzas y covarianzas de los factores
Cognitive ~~ Cognitive + Achieve + Adjust + Risk
Achieve ~~ Achieve + Adjust + Risk
Adjust ~~ Adjust + Risk
Risk ~~ Risk
'

# Ajustar el modelo a los datos
fit.cfa <- cfa(model = cfa.model, data = data)


# Pedir un resumen con índices de ajuste, estimaciones estandarizadas y R^2 de los ítems (varianza de cada ítem explicada por el factor)
summary(fit.cfa, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Examinar los residuos estandarizados y correlaciones residuales
lavaan::residuals(fit.cfa, type = "standardized.mplus")
lavaan::residuals(fit.cfa, type = "cor.bollen")


# calculate factor reliability coefficients (semTools)
# install.packages(semTools)
# library(semTools)
reliability(fit.cfa)


##########
# STEP 2 #
##########

# Vamos a sustituir ahora las covarianzas entre factores por efectos directos (recuerda que los autores plantearon inicialmente que no había efecto entre Achievement y Adjustment)

# Construir el modelo en lenguaje de lavaan y guardarlo en "model"
sem.model <- '
# Efectos directos de los factores en los indicadores
 Cognitive =~ verbal + visual + memory
 Achieve =~ read + math + spell
 Adjust =~ motive + harmony + stable
 Risk =~ parent + ses 

# Efectos directos del modelo estructural
 Achieve ~ a*Cognitive + b*Risk
 Adjust ~ c*Cognitive + d*Risk 

# Varianzas de los indicadores
 verbal ~~ verbal
 visual ~~ visual
 memory ~~ memory
 read ~~ read
 math ~~ math
 spell ~~ spell
 parent ~~ parent
 ses ~~ ses
 motive ~~ motive
 harmony ~~ harmony 
 stable ~~ stable

# Varianzas y covarianzas de los factores (por defecto, lavaan libera la covarianza entre variables endogenas en un modelo estructural cuando no hay un efecto directo entre ellas; por lo tanto, este parametro se fija explicitamente en cero en este analisis)

 Cognitive ~~ Cognitive + Risk
 Achieve ~~ Achieve + 0*Adjust
 Adjust ~~ Adjust
 Risk ~~ Risk

# Vamos a incluir estas restricciones en los efectos directos porque, si no lo hacemos, nos estimará efectos de regresión extraños, como un efecto negativo de la capacidad cognitiva (Cognitive) en el logro escolar (Achievement), que realmente debería ser positivo.
 a > 0
 b < 0
 c > 0
 d < 0
'

# Ajustar el modelo a los datos
fit.sem <- sem(model = sem.model, data = data)


# Pedir un resumen con índices de ajuste, estimaciones estandarizadas y R^2 de los ítems (varianza de cada ítem explicada por el factor)
summary(fit.sem, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Examinar los residuos estandarizados y correlaciones residuales
lavaan::residuals(fit.sem, type = "standardized.mplus")
lavaan::residuals(fit.sem, type = "cor.bollen")

# Evaluamos el desajuste mediante la diferencia de chi-cuadrado entre el modelo CFA (solo modelo de medida) y el modelo que incluye modelo estructural y de medida
lavTestLRT(fit.cfa, fit.sem)

# También podemos evaluar el ajuste solo de la parte estructural usando el RMSEA-P de McDonald y Ho (2002)
lrt   <- lavTestLRT(fit.cfa, fit.sem)
dchi  <- lrt$`Chisq diff`[2]
ddf   <- lrt$`Df diff`[2]
N     <- lavInspect(fit.sem, "ntotal")
rmsea_p <- sqrt(max(dchi - ddf, 0) / (ddf * (N - 1)))
rmsea_p


###############################
# Reespecificacion del modelo #
###############################

# Los residuos estandarizados y correlaciones residuales nos indican que la relacion entre los ítems de Achievement y los de Adjustment no está correctamente reproducida por el modelo. Vamos a optar por permitir que covarien los factores de Achievement y Adjustment.

sem.model <- '
# Efectos directos de los factores en los indicadores
 Cognitive =~ verbal + visual + memory
 Achieve =~ read + math + spell
 Adjust =~ motive + harmony + stable
 Risk =~ parent + ses 

# Efectos directos del modelo estructural
 Achieve ~ a*Cognitive + b*Risk
 Adjust ~ c*Cognitive + d*Risk 

# Varianzas de los indicadores
 verbal ~~ verbal
 visual ~~ visual
 memory ~~ memory
 read ~~ read
 math ~~ math
 spell ~~ spell
 parent ~~ parent
 ses ~~ ses
 motive ~~ motive
 harmony ~~ harmony 
 stable ~~ stable

# Varianzas y covarianzas de los factores
 Cognitive ~~ Cognitive + Risk
 Achieve ~~ Achieve
 Adjust ~~ Adjust
 Risk ~~ Risk

# Vamos a incluir estas restricciones en los efectos directos porque, si no lo hacemos, nos estimará efectos de regresión extraños, como un efecto negativo de la capacidad cognitiva (Cognitive) en el logro escolar (Achievement), que realmente debería ser positivo.
 a > 0
 b < 0
 c > 0
 d < 0
'

# Ajustar el modelo a los datos
fit.sem <- sem(model = sem.model, data = data)


# Pedir un resumen con índices de ajuste, estimaciones estandarizadas y R^2 de los ítems (varianza de cada ítem explicada por el factor)
summary(fit.sem, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Examinar los residuos estandarizados y correlaciones residuales
lavaan::residuals(fit.sem, type = "standardized.mplus")
lavaan::residuals(fit.sem, type = "cor.bollen")
