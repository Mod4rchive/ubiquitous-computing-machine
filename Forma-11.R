#-------------------------------------------------------------------------------
#         DESARROLLO DE PEP 2 DE INFERENCIA Y MODELOS ESTADÍSTICOS
#-------------------------------------------------------------------------------
# Integrantes: Carlos Castillo        20.201.274-4
#              Joaquín Torres         19.091.702-9
#
# Sección: B-2
# Profesora: Jacqueline Kóhler
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                                 PREGUNTA 1
#-------------------------------------------------------------------------------
cat("\n------------------------------------------------------")
cat("\n                 Pregunta 1")
cat("\n------------------------------------------------------")

# Se ingresa la tabla de datos por medio de GUI
data <- read.csv2(choose.files(),encoding = "UTF-8", dec=",", sep = ";" );

snowtrooper <- filter(data, division == "Snowtrooper");


eval_instructor <- snowtrooper$eval_instructor;
eval_capitan <- snowtrooper$eval_capitan;
eval_comandante <- snowtrooper$eval_comandante;
eval_general <- snowtrooper$eval_comandante;

instancia <- factor (1:100)

datos <- data.frame ( instancia , eval_instructor , eval_capitan , eval_comandante , eval_general )

# Llevar data frame a formato largo .
datos <- datos %>% pivot_longer (c("eval_instructor", "eval_capitan", "eval_comandante",
                                   "eval_general") ,
                                 names_to = "oficiales", values_to = "evaluacion")

datos [["oficiales"]] <- factor ( datos [["oficiales"]])

# Se pide ver las diferencias estadísticas de exigencia entre distintos grupos
# por lo tanto se debe realizar una prueba ANOVA de variables correlacionadas
# (distintos valores para los mismos sujetos).

# Formulación de hipótesis
# H0: No hay diferencias significativas en los niveles de exigencia de los
#     distintos oficiales evaluadores.
# 
# HA: Si hay diferencias significativas en los niveles de exigencia de los
#     distintos oficiales evaluadores.

# Se utiliza alfa = 0.01

# Condiciones de prueba ANOVA
# 1. La escala con que se mide la variable dependiente tiene las propiedades de 
# una escala de intervalos iguales: Se mide con números decimales.
#   
# 2. Las mediciones son independientes al interior de cada grupo: 
#   Se mide a cada individuo por separado.
#   
# 3. Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) 
# una distribución normal.
# 

g <- ggqqplot ( datos , x = "evaluacion", y = "oficiales", color = "oficiales")
g <- g + facet_wrap (~ oficiales )
g <- g + rremove ("x.ticks") + rremove ("x.text")
g <- g + rremove ("y.ticks") + rremove ("y.text")
g <- g + rremove ("axis.title")
print ( g )

# Se comprueba que los datos son normales, forman parte del area sombreada del
# gráfico Q-Q sin valores muy extremos.
# 
# 4. La matriz de varianzas-covarianzas es esférica. 
# 
prueba <- ezANOVA(data = datos, dv = evaluacion , within = oficiales,
                  wid = instancia, return_aov = TRUE )

prueba$`Mauchly's Test for Sphericity`
# La prueba con alfa=0.01 tiene p=0 por lo tanto se cumple que la matriz de 
# varianzas-covarianzas no es esférica.

# Se puede realizar ANOVA
# 
# 
# Se realiza la prueba ANOVA (prueba)
cat("\n Prueba ANOVA: \n")
print(summary(prueba$aov))

# El valor p obtenido en la prueba anova del estadístico F es 3 grados de 
# significancia menor que el alfa elegido (0.01), por lo tanto se rechaza la
# hipótesis nula a favor de la alternativa: Si hay diferencias significativas 
# en los niveles de exigencia de los distintos oficiales evaluadores.


# Procedimiento post-hoc HSD de Tukey .
mixto <- lme ( evaluacion ~ oficiales , data = datos , random = ~1| instancia )
medias <- emmeans ( mixto , "oficiales")
tukey <- pairs ( medias , adjust = "tukey")

cat ("\n\n Prueba HSD de Tukey \n\n")
print ( tukey )

# Se encuentra que las parejas de aquellos soldados evaluados por capitán-comandante;
# capitán-general; capitán-instructor son las mas diferentes en método de evaluación.
# 
# Con esto podemos concluir que el capitán es el único que se diferencia en 
# método de evaluación con los demás.


#-------------------------------------------------------------------------------
#                                 PREGUNTA 2
#-------------------------------------------------------------------------------
cat("\n------------------------------------------------------")
cat("\n                 Pregunta 2")
cat("\n------------------------------------------------------")

# Se pide realizar una regresión logística para saber si los clones y los reclutas
# son distinguibles entre ellos; se pide evaluar un modelo clasificador que contemple 3
# variables predicadoras: armadura, presición y eficiencia; se eligieron estas pues
# son representativas de las habilidades de cada uno.

# Se define la semilla para el generador pseudo-aleatorio
set.seed(4432)

# Se utiliza la variable es_clon como variable binaria
Slogico <- ifelse ( data$es_clon == "S" , 1, 0)
data2 <- cbind( Slogico , data )
data2$es_clon <- data2$Slogico
data2$Slogico <- NULL

# Se crea la muestra de sample size = 400
muestra_total <- sample_n(data2, size = 400)
muestra_total[["id"]] <- seq(1:400)

# Se seleccionan los datos para generar el modelo (entrenamiento) y los para
# probar el modelo (test)
entrenamiento <- filter(muestra_total, id<321)
test <- filter(muestra_total, 320<id & id<401)

# Se ajusta el modelo logistico,
# utilizando las variables armadura, presición y eficiencia.
modelo <- glm( es_clon ~ armadura + presicion + eficiencia , family = binomial ( link = "logit") ,
               data = entrenamiento )
cat("\nModelo Logístico obtenido: \n")
print ( summary ( modelo ) )

# Una vez obtenido el modelo, se realiza la evaluacion considerando lo 
# siguiente:
# - Primero, se seleccionan aquellos casos que esten fuera del rango de la 
#  distancia de cook, es decir, que tengan una distancia de cook mayor a 1.
observaciones <- data.frame ( respuesta_predicha = fitted ( modelo ) ) 
observaciones [["distancia_Cook"]] <- cooks.distance ( modelo ) 

distanciaCook <- which ( observaciones [["distancia_Cook"]] > 1) 
cat ("- \n\nResiduos con una distancia de Cook alta :",distanciaCook , "\n")

# Como resultado, no presenta ningun caso que no cumpla esta condición, 
# por lo que el modelo no presenta datos extremos.

# - Segundo, se realiza la prueba de durwin watson para comprobar la 
#   independencia de los residuos
cat (" \n\nPrueba de Durbin - Watson para autocorrelaciones ") 
cat (" entre errores :\n") 
print ( durbinWatsonTest ( modelo ) ) 

# El valor p obtenido es de 0.086, siendo mayor al nivel de significancia 
# a evaluar (alfa = 0.01), por lo que el modelo no presenta autocorrelación.

# - Finalmente, se comprobar la multicolinealidad, evaluando el VIF promedio. 
vifs <- vif ( modelo ) 
cat ("\n\n Verificar la multicolinealidad :\n") 
cat ("- VIFs :\n") 
print ( vifs ) 
cat ("- Tolerancias :\n") 
print (1 / vifs ) 
cat ("- VIF medio :", mean ( vifs ) , "\n")

# De acuerdo al VIF medio, este tiene un valor de 1.023585, por lo que el modelo
# no presenta multicoleanidad severa.


#-------------------------------------------------------------------------------
#                                 PREGUNTA 3
#-------------------------------------------------------------------------------
# Se necesita conocer si existe alguna diferencia entre el nivel de delincuencia
# de una comuna de barrio alto, clase media y bajo.
# 
# Comuna de barrio alto mide la delincuencia en el numero de "portonazos" al mes
# (el delito mas común) al mes: 0 10 9 25 12 13 14 15
# 
# Comuna de barrio clase media mide la delincuencia en "sensación de inseguridad"
# de 1 a 10, donde 10 es mayor inseguridad y 1 es menor inseguridad:
# 1 3 7 2 5 1 9 10 10 5
# 
# Comuna de barrio clase baja mide la delincuencia en número de asaltos al mes:
# 13 25 32 21 18 0 12 18 8 6 3
# 
# 
# H0: No hay diferencia significativa en el nivel de delincuencia entre los 3 
#   tipos de comunas.
# HA: Hay diferencia significativa en el nivel de delincuencia entre los 3 
#   tipos de comunas.
