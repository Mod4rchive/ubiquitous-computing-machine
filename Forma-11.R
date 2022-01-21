#-------------------------------------------------------------------------------
#         DESARROLLO DE PEP 2 DE INFERENCIA Y MODELOS ESTADÍSTICOS
#-------------------------------------------------------------------------------
# Integrantes: Carlos Castillo        20.201.274-4
#              Joaquín Torres         19.091.702-9
#
# Sección: B-2
# Profesora: Jacqueline Kóhler
#-------------------------------------------------------------------------------


############# PREGUNTA 1

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
# La prueba con alfa=0.1 tiene p=0 por lo tanto se cumple que la matriz de 
# varianzas-covarianzas no es esférica.

# Se puede realizar ANOVA
# 
# 
# Se realiza la prueba ANOVA (prueba)

print(summary(prueba$aov))

# El valor p obtenido en la prueba anova del estadístico F es 3 grados de 
# significancia menor que el alfa elegido (0.1), por lo tanto se rechaza la
# hipótesis nula a favor de la alternativa: Si hay diferencias significativas 
# en los niveles de exigencia de los distintos oficiales evaluadores.


# Procedimiento post-hoc HSD de Tukey .
mixto <- lme ( evaluacion ~ oficiales , data = datos , random = ~1| instancia )
medias <- emmeans ( mixto , "oficiales")
tukey <- pairs ( medias , adjust = "tukey")

cat ("\n\ nPrueba HSD de Tukey \n\n")
print ( tukey )

# Se encuentra que las parejas de aquellos soldados evaluados por capitán-comandante;
# capitán-general; capitán-instructor son las mas diferentes en método de evaluación.
# 
# Con esto podemos concluir que el capitán es el único que se diferencia en 
# método de evaluación con los demás.
# 
# 
# 
# ################# PREGUNTA 2
# Se pide realizar una regresión logística para saber si los clones y los reclutas
# son distinguibles entre ellos; se pide evaluar un modelo clasificador que contemple 3
# variables predicadoras: fuerza, resistencia y agilidad; se eligieron estas pues
# son representativas de las habilidades de cada uno.
# 
# Se define la semilla para el generador pseudo-aleatorio
set.seed(4432)

# Se utiliza la variable es_clon como variable binaria
Slogico <- ifelse ( data$es_clon == "S" , 1, 0)
data2 <- cbind( Slogico , data )
data2$es_clon <- data2$Slogico
data2$Slogico <- NULL

predictor <- data.frame(c("fuerza", "resistencia", "agilidad"))
colnames(predictor) <- c("predictor")

# Se crea la muestra de sample size = 400
muestra_total <- sample_n(data2, size = 400)
muestra_total[["id"]] <- seq(1:400)

# Se seleccionan los datos para generar el modelo (entrenamiento) y los para
# probar el modelo (test)
entrenamiento <- filter(muestra_total, id<321)
test <- filter(muestra_total, 320<id & id<401)

