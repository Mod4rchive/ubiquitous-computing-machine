#-------------------------------------------------------------------------------
#         DESARROLLO DE PEP 2 DE INFERENCIA Y MODELOS ESTADÍSTICOS
#-------------------------------------------------------------------------------
# Integrantes: Carlos Castillo        20.201.274-4
#              Joaquín Torres         19.091.702-9
#
# Sección: B-2
# Profesora: Jacqueline Kóhler
#-------------------------------------------------------------------------------

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

print(shapiro.test(eval_instructor))
print(shapiro.test(eval_capitan))
print(shapiro.test(eval_comandante))
print(shapiro.test(eval_general))

