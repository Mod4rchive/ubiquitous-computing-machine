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
data <- read.csv(file.choose(), head = TRUE, sep=";");

# Se pide ver las diferencias estadísticas de exigencia entre distintos grupos
# por lo tanto se debe realizar una prueba ANOVA.

# Formulación de hipótesis
# H0: No hay diferencias significativas en los niveles de exigencia de los
#     distintos oficiales evaluadores.
# 
# HA: Si hay diferencias significativas en los niveles de exigencia de los
#     distintos oficiales evaluadores.
