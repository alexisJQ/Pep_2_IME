
library(car)
library(ggpubr)
library(tidyr)
library(MASS)
require(ez)

cat("\n\n")
cat("Primer enunciado: tiempo de ejecución de dos algoritmos\n")
cat("=======================================================\n")
cat("\n\n")

texto1 <-("
Instancia 'Tiempo A6 (ms)'   Instancia 'Tiempo B12 (ms)'
'fl1400'            337977     'd1291'            335566
'pcb1173'           303634     'd657'              52696
'rat575'             33349   'fl1577'            3192222
'rl1323'            243679  'nrw1379'             393213
'u1060'            3453176   'pr1002'             162808
'u1432'             398653   'pr2392'            8765321
'u1817'             876432   'rat783'              76857
'u2152'            3073534   'rl1304'             231254
'u574'              112326   'rl1889'             854213
'u724'               55026   'vm1084'             543215
")

# Obtenemos los datos en formato ancho
dw1 <- read.table(
  file = textConnection(texto1),
  header = TRUE,
  stringsAsFactors = FALSE
)
names(dw1) <- c("Instancias A6", "Tiempo A6",
                "Instancias B12", "Tiempo B12")

# Llevamos los datos a formato largo
Instancia <- c(dw1[[1]], dw1[[3]])
Tiempo <- c(dw1[[2]], dw1[[4]])
Algoritmo <- c(rep("A6", nrow(dw1)), rep("B12", nrow(dw1)))
dl1 <- data.frame(Instancia, Algoritmo, Tiempo)
dl1[["Instancia"]] <- factor(dl1[["Instancia"]])
dl1[["Algoritmo"]] <- factor(dl1[["Algoritmo"]])

# Revisemos un histograma de los datos
p1 <- gghistogram(
  dl1,
  x = "Tiempo",
  xlab = "Algoritmo",
  color = "Algoritmo", fill = "Algoritmo",
  bins = 5
)
p1 <- p1 + facet_grid(~ Algoritmo)

# Claramente el comportamiento no es el de una distribución
# normal. En general, los tiempos suelen mostrar una
# distribución exponencial.

# En la lectura sobre transformaciones de datos, una opción
# es usar el logaritmo de los valores de tiempo en ms.
Log.tiempo <- log(dl1[["Tiempo"]])
dl1 <- cbind(dl1, Log.tiempo)

# Revisemos el histograma de los datos transformados
p2 <- gghistogram(
  dl1,
  x = "Log.tiempo",
  xlab = "Algoritmo",
  color = "Algoritmo", fill = "Algoritmo",
  bins = 5
)
p2 <- p2 + facet_grid(~ Algoritmo)

# Ahora parecen estar más cercano a un comportamiento normal
# Verifiquemos con una prueba de normalidad

st1 <- by(
  data = dl1[["Log.tiempo"]],
  INDICES = dl1[["Algoritmo"]],
  FUN = shapiro.test
)

cat("\n\n")
cat("Pruebas de normalidad para log(Tiempo) de cada algoritmo\n")
cat("--------------------------------------------------------\n\n")
print(st1)

# Vemos que con esta transformación obtenemos datos con
# distribuciones aproximadamente normales.
# Podemos entonces continuar con una prueba paramétrica

t1 <- t.test(Log.tiempo ~ Algoritmo, data = dl1)
cat("\n\n")
cat("Pruebas T de Student para log(Tiempo) de cada algoritmo\n")
cat("-------------------------------------------------------\n")
print(t1)

# El test nos dice que no hay sufieciente evidencia para
# considerar que hay diferencia entre las medias del log
# de los tiempos de ejecución requeridos por los algoritmos.
# Sí debemos recordar que se necesita aplicar la transformación
# inversa para reportar en la escala original de la variable
# de interes, 'Tiempo' en este caso.
# Pero debe recordarse que la diferencia de logs de tiempos,
# corresponde a la razón de medias geométricas de los tiempos.

cat("\n")
cat("Intervalo con 95% confianza transformado de vuelta\n")
cat("--------------------------------------------------\n")
conf.int <- round(exp(t1[["conf.int"]]), 2)
cat("[", conf.int[1], ", ", conf.int[2], "]", "\n", sep = "")

# Como el 1 está incluido dentro del intervalo de confianza,
# no podemos descartar que las medias geométricas del tiempo
# requerido por cada algoritmo son iguales.



cat("\n\n")
cat("\n\n")
cat("Segundo enunciado: resultados de algoritmos de clasificación\n")
cat("============================================================\n")
cat("\n\n")

texto2 <- ("
Dataset            C1       C3       C5       C6       C7
'credit'        84,93    85,07    84,93    85,22    83,33
'eucalyptus'    64,28    58,71    64,01    59,52    59,40
'glass'         71,58    73,83    71,10    75,69    73,33
'hepatitis'     79,46    83,79    79,46    82,50    81,25
'hungarian-14'  78,64    84,39    78,64    84,38    81,97
'hypothyroid'   99,28    98,54    99,28    98,62    98,97
'iris'          93,33    92,67    93,33    92,00    93,33
'mushroom'     100,00    99,95   100,00    99,84   100,00
'optdigits'     78,97    96,90    81,01    94,20    91,80
'page-blocks'   96,62    96,95    96,66    94,15    96,97
'pendigits'     89,05    97,82    89,87    94,81    95,67
'pima-diabetes' 73,70    75,01    73,56    74,75    72,67
'primary-tumor' 40,11    47,49    40,11    49,55    38,31
'solar-flare-C' 88,86    88,54    88,86    87,92    86,05
'solar-flare-m' 90,10    87,92    90,10    86,99    85,46
'solar-flare-X' 97,84    97,84    97,84    94,41    95,99
'sonar'         74,48    81,26    74,45    80,79    78,36
'waveform'      74,38    84,92    74,90    83,62    79,68
'yeast'         57,01    56,74    57,01    57,48    56,26
")

# Obtenemos los datos en formato ancho
dw2 <- read.table(
  file = textConnection(texto2),
  header = TRUE,
  dec = ","    # Esto porque se usa coma decimal en los datos
)

# Llevamos los datos a formato largo
dl2 <- gather(
  data = dw2,
  key = "Algoritmo",
  value = "Resultado",
  -"Dataset"
)
dl2[["Dataset"]] <- factor(dl2[["Dataset"]])
dl2[["Algoritmo"]] <- factor(dl2[["Algoritmo"]])

# Revisemos los datos con un diagrama de cajas
p3 <- ggboxplot(
  dl2,
  x = "Algoritmo", y = "Resultado",
  xlab = "Algoritmo",
  color = "Algoritmo"
)

# Vemos que existen valores atípicos para todos los
# clasificadores evaluados y una evidente asimetría con
# C3, C6 y C7.
# Más aún, debemos recordar que el rendimiento de algoritmos
# clasificadores suele medirse como el porcentaje de casos
# clasificados correctamente. Luego, a pesar de ser una
# variable numérica, está limitada a un intervalo fijo.
# En estos casos, se recomienda transformarlos a una escala
# abierta. Es común usar el arcoseno, aunque últimamente
# se está prefiriendo la función logit.
# [Ver por ejemplo http://strata.uga.edu/8370/rtips/proportions.html
# Usaremos la función logit() implementada en el paquete 'car'

Logit.resultado <- logit(dl2[["Resultado"]], adjust = 0.01)
dl2 <- cbind(dl2, Logit.resultado)

# Revisemos los datos transformados
p4 <- ggboxplot(
  dl2,
  x = "Algoritmo", y = "Logit.resultado",
  xlab = "Algoritmo",
  color = "Algoritmo"
)

# Ahora los datos parecen más simétricos, aunque C1 y C5 muestran
# valores atípicos, pero que están más cerca al resto que
# en los datos originales.
# Verifiquemos con una prueba de normalidad

st2 <- by(
  data = dl2[["Logit.resultado"]],
  INDICES = dl2[["Algoritmo"]],
  FUN = shapiro.test
)

cat("\n\n")
cat("Pruebas de normalidad para logit(Resultado) de cada algoritmo\n")
cat("-------------------------------------------------------------\n")
print(st2)

# Vemos que con esta transformación obtenemos datos que
# podemos trabajar con una prueba paramétrica.
# En este caso, un análisis de varianza con medidas repetidas

ez.aov <- ezANOVA(
  data = dl2,
  dv = Logit.resultado,
  wid = Dataset,
  within = Algoritmo,
  type = 3,
  return_aov = TRUE
)

cat("\n\n")
cat("Tabla ANOVA para el logit(Resultado) de cada algoritmo\n")
cat("------------------------------------------------------\n")
print(summary(ez.aov[["aov"]]))
cat("\n")
cat("Prueba de esfericidad para el logit(Resultado)\n")
cat("----------------------------------------------\n")
print(ez.aov[["Mauchly's Test for Sphericity"]])
cat("\n")
cat("Correcciones de esfericidad para el logit(Resultado)\n")
cat("----------------------------------------------------\n")
print(ez.aov[["Sphericity Corrections"]])


# Podemos ver que los datos tienen problemas de esfericidad,
# por lo que debemos considerar las correcciones de
# Greenhouse-Geisser o las de Huynh-Feldt.
# En este caso, ambas indican que no hay diferencias
# significativas (p = .124 y p = .122 respectivamente) entre
# las medias del logit de los porcentajes de acierto de los
# los algoritmos de clasificación estudiados.
