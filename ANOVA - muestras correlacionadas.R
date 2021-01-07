
library(ggpubr)
library(tidyr)

# Usaremos las funciones para hacer esta función
require(ez)

# Los datos como texto
texto <- ("
Instancia	Optimo	R	R2	R3	G
'brock400_2'	29	16.6	19.3	20.2	22
'brock400_4'	33	16.8	19.3	20.4	22
'C2000.9'	80	51.2	59.6	62.4	66
'c-fat500-10'	126	125	125	125	126
'hamming10-2'	512	243.2	419.1	422.4	512
'johnson32-2-4'	16	16	16	16	16
'keller6'	59	34.5	43	45.5	48.2
'MANN_a81'	1100	1082.2	1082.2	1082.2	1095.9
'p-hat1500-1'	12	6.9	8.1	8.9	10
'p-hat1500-3'	94	42.8	77.7	84.6	86
'san1000'	15	7.6	7.6	7.7	10
'san400_0.7_1'	40	19.6	20.5	20.5	21
'san400_0.9_1'	100	44.1	54.4	56.4	92
'frb100-40'	100	66.4	76.7	80.5	82
'frb59-26-1'	59	39.2	45.9	48.3	48
'1et.2048'	316	232.4	268.4	280.9	292.4
'1zc.4096'	379	253.8	293.2	307.4	328.5
'2dc.2048'	24	15.6	18.7	19.9	21
")

# Obtenemos los datos en formato ancho
dw <- read.table(textConnection(texto), header = TRUE)

# Una primera aproximación es comparar los tratamientos con una gráfico
# de cajas. Para eso, primero, llevamos los datos a formato largo.
dl <- gather(
  data = dw,
  key = "Algoritmo",
  value = "Mejor.sol",
  "R", "R2", "R3", "G"
)
dl[["Instancia"]] <- factor(dl[["Instancia"]])
dl[["Algoritmo"]] <- factor(dl[["Algoritmo"]])

p1 <- ggboxplot(
  dl,
  x = "Algoritmo", y = "Mejor.sol",
  xlab = "Algoritmo", ylab = "Mejor solución",
  color = "Algoritmo",
  add = "jitter",
  add.params = list(color = "Algoritmo", fill = "Algoritmo")
)
print(p1)

# Parece haber problemas de escala que impiden ver bien los datos.
# Obvio!
# No nos sirven los valor reportados para el análisis, puesto que
# queremos comparar el rendimiento de los algoritmos, que es relativo
# al valor óptimo de cada instancia.
# Preparemos los datos:
datos.algos <- dw[, -(1:2)]
err.algos <- (dw[[2]] - datos.algos) / dw[[2]]

cat("\n\n")
cat("Porcentaje de error cometido por los algorimos:\n")
cat("-----------------------------------------------\n")
print(round(err.algos * 100, 2))

# Sin embargo, ahora nuestra variable, si bien es numérica, está
# confinada al intervalo [0, 1].
# Esto no suele ser un problema, pero existe la recomendación de
# cambiar la escala, usando el logit o el arcoseno de la variable,
# que la extiende más allá de este intervalo.
# Usemos la propuesta con el arcoseno, más específicamente la
# "arcsine square root transformation", también llamada
# transformación angular.
err.asin <- apply(err.algos, c(1, 2), function(x) asin(sqrt(x)))

cat("\n\n")
cat("Error cometido por los algorimos como variable continua:\n")
cat("--------------------------------------------------------\n")
print(round(err.asin, 3))

# Juntamos los datos nuevamente
datos.wide <- data.frame(Instancia = dw[[1]], err.asin)
datos.long <- gather(
  data = datos.wide,
  key = "Algoritmo",
  value = "Mejor.sol",
  "R", "R2", "R3", "G"
)
datos.long[["Instancia"]] <- factor(datos.long[["Instancia"]])
datos.long[["Algoritmo"]] <- factor(datos.long[["Algoritmo"]])

# Y graficamos nuevamente
p2 <- ggboxplot(
  datos.long,
  x = "Algoritmo", y = "Mejor.sol",
  xlab = "Algoritmo", ylab = "Mejor solución",
  color = "Algoritmo",
  add = "jitter",
  add.params = list(color = "Algoritmo", fill = "Algoritmo")
)
print(p2)
p3 <- ggqqplot(
  datos.long,
  x = "Mejor.sol",
  color = "Algoritmo"
)
p3 <- p3 + facet_grid(~ Algoritmo)
print(p3)


# Podemos ver que hay presencia de valores atípicos (cuando se
# alcanza el óptimo!) pero, en general, no hay evidentes patrones
# que nos preocupen tanto.
# Así, podemos seguir adelante con el análisis, usando un nivel
# de significación exigente.
alpha <- .01

# ¿Qué hay de las otras condiciones? Recordemos:
# 1. La variable dependiente tiene escala de intervalo
# 2. Las muestras son independientes al *interior* de los grupos
# 3. La matriz de varianzas-covarianzas es esférica

# La condición 1 se verifica con el preprocesamiento que hemos
# dado a los datos.

# La condición 2 solo podemos suponerla...

# Las condición 3 la revisa la rutina ezANOVA...
# Dejémosla pendiente.


# ----------------------------------------------------
# Hipótesis
# ----------------------------------------------------

# ¿Cuáles serían las hipótesis en este caso?
#
# Luego, en este caso:
# H0: Los algoritmos alcanzan la misma media de la transformación
#     angular del error relativo de cometen (mismo rendimiento promedio)
# HA: La media de la transformación angular del error relativo de al
#     menos uno de los algoritmos es diferente a las medias del resto
#

# Usamos la función ezANOVA() para el análisis.
# En esta ocasión, no pedimos que nos retorne un objeto 'aov' porque
# la función 'TukeyHSD()' no está implementada para medidas repetidas.
ez.aov <- ezANOVA(
  data = datos.long,
  dv = Mejor.sol,
  wid = Instancia,
  within = Algoritmo,
  type = 3,
  return_aov = FALSE
)
cat("\n\n")
cat("Tabla ANOVA obtenida con ezANOVA\n")
cat("--------------------------------\n")
print(ez.aov)

# Podemos ver que hay problemas con la esfericidad, por lo que no
# podemos usar el p-valor que asume esta condición (p = 2.29e-09).
# Sin embargo, tanto con la corrección Greenhouse-Geisser como con la
# de Huynh-Feldt, se tiene que p < .001, menor al nivel de
# significación establecido.
# Luego, hay diferencias significativas entre el rendimiento
# promedio de los algoritmos.

# Entonces, corresponde hacer hacer un análisis post hoc para
# establecer qué algoritmos contribuyen a esta diferencia.

# Inicialmente podemos hacer pruebas T de Student entre pares de
# tratamientos, pero teniendo cuidado de usar pruebas para datos
# apareados.
mc <- pairwise.t.test(datos.long[["Mejor.sol"]], datos.long[["Algoritmo"]],
                      paired = TRUE, p.adjust.method = "holm")
cat("\n\n")
cat("Comparaciones múltiples entre los tratamientos:\n")
cat("-----------------------------------------------\n")
print(mc)

# Este método simple encuentra diferencias significativas ¡entre todos
# los pares de algoritmos!

# Podemos intentar confirmar estos resultados usando la implementación del
# método de Tukey descrito en VarssarStats, o usando modelos mixtos

library(emmeans)
library(nlme)

mix <- lme(Mejor.sol ~ Algoritmo, random = ~1|Instancia,
           data = datos.long)

cat("\n\n")
cat("Tabla ANOVA construida con un modelo mixto\n")
cat("-----------------------------------------\n")
print(anova(mix))
cat("\n\n")

em <- emmeans(mix, "Algoritmo")
tem <- pairs(em)
cat("\n\n")
cat("Comparaciones de las diferencias de los tratamientos con EMM\n")
cat("------------------------------------------------------------\n")
print(tem)
cat("\n\n")
cat("Intervalos de confianza con EMM\n")
cat("-------------------------------\n")
print(confint(tem, level = 1 - alpha))
cat("\n\n")

# Este método, mucho más sofisticado, encuentra diferencias
# significativas con el nivel de significación definido para todos
# los pares de algoritmos con la excepción de los pares G-R3 y R2-R3.

# Importante: las medias reportadas aquí, al igual que los
# intervalos de confianza, corresponden a los datos transformados,
# es decir, son diferencias en ángulos de los errores relativos
# cometidos por los algoritmos.
# Se debe aplicar la transformación inversa (no trivial) para obtener
# estimaciones e intervalos de confianza para las variables originales.

