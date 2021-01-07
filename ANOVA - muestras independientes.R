library(ez)
library(ggpubr)
library(knitr)
library(tidyr)


# Definimos los datos
duraciones <- c("0 día", "2 días", "4 días", "6 días")
t1 <- c(26, 27, 28, 28, 33)
t2 <- c(22, 23, 24, 27, 27)
t3 <- c(19, 20, 21, 23, 27)
t4 <- c(19, 20, 23, 24, 24)
dx4 <- list(t1, t2, t3, t4)
datos.wide <- data.frame(dx4)
colnames(datos.wide) <- duraciones


# Pero los procedimientos para hacer ANOVA, y muchas rutinas para
# graficar, requieren los datos en formalo largo (long).
dl <- gather(
  data = datos.wide,
  key = "Duración",
  value = "Errores",
  duraciones
)
Desarrollador <- factor(1:nrow(dl))
datos.long <- cbind(Desarrollador, dl)
datos.long[["Duración"]] <- factor(datos.long[["Duración"]])

# Una primera aproximación es comparar los grupos con una gráfico de
# cajas.
p1 <- ggboxplot(
  datos.long,
  x = "Duración", y = "Errores",
  xlab = "Duración capacitación", ylab = "Errores",
  color = "Duración",
  add = "jitter",
  add.params = list(color = "Duración", fill = "Duración")
)
print(p1)


# En general, parece haber una disminución en el número de errores.


# ----------------------------------------------------
# Hipótesis
# ----------------------------------------------------

# ¿Cuáles serían las hipótesis en este caso? 
# Usemos las definiciones en la sección 5.5 de OpenIntro Statistics:
# H0: The mean outcome is the same across all groups.
# HA: At least one mean is different.
# 
# Luego, en este caso:
# H0: La media del nº de pruebas unitarias falladas por sprint es la
#     misma en todos los grupos de desarrolladores 
# HA: La media de pruebas falladas de al menos un grupo de
#     desarrolladores es distinta 
# 


# ----------------------------------------------------
# Verificaciones
# ----------------------------------------------------

# Estas se explican en el capítulo 14 de VarssarStats:
# 1. La variable dependiente tiene escala de intervalo
# 2. Las muestras son independientes y obtenidas aleatoriamente
# 3. Se puede asumir que las poblaciones son aproximadamente normales
# 4. Las muestras tienen varianzas similares

# La condición 1 se verifica, puesto que 10 errores, por ejemplo, es el
# doble de 5 errores y la mitad de 20 errores.

# La condición 2 se verifica en el enunciado.

# Para la condición 3, podemos usar un gráfico QQ
p2 <- ggqqplot(
  datos.long,
  x = "Errores",
  color = "Duración"
)
p2 <- p2 + facet_wrap(~ Duración)

# Podemos ver que, si vien hay un par de puntos más o menos 
# problemáticos por aquí por allá, no hay desviaciones importantes en
# los datos. 

# Para confirmar, podríamos aplicar alguna prueba de normalidad.
# Con muestras tan pequeñas, podría convenir usar la prueba de 
# Shapiro-Wilk.
# [A Ghasemi, S Zahediasl (2012). Normality tests for statistical
# analysis: a guide for non-statisticians. International journal of
# endocrinology and metabolism, 10(2), 486-9].

spl <- apply(datos.wide, 2, shapiro.test)
cat("\n\n")
cat("Pruebas de normalidad en cada grupo\n")
cat("-----------------------------------\n")
cat("\n")
print(spl)

# En principio, no habría razones para suponer que los datos no vienen
# de poblaciones normales.

# Falta entonces la condición 4. El diagrama de cajas sugiere que hay
# algún cambio en la variabilidad de los equipos al introducir
# capacitación.
# Podemos confirmar aplicando una prueba de homocedasticidad
# (o homogeneidad de varianzas). En general, se recomienda la prueba de
# Levene.
# [Ver por ejemplo, www.johndcook.com/blog/2018/05/16/f-bartlett-levene/]

library(car)

lts <- leveneTest(Errores ~ Duración, datos.long)
cat("\n\n")
cat("Pruebas de homocedasticidad\n")
cat("---------------------------\n")
cat("\n")
print(lts)

# Vemos que no hay razones para creer que hay problemas con la varianza.
# Luego, podemos continuar con al análisis.


# ----------------------------------------------------
# Procedimiento manual del capítulo 14 de VarssarStats
# ----------------------------------------------------

# Contamos observaciones por grupo y en total
N.por.grupo <- apply(datos.wide, 2, length)
N.total <- sum(N.por.grupo)
k <- ncol(datos.wide)

# Obtenemos la media de cada grupo y media global
media.por.grupo <- apply(datos.wide, 2, mean)
media.total <- mean(unlist(datos.wide))

# Obtenemos la suma de las desviaciones cuadradas observadas en cada
# grupo y globalmente
SS.en.cada.grupo <- sapply(
  1:k,
  function(i) sum((datos.wide[, i] - media.por.grupo[i])^2)
)
SS.total <- sum((unlist(datos.wide) - media.total)^2)

# Obtenemos la suma de las desviaciones cuadradas al interior de los
# grupos.
SS.wg <- sum(SS.en.cada.grupo)

# Y podríamos obtener la suma de las desviaciones cuadradas entre los
# grupos: SS.bg <- SS.total - SS.wg

# Pero queda conceptualmente mas claro si repetimos el procedimiento
# trabajando con las desviaciones de las medias de cada grupo.
S.de.cada.grupo <- (media.por.grupo - media.total)^2
S.ponderada.de.cada.grupo <- N.por.grupo * S.de.cada.grupo
SS.bg <- sum(S.ponderada.de.cada.grupo)

# Ahora, los grados de libertad
df.bg <- ncol(datos.wide) - 1
df.wg <- sum(N.por.grupo - 1)
# o equivalentemente: df.wg <- N.total - ncol(datos.wide)

# Podemos obtener las medias cuadradas
MS.bg <- SS.bg / df.bg
MS.wg <- SS.wg / df.wg

# Y ahora el estadístico
F <- MS.bg / MS.wg

# Ahora obtenemos un p-valor
Pr <- 1 -pf(F, df.bg, df.wg)
pv <- round(Pr, 3)
if(pv < 0.001) {
  pvs <- "<.001"
} else {
  pvs <- sub(pattern = "0.", replacement=".", x = sprintf("%1.3f", pv))
}

# Creamos una tabla con esta información
Source <- c("Between groups (effect)", "Within groups (error)", "TOTAL")
Df <- c(df.bg, df.wg, N.total - 1)
P <- c(pvs, "   ", "   ")

r1 <- round(c(SS.bg, MS.bg, F), 2)
r2 <- round(c(SS.wg, MS.wg, 0), 2)
r3 <- round(c(SS.total, 0, 0), 2)
rb <- rbind(r1, r2, r3)
colnames(rb) <- c("SS", "MS", "F")

tabla.aov <- data.frame(Source, Df, rb, P)
rownames(tabla.aov) <- NULL
kt <- kable(
  tabla.aov,
  format = "pandoc",
  format.args = list(zero.print = FALSE)
)

cat("\n\n")
cat("Tabla ANOVA construida según VarssarStats\n")
cat("-----------------------------------------")
print(kt)
cat("\n\n")


# ----------------------------------------------------
# Usando las funciones del paquete ez
# ----------------------------------------------------

# La función ezANOVA() no acepta (directamente) nombres de las columnas
# en variables de texto. Por eso, usamos los nombres que hemos fijado de
# "forma dura". 
ez.aov <- ezANOVA(
  data = datos.long, 
  dv = Errores,
  wid = Desarrollador,
  between = Duración,
  type = 3,
  return_aov = TRUE
)
print(ez.aov)

# Podemos ver que el objeto que devuelve ezANOVA() contiene una prueba
# de homocedasticidad de Levene, por lo que no sería necesario hacerla
# por separado si vamos a usar esta función.

# Otra cosa a notar, es que esta función nos entrega automáticamente un
# tamaño del efecto, medido con un estadístico llamado 'generalized eta
# squared', que parece ser la más recomendada para medir cuánta de la
# varianza medida se puede atribuir al factor en estudio. Recordar que
# esta medida trata de responder a la pregunta ¿qué tan importante fue
# el efecto?, lo que no tiene relación con su significación estadística.

# Podemos el el resultado gráficamente.
ezp <- ezPlot(
  data = datos.long,
  dv = Errores,
  wid = Desarrollador,
  between = Duración,
  type = 3,
  x = Duración
)
print(ezp)


# ----------------------------------------------------
# Análisis post-hoc
# ----------------------------------------------------

# En el capítulo 14 de VarssarStats se presenta el procedimiento para
# obtener contrastes y ajustes con el método de la diferencia
# significativa honesta propuesta por Tukey, el que se puede traducir
# en el siguiente código:

# Obtenemos las diferencias entre todos los pares de grupos
diferencias <- outer(media.por.grupo, media.por.grupo, "-")
triang <- lower.tri(diferencias)
difs <- diferencias[triang]

# Ahora obtenemos los estadísticos Q (Tukey)
N.ps <- length(N.por.grupo) / sum(1 / N.por.grupo)
den <- sqrt(MS.wg / N.ps)
Qs <- difs / den

# Para obtener los intervalos con un nivel de confianza dado
alpha <- 0.05
qalpha <- qtukey(1 - alpha, k, df.wg)
mealpha <- qalpha * den
ics.l <- difs - mealpha
ics.u <- difs + mealpha

# Finalmente necesitamos los p-valores ajustados
pvals <- ptukey(abs(Qs), length(N.por.grupo), df.wg, lower.tail = FALSE)

# Para contruir la tabla, necesitamos los pares de diferencias
nombres <- names(media.por.grupo)
pares <- outer(nombres, nombres, "paste", sep = "-")
contrastes <- pares[triang]

# Creamos la tabla
m <- length(contrastes)
dnames <- list(contrastes, c("diff", "lwr", "upr","p adj"))
tabla.tukey <- array(c(difs, ics.l, ics.u, pvals), c(m, 4), dnames)

# Y la mostramos en pantalla como la función TukeyHSD()
cat("\n\n")
cat("Comparaciones múltiples entre los grupos según VarssarStats\n")
cat("-----------------------------------------------------------\n")
cat("  Tukey multiple comparisons of means\n")
cat("    ", (1-alpha)*100, "% family-wise confidence level", "\n", sep = "")
cat("\n")
cat("Fit: \n")
print(ez.aov[["aov"]][["call"]])
cat("\n")
cat("$Duración\n")
print(tabla.tukey, row.names = FALSE, justify = "left")
cat("\n\n")


# Por otro lado, hay muchos métodos disponibles en R.
# 
# Primero, usemos la implementación disponible para el método de Tukey
# visto más arriba, que implementa la función TukeyHSD() de R.
# Sin embargo, esta función requiere de un objeto 'aov', que pudimos
# obtener a pesar de usar la implementación de ANOVA del paquete 'ez'
# porque dimos el argumento 'return_aov = TRUE' al llamar la función.
# Hay un segundo parámetro importante, el nivel de confianza para los
# intervalos de confianza que nos entrega la función. Si no damos un
# valor, se asume 95%.
mt <- TukeyHSD(ez.aov[["aov"]])
cat("\n\n")
cat("Comparaciones múltiples entre los grupos:\n")
cat("-----------------------------------------\n")
print(mt)

# También podemos graficar esta comparación
# (se especifica 'las = 1' para que las etiquetas de los ejes se muestren
# de forma horizontal).

plot(mt, las = 1)

# Otra alternativa es aplicar pruebas T de Student para comparar todos 
# los pares de grupos. Esto se hace con la función pairwise.t.test(),
# que recibe los mismos argumentos que la función t.test() más uno extra
# que indica el método para ajustar el nivel de significación para las
# pruebas múltiples. Sin embargo, para facilidad del usuario, en vez de
# reportar los niveles de significación de cada prueba, ajusta los
# p-valores en concordancia, haciéndolos así comparables con el nivel
# nominal. Por esto este parámetro extra se llama 'p.adjust.method'.
# Los métodos disponibles puede verse al imprimir la variable global
# que las contiene:
cat("\n\n")
cat("Métodos de ajuste para comparaciones múltiples disponibles:\n")
cat("-----------------------------------------------------------\n")
print(p.adjust.methods)
cat("\n")

# Usemos uno clásico: el método de Sture Holm
mc <- pairwise.t.test(datos.long[["Errores"]], datos.long[["Duración"]],
                      paired = FALSE, p.adjust.method = "holm")
cat("\n\n")
cat("Comparaciones múltiples entre los grupos:\n")
cat("-----------------------------------------\n")
print(mc)


# ----------------------------------------------------
# ¿Qué interpretamos?
# ----------------------------------------------------

# Coinciden en que hay una diferencia significativa entre no
# hacer capacitación y tener 4 o 6 días de capacitación, y que en
# realidad, da lo mismo tener 4 o 6 días de capacitación.
# Conviene contratar 4 días de capacitación para los desarrolladores.




