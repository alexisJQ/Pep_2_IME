
library(ggpubr)
library(knitr)
library(tidyr)

# Usaremos las funciones para hacer esta función
require(ez)

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
Desarrollador <- factor(rep(1:nrow(datos.wide), ncol(datos.wide)))
datos.long <- cbind(Desarrollador, dl)
datos.long[["Duración"]] <- factor(datos.long[["Duración"]])

# Una primera aproximación es comparar los tratamientos con una gráfico
# de cajas.
p1 <- ggboxplot(
  datos.long,
  x = "Duración", y = "Errores",
  xlab = "Duración capacitación", ylab = "Errores",
  color = "Duración",
  add = "jitter",
  add.params = list(color = "Duración", fill = "Duración")
)
print(p1)


# Como son los mismos datos que analizamos como grupos independientes,
# se observa una disminución en el número de errores a medida que se
# aumentan los días de capacitación.


# ----------------------------------------------------
# Hipótesis
# ----------------------------------------------------

# ¿Cuáles serían las hipótesis en este caso?
#
# Luego, en este caso:
# H0: La media del nº de pruebas unitarias falladas por sprint es la
#     misma en todas las mediciones aplicadas a los desarrolladores
# HA: La media de pruebas falladas en al menos una medición aplicada a
#     los desarrolladores es distinta
#


# ----------------------------------------------------
# Verificaciones
# ----------------------------------------------------

# Estas se explican en el capítulo 15 de VarssarStats:
# 1. La variable dependiente tiene escala de intervalo
# 2. Las muestras son independientes al *interior* de los grupos
# 3. Se puede asumir que las poblaciones son aproximadamente normales
# 4. Las muestras tienen varianzas similares
# 5. La matriz de varianzas-covarianzas es esférica

# La condición 1 se verifica, ya que en 5 y 15 errores hay 10 errores
# de diferencia, lo mismo que entre 10 y 20.

# La condición 2 también se verifica en el enunciado.

# Para la condición 3, obtenemos el mismo gráfico QQ que vimos para el
# caso de muestras independientes (porque usamos los mimos valores).
p2 <- ggqqplot(
  datos.long,
  x = "Errores",
  color = "Duración"
)
p2 <- p2 + facet_wrap(~Duración)

# Luego, aunque hay un par de puntos más o menos problemáticos, no hay
# desviaciones importantes en los datos, lo que fue confirmado con
# pruebas de normalidad de Shapiro-Wilk.

# También revisamos que se cumple la homocedasticidad con estos datos,
# condición 4, usando la prueba de Levene.

# Falta entonces la condición de esfericidad. La verdad es que esto no
# es fácil y es un conjunto de supociciones que se deben manejar.
# Una alternativa simple es decir que todos los pares de diferencias
# entre mediciones (en este caso 0/2, 0/4, 0/6, 2/4, 2/6 y 4/6 días)
# tienen igual varianza.
# Por fortuna, existe una prueba de esferidad propuesta por John W.
# Mauchly, aunque no tiene mucho poder estadístico con muestras
# reducidas.
# [JW Mauchly (1940). "Significance test for sphericity of a normal
# n-variate distribution." The Annals of Mathematical Statistics, 11,
# 204-209].

# Esta prueba está implementada en R, pero requiere de un modelo ya
# construido para hacer las estimaciones. Posterguemos esta prueba
# hasta que usemos la función ezANOVA().

# En todo caso, siempre se hace los cálculos de ANOVA-RM suponiendo que
# se cumple la condición de esfericidad. Si se llega a determinar que
# esta en realidad no se cumple, se puede aplicar una corrección de los
# grados de libertad y, por lo tanto, de los p-valores que se estimen.
# Los paquetes estadísticos, en general, suelen reportar estas
# correcciones para ser consideradas. Esta propuesta es la que también
# adopta la función ezANOVA().


# ----------------------------------------------------
# Procedimiento manual del capítulo 15 de VarssarStats
# ----------------------------------------------------

# Contamos observaciones por tratamiento y en total
N.por.tratamiento <- apply(datos.wide, 2, length)
N.total <- sum(N.por.tratamiento)
N.sujetos <- nrow(datos.wide)
k <- ncol(datos.wide)

# Obtenemos la media de cada tratamiento y media global
media.por.tratamiento <- apply(datos.wide, 2, mean)
media.total <- mean(unlist(datos.wide))

# Obtenemos la suma de las desviaciones cuadradas observadas en cada
# tratamiento y globalmente
SS.en.cada.tratamiento <- sapply(
  1:k,
  function(i) sum((datos.wide[, i] - media.por.tratamiento[i])^2)
)
SS.total <- sum((unlist(datos.wide) - media.total)^2)

# Obtenemos la suma de las desviaciones cuadradas al interior de los
# tratamientos.
SS.wg <- sum(SS.en.cada.tratamiento)

# Pero ahora, también tenemos medias por sujeto (desarrollador, en este
# caso).
media.por.sujeto <- apply(datos.wide, 1, mean)

# Podemos obtener la suma de las desviaciones cuadradas observadas para
# cada sujeto
S.por.sujeto <- (media.por.sujeto - media.total)^2
mediciones.válidas <- apply(datos.wide, c(1, 2), is.finite)
mediciones.por.sujeto <- apply(mediciones.válidas, 1, sum)
S.ponderada.por.sujeto <- mediciones.por.sujeto * S.por.sujeto
SS.subj <- sum(S.ponderada.por.sujeto)

# Y ahora obtener la suma de las desviaciones cuadradas que no puede
# explicarse y que se debe al azar
SS.error <- SS.wg - SS.subj

# Por otro lado necesitamos las desviaciones cuadradas de las medias
# entre los tratamientos.
S.de.cada.tratamiento <- (media.por.tratamiento - media.total)^2
S.ponderada.de.cada.tratamiento <- N.por.tratamiento * S.de.cada.tratamiento
SS.bg <- sum(S.ponderada.de.cada.tratamiento)

# Ahora, los grados de libertad
df.bg <- k - 1
df.wg <- sum(N.por.tratamiento - 1)
df.subj <- N.sujetos - 1
df.error <- df.wg - df.subj

# Podemos obtener las medias de las desviaciones cuadradas relevantes
MS.bg <- SS.bg / df.bg
MS.error <- SS.error / df.error

# Y ahora el estadístico
F <- MS.bg / MS.error

# Ahora obtenemos un p-valor
Pr <- 1 - pf(F, df.bg, df.error)
pv <- round(Pr, 3)
if(pv < 0.001) {
  pvs <- "<.001"
} else {
  pvs <- sub(pattern = "0.", replacement=".", x = sprintf("%1.3f", pv))
}

# Creamos una tabla con esta información
Source <- c("Between groups (effect)", "Within groups", "- Error",
            "- Subjects", "TOTAL")
Df <- c(df.bg, df.wg, df.error, df.subj, N.total - 1)
P <- c(pvs, "   ", "    ", "   ", "   ")

r1 <- round(c(SS.bg, MS.bg, F), 2)
r2 <- round(c(SS.wg, 0, 0), 2)
r3 <- round(c(SS.error, MS.error, 0), 2)
r4 <- round(c(SS.subj, 0, 0), 2)
r5 <- round(c(SS.total, 0, 0), 2)
rb <- rbind(r1, r2, r3, r4, r5)
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
  within = Duración,
  type = 3,
  return_aov = TRUE
)
print(ez.aov)


# Podemos el el resultado gráficamente.
ezp <- ezPlot(
  data = datos.long,
  dv = Errores,
  wid = Desarrollador,
  within = Duración,
  type = 3,
  x = Duración
)
print(ezp)


# Podemos ver que, asumiendo esfericidad, se tiene un p-valor < .001.

# La función también reporta un p-valor = 0.478 para la prueba de
# esfericidad, por lo que estos datos sí estarían cumpliendo esa
# condición.

# Si esto no fuera así, y la prueba de esfericidad resultara
# estadísticamente significativa, habría que considerar las correcciones
# de Greenhouse-Geisser o las de Huynh-Feldt, ambas reportadas por la
# función, y considerar en consecuencia p-valores mayores a los
# estimados asumiendo esfericidad (.0004764 y .0000295 respectivamente,
# en vez del .0000021 original).

# Entonces, corresponde hacer hacer un análisis post hoc.


# ----------------------------------------------------
# Análisis post-hoc
# ----------------------------------------------------

# Aquí, todavía podemos hacer pruebas T de Student entre pares de
# tratamientos, pero teniendo cuidado de usar pruebas para datos
# apareados.
mc <- pairwise.t.test(datos.long[["Errores"]], datos.long[["Duración"]],
                      paired = TRUE, p.adjust.method = "holm")
cat("\n\n")
cat("Comparaciones múltiples entre los tratamientos:\n")
cat("-----------------------------------------------\n")
print(mc)


# Pero ahora no tenemos disponibles el método de Tukey
# No funciona: mt <- TukeyHSD(ez.aov[["aov"]])
# Una opción es implementar lo que se describe en VarssarStats:

# ---------------------------------------------------------------
# Procedimiento manual para obtener contrastes y ajustes de Tukey
# [capítulo 14, ajustando según capítulo 15]
# ---------------------------------------------------------------

# Obtenemos las diferencias entre todos los pares de tratamientos
diferencias <- outer(media.por.tratamiento, media.por.tratamiento, "-")
triang <- upper.tri(diferencias)
difs <- diferencias[triang]

# Ahora obtenemos los estadísticos Q (Tukey)
den <- sqrt(MS.error / N.sujetos)
Qs <- difs / den

# Para obtener los intervalos con un nivel de confianza dado
?? <- 0.05
q?? <- qtukey(1 - ??, k, df.error)
me?? <- q?? * den
ics.l <- difs - me??
ics.u <- difs + me??

# Finalmente necesitamos los p-valores ajustados
pvals <- ptukey(abs(Qs), k, df.error, lower.tail = FALSE)

# Para contruir la tabla, necesitamos los pares de diferencias
nombres <- names(media.por.tratamiento)
pares <- outer(nombres, nombres, "paste", sep = " - ")
contrastes <- pares[triang]

# Creamos la tabla
m <- length(contrastes)
dnames <- list(contrastes, c("diff", "lwr", "upr","p adj"))
valores <- round(c(difs, ics.l, ics.u, pvals), 4)
tabla.tukey <- array(valores, c(m, 4), dnames)

# Y la mostramos en pantalla como la función TukeyHSD()
cat("\n\n")
cat("Comparaciones múltiples entre tratamientos según VarssarStats\n")
cat("-------------------------------------------------------------\n")
cat("  Tukey multiple comparisons of means\n")
cat("    ", (1-??)*100, "% family-wise confidence level", "\n", sep = "")
cat("\n")
cat("$Duración\n")
print(tabla.tukey, row.names = FALSE, justify = "left")
cat("\n\n")


# Con esto, podemos concluir que dar 2 días de capacitación a los
# desarrolladores disminuye *significativamente* el número de errores
# promedio que comenten sin capacitación. También vemos que entregar
# otros 2 días de capacitación (4 en total) permite disminuir todavía
# más el número de errores promedio que cometen los desarrolladores.
# Pero otros dos días de capacitación no tiene un efecto
# estadísticamente significativo en el número promedio de errores que
# estos cometen.



# ---------------------------------------------------------------
# Tukey implementado en R (extra, no evaluado en este curso)
# ---------------------------------------------------------------

# Aunque está fuera del ámbito de este curso, hoy en día van ganando
# terreno el uso de modelos mixtos.
# Esto es fácil si el factor que produce el efecto lo consideramos fijo,
# y consideramos que los sujetos fueron elegidos al azar... un factor
# fijo y otro aleatorio.
# Ya hay varias implementaciones de análisis con modelos mixtos en R.
# Un artículo que los revisa bien se encuentra en:
#     https://rpsychologist.com/r-guide-longitudinal-lme-lmer

# Usemos el paquete 'nlme' en este caso
library(nlme)

# Ahora, obtengamos un modelo mixto con 'Duración' como factor fijo y
# 'Desarrollador' como factor aleatorio.
mix <- lme(Errores ~ Duración, data = datos.long, random = ~1|Desarrollador)

# Pero los modelos mixtos son muy flexibles y se les puede consultar
# mucha información. Una tabla ANOVA es solo una de esas. Luego, hay que
# hacerlo explícitamente.

cat("\n\n")
cat("Tabla ANOVA construida con un modelo mixto\n")
cat("-----------------------------------------\n")
print(anova(mix))
cat("\n\n")

# Los modelos mixtos también permite hacer muchos tipos de "contrastes",
# esto es, comparaciones entre grupos.
# Hay paquetes de R dedicados a esto, en particular el paquete 'emmeans'
# (estimated marginal means, EMM) que tiene relación con cómo se
# calculan las medias cuando estamos "juntando" medias de varios grupos.
# o tratamientos. Pero también esto está fuera del alcance de este curso.
# En fin, por defecto, el método pairs() de este paquete asume el ajuste
# de las comparaciones de pares de tratamientos con el método de Tukey.
# Por supuesto, se puede cambiar si se quiere otro tipo de ajuste.

library(emmeans)
em <- emmeans(mix, "Duración")
tem <- pairs(em)
# print(em)
cat("\n\n")
cat("Comparaciones de las diferencias de los tratamientos con EMM\n")
cat("------------------------------------------------------------\n")
print(tem)
cat("Intervalos de confianza con EMM\n")
cat("-------------------------------\n")
print(confint(tem))
cat("\n\n")


# De forma similar, se puede obtener los mismos resultados con el
# paquete 'lsmeans' (Least square means), que otra forma en que los
# estadísticos llaman a las medias marginales.
#
# library(lsmeans)
# lsm <- lsmeans(mix, "Duración")
# tlsm <- pairs(lsm, adjust = "tukey")
# print(tlsm)


# Note que, a pesar que aparece frecuentemente el siguiente código como
# opción en los foros (con el paquete 'multcomp'), esta no sirve porque
# se usa el contraste de Tukey, es decir todos los pares de diferencias,
# pero no se ajustan los p-valores con el estadístico Q de Tukey.
#
# library(multcomp)
# ht <- glht(mix, linfct = mcp(Duración = "Tukey"))
# print(summary(ht))
