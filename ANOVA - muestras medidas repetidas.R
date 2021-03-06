
library(ggpubr)
library(knitr)
library(tidyr)

# Usaremos las funciones para hacer esta funci�n
require(ez)

# Definimos los datos
duraciones <- c("0 d�a", "2 d�as", "4 d�as", "6 d�as")
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
  key = "Duraci�n",
  value = "Errores",
  duraciones
)
Desarrollador <- factor(rep(1:nrow(datos.wide), ncol(datos.wide)))
datos.long <- cbind(Desarrollador, dl)
datos.long[["Duraci�n"]] <- factor(datos.long[["Duraci�n"]])

# Una primera aproximaci�n es comparar los tratamientos con una gr�fico
# de cajas.
p1 <- ggboxplot(
  datos.long,
  x = "Duraci�n", y = "Errores",
  xlab = "Duraci�n capacitaci�n", ylab = "Errores",
  color = "Duraci�n",
  add = "jitter",
  add.params = list(color = "Duraci�n", fill = "Duraci�n")
)
print(p1)


# Como son los mismos datos que analizamos como grupos independientes,
# se observa una disminuci�n en el n�mero de errores a medida que se
# aumentan los d�as de capacitaci�n.


# ----------------------------------------------------
# Hip�tesis
# ----------------------------------------------------

# �Cu�les ser�an las hip�tesis en este caso?
#
# Luego, en este caso:
# H0: La media del n� de pruebas unitarias falladas por sprint es la
#     misma en todas las mediciones aplicadas a los desarrolladores
# HA: La media de pruebas falladas en al menos una medici�n aplicada a
#     los desarrolladores es distinta
#


# ----------------------------------------------------
# Verificaciones
# ----------------------------------------------------

# Estas se explican en el cap�tulo 15 de VarssarStats:
# 1. La variable dependiente tiene escala de intervalo
# 2. Las muestras son independientes al *interior* de los grupos
# 3. Se puede asumir que las poblaciones son aproximadamente normales
# 4. Las muestras tienen varianzas similares
# 5. La matriz de varianzas-covarianzas es esf�rica

# La condici�n 1 se verifica, ya que en 5 y 15 errores hay 10 errores
# de diferencia, lo mismo que entre 10 y 20.

# La condici�n 2 tambi�n se verifica en el enunciado.

# Para la condici�n 3, obtenemos el mismo gr�fico QQ que vimos para el
# caso de muestras independientes (porque usamos los mimos valores).
p2 <- ggqqplot(
  datos.long,
  x = "Errores",
  color = "Duraci�n"
)
p2 <- p2 + facet_wrap(~Duraci�n)

# Luego, aunque hay un par de puntos m�s o menos problem�ticos, no hay
# desviaciones importantes en los datos, lo que fue confirmado con
# pruebas de normalidad de Shapiro-Wilk.

# Tambi�n revisamos que se cumple la homocedasticidad con estos datos,
# condici�n 4, usando la prueba de Levene.

# Falta entonces la condici�n de esfericidad. La verdad es que esto no
# es f�cil y es un conjunto de supociciones que se deben manejar.
# Una alternativa simple es decir que todos los pares de diferencias
# entre mediciones (en este caso 0/2, 0/4, 0/6, 2/4, 2/6 y 4/6 d�as)
# tienen igual varianza.
# Por fortuna, existe una prueba de esferidad propuesta por John W.
# Mauchly, aunque no tiene mucho poder estad�stico con muestras
# reducidas.
# [JW Mauchly (1940). "Significance test for sphericity of a normal
# n-variate distribution." The Annals of Mathematical Statistics, 11,
# 204-209].

# Esta prueba est� implementada en R, pero requiere de un modelo ya
# construido para hacer las estimaciones. Posterguemos esta prueba
# hasta que usemos la funci�n ezANOVA().

# En todo caso, siempre se hace los c�lculos de ANOVA-RM suponiendo que
# se cumple la condici�n de esfericidad. Si se llega a determinar que
# esta en realidad no se cumple, se puede aplicar una correcci�n de los
# grados de libertad y, por lo tanto, de los p-valores que se estimen.
# Los paquetes estad�sticos, en general, suelen reportar estas
# correcciones para ser consideradas. Esta propuesta es la que tambi�n
# adopta la funci�n ezANOVA().


# ----------------------------------------------------
# Procedimiento manual del cap�tulo 15 de VarssarStats
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

# Pero ahora, tambi�n tenemos medias por sujeto (desarrollador, en este
# caso).
media.por.sujeto <- apply(datos.wide, 1, mean)

# Podemos obtener la suma de las desviaciones cuadradas observadas para
# cada sujeto
S.por.sujeto <- (media.por.sujeto - media.total)^2
mediciones.v�lidas <- apply(datos.wide, c(1, 2), is.finite)
mediciones.por.sujeto <- apply(mediciones.v�lidas, 1, sum)
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

# Y ahora el estad�stico
F <- MS.bg / MS.error

# Ahora obtenemos un p-valor
Pr <- 1 - pf(F, df.bg, df.error)
pv <- round(Pr, 3)
if(pv < 0.001) {
  pvs <- "<.001"
} else {
  pvs <- sub(pattern = "0.", replacement=".", x = sprintf("%1.3f", pv))
}

# Creamos una tabla con esta informaci�n
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
cat("Tabla ANOVA construida seg�n VarssarStats\n")
cat("-----------------------------------------")
print(kt)
cat("\n\n")


# ----------------------------------------------------
# Usando las funciones del paquete ez
# ----------------------------------------------------

# La funci�n ezANOVA() no acepta (directamente) nombres de las columnas
# en variables de texto. Por eso, usamos los nombres que hemos fijado de
# "forma dura".
ez.aov <- ezANOVA(
  data = datos.long,
  dv = Errores,
  wid = Desarrollador,
  within = Duraci�n,
  type = 3,
  return_aov = TRUE
)
print(ez.aov)


# Podemos el el resultado gr�ficamente.
ezp <- ezPlot(
  data = datos.long,
  dv = Errores,
  wid = Desarrollador,
  within = Duraci�n,
  type = 3,
  x = Duraci�n
)
print(ezp)


# Podemos ver que, asumiendo esfericidad, se tiene un p-valor < .001.

# La funci�n tambi�n reporta un p-valor = 0.478 para la prueba de
# esfericidad, por lo que estos datos s� estar�an cumpliendo esa
# condici�n.

# Si esto no fuera as�, y la prueba de esfericidad resultara
# estad�sticamente significativa, habr�a que considerar las correcciones
# de Greenhouse-Geisser o las de Huynh-Feldt, ambas reportadas por la
# funci�n, y considerar en consecuencia p-valores mayores a los
# estimados asumiendo esfericidad (.0004764 y .0000295 respectivamente,
# en vez del .0000021 original).

# Entonces, corresponde hacer hacer un an�lisis post hoc.


# ----------------------------------------------------
# An�lisis post-hoc
# ----------------------------------------------------

# Aqu�, todav�a podemos hacer pruebas T de Student entre pares de
# tratamientos, pero teniendo cuidado de usar pruebas para datos
# apareados.
mc <- pairwise.t.test(datos.long[["Errores"]], datos.long[["Duraci�n"]],
                      paired = TRUE, p.adjust.method = "holm")
cat("\n\n")
cat("Comparaciones m�ltiples entre los tratamientos:\n")
cat("-----------------------------------------------\n")
print(mc)


# Pero ahora no tenemos disponibles el m�todo de Tukey
# No funciona: mt <- TukeyHSD(ez.aov[["aov"]])
# Una opci�n es implementar lo que se describe en VarssarStats:

# ---------------------------------------------------------------
# Procedimiento manual para obtener contrastes y ajustes de Tukey
# [cap�tulo 14, ajustando seg�n cap�tulo 15]
# ---------------------------------------------------------------

# Obtenemos las diferencias entre todos los pares de tratamientos
diferencias <- outer(media.por.tratamiento, media.por.tratamiento, "-")
triang <- upper.tri(diferencias)
difs <- diferencias[triang]

# Ahora obtenemos los estad�sticos Q (Tukey)
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

# Y la mostramos en pantalla como la funci�n TukeyHSD()
cat("\n\n")
cat("Comparaciones m�ltiples entre tratamientos seg�n VarssarStats\n")
cat("-------------------------------------------------------------\n")
cat("  Tukey multiple comparisons of means\n")
cat("    ", (1-??)*100, "% family-wise confidence level", "\n", sep = "")
cat("\n")
cat("$Duraci�n\n")
print(tabla.tukey, row.names = FALSE, justify = "left")
cat("\n\n")


# Con esto, podemos concluir que dar 2 d�as de capacitaci�n a los
# desarrolladores disminuye *significativamente* el n�mero de errores
# promedio que comenten sin capacitaci�n. Tambi�n vemos que entregar
# otros 2 d�as de capacitaci�n (4 en total) permite disminuir todav�a
# m�s el n�mero de errores promedio que cometen los desarrolladores.
# Pero otros dos d�as de capacitaci�n no tiene un efecto
# estad�sticamente significativo en el n�mero promedio de errores que
# estos cometen.



# ---------------------------------------------------------------
# Tukey implementado en R (extra, no evaluado en este curso)
# ---------------------------------------------------------------

# Aunque est� fuera del �mbito de este curso, hoy en d�a van ganando
# terreno el uso de modelos mixtos.
# Esto es f�cil si el factor que produce el efecto lo consideramos fijo,
# y consideramos que los sujetos fueron elegidos al azar... un factor
# fijo y otro aleatorio.
# Ya hay varias implementaciones de an�lisis con modelos mixtos en R.
# Un art�culo que los revisa bien se encuentra en:
#     https://rpsychologist.com/r-guide-longitudinal-lme-lmer

# Usemos el paquete 'nlme' en este caso
library(nlme)

# Ahora, obtengamos un modelo mixto con 'Duraci�n' como factor fijo y
# 'Desarrollador' como factor aleatorio.
mix <- lme(Errores ~ Duraci�n, data = datos.long, random = ~1|Desarrollador)

# Pero los modelos mixtos son muy flexibles y se les puede consultar
# mucha informaci�n. Una tabla ANOVA es solo una de esas. Luego, hay que
# hacerlo expl�citamente.

cat("\n\n")
cat("Tabla ANOVA construida con un modelo mixto\n")
cat("-----------------------------------------\n")
print(anova(mix))
cat("\n\n")

# Los modelos mixtos tambi�n permite hacer muchos tipos de "contrastes",
# esto es, comparaciones entre grupos.
# Hay paquetes de R dedicados a esto, en particular el paquete 'emmeans'
# (estimated marginal means, EMM) que tiene relaci�n con c�mo se
# calculan las medias cuando estamos "juntando" medias de varios grupos.
# o tratamientos. Pero tambi�n esto est� fuera del alcance de este curso.
# En fin, por defecto, el m�todo pairs() de este paquete asume el ajuste
# de las comparaciones de pares de tratamientos con el m�todo de Tukey.
# Por supuesto, se puede cambiar si se quiere otro tipo de ajuste.

library(emmeans)
em <- emmeans(mix, "Duraci�n")
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
# estad�sticos llaman a las medias marginales.
#
# library(lsmeans)
# lsm <- lsmeans(mix, "Duraci�n")
# tlsm <- pairs(lsm, adjust = "tukey")
# print(tlsm)


# Note que, a pesar que aparece frecuentemente el siguiente c�digo como
# opci�n en los foros (con el paquete 'multcomp'), esta no sirve porque
# se usa el contraste de Tukey, es decir todos los pares de diferencias,
# pero no se ajustan los p-valores con el estad�stico Q de Tukey.
#
# library(multcomp)
# ht <- glht(mix, linfct = mcp(Duraci�n = "Tukey"))
# print(summary(ht))
