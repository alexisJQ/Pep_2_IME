library(ez)
library(ggpubr)
library(knitr)
library(tidyr)


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
Desarrollador <- factor(1:nrow(dl))
datos.long <- cbind(Desarrollador, dl)
datos.long[["Duraci�n"]] <- factor(datos.long[["Duraci�n"]])

# Una primera aproximaci�n es comparar los grupos con una gr�fico de
# cajas.
p1 <- ggboxplot(
  datos.long,
  x = "Duraci�n", y = "Errores",
  xlab = "Duraci�n capacitaci�n", ylab = "Errores",
  color = "Duraci�n",
  add = "jitter",
  add.params = list(color = "Duraci�n", fill = "Duraci�n")
)
print(p1)


# En general, parece haber una disminuci�n en el n�mero de errores.


# ----------------------------------------------------
# Hip�tesis
# ----------------------------------------------------

# �Cu�les ser�an las hip�tesis en este caso? 
# Usemos las definiciones en la secci�n 5.5 de OpenIntro Statistics:
# H0: The mean outcome is the same across all groups.
# HA: At least one mean is different.
# 
# Luego, en este caso:
# H0: La media del n� de pruebas unitarias falladas por sprint es la
#     misma en todos los grupos de desarrolladores 
# HA: La media de pruebas falladas de al menos un grupo de
#     desarrolladores es distinta 
# 


# ----------------------------------------------------
# Verificaciones
# ----------------------------------------------------

# Estas se explican en el cap�tulo 14 de VarssarStats:
# 1. La variable dependiente tiene escala de intervalo
# 2. Las muestras son independientes y obtenidas aleatoriamente
# 3. Se puede asumir que las poblaciones son aproximadamente normales
# 4. Las muestras tienen varianzas similares

# La condici�n 1 se verifica, puesto que 10 errores, por ejemplo, es el
# doble de 5 errores y la mitad de 20 errores.

# La condici�n 2 se verifica en el enunciado.

# Para la condici�n 3, podemos usar un gr�fico QQ
p2 <- ggqqplot(
  datos.long,
  x = "Errores",
  color = "Duraci�n"
)
p2 <- p2 + facet_wrap(~ Duraci�n)

# Podemos ver que, si vien hay un par de puntos m�s o menos 
# problem�ticos por aqu� por all�, no hay desviaciones importantes en
# los datos. 

# Para confirmar, podr�amos aplicar alguna prueba de normalidad.
# Con muestras tan peque�as, podr�a convenir usar la prueba de 
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

# En principio, no habr�a razones para suponer que los datos no vienen
# de poblaciones normales.

# Falta entonces la condici�n 4. El diagrama de cajas sugiere que hay
# alg�n cambio en la variabilidad de los equipos al introducir
# capacitaci�n.
# Podemos confirmar aplicando una prueba de homocedasticidad
# (o homogeneidad de varianzas). En general, se recomienda la prueba de
# Levene.
# [Ver por ejemplo, www.johndcook.com/blog/2018/05/16/f-bartlett-levene/]

library(car)

lts <- leveneTest(Errores ~ Duraci�n, datos.long)
cat("\n\n")
cat("Pruebas de homocedasticidad\n")
cat("---------------------------\n")
cat("\n")
print(lts)

# Vemos que no hay razones para creer que hay problemas con la varianza.
# Luego, podemos continuar con al an�lisis.


# ----------------------------------------------------
# Procedimiento manual del cap�tulo 14 de VarssarStats
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

# Y podr�amos obtener la suma de las desviaciones cuadradas entre los
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

# Y ahora el estad�stico
F <- MS.bg / MS.wg

# Ahora obtenemos un p-valor
Pr <- 1 -pf(F, df.bg, df.wg)
pv <- round(Pr, 3)
if(pv < 0.001) {
  pvs <- "<.001"
} else {
  pvs <- sub(pattern = "0.", replacement=".", x = sprintf("%1.3f", pv))
}

# Creamos una tabla con esta informaci�n
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
  between = Duraci�n,
  type = 3,
  return_aov = TRUE
)
print(ez.aov)

# Podemos ver que el objeto que devuelve ezANOVA() contiene una prueba
# de homocedasticidad de Levene, por lo que no ser�a necesario hacerla
# por separado si vamos a usar esta funci�n.

# Otra cosa a notar, es que esta funci�n nos entrega autom�ticamente un
# tama�o del efecto, medido con un estad�stico llamado 'generalized eta
# squared', que parece ser la m�s recomendada para medir cu�nta de la
# varianza medida se puede atribuir al factor en estudio. Recordar que
# esta medida trata de responder a la pregunta �qu� tan importante fue
# el efecto?, lo que no tiene relaci�n con su significaci�n estad�stica.

# Podemos el el resultado gr�ficamente.
ezp <- ezPlot(
  data = datos.long,
  dv = Errores,
  wid = Desarrollador,
  between = Duraci�n,
  type = 3,
  x = Duraci�n
)
print(ezp)


# ----------------------------------------------------
# An�lisis post-hoc
# ----------------------------------------------------

# En el cap�tulo 14 de VarssarStats se presenta el procedimiento para
# obtener contrastes y ajustes con el m�todo de la diferencia
# significativa honesta propuesta por Tukey, el que se puede traducir
# en el siguiente c�digo:

# Obtenemos las diferencias entre todos los pares de grupos
diferencias <- outer(media.por.grupo, media.por.grupo, "-")
triang <- lower.tri(diferencias)
difs <- diferencias[triang]

# Ahora obtenemos los estad�sticos Q (Tukey)
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

# Y la mostramos en pantalla como la funci�n TukeyHSD()
cat("\n\n")
cat("Comparaciones m�ltiples entre los grupos seg�n VarssarStats\n")
cat("-----------------------------------------------------------\n")
cat("  Tukey multiple comparisons of means\n")
cat("    ", (1-alpha)*100, "% family-wise confidence level", "\n", sep = "")
cat("\n")
cat("Fit: \n")
print(ez.aov[["aov"]][["call"]])
cat("\n")
cat("$Duraci�n\n")
print(tabla.tukey, row.names = FALSE, justify = "left")
cat("\n\n")


# Por otro lado, hay muchos m�todos disponibles en R.
# 
# Primero, usemos la implementaci�n disponible para el m�todo de Tukey
# visto m�s arriba, que implementa la funci�n TukeyHSD() de R.
# Sin embargo, esta funci�n requiere de un objeto 'aov', que pudimos
# obtener a pesar de usar la implementaci�n de ANOVA del paquete 'ez'
# porque dimos el argumento 'return_aov = TRUE' al llamar la funci�n.
# Hay un segundo par�metro importante, el nivel de confianza para los
# intervalos de confianza que nos entrega la funci�n. Si no damos un
# valor, se asume 95%.
mt <- TukeyHSD(ez.aov[["aov"]])
cat("\n\n")
cat("Comparaciones m�ltiples entre los grupos:\n")
cat("-----------------------------------------\n")
print(mt)

# Tambi�n podemos graficar esta comparaci�n
# (se especifica 'las = 1' para que las etiquetas de los ejes se muestren
# de forma horizontal).

plot(mt, las = 1)

# Otra alternativa es aplicar pruebas T de Student para comparar todos 
# los pares de grupos. Esto se hace con la funci�n pairwise.t.test(),
# que recibe los mismos argumentos que la funci�n t.test() m�s uno extra
# que indica el m�todo para ajustar el nivel de significaci�n para las
# pruebas m�ltiples. Sin embargo, para facilidad del usuario, en vez de
# reportar los niveles de significaci�n de cada prueba, ajusta los
# p-valores en concordancia, haci�ndolos as� comparables con el nivel
# nominal. Por esto este par�metro extra se llama 'p.adjust.method'.
# Los m�todos disponibles puede verse al imprimir la variable global
# que las contiene:
cat("\n\n")
cat("M�todos de ajuste para comparaciones m�ltiples disponibles:\n")
cat("-----------------------------------------------------------\n")
print(p.adjust.methods)
cat("\n")

# Usemos uno cl�sico: el m�todo de Sture Holm
mc <- pairwise.t.test(datos.long[["Errores"]], datos.long[["Duraci�n"]],
                      paired = FALSE, p.adjust.method = "holm")
cat("\n\n")
cat("Comparaciones m�ltiples entre los grupos:\n")
cat("-----------------------------------------\n")
print(mc)


# ----------------------------------------------------
# �Qu� interpretamos?
# ----------------------------------------------------

# Coinciden en que hay una diferencia significativa entre no
# hacer capacitaci�n y tener 4 o 6 d�as de capacitaci�n, y que en
# realidad, da lo mismo tener 4 o 6 d�as de capacitaci�n.
# Conviene contratar 4 d�as de capacitaci�n para los desarrolladores.




