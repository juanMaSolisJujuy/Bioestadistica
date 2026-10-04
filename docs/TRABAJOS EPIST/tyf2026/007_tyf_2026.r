# =============================================================================
# Bioestadística - Facultad de Ciencias Agrarias - UNJu - Licenciatura en Ciencias Biológicas - AÑO 2026
# Trabajo Práctico 7: Inferencia para Pequeñas Muestras (Distribuciones F de Snedecor y t de Student)
# Ejercicios de práctica de clase
# Jefe de Trabajos Prácticos: Mg. Ing. Juan Manuel Solís; Ayudante 2da: Sr. Daniel Vilca
# =============================================================================
#
# ORDEN DE LAS ACTIVIDADES
# ----------------
#   PARTE A - ACTIVIDADES GUIADAS
#     SECCIÓN 1 - Inferencia de una media poblacional con sigma desconocido (t para una muestra)
#     SECCIÓN 2 - Prueba F de Snedecor para la comparación de dos varianzas
#     SECCIÓN 3 - Pruebas t para la comparación de dos medias a partir de muestras independientes con varianzas homogéneas
#     SECCIÓN 4 - Prueba t para dos muestras independientes con varianzas heterogéneas (corrección de Welch)
#     SECCIÓN 5 - Prueba t para muestras apareadas (no independientes)
#   PARTE B - EJERCICIOS DE APLICACIÓN (Ejercicios 1 a 9)
#
# Instrucciones generales
# -----------------------
# En cada ejemplo y ejercicio:
#   (a) identifique la pregunta de investigación, los objetivos, las variables y el tipo de variable
#   (b) identifique la prueba estadística aplicada. Describa condiciones/supuestos para su aplicación.
#   (c) plantee las hipótesis nula (H0) y alternativa (H1) y defina el valor de α
#   (d) ejecute los cálculos utilizando los objetos de R previamente definidos
#   (e) interprete el estadístico de prueba, el p-valor y aplique la regla de decisión (si corresponde)
#   (f) elabore una conclusión o SÍNTESIS integrada en el contexto del problema
#### ---------------------------------------------------------------------------------------------------#######
#   NOTA IMPORTANTE 1: ALGUNOS CÁLCULOS/FUNCIONES SON PROVISTAS, OTRAS LAS TENDRÁ QUE HACER USTED
#   NOTA IMPORTANTE 2: algunas secuencias implican solo lectura; en otras implica algún cálculo y decisiones
#### ---------------------------------------------------------------------------------------------------#######
#
# Funciones disponibles (R base): 
#
#   c(...)            # combina elementos en un vector
#   mean(...)         # calcula la media aritmética muestral
#   sd(...)           # calcula la desviación estándar muestral
#   var(...)          # calcula la varianza muestral
#   length(...)       # devuelve el número de observaciones (tamaño muestral n)
#   sqrt(...)         # calcula la raíz cuadrada de un número
#   abs(...)          # calcula el valor absoluto
#   qt(...)           # devuelve el cuantil de la distribución t de Student
#   pt(...)           # devuelve la probabilidad acumulada de la distribución t de Student
#   t.test(x, y, var.equal = ..., alternative = ..., paired = ...)  # prueba t de Student e intervalos de confianza
#   var.test(x, y, alternative = ...)  # prueba F para el cociente de varianzas
#   cat(...)          # imprime mensajes concatenados en consola
#   round(...)        # redondea un número al total de decimales especificado
#
# Estadísticos "manuales" (si prefiere cálculos manuales; nosotros recomendamos usar funciones específicas):
#   Una media:
#     tc = (mean(x) - mu0) / (sd(x) / sqrt(n))
#     p_valor = 2 * (1 - pt(abs(tc), df = n - 1))
#     ic = mean(x) + c(-1, 1) * qt(1 - alfa/2, df = n - 1) * (sd(x) / sqrt(n))
#   Dos varianzas:
#     F_calc = var(x1) / var(x2)
#   Dos medias independientes con varianzas homogéneas:
#     sp2 = ((n1 - 1)*var(x1) + (n2 - 1)*var(x2)) / (n1 + n2 - 2)
#     t_calc = (mean(x1) - mean(x2)) / sqrt(sp2 * (1/n1 + 1/n2))

# NOTA IMPORTANTE: recuerde que en los ejemplos se utilizaron valores simulados y deben interpretarse con fines
# didácticos, y no como evidencia científica.


# *****************************************************************************
# *****************************************************************************
#                      PARTE A - ACTIVIDADES GUIADAS
# *****************************************************************************
# *****************************************************************************


# #############################################################################
# SECCIÓN 1 - INFERENCIA DE UNA MEDIA POBLACIONAL CON SIGMA DESCONOCIDO
# #############################################################################

# ─────────────────────────────────────────────────────────────────────────────
# EJEMPLO 1 - ACTIVIDAD GUIADA
# ─────────────────────────────────────────────────────────────────────────────
# Contexto: Evaluación del crecimiento de la radícula en plántulas de poroto (Phaseolus vulgaris) en ensayo de germinación controlado.
# Situación: Se requiere comprobar si la longitud media de la radícula difiere significativamente del valor patrón de referencia biológico de 7.5 cm en condiciones óptimas de laboratorio, 
# analizando una muestra aleatoria de 15 plántulas.
# Referencia: GRUPO 14 ¿Cómo afectan distintas concentraciones NaCl, en la germinación y el crecimiento inicial de la plántula 
# de poroto (Phaseolus vulgaris)? 


# PASO 1 - DEFINICIÓN DE VARIABLE ALEATORIA Y PARÁMETROS (LEER)
# ─────────────────────────────────────────────────────────────────────────────
# Variable aleatoria (X): Longitud de la radícula de plántulas de Phaseolus vulgaris (en cm). Variable cuantitativa continua.
# Parámetro poblacional (µ): Longitud media verdadera de la radícula en la población bajo las condiciones del ensayo (en cm).
# Parámetro de referencia (µ0): Valor poblacional de referencia o estándar biológico (µ0 = 7.5 cm).
# Estimador puntual (X̄): Media muestral calculada a partir de las observaciones.
# Desviación estándar poblacional (σ): Desconocida. Se estima mediante la desviación estándar muestral (s).


# PASO 2 - CARGA DE DATOS EN OBJETOS DE R (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
# Longitud de radícula de plántulas de poroto medidas en centímetros (cm):
radicula = c(6.8, 7.2, 6.5, 7.0, 6.9, 7.4, 6.6, 7.1, 6.7, 7.3, 6.8, 7.0, 6.4, 7.2, 6.9)

n_obs = length(radicula)
media_m = mean(radicula)
sd_m = sd(radicula)
se_m = sd_m / sqrt(n_obs) # Error estándar

# PASO 3 - VERIFICACIÓN DE CONDICIONES DE APLICABILIDAD (RESPONDER/CALCULAR)
# ─────────────────────────────────────────────────────────────────────────────
# 1. ¿Se debe verificar un muestreo aleatorio independiente de plántulas individuales?¿O la selección debe respetar un patrón o agrupamiento dado?.

# RESPUESTA: ->

# 2. ¿Los datos provienen de una distribución aproximadamente Normal en la población?.

# ACTIVIDAD: realice la prueba de normalidad de los datos responda.
# Prueba ...


# RESPUESTA (Conclusión de la prueba): -> 


# 3. Dado que la varianza poblacional σ² es desconocida y el tamaño muestral es pequeño (n = 15 < 30), 
# el estadístico adecuado sigue una distribución t de Student con (n - 1) grados de libertad (LEER)


# PASO 4 - PLANTEO DE DOCIMACIA DE HIPÓTESIS (LEER Y RESPONDER DONDE CORRESPONDA)
# ─────────────────────────────────────────────────────────────────────────────
# 1) Hipótesis
# H0: µ = 7.5 cm  (La media de la longitud radicular equivale al valor patrón de 7.5 cm)
# H1: µ ≠ 7.5 cm  (La media de la longitud radicular difiere del valor patrón de 7.5 cm)

# ¿Por qué se plantea una H1 por 'distinto'? ¿Se trata de una prueba uni o bilateral?

# RESPUESTA: ->

# 2) Nivel de significación
# α  = 0.05       (Nivel de significación del 5%)

# 3) Estadístico de prueba
# CÁLCULO MANUAL DEL ESTADÍSTICO DE PRUEBA T
# ─────────────────────────────────────────────────────────────────────────────
# Fórmula del estadístico de prueba: tc = (X̄ - µ₀) / (s / √n)
mu0 = 7.5
tc = (media_m - mu0) / (sd_m / sqrt(n_obs))

# 4) Regla de decisión

# Se rechaza H0 si y solo si p-valor ...
# RESPUESTA: ->

# 5) Cálculos
# ─────────────────────────────────────────────────────────────────────────────
# Con la función t.test()

alternativa = "two.sided"        # Elegir entre 'less', 'two.sided' y "greater", según sea una prueba unilateral izquierda, bilateral o unilateral derecha respectivamente
prueba_t = t.test(radicula, mu = mu0, alternative = alternativa, conf.level = 1-alfa) # Realizamos la prueba con la función t.test y la almacenamos con el nombre prueba_t
print(prueba_t)    # Visualizamos la salida de la prueba t
p_valor = prueba_t$p.value   # Almacenamos el p-valor de la salida de la prueba_t, con el nombre p_valor

# 6) Decisión
# ─────────────────────────────────────────────────────────────────────────────

if (p_valor < 0.05) {
  print("Decisión: Se rechaza la hipótesis nula (H0) al nivel de significación α = 0.05.\n")
} else {
  print("Decisión: No se rechaza la hipótesis nula (H0) al nivel de significación α = 0.05.\n")
}

# 7) Conclusión

# RESPUESTA: ->

# INTERVALO DE CONFIANZA BILATERAL (95%)
# ─────────────────────────────────────────────────────────────────────────────
# Fórmula: IC₉₅% = X̄ ± t_(1-α/2; n-1) * (s / √n)
alfa = 0.05
ic_95 = t.test(radicula, mu = mu0, alternative = alternativa, conf.level = 1-alfa)
print(prueba_t$conf.int) 

# Resultado esperado IC 95%: [6.7576, 7.0824] cm
# Interpretación/lectura

# RESPUESTA: ->

# PASO 5 - INTERPRETACIÓN FINAL EN EL CONTEXTO BIOLÓGICO
# ─────────────────────────────────────────────────────────────────────────────

# RESPUESTA: -> 

# =============================================================================


# #############################################################################
# SECCIÓN 2 - PRUEBA F DE SNEDECOR PARA LA COMPARACIÓN DE DOS VARIANZAS
# #############################################################################

# ─────────────────────────────────────────────────────────────────────────────
# EJEMPLO 1 - ACTIVIDAD GUIADA
# ─────────────────────────────────────────────────────────────────────────────
# Trabajo: "Calidad de agua en ambientes periglaciares comparación: entre áreas
# con y sin actividad minera"
# Integrantes: Grupo 6
# (Datos simulados con fines didácticos)

# Contexto: Calidad de agua en arroyos periglaciares con y sin influencia de actividad minera.
# Situación: Se midió la turbidez del agua mediante la profundidad de visibilidad con disco de Secchi (cm) en dos arroyos
# periglaciares: una cuenca con influencia minera y una cuenca de referencia (reserva natural), con 10 mediciones por sitio.
# Se desea comparar la variabilidad de la turbidez entre ambos sitios.
 
 
# PASO 1 - DEFINICIÓN DE VARIABLE ALEATORIA Y PARÁMETROS (LEER)
# ─────────────────────────────────────────────────────────────────────────────
# Variable aleatoria (X): Profundidad de visibilidad con disco de Secchi, indicador de turbidez (en cm). Variable cuantitativa continua.
# Factor en estudio: Sitio (cuenca con influencia minera vs. cuenca de referencia).
# Parámetros poblacionales (σ1², σ2²): Varianza verdadera de la profundidad de Secchi en la cuenca minera (σ1²) y en la de referencia (σ2²).
# Estimadores puntuales (s1², s2²): Varianzas muestrales de cada sitio.
# Parámetro de comparación: cociente de varianzas poblacionales σ1² / σ2².
 
 
# PASO 2 - CARGA DE DATOS EN OBJETOS DE R (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
# Profundidad de Secchi (cm):
turbidez_control = c(120, 125, 118, 122, 130, 124, 119, 127, 121, 126)
turbidez_minera  = c(85, 105, 70, 115, 60, 125, 75, 110, 95, 65)
 
n1 = length(turbidez_minera);  var1 = var(turbidez_minera)
n2 = length(turbidez_control); var2 = var(turbidez_control)
 
 
# PASO 3 - VERIFICACIÓN DE CONDICIONES DE APLICABILIDAD (RESPONDER/CALCULAR)
# ─────────────────────────────────────────────────────────────────────────────
# 1. ¿Las dos muestras deben ser independientes entre sí? ¿Y las observaciones dentro de cada sitio?
 
# RESPUESTA: ->
 
# 2. ¿Los datos de ambos sitios provienen de distribuciones aproximadamente Normales? (La prueba F es muy sensible a desvíos de la normalidad)
 
# ACTIVIDAD: realice la prueba de normalidad de los datos de ambos grupos y responda.
# Prueba ...
 
 
# RESPUESTA (Conclusión de la prueba): ->
 
# 3. Dado que se comparan dos varianzas de poblaciones normales con muestras pequeñas, el estadístico F
# (cociente de varianzas muestrales) sigue una distribución F de Snedecor con (n1 - 1) y (n2 - 1) grados de libertad (LEER)
 
 
# PASO 4 - PLANTEO DE DOCIMASIA DE HIPÓTESIS (LEER Y RESPONDER DONDE CORRESPONDA)
# ─────────────────────────────────────────────────────────────────────────────
# 1) Hipótesis
# H0: σ1² / σ2² = 1  (La variabilidad de la turbidez es la misma en ambos sitios)
# H1: σ1² / σ2² ≠ 1  (La variabilidad de la turbidez es distinta entre ambos sitios)
 
# ¿Por qué se plantea una H1 por 'distinto'? ¿Se trata de una prueba uni o bilateral?
 
# RESPUESTA: ->
 
# 2) Nivel de significación
alfa = 0.05
 
# 3) Estadístico de prueba
# CÁLCULO MANUAL DEL ESTADÍSTICO DE PRUEBA F
# ─────────────────────────────────────────────────────────────────────────────
# Fórmula del estadístico de prueba: F_calc = s1² / s2²
F_calc = var1 / var2
 
# 4) Regla de decisión
 
# Se rechaza H0 si y solo si p-valor ...
# RESPUESTA: ->
 
# 5) Cálculos
# ─────────────────────────────────────────────────────────────────────────────
# Con la función var.test()
 
alternativa = "two.sided"        # Elegir entre 'less', 'two.sided' y "greater"
prueba_F = var.test(turbidez_minera, turbidez_control, alternative = alternativa, conf.level = 1 - alfa)
print(prueba_F)
p_valor = prueba_F$p.value
 
# 6) Decisión
# ─────────────────────────────────────────────────────────────────────────────
if (p_valor < alfa) {
  cat("Decisión: Se rechaza la hipótesis nula (H0) al nivel de significación α =", alfa)
} else {
  cat("Decisión: No se rechaza la hipótesis nula (H0) al nivel de significación α =", alfa)
}
 
# 7) Conclusión
 
# RESPUESTA: ->
 
# INTERVALO DE CONFIANZA PARA EL COCIENTE DE VARIANZAS (95%)
# ─────────────────────────────────────────────────────────────────────────────
print(prueba_F$conf.int)
 
# Resultado esperado IC 95%: [8.687; 140.8043]  (F_calc = 34.9738)
# Interpretación/lectura (¿el intervalo contiene el valor 1?)
 
# RESPUESTA: ->
 
# PASO 5 - INTERPRETACIÓN FINAL EN EL CONTEXTO BIOLÓGICO
# ─────────────────────────────────────────────────────────────────────────────
 
# RESPUESTA: ->
 
# =============================================================================
 

# #############################################################################
# SECCIÓN 3 - PRUEBAS t PARA LA COMPARACIÓN DE DOS MEDIAS
# #############################################################################
# Recordatorio: con dos muestras independientes, la prueba F (Sección 2) se aplica a priori
# para decidir la versión de la prueba t:
#   - Si no se rechaza H0 de la prueba F  -> t con varianzas homogéneas (var.equal = TRUE)   -> 3A
#   - Si se rechaza H0 de la prueba F     -> t con varianzas heterogéneas (var.equal = FALSE) -> 3B
# Con muestras apareadas no corresponde la prueba F: se trabaja sobre las diferencias        -> 3C


# EJEMPLO 3 - PRUEBA t DE STUDENT PARA DOS MUESTRAS INDEPENDIENTES CON VARIANZAS HOMOGÉNEAS
# ─────────────────────────────────────────────────────────────────────────────
 
 
# PASO 1 - DEFINICIÓN DE VARIABLE ALEATORIA Y PARÁMETROS (LEER)
# ─────────────────────────────────────────────────────────────────────────────
# Variable aleatoria (X): Elongación de la radícula de plántulas de Phaseolus vulgaris (en cm). Variable cuantitativa continua.
# Factor en estudio: Condición de germinación (control con agua destilada vs. solución salina con NaCl).
# Parámetros poblacionales (µ1, µ2): Elongación media verdadera de la radícula en el grupo control (µ1) y en el grupo con NaCl (µ2).
# Parámetros de variabilidad (σ1², σ2²): Varianzas poblacionales de cada condición. Desconocidas; se estiman mediante s1² y s2².
# Estimador puntual de la diferencia: X̄1 - X̄2.
 
 
# PASO 2 - CARGA DE DATOS EN OBJETOS DE R (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
# Elongación de la radícula (cm):
radicula_control = c(7.8, 8.2, 7.5, 8.0, 7.2, 7.9, 8.1, 7.6, 8.4, 7.7)
radicula_nacl    = c(5.4, 6.1, 5.8, 4.9, 6.3, 5.2, 5.7, 6.0, 5.1, 5.5)
 
n1 = length(radicula_control); media1 = mean(radicula_control); sd1 = sd(radicula_control)
n2 = length(radicula_nacl);    media2 = mean(radicula_nacl);    sd2 = sd(radicula_nacl)
 
 
# PASO 3 - VERIFICACIÓN DE CONDICIONES DE APLICABILIDAD (RESPONDER/CALCULAR)
# ─────────────────────────────────────────────────────────────────────────────
# 1. ¿Las dos muestras deben ser independientes entre sí? ¿Se trata de plántulas distintas en cada condición o de mediciones repetidas sobre las mismas?
 
# RESPUESTA: ->
 
# 2. ¿Los datos de ambas condiciones provienen de distribuciones aproximadamente Normales?
 
# ACTIVIDAD: realice la prueba de normalidad de los datos de ambos grupos (Shapiro-Wilk y/o QQ-plot) y responda.
# Prueba ...
 
 
# RESPUESTA (Conclusión de la prueba): ->
 
# 3. La homogeneidad de varianzas (σ1² = σ2²) se verifica en el PASO 4 mediante la prueba F, y su resultado define
# qué versión de la prueba t de Student se aplica para comparar las medias (LEER)
 
 
# PASO 4 - DOCIMASIA PREVIA: HOMOGENEIDAD DE VARIANZAS (PRUEBA F DE SNEDECOR)
# ─────────────────────────────────────────────────────────────────────────────
# 1) Hipótesis
# H0: σ1² / σ2² = 1  (Las varianzas poblacionales de ambas condiciones son iguales)
# H1: σ1² / σ2² ≠ 1  (Las varianzas poblacionales de ambas condiciones son distintas)
 
# ¿Por qué esta prueba se plantea como bilateral? ¿Qué distribución sigue el estadístico y con qué grados de libertad?
 
# RESPUESTA: ->
 
# 2) Nivel de significación
alfa = 0.05
 
# 3) Estadístico de prueba
# Fórmula del estadístico de prueba: F_calc = s1² / s2²
F_calc = var(radicula_control) / var(radicula_nacl)
 
# 4) Regla de decisión
 
# Se rechaza H0 si y solo si p-valor ...
# RESPUESTA: ->
 
# 5) Cálculos
prueba_F = var.test(radicula_control, radicula_nacl, alternative = "two.sided")
print(prueba_F)
p_valor_F = prueba_F$p.value
 
# 6) Decisión
if (p_valor_F < alfa) {
  print(paste("Decisión: Se rechaza H0 (varianzas heterogéneas) al nivel de significación α =", alfa))
} else {
  print(paste("Decisión: No se rechaza H0 (varianzas homogéneas) al nivel de significación α =", alfa))
}
 
# 7) Conclusión (¿qué versión de la prueba t corresponde aplicar?)
 
# RESPUESTA: ->
 
 
# PASO 5 - DOCIMASIA DE LA COMPARACIÓN DE MEDIAS (PRUEBA t PARA DOS MUESTRAS INDEPENDIENTES)
# ─────────────────────────────────────────────────────────────────────────────
# 1) Hipótesis
# H0: µ1 - µ2 = 0  (La salinidad no afecta la elongación media de la radícula)
# H1: µ1 - µ2 > 0  (La elongación media del control es mayor que la del tratamiento salino)
 
# ¿Por qué se plantea una H1 por 'mayor'? ¿Se trata de una prueba uni o bilateral?
 
# RESPUESTA: ->
 
# 2) Nivel de significación
alfa = 0.05
 
# 3) Estadístico de prueba
# Fórmula (varianzas homogéneas): sp2 = ((n1 - 1)*s1² + (n2 - 1)*s2²) / (n1 + n2 - 2)
#                                  tc = (X̄1 - X̄2) / sqrt(sp2 * (1/n1 + 1/n2))
sp2 = ((n1 - 1) * var(radicula_control) + (n2 - 1) * var(radicula_nacl)) / (n1 + n2 - 2)
tc = (media1 - media2) / sqrt(sp2 * (1 / n1 + 1 / n2))
 
# 4) Regla de decisión
 
# Se rechaza H0 si y solo si p-valor ...
# RESPUESTA: ->
 
# 5) Cálculos
# Con la función t.test()
alternativa = "greater"          # Elegir entre 'less', 'two.sided' y "greater"
varianzas_iguales = TRUE         # TRUE si las varianzas son homogéneas; FALSE si son heterogéneas (según el resultado de la prueba F)
prueba_t = t.test(radicula_control, radicula_nacl, var.equal = varianzas_iguales,
                  alternative = alternativa, conf.level = 1 - alfa)
print(prueba_t)
p_valor = prueba_t$p.value
 
# 6) Decisión
if (p_valor < alfa) {
  print(paste("Decisión: Se rechaza la hipótesis nula (H0) al nivel de significación α =", alfa))
} else {
  print(paste("Decisión: No se rechaza la hipótesis nula (H0) al nivel de significación α =", alfa))
}
 
# 7) Conclusión
 
# RESPUESTA: ->
 
# INTERVALO DE CONFIANZA PARA LA DIFERENCIA DE MEDIAS (95%)
# ─────────────────────────────────────────────────────────────────────────────
prueba_t = t.test(radicula_control, radicula_nacl, var.equal = varianzas_iguales,
                  alternative = "two.sided", conf.level = 1 - alfa) # Ahora planteamos la prueba bilateral para obtener IC bilateral
print(prueba_t$conf.int)
 
# Interpretación/lectura (recuerde que la prueba es unilateral: el intervalo es una cota inferior)
 
# RESPUESTA: ->
 
# PASO 6 - INTERPRETACIÓN FINAL EN EL CONTEXTO BIOLÓGICO
# ─────────────────────────────────────────────────────────────────────────────
 
# RESPUESTA: ->
 
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# 3B - MUESTRAS INDEPENDIENTES CON VARIANZAS HETEROGÉNEAS
# ─────────────────────────────────────────────────────────────────────────────
# Cuando la prueba F rechaza la igualdad de varianzas, se utiliza la prueba t para
# dos muestras independientes con varianzas desiguales (grados de libertad ajustados
# de Satterthwaite):
#
#   t.test(x1, x2, var.equal = FALSE, alternative = "two.sided")
#
# NOTA: el script original no incluía ejemplo resuelto ni ejercicio para este caso.
# Espacio reservado para incorporarlos:

# Su código aquí:

# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# 3C - MUESTRAS APAREADAS (OBSERVACIONES DEPENDIENTES)
# ─────────────────────────────────────────────────────────────────────────────

# EJEMPLO 4 - PRUEBA t DE STUDENT PARA MUESTRAS APAREADAS
# Evaluación de clorofila antes y después de aplicar bioestimulante
# ─────────────────────────────────────────────────────────────────────────────
# Pregunta de investigación:
#   ¿La aplicación foliar de un bioestimulante incrementa el índice de clorofila (SPAD)
#   en hojas de poroto respecto al estado previo al tratamiento?
#
# Objetivo:
#   Determinar si la media de las diferencias antes y después de la aplicación
#   es significativamente mayor a cero.
#
# Variables:
#   y  : Índice relativo de clorofila (unidades SPAD) [Cuantitativa continua]
#   x1 : Mediciones en hojas ANTES del tratamiento (vector con n = 7)
#   x2 : Mediciones en las MISMAS hojas DESPUÉS del tratamiento (vector con n = 7)
#
# Prueba estadística elegida:
#   Prueba t de Student para muestras apareadas (unilateral derecha).
# ----------------------------------------------------------------------------

# Datos de clorofila (SPAD)
x1 = c(32.1, 34.5, 31.0, 33.8, 35.2, 30.9, 33.0)   # Antes
x2 = c(36.4, 38.2, 35.1, 37.0, 39.5, 34.8, 36.9)   # Después

# Parámetros del análisis
alfa = 0.05
tipo_prueba = "unilateral derecha"   # Se evalúa si el incremento (x2 - x1) es significativo

# Hipótesis
# H0: µ_d = 0  (La diferencia media entre Antes y Después es igual a cero)
# H₁: µ_d > 0  (La clorofila Después es mayor que Antes; µ_Después - µ_Antes > 0)
# α = 0.05

# Cálculo de diferencias y prueba t apareada
res_t2 = t.test(x2, x1, paired = TRUE, alternative = "greater")
res_t2

# Decisión y conclusión


# Resultado esperado: diferencia media = 3.9143; t = 26.911; gl = 6; p-valor = 8.7e-08
# Conclusión: La aplicación del bioestimulante incrementó de manera significativa
# (p < 0.05) los niveles de clorofila foliar en las plantas de poroto evaluadas.

# =============================================================================

##############################################################################
# SECCIÓN 4 - PRUEBAS t PARA LA COMPARACIÓN DE DOS MEDIAS con VARIANZAS HETEROGÉNEAS
# #############################################################################
# Recordatorio: con dos muestras independientes, la prueba F (Sección 2) se aplica a priori
# para decidir la versión de la prueba t:
#   - Si no se rechaza H0 de la prueba F  -> t con varianzas homogéneas (var.equal = TRUE)   -> 3A
#   - Si se rechaza H0 de la prueba F     -> t con varianzas heterogéneas (var.equal = FALSE) -> 3B
# Con muestras apareadas no corresponde la prueba F: se trabaja sobre las diferencias        -> 3C


# ─────────────────────────────────────────────────────────────────────────────
# 3B - MUESTRAS INDEPENDIENTES CON VARIANZAS HETEROGÉNEAS
# ─────────────────────────────────────────────────────────────────────────────

# EJEMPLO 4 - PRUEBA t DE STUDENT PARA DOS MUESTRAS INDEPENDIENTES CON VARIANZAS IGUALES
# ─────────────────────────────────────────────────────────────────────────────

# #############################################################################
# SECCIÓN 4 - PRUEBA t DE STUDENT PARA DOS MUESTRAS INDEPENDIENTES
#             CON VARIANZAS HETEROGÉNEAS (CORRECCIÓN DE WELCH)
# #############################################################################

# ─────────────────────────────────────────────────────────────────────────────
# EJEMPLO 1 - ACTIVIDAD GUIADA
# ─────────────────────────────────────────────────────────────────────────────
# Trabajo: "Calidad de agua en ambientes periglaciares comparación: entre áreas
# con y sin actividad minera"
# Integrantes: Grupo 6
# (Datos simulados con fines didácticos)

# Contexto: Calidad de agua en arroyos periglaciares con y sin influencia de
# actividad minera. Se midió la turbidez del agua mediante la profundidad de
# visibilidad con disco de Secchi (cm) en dos arroyos periglaciares: una cuenca
# con influencia minera y una cuenca de referencia (reserva natural).
#
# IMPORTANTE: En esta sección se simulan datos con las MISMAS medias que en la
# Sección 3, pero con DISTINTA VARIANZA entre sitios, de modo que la prueba F
# de Snedecor rechace la homogeneidad de varianzas y sea necesario aplicar la
# prueba t de Student con varianzas heterogéneas (corrección de Welch,
# var.equal = FALSE).


# PASO 1 - DEFINICIÓN DE VARIABLE ALEATORIA Y PARÁMETROS (LEER)
# ─────────────────────────────────────────────────────────────────────────────
# Variable aleatoria (X): Profundidad de visibilidad con disco de Secchi,
# indicador de turbidez (en cm). Variable cuantitativa continua.
# Factor en estudio: Sitio (cuenca con influencia minera vs. cuenca de referencia).
# Parámetros poblacionales (µ1, µ2): Media verdadera de la profundidad de Secchi
# en la cuenca minera (µ1) y en la de referencia (µ2).
# Parámetros poblacionales (σ1², σ2²): Varianza verdadera de la profundidad de
# Secchi en la cuenca minera (σ1²) y en la de referencia (σ2²). En esta sección
# se asume σ1² ≠ σ2².
# Estimadores puntuales (x̄1, x̄2): Medias muestrales de cada sitio.
# Estimadores puntuales (s1², s2²): Varianzas muestrales de cada sitio.
# Parámetro de comparación: diferencia de medias poblacionales (µ1 - µ2).


# PASO 2 - CARGA DE DATOS EN OBJETOS DE R (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
# Profundidad de Secchi (cm) - datos simulados con medias similares a la
# Sección 3 pero con varianzas marcadamente distintas entre sitios:

turbidez_control = c(118, 121, 119, 122, 120, 123, 117, 124, 120, 121)
turbidez_minera  = c(34, 138,  88,  41,  52, 107,  66, 140,  89, 128)

n1 = length(turbidez_minera);  var1 = var(turbidez_minera);  media1 = mean(turbidez_minera)
n2 = length(turbidez_control); var2 = var(turbidez_control); media2 = mean(turbidez_control)



# PASO 3 - VERIFICACIÓN DE CONDICIONES DE APLICABILIDAD (RESPONDER/CALCULAR)
# ─────────────────────────────────────────────────────────────────────────────
# 1. ¿Las dos muestras deben ser independientes entre sí? ¿Y las observaciones
#    dentro de cada sitio?

# RESPUESTA: ->


# 2. ¿Los datos de ambos sitios provienen de distribuciones aproximadamente
#    Normales? (La prueba t es robusta, pero conviene verificarlo)

# ACTIVIDAD: realice la prueba de normalidad de los datos de ambos grupos y responda.

# Prueba ...
shapiro_control = shapiro.test(turbidez_control)
shapiro_minera  = shapiro.test(turbidez_minera)
print(shapiro_control)
print(shapiro_minera)

# RESPUESTA (Conclusión de la prueba): ->


# 3. Verificación de homogeneidad de varianzas (prueba F de Snedecor)
# ─────────────────────────────────────────────────────────────────────────────
# Se aplica la prueba F para decidir si corresponde usar var.equal = TRUE
# (homogéneas) o var.equal = FALSE (heterogéneas, corrección de Welch).

prueba_F = var.test(turbidez_minera, turbidez_control, alternative = "two.sided")
print(prueba_F)

p_valor_F = prueba_F$p.value

if (p_valor_F < 0.05) {
  print("Decisión: Se rechaza H0 (varianzas heterogéneas) -> usar var.equal = FALSE")
} else {
  print("Decisión: No se rechaza H0 (varianzas homogéneas) -> usar var.equal = TRUE")
}


# PASO 4 - PLANTEO DE DOCIMASIA DE HIPÓTESIS (LEER Y RESPONDER DONDE CORRESPONDA)
# ─────────────────────────────────────────────────────────────────────────────
# 1) Hipótesis
# H0: µ1 - µ2 = 0  (La profundidad media de visibilidad es la misma en ambos sitios)
# H1: µ1 - µ2 ≠ 0  (La profundidad media de visibilidad difiere entre sitios)

# ¿Por qué se plantea una H1 por 'distinto'? ¿Se trata de una prueba uni o bilateral?

# RESPUESTA: ->


# 2) Nivel de significación
alfa = 0.05


# 3) Estadístico de prueba
# En el caso de varianzas heterogéneas se utiliza el estadístico t de Welch,
# que ajusta los grados de libertad mediante la fórmula de Welch-Satterthwaite.
# Fórmula del estadístico: t_calc = (x̄1 - x̄2) / sqrt(s1²/n1 + s2²/n2)


# 4) Regla de decisión

# Se rechaza H0 si y solo si p-valor ...
# RESPUESTA: ->


# 5) Cálculos
# ─────────────────────────────────────────────────────────────────────────────
# Con la función t.test() especificando var.equal = FALSE

alternativa = "two.sided"        # Elegir entre 'less', 'two.sided' y "greater"
prueba_t = t.test(turbidez_minera, turbidez_control,
                  alternative = alternativa,
                  var.equal = FALSE,
                  conf.level = 1 - alfa)
print(prueba_t)
p_valor = prueba_t$p.value


# 6) Decisión
# ─────────────────────────────────────────────────────────────────────────────
if (p_valor < alfa) {
  print("Decisión: Se rechaza la hipótesis nula (H0) al nivel de significación α")
} else {
  print("Decisión: No se rechaza la hipótesis nula (H0) al nivel de significación α")
}


# 7) Conclusión

# RESPUESTA: ->


# INTERVALO DE CONFIANZA PARA LA DIFERENCIA DE MEDIAS (95%)
# ─────────────────────────────────────────────────────────────────────────────
prueba_t = t.test(turbidez_minera, turbidez_control,
                  alternative = "two.sided",
                  var.equal = FALSE,
                  conf.level = 1 - alfa)
print(prueba_t$conf.int)

# Interpretación/lectura (¿el intervalo contiene el valor 0?)

# RESPUESTA: ->


# PASO 5 - INTERPRETACIÓN FINAL EN EL CONTEXTO BIOLÓGICO
# ─────────────────────────────────────────────────────────────────────────────

# RESPUESTA: ->


# =============================================================================
# NOTA DIDÁCTICA:
# ─────────────────────────────────────────────────────────────────────────────
# La diferencia entre la Sección 3 y la Sección 4 radica únicamente en la
# verificación previa de homogeneidad de varianzas mediante la prueba F.
#
#   - Si varianzas HOMOGÉNEAS  -> t.test(..., var.equal = TRUE)   (Sección 3)
#   - Si varianzas HETEROGÉNEAS -> t.test(..., var.equal = FALSE)  (Sección 4)
#
# En ambos casos la hipótesis nula y la interpretación biológica son las mismas;
# lo que cambia es el estadístico de prueba y los grados de libertad efectivos,
# que en el caso de Welch ya no son un número entero.
# =============================================================================


##############################################################################
# SECCIÓN 5 - PRUEBAS t PARA MUESTRAS APAREADAS (NO INDEPENDIENTES)
# #############################################################################

# ─────────────────────────────────────────────────────────────────────────────
# EJEMPLO 5 - ACTIVIDAD GUIADA: Prueba t para muestras apareadas
# Efecto de un inoculante micorrícico en la masa radicular
# ─────────────────────────────────────────────────────────────────────────────
# Contexto: Evaluación del efecto de un inoculante con micorrizas sobre el volumen radicular de plantines de tomate.
# Situación: Se midió el volumen radicular (cm³) en plantines de tomate en un diseño de pares formados por similitud de tamaño inicial.
# Un individuo de cada par recibió inoculante y el otro actuó como control (6 pares). Se desea comprobar si el inoculante incrementa
# el volumen radicular, con un nivel de significación del 1%.


# PASO 1 - DEFINICIÓN DE VARIABLE ALEATORIA Y PARÁMETROS (LEER)
# ─────────────────────────────────────────────────────────────────────────────
# Variable aleatoria (X): Volumen radicular de plantines de tomate (en cm³). Variable cuantitativa continua.
# Factor en estudio: Aplicación de inoculante micorrícico (control vs. inoculado).
# Diseño: Pares de plantines formados por similitud de tamaño inicial; un integrante de cada par recibe el inoculante y el otro es el control.
# Variable de análisis (d): Diferencia dentro de cada par, d = x2 - x1 (Inoculado - Control).
# Parámetro poblacional (µ_d): Diferencia media verdadera del volumen radicular entre plantines inoculados y control (en cm³).
# Estimador puntual (d̄): Media muestral de las diferencias.
# Desviación estándar poblacional de las diferencias (σ_d): Desconocida. Se estima mediante la desviación estándar muestral de las diferencias (s_d).


# PASO 2 - CARGA DE DATOS EN OBJETOS DE R (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
# Volumen radicular (cm³):
x1 = c(12.3, 14.1, 11.5, 13.0, 15.2, 12.8)   # Control
x2 = c(15.0, 16.8, 13.9, 15.5, 18.0, 15.1)   # Inoculado

# Diferencias dentro de cada par (Inoculado - Control)
d = x2 - x1

n_pares = length(d)
media_d = mean(d)
sd_d = sd(d)
se_d = sd_d / sqrt(n_pares) # Error estándar de la media de las diferencias


# PASO 3 - VERIFICACIÓN DE CONDICIONES DE APLICABILIDAD (RESPONDER/CALCULAR)
# ─────────────────────────────────────────────────────────────────────────────
# 1. ¿Las observaciones del grupo control y del grupo inoculado son independientes entre sí?
# ¿Por qué este diseño requiere una prueba t de Student para muestras apareadas?

# RESPUESTA: ->

# 2. ¿Las DIFERENCIAS (d) provienen de una distribución aproximadamente Normal en la población?
# (Con muestras apareadas la normalidad se verifica sobre las diferencias, no sobre cada grupo)

# ACTIVIDAD: realice la prueba de normalidad de las diferencias (Shapiro-Wilk y/o QQ-plot) y responda.
# Prueba ...


# RESPUESTA (Conclusión de la prueba): ->

# 3. Dado que la varianza poblacional de las diferencias σ_d² es desconocida y el número de pares es pequeño (n = 6 < 30),
# el estadístico adecuado sigue una distribución t de Student con (n - 1) grados de libertad, siendo n el número de pares (LEER)


# PASO 4 - PLANTEO DE DOCIMASIA DE HIPÓTESIS (LEER Y RESPONDER DONDE CORRESPONDA)
# ─────────────────────────────────────────────────────────────────────────────
# 1) Hipótesis
# H0: µ_d = 0  (El inoculante no incrementa el volumen radicular)
# H1: µ_d > 0  (El inoculante incrementa el volumen radicular: µ_Inoculado - µ_Control > 0)

# ¿Por qué se plantea una H1 por 'mayor'? ¿Se trata de una prueba uni o bilateral? ¿De qué cola?

# RESPUESTA: ->

# 2) Nivel de significación
alfa = 0.01       # (Nivel de significación del 1%)

# 3) Estadístico de prueba
# CÁLCULO MANUAL DEL ESTADÍSTICO DE PRUEBA T
# ─────────────────────────────────────────────────────────────────────────────
# Fórmula del estadístico de prueba: tc = (d̄ - 0) / (s_d / √n)
tc = media_d / (sd_d / sqrt(n_pares))

# 4) Regla de decisión

# Se rechaza H0 si y solo si p-valor ...
# RESPUESTA: ->

# 5) Cálculos
# ─────────────────────────────────────────────────────────────────────────────
# Con la función t.test()

alternativa = "greater"        # Elegir entre 'less', 'two.sided' y "greater", según sea una prueba unilateral izquierda, bilateral o unilateral derecha respectivamente
prueba_t = t.test(x2, x1, paired = TRUE, alternative = alternativa, conf.level = 1 - alfa) # Realizamos la prueba apareada y la almacenamos con el nombre prueba_t
print(prueba_t)    # Visualizamos la salida de la prueba t
p_valor = prueba_t$p.value   # Almacenamos el p-valor de la salida de la prueba_t, con el nombre p_valor
print(p_valor)

# 6) Decisión
# ─────────────────────────────────────────────────────────────────────────────
if (p_valor < alfa) {
  print("Decisión: Se rechaza la hipótesis nula (H0) al nivel de significación α")
} else {
  print("Decisión: No se rechaza la hipótesis nula (H0) al nivel de significación α")
}

# 7) Conclusión

# RESPUESTA: ->

# INTERVALO DE CONFIANZA UNILATERAL (99%) PARA LA DIFERENCIA MEDIA
# ─────────────────────────────────────────────────────────────────────────────
# Con alternative = "greater", el intervalo es una cota inferior: [límite inferior; Inf)
prueba_t = t.test(x2, x1, paired = TRUE, alternative = "two.sided", conf.level = 1 - alfa) # Ahora planteamos la prueba bilateral.
print(prueba_t$conf.int)

# Interpretación/lectura

# RESPUESTA: ->

# PASO 5 - INTERPRETACIÓN FINAL EN EL CONTEXTO BIOLÓGICO
# ─────────────────────────────────────────────────────────────────────────────

# RESPUESTA: ->

# =============================================================================


# *****************************************************************************
# *****************************************************************************
#                    PARTE B - EJERCICIOS DE APLICACIÓN
# *****************************************************************************
# *****************************************************************************
# Identifique en cada caso la prueba estadística adecuada.


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 1 - PESO DE 100 SEMILLAS DE QUINUA (Chenopodium quinoa)
# ─────────────────────────────────────────────────────────────────────────────
#
# Se evalúa la calidad fisiológica de un ecotipo de quinua (Chenopodium quinoa) cultivado en la Puna jujeña. El peso estándar promedio de referencia para 100 semillas de esta variedad es µ₀ = 3.20 g. Un investigador toma una muestra aleatoria de 12 lotes de 100 semillas.
#
# Peso de 100 semillas de quinua en gramos (g):
peso_quinua = c(3.12, 3.08, 3.15, 3.05, 3.10, 3.14, 3.07, 3.09, 3.11, 3.06, 3.13, 3.08)
#
# a) Identifique la variable aleatoria bajo estudio, su tipo y el parámetro de interés.
# b) Plantee las hipótesis nula (H0) y alternativa (H₁) para determinar si el peso medio difiere del estándar de 3.20 g a un nivel α = 0.05.
# c) Calcule manualmente la media muestral, la desviación estándar y el estadístico de prueba t.
# d) Determine el p-valor, tome una decisión estadística y construya el intervalo de confianza bilateral del 95%.
# e) Verifique sus resultados utilizando la función t.test() de R e interprete la conclusión en el contexto agronómico.

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 2 - Comparación de variabilidad en el peso de frutos de pimiento
# ─────────────────────────────────────────────────────────────────────────────
# Un horticultor desea saber si dos modalidades de fertirriego (Sistemas A y B)
# producen diferencias en la uniformidad (variabilidad) del peso de los frutos.
#
# a) Formule las hipótesis correspondientes únicamente para evaluar la variabilidad (varianzas).
# b) Defina la variable respuesta y el factor en estudio.
# c) Aplique la prueba F de Snedecor en R con α = 0.05.
# d) Responda si uno de los sistemas produce frutos de peso más uniforme que el otro.
#
# Datos de peso de fruto (g):
x1 = c(185, 190, 188, 182, 187, 189)   # Sistema A
x2 = c(170, 205, 165, 210, 180, 195)   # Sistema B

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 3 - Efecto de un inoculante micorrícico en la masa radicular (Apareado)
# ─────────────────────────────────────────────────────────────────────────────
# Para evaluar el efecto de un inoculante con micorrizas, se midió el volumen
# radicular (cm³) en plantines de tomate en un diseño de pares formados por
# similitud de tamaño inicial. Un individuo de cada par recibió inoculante y el otro actuó como control.
#
# a) ¿Por qué este diseño requiere una prueba t de Student para muestras apareadas?
# b) Plantee H0 y H₁ para evaluar si el inoculante incrementa el volumen radicular.
# c) Ejecute la prueba t apareada en R usando alfa = 0.01.
# d) Redacte la conclusión biológica en función del p-valor hallado.
#
# Datos de volumen radicular (cm³):
x1 = c(12.3, 14.1, 11.5, 13.0, 15.2, 12.8)   # Control
x2 = c(15.0, 16.8, 13.9, 15.5, 18.0, 15.1)   # Inoculado

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 4 - Comparación de la capacidad de germinación en dos lotes de semilla
# ─────────────────────────────────────────────────────────────────────────────
# Un laboratorio de análisis de semillas evalúa el vigor germinativo (%) de dos
# lotes de semillas de maíz (Lote 1 y Lote 2) almacenados bajo distintas condiciones.
#
# a) Identifique las variables, los objetivos y plantee las hipótesis H0 y H₁.
# b) Verifique la homogeneidad de varianzas entre ambos lotes con la prueba F (α = 0.05).
# c) Realice la prueba t de Student adecuada según el resultado de las varianzas.
# d) Interprete el p-valor obtenido y elabore una conclusión técnico-agronómica.
#
# Datos de porcentaje de germinación (%):
x1 = c(88, 92, 85, 89, 90, 87)   # Lote 1
x2 = c(78, 82, 80, 75, 84, 79)   # Lote 2

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 5 - DIÁMETRO A LA ALTURA DEL PECHO EN ALGARROBO BLANCO (Prosopis alba)
# ─────────────────────────────────────────────────────────────────────────────
#
# En un proyecto de restauración ecológica en la cuenca del Río Grande, se mide el Diámetro a la Altura del Pecho (DAP) en una parcela de regeneración de algarrobo blanco (Prosopis alba). Se desea verificar si el DAP medio difiere del valor objetivo de desarrollo de 12.0 cm fijado para esa etapa fenológica.
#
# Diámetro a la Altura del Pecho (DAP) en centímetros (cm):
dap_algarrobo = c(11.2, 11.8, 12.1, 11.5, 11.9, 11.4, 11.7, 12.0, 11.3, 11.6, 11.8, 11.5, 12.2, 11.6)
#
# a) Defina la variable de interés, la unidad de medida y la hipótesis estadística a contrastar.
# b) Plantee la prueba t para una muestra con α = 0.05.
# c) Obtenga el estadístico t calculado, los grados de libertad y el p-valor correspondiente.
# d) Construya e interprete el intervalo de confianza del 95% para el DAP promedio poblacional.
# e) Redacte una síntesis técnica dirigida al equipo de manejo forestal.

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 6 - Resistencia a la penetración del suelo con dos tipos de labranza
# ─────────────────────────────────────────────────────────────────────────────
# Se desea comprobar si la labranza reducida genera una mayor resistencia a la
# penetración del suelo (MPa) a 20 cm de profundidad en comparación con la labranza
# convencional. Se tomaron muestras pequeñas independientes en parcelas contiguas.
#
# a) Defina el tipo de prueba (unilateral o bilateral) e identifique el parámetro a comparar.
# b) Formule H0 y H₁ considerando α = 0.05.
# c) Evalúe la hipótesis de igualdad de varianzas.
# d) Ejecute la prueba t para dos muestras independientes e indique la decisión final.
#
# Datos de resistencia a la penetración (MPa):
x1 = c(2.4, 2.7, 2.9, 2.5, 2.8)   # Labranza Reducida
x2 = c(1.8, 2.1, 1.9, 2.0, 1.7)   # Labranza Convencional

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 7 - CONCENTRACIÓN DE CLOROFILA TOTAL EN Nicotiana tabacum
# ─────────────────────────────────────────────────────────────────────────────
#
# Se evalúa el efecto del estrés hídrico moderado en plantas de tabaco (Nicotiana tabacum). En condiciones normales de riego, la concentración media histórica de clorofila total es de µ₀ = 45.0 µg/cm². Se mide la concentración foliar de clorofila en 10 plantas sometidas al tratamiento hídrico restringido.
#
# Concentración de clorofila total medida en microgramos por centímetro cuadrado (µg/cm²):
clorofila = c(41.2, 43.5, 39.8, 42.0, 40.5, 41.8, 42.2, 38.9, 40.1, 41.0)
#
# a) Indique la variable explicativa y la variable respuesta, detallando la escala de medición.
# b) Formule las hipótesis H0 y H₁ con notación matemática adecuada (µ₀ = 45.0 µg/cm²).
# c) Implemente en R el cálculo manual del estadístico tc, p-valor e intervalo de confianza del 95%.
# d) Aplique la función t.test() para contrastar los resultados obtenidos manualmente.
# e) ¿Qué efecto biológico sobre el aparato fotosintético sugiere la evidencia observada?

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 8 - TIEMPO DE METAMORFOSIS EN RENACUAJOS DE Gastrotheca christiani
# ─────────────────────────────────────────────────────────────────────────────
#
# En un estudio ecofisiológico en bosques de Yungas, se registra la duración en días de la fase larval de la rana marsupial (Gastrotheca christiani). El tiempo medio de metamorfosis reportado como patrón de referencia es µ₀ = 60 días. Se analizan 15 individuos criado en condiciones de microhábitat controlado.
#
# Tiempo de metamorfosis registrado en días (días):
dias_metamorfosis = c(56, 58, 55, 57, 59, 54, 58, 56, 57, 55, 60, 56, 57, 58, 55)
#
# a) Identifique la unidad experimental, la variable medida y el tamaño de muestra (n).
# b) Establezca las hipótesis estadísticas bilaterales a un nivel de significación del 5% (α = 0.05).
# c) Calcule el estadístico de prueba t y determine si cae en la región de rechazo.
# d) Construya el intervalo de confianza bilateral al 95% para la media del tiempo de metamorfosis.
# e) Elabore la conclusión biológica vinculando el resultado con las condiciones del microhábitat.

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 9 - CONCENTRACIÓN DE FLAVONOIDES TOTALES EN Polylepis tomentella
# ─────────────────────────────────────────────────────────────────────────────
#
# Se analiza el contenido de metabolitos secundarios (flavonoides totales) en extractos foliares de queñoa (Polylepis tomentella) provenientes de la Prepuna. El estándar de calidad para extractos medicinales requiere un nivel medio de µ₀ = 18.50 mg/g de tejido seco. Se analiza una muestra de 11 arbustos.
#
# Concentración de flavonoides en miligramos por gramo de tejido seco (mg/g):
flavonoides = c(16.8, 17.2, 16.5, 17.0, 16.9, 17.4, 16.6, 17.1, 16.7, 17.3, 16.8)
#
# a) Plantee las hipótesis nula y alternativa para el contenido medio de flavonoides a α = 0.05.
# b) Compute la media muestral, el desvío estándar muestral y el error estándar de la media.
# c) Calcule el valor del estadístico t, los grados de libertad y el p-valor bilateral.
# d) Indique si la concentración media observada satisface el estándar de calidad requerido (µ₀ = 18.50 mg/g).
# e) Ejecute la verificación mediante t.test() en R y redacte la síntesis final del problema.

# Su código aquí:

# =============================================================================