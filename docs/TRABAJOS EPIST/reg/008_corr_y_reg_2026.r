# =============================================================================
# Bioestadística - Facultad de Ciencias Agrarias - UNJu - Licenciatura en Ciencias Biológicas - AÑO 2026
# Trabajo Práctico 8: Asociación entre Variables (Correlación y Regresión Lineal Simple)
# Ejercicios de práctica de clase
# Jefe de Trabajos Prácticos: Mg. Ing. Juan Manuel Solís; Ayudante 2da: Sr. Daniel Vilca
# =============================================================================
#
# ORDEN DE LAS ACTIVIDADES
# ------------------------
#   PARTE A - ACTIVIDAD GUIADA (Correlación y Regresión Lineal Simple)
#   PARTE B - EJERCICIOS DE APLICACIÓN (Ejercicios 1 a 5)
#
# Instrucciones generales
# -----------------------
# En cada ejemplo y ejercicio:
#   (a) identifique la pregunta de investigación, los objetivos, las variables y el tipo de variable
#   (b) realice un gráfico de dispersión inicial para explorar la relación e interpretar
#   (c) calcule r como estimador de rho e interprete su magnitud y sentido
#   (d) plantee las hipótesis nula (H0) y alternativa (H1), defina alfa y realice la prueba t de significación de r
#   (e) evalúe la conveniencia de proponer un modelo de regresión lineal simple a partir del r estimado
#   (f) ajuste el modelo de regresión lineal simple (y = beta0 + beta1 * x), interprete coeficientes, R2 y prediga
#   (g) elabore una conclusión o SÍNTESIS integrada en el contexto del problema
#### ---------------------------------------------------------------------------------------------------#######
#   NOTA IMPORTANTE 1: ALGUNOS CÁLCULOS Y FUNCIONES SON PROVISTAS, OTRAS LAS TENDRÁ QUE HACER USTED
#   NOTA IMPORTANTE 2: algunas secuencias implican solo lectura; en otras implica algún cálculo y decisiones
#### ---------------------------------------------------------------------------------------------------#######
#
# Funciones disponibles (R base):
#
#   c(...)            # combina elementos en un vector
#   length(...)       # devuelve el número de observaciones (tamaño muestral n)
#   mean(...)         # calcula la media aritmética muestral
#   sd(...)           # calcula la desviación estándar muestral
#   var(...)          # calcula la varianza muestral
#   sqrt(...)         # calcula la raíz cuadrada de un número
#   abs(...)          # calcula el valor absoluto
#   plot(x, y, ...)   # genera un gráfico de dispersión entre dos variables cuantitativas
#   cor(x, y)         # calcula el coeficiente de correlación lineal de Pearson (r)
#   cor.test(x, y, alternative = ...) # prueba t de significación para el coeficiente de correlación (rho)
#   lm(y ~ x)         # ajusta un modelo de regresión lineal simple por mínimos cuadrados
#   summary(...)      # muestra el resumen detallado del modelo ajustado (coeficientes, R2, p-valores)
#   abline(...)       # superpone la recta de regresión ajustada sobre el gráfico de dispersión
#   fitted(...)       # obtiene los valores ajustados (predichos por el modelo) para los datos observados
#   predict(...)      # predice el valor de y para nuevos valores de x
#
# Estadísticos "manuales" (si prefiere cálculos manuales; recomendamos usar funciones específicas de base en R):
#   Coeficiente de correlación:
#     r = sum((x - mean(x)) * (y - mean(y))) / sqrt(sum((x - mean(x))^2) * sum((y - mean(y))^2))
#   Estadístico t para correlación:
#     tc = r * sqrt(n - 2) / sqrt(1 - r^2)
#   Coeficientes de regresión:
#     b1 = sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
#     b0 = mean(y) - b1 * mean(x)

# NOTA IMPORTANTE: recuerde que en los ejemplos se utilizaron valores simulados y deben interpretarse con fines
# didácticos, y no como evidencia científica.


# *****************************************************************************
# *****************************************************************************
#                      PARTE A - ACTIVIDAD GUIADA
# *****************************************************************************
# *****************************************************************************


# #############################################################################
# SECCIÓN ÚNICA - ANÁLISIS DE CORRELACIÓN Y REGRESIÓN LINEAL SIMPLE
# #############################################################################

# ─────────────────────────────────────────────────────────────────────────────
# EJEMPLO 1 - ACTIVIDAD GUIADA
# ─────────────────────────────────────────────────────────────────────────────
# Contexto: Evaluación del crecimiento inicial de la plántula de poroto (Phaseolus vulgaris) bajo estrés salino por NaCl.
# Situación: Se midió la longitud de la radícula (cm) en plántulas de poroto expuestas a concentraciones crecientes de NaCl (0 a 120 mM) en ensayo controlado.
# Referencia: GRUPO 14 - ¿Cómo afectan distintas concentraciones NaCl, en la germinación y el crecimiento inicial de la plántula de poroto (Phaseolus vulgaris)?
# (Datos simulados con fines didácticos)


# PASO 1 - DEFINICIÓN DE PREGUNTA, OBJETIVOS Y VARIABLES (LEER)
# ─────────────────────────────────────────────────────────────────────────────
# Pregunta de investigación: ¿Existe una relación lineal negativa entre la concentración de NaCl y la longitud de la radícula en plántulas de poroto? ¿Es posible predecir el crecimiento radicular a partir de la concentración salina?
# Objetivo: Evaluar la asociación lineal entre la concentración de NaCl (mM) y la longitud de la radícula (cm), y ajustar un modelo de regresión lineal predictivo.
# Variable predictora (x): Concentración de salinidad por NaCl (en mM). Variable cuantitativa continua.
# Variable respuesta (y): Longitud de la radícula de Phaseolus vulgaris (en cm). Variable cuantitativa continua.
# Parámetros poblacionales de interés: Coeficiente de correlación poblacional (ρ), pendiente (β1) e intercepto (β0).


# PASO 2 - CARGA DE DATOS EN OBJETOS DE R Y EXPLORACIÓN GRÁFICA (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
# Concentración de NaCl (mM):
x = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120)

# Longitud de la radícula de plántulas de poroto (cm):
y = c(7.8, 6.4, 7.4, 5.6, 6.7, 4.9, 5.6, 3.9, 4.9, 3.5, 4.5, 3.2, 3.2)

n = length(x)

# Gráfico de dispersión inicial para explorar la relación:
plot(x, y, main = "Diagrama de Dispersión: NaCl vs Longitud de Radícula",
     xlab = "Concentración de NaCl (mM)", ylab = "Longitud de Radícula (cm)")

# INTERPRETACIÓN DEL GRÁFICO INICIAL:

# # Respuesta: ...

# PASO 3 - ESTIMACIÓN DEL COEFICIENTE DE CORRELACIÓN DE PEARSON (r) (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
r = cor(x, y)
round(r,2)

# Interpretación de r: 

# Respuesta: ...

# PASO 4 - PRUEBA DE SIGNIFICACIÓN PARA EL COEFICIENTE DE CORRELACIÓN (ρ) (RESPONDER/EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────

# 1) Planteo de hipótesis:
# H₀: ρ = 0  (No existe correlación lineal entre la concentración de NaCl y la longitud de la radícula)
# H₁: ρ < 0  (Existe una correlación lineal negativa significativa)

# 12) Configuración de parámetros de la prueba:
alfa = 0.05

#  Especificación del tipo_prueba:
#    "unilateral izquierda" -> alternative = "less"
#    "unilateral derecha"   -> alternative = "greater"
#    "bilateral"            -> alternative = "two.sided"

alternativa = "less" # ¿Por qué?

# Respuesta: ...

# 3) Definición del estadístico de prueba: 

# Respuesta: ...

# 4) Regla de decisión:

# Se rechaza H0 si y solo si ...

# Respuesta: ...

# 5) Cálculos:
prueba_r = cor.test(x, y, alternative = alternativa, conf.level = 1 - alfa)
print(prueba_r)
p_valor_r = prueba_r$p.value; p_valor_r

# 6 y 7) Decisión y conclusión

# Respuesta: ...

# Evaluación de adecuación del modelo de regresión:
# Dado que existe una relación altamente significativa, 
# es adecuado proponer un modelo de regresión lineal simple.


# PASO 5 - AJUSTE DEL MODELO DE REGRESIÓN LINEAL SIMPLE (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
# Modelo lineal: y = β₀ + β₁ * x + ε
modelo = lm(y ~ x)

# Análisis diagnóstico

shapiro.test(resid(modelo))
plot(modelo, which = 1)
plot(modelo, which = 2)

# Interpretación del modelo
resumen_m = summary(modelo)
print(resumen_m)

b0 = coef(modelo)[1]    # Ordenada al origen (intercepto)
b1 = coef(modelo)[2]    # Pendiente
r2 = sigma(modelo)^2


# Interpretación de coeficientes:

# Respuesta: ...

# PASO 6 - REHACER EL GRÁFICO DE DISPERSIÓN INCLUYENDO LA RECTA DE AJUSTE (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
plot(x, y, main = "Ajuste de Regresión Lineal: NaCl vs Longitud de Radícula",
     xlab = "Concentración de NaCl (mM)", ylab = "Longitud de Radícula (cm)")
abline(modelo, col = "red", lwd = 2)


# PASO 7 - ESTIMACIÓN DE VALORES AJUSTADOS Y PREDICCIONES (EJECUTAR)
# ─────────────────────────────────────────────────────────────────────────────
# Valores ajustados para la muestra:
y_ajustados = fitted(modelo)
print(y_ajustados)

# Los valores ajustados representan ...

# Respuesta: ...

# Predicción para concentraciones de NaCl no medidas directamente (ej. 25, 55, 75 mM):
nuevas_concentraciones = data.frame(x = c(25, 55, 75)) # Se debe construir un "data.frame" con los valores de x para los cuales se quiere predecir y
predicciones = predict(modelo, newdata = nuevas_concentraciones)
print(data.frame(NaCl_mM = nuevas_concentraciones$x, 
  Radicula_Predicha_cm = round(predicciones, 2)))


# PASO 8 - CONCLUSIÓN Y SÍNTESIS EN EL CONTEXTO BIOLÓGICO (LEER Y COMPLETAR)
# ─────────────────────────────────────────────────────────────────────────────

#Respuesta: ...

# =============================================================================


# *****************************************************************************
# *****************************************************************************
#                    PARTE B - EJERCICIOS DE APLICACIÓN
# *****************************************************************************
# *****************************************************************************


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 1 - LIBERACIÓN DE NITRÓGENO Y FÓSFORO SEGÚN LA PROFUNDIDAD EN EMBALSES
# ─────────────────────────────────────────────────────────────────────────────
#
# Contexto: En un estudio sobre dinámica de nutrientes en embalses de agua dulce, se evalúa la relación entre la profundidad del punto de muestreo (m) y la concentración de fósforo total (mg/L) presente en la columna de agua.
# Referencia: GRUPO 4 - Liberación de Nitrógeno y Fósforo por el sedimento de los embalses.
# (Datos simulados con fines didácticos)
#
# Profundidad de muestreo (m):
profundidad = c(1, 3, 5, 8, 10, 12, 15, 18, 20, 22, 25)

# Concentración de Fósforo (mg/L):
fosfatos = c(0.012, 0.018, 0.022, 0.035, 0.041, 0.050, 0.068, 0.082, 0.095, 0.108, 0.125)

# a) Identifique la pregunta de investigación, los objetivos, la variable respuesta y la variable predictora.
# b) Genere un gráfico de dispersión e interprete la relación observada entre la profundidad y la concentración de fósforo.
# c) Calcule el coeficiente de correlación lineal de Pearson (r) e interprete su signo y magnitud.
# d) Plantee las hipótesis H0 y H1 para evaluar si la correlación es positiva significativa (α = 0.05). Ejecute cor.test().
# e) Justifique si corresponde ajustar un modelo de regresión lineal simple. De ser así, obtenga la recta de ajuste y R2.
# f) Rehaga el gráfico de dispersión agregando la recta ajustada. Prediga la concentración de fósforo a 14 m de profundidad.

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 2 - RELACIÓN ENTRE TURBIDEZ Y pH EN AMBIENTES PERIGLACIARES
# ─────────────────────────────────────────────────────────────────────────────
#
# Contexto: En un monitoreo de calidad de agua en arroyos de alta montaña, se investiga si existe relación entre la profundidad de visibilidad del Disco de Secchi (cm, indicador inverso de turbidez) y el pH del agua en cuencas con influencia minera.
# Referencia: GRUPO 6 - Calidad de agua en ambientes periglaciares: comparación entre áreas con y sin actividad minera.
# (Datos simulados con fines didácticos)
#
# Profundidad de Disco de Secchi (cm):
secchi = c(45, 50, 60, 65, 75, 80, 90, 95, 105, 110, 120)

# pH del agua:
ph = c(5.2, 5.4, 5.8, 6.0, 6.3, 6.5, 6.8, 7.0, 7.1, 7.3, 7.5)

# a) Defina la variable dependiente (y) y la variable independiente (x) en este ensayo ecotoxicológico.
# b) Realice el diagrama de dispersión inicial y estime el coeficiente de correlación r.
# c) Plantee H0 y H1 para comprobar si la correlación lineal es distinta de cero (prueba bilateral, α = 0.05).
# d) Ajuste el modelo de regresión lineal simple y determine la recta estimada y el coeficiente R2.
# e) Grafique los puntos junto con la línea de regresión y estime el pH esperado para una transparencia de Secchi de 85 cm.

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 3 - CRECIMIENTO DE POROTO EN REGOLITO MARCIANO SIMULADO
# ─────────────────────────────────────────────────────────────────────────────
#
# Contexto: En un ensayo de exobiología agrícola, se analiza cómo se relaciona la proporción de regolito marciano simulado (%) mezclado con sustrato orgánico y la altura final del tallo (cm) en Phaseolus vulgaris en estadio fenológico V4.
# Referencia: GRUPO 9 - Evaluación del crecimiento vegetativo de Phaseolus vulgaris en regolito marciano simulado.
# (Datos simulados con fines didácticos)
#
# Proporción de regolito marciano (%):
regolito = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

# Altura del tallo a los 30 días (cm):
altura_tallo = c(28.5, 27.2, 25.8, 24.1, 22.0, 19.8, 17.5, 15.2, 12.9, 10.1, 7.5)

# a) Reconozca la variable predictora, la variable respuesta y el objetivo del ensayo.
# b) Elabore el gráfico de dispersión inicial y calcule el coeficiente de correlación r.
# c) Efectúe la prueba t para r con hipótesis alternativa unilateral izquierda (α = 0.05).
# d) Calcule e interprete la pendiente (b1), la ordenada al origen (b0) y el coeficiente de determinación (R2).
# e) Rehaga el gráfico con la recta de ajuste y calcule la altura estimada para un sustrato con 45% de regolito.

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 4 - RESISTENCIA BACTERIANA: DOSIS DE AMOXICILINA Y HALO DE INHIBICIÓN
# ─────────────────────────────────────────────────────────────────────────────
#
# Contexto: Se evalúa la respuesta de cepas de Streptococcus pyogenes frente a concentraciones crecientes de Amoxicilina (µg/mL) en ensayos de antibiograma, registrando el diámetro del halo de inhibición (mm).
# Referencia: GRUPO 11 - Análisis de la resistencia bacteriana de Streptococcus pyogenes por administración de Amoxicilina.
# (Datos simulados con fines didácticos)
#
# Concentración de Amoxicilina (µg/mL):
dosis = c(2, 4, 8, 16, 32, 64, 128, 256)

# Diámetro del halo de inhibición (mm):
halo = c(12.5, 15.0, 18.2, 21.5, 24.8, 27.2, 30.1, 33.0)

# a) Identifique las variables y explicite el tipo de escala de medición de cada una.
# b) Represente gráficamente los datos mediante un diagrama de dispersión.
# c) Determine la fuerza y sentido de la asociación mediante el coeficiente r de Pearson.
# d) Plantee las hipótesis estadísticas bilaterales (α = 0.01) y decida sobre la significación de la correlación.
# e) Ajuste el modelo lineal por lm(), reporte el R2 e interprete la pendiente del modelo en el contexto microbiológico.

# Su código aquí:

# ─────────────────────────────────────────────────────────────────────────────


# ─────────────────────────────────────────────────────────────────────────────
# EJERCICIO 5 - TEMPERATURA MEDIA Y GERMINACIÓN EN ECOTIPOS DE QUINUA
# ─────────────────────────────────────────────────────────────────────────────
#
# Contexto: En un estudio ecofisiológico en la Puna jujeña, se analiza la relación entre la temperatura media de incubación (°C) y el tiempo necesario para alcanzar el 50% de germinación (horas) en semillas de quinua (Chenopodium quinoa).
# (Datos simulados con fines didácticos)
#
# Temperatura de incubación (°C):
temp = c(8, 10, 12, 15, 18, 20, 22, 25, 28, 30)

# Tiempo para alcanzar T50 de germinación (horas):
t50_horas = c(96, 82, 70, 55, 42, 35, 28, 22, 18, 15)

# a) Defina la variable independiente (x) y la variable dependiente (y).
# b) Construya el diagrama de dispersión inicial e interprete la tendencia de los puntos.
# c) Obtenga el coeficiente de correlación de Pearson (r) y realice la prueba t de significación unilateral izquierda (α = 0.05).
# d) Estime la ecuación de regresión lineal y el coeficiente R2.
# e) Superponga la recta de ajuste sobre el gráfico de dispersión y estime el T50 a una temperatura de 16 °C.

# Su código aquí:

# =============================================================================