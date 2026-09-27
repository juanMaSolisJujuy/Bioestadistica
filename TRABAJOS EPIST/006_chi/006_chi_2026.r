# =============================================================================
#  Bioestadística — Facultad de Ciencias Agrarias - UNJu — Licenciatura en Ciencias Biológicas - AÑO 2026
#  Trabajo Práctico 6: Distribución Chi Cuadrado
#  Ejercicios de práctica de clase
#  Jefe de Trabajos Prácticos: Ing. Juan Manuel Solís; Ayudante 2da: Sr. Daniel Vilca
# =============================================================================

#  Instrucción general
#  Para cada trabajo presentado durante la materia de Epistemología, se solicitará responder una
#  pregunta de investigación que suponga la realización de una prueba inferencial basada en chi cuadrado.
#  Debe identificar la variable aletoria, el nombre de la prueba estadística que debe aplicarse,
#  los supuestos distribuciones si corresponde, el estadístico de prueba y la estimación del parámetro
#  asociado.

# ============================================================================
# EJEMPLO 1
# Trabajo ¿Cómo afectan distintas concentraciones NaCl, en la germinación y el crecimiento inicial de la plántula de poroto (Phaseolus vulgaris)? 
# Barbero Celeste, Ruiz Loba Camila y Valeriano Mariel
# (Datos obtenidos del trabajo Responses of linseed genotypes to salinity stress during early seedling growth
# Hosna Kohinoor1*, M. Shalim Uddin1, N. A. Sultana1, Debi Rani Dutta1 and Abul Kashem Chowdhury)

# 1) Reconocer la pregunta de investigación, los objetivos del trabajo, y la o las variables analizadas.
# 2) Los siguientes son los promedios de la longitud radicular en cm de plántulas recién germinadas de
# diferentes genotipos de poroto, en condiciones de ausencia de salinidad.

raiz = c(
  4.25, 3.88, 4.40, 5.13, 5.18, 4.93, 3.68, 3.75, 4.48,
  4.13, 2.80, 4.26, 2.05, 2.42, 3.33, 2.52, 2.88, 2.59,
  2.31, 2.86, 3.20, 3.00, 3.83, 2.65, 3.57, 3.67, 4.06,
  4.10, 4.43
)

# a) ¿Se puede asumir que la población de longitudes radiculares de plántulas de diferentes genotipos de porotos es normal?
# b) ¿Se puede asumir que dicha población es homogénea? Realice una estimación de la variancia poblacional.


# ============================================================================
# EJEMPLO 2
# Trabajo: "Desarrollo embrionario de Rhinella arenarum en un ambiente urbano
# y en un ambiente natural"
# Nina Antonela Aylen, Catacata Isabela Alejandra, Mendoza Nina Itatí Guadalupe
# (Datos simulados con fines didácticos)

# 1) Reconocer la pregunta de investigación, los objetivos del trabajo, y la o las variables analizadas.
# 2) La siguiente es una tabla de contingencia con las frecuencias observadas
# de presencia/ausencia de variaciones morfológicas externas en embriones y
# larvas tempranas (estadios 1–25 de Gosner) de Rhinella arenarum, según el
# tipo de ambiente (urbano vs. natural).

# Ambiente urbano:  42 con variación / 78 sin variación
# Ambiente natural: 18 con variación / 102 sin variación

tabla = matrix(c(42, 78,18, 102),nrow = 2, byrow = TRUE)
colnames(tabla) <- c("Variación presente", "Variación ausente")
rownames(tabla) <- c("Ambiente urbano", "Ambiente natural")

tabla

# a) ¿Existe asociación entre el tipo de ambiente y la presencia de variaciones
#    morfológicas externas en el desarrollo temprano de R. arenarum?
# b) Estime las frecuencias esperadas y los grados de libertad.
# c) ¿En este ejercicio corresponde hacer una prueba de normalidad?
# d) ¿Corresponde realizar un ajuste por continuidad?

# ============================================================================
# EJEMPLO 3
# Trabajo: "Influencia del alumbrado LED sobre poblaciones de lepidópteros
# nocturnos del Parque Lineal Xibi-Xibi en San Salvador de Jujuy"
# Guzmán Lara Valeria, Liberatori Celeste, Salas Camila Victoria,
# Saavedra Mauricio Arturo A., Vargas Ana Melina Gianella
# (Datos simulados con fines didácticos - 2026)

# 1) Reconocer la pregunta de investigación, los objetivos del trabajo, y la o las variables analizadas.
# 2) La siguiente es una tabla de frecuencias observadas del número de polillas
# nocturnas capturadas por trampa de red en un intervalo fijo de tiempo, en
# 100 trampas distribuidas en sectores iluminados del Parque Lineal Xibi-Xibi.

# Número de polillas por trampa:  0    1    2    3    4
# Frecuencia observada (trampas): 40   32   18   7    3

polillas = c("0" = 40, "1" = 32, "2" = 18, "3" = 7, "4" = 3)
polillas

# a) ¿Se puede asumir que el número de polillas capturadas por trampa sigue
#    una distribución de Poisson con lambda = 1?
# b) Calcule las frecuencias esperadas bajo el modelo Poisson.

# ============================================================================
# EJEMPLO 4
# Trabajo: "Regeneración del sotobosque nativo bajo la expansión de
# Ligustrum lucidum en las Yungas"
# Aramayo, A., Baldo, S., Díaz, C., Muñoz, P., Ritú, I., Ritzer, I.
# (Datos simulados con fines didácticos)

# 1) Reconocer la pregunta de investigación, los objetivos del trabajo, y la o las variables analizadas.
# 2) La siguiente es una distribución de frecuencias observadas del número de plántulas
# y renovales de especies nativas del sotobosque, registradas en parcelas
# agrupadas según cuatro niveles de cobertura de Ligustrum lucidum
# (criterio de cuartiles: 1 = bajo, 2 = medio-bajo, 3 = medio-alto, 4 = alto).

regeneracion = c("1" = 85, "2" = 62, "3" = 40, "4" = 23)

regeneracion

# a) Bajo la hipótesis nula de que la cobertura de Ligustrum lucidum no tiene
#    efecto sobre la regeneración, ¿como se distribuyen las frecuencias observadas
#    entre los cuatro niveles de cobertura?
# b) Aplique la prueba de chi-cuadrado de bondad de ajuste a frecuencias
#    teóricas y concluya.

# ============================================================================
# EJERCICIOS DE CLASE
# ============================================================================
# EJEMPLO 5 — Inferencia para la varianza (intervalo de confianza)
# Trabajo: "Variabilidad del peso de semillas de quinua (Chenopodium quinoa)
# en la Puna jujeña"
# (Datos simulados con fines didácticos)

# Los siguientes son los pesos (g) de 15 semillas de quinua seleccionadas
# al azar de un lote de la Puna jujeña.

peso = c(2.8, 3.1, 2.9, 3.4, 3.0, 2.7, 3.2, 3.3, 2.6, 3.5, 2.9, 3.1, 3.0, 3.2, 2.8)

# a) Estime la varianza muestral e intervalos de confianza del 95% para la
#    varianza poblacional del peso de semillas.
# b) ¿Se puede afirmar que la desviación estándar poblacional es menor a 0,5 g?
#    Justifique con el intervalo obtenido.

# ============================================================================


# ============================================================================
# EJEMPLO 6 — Prueba de independencia (tabla de contingencia 3x2)
# Trabajo: "Hábitos de nidificación de abejas nativas (Meliponini) en tres
# tipos de sustrato en las Yungas jujeñas"
# (Datos simulados con fines didácticos)

# La siguiente es una tabla de contingencia con las frecuencias observadas
# de nidos de abejas nativas según el tipo de sustrato (árbol vivo, tronco
# caído, suelo) y la presencia/ausencia de actividad reproductiva.

nidos = matrix(c(35, 15, 20, 30, 10, 40), nrow = 3, byrow = TRUE)

colnames(nidos) = c("Con actividad", "Sin actividad")
rownames(nidos) = c("Árbol vivo", "Tronco caído", "Suelo")

nidos

# a) ¿Existe asociación entre el tipo de sustrato y la presencia de actividad
#    reproductiva en los nidos de abejas nativas? 
# b) Calcule las frecuencias esperadas.

# ============================================================================


# ============================================================================
# EJEMPLO 7 — Prueba de ajuste a distribución binomial
# Trabajo: "Proporción de semillas germinadas de algarrobo (Prosopis alba)
# en condiciones controladas"
# (Datos simulados con fines didácticos)

# Se sembraron 10 semillas de algarrobo (sin escarificar) en cada una de 50 bandejas y se
# registró el número de semillas germinadas por bandeja.

germinadas = c("0" = 2, "1" = 5, "2" = 10, "3" = 13, "4" = 10, "5" = 6, "6" = 3, "7" = 1)

germinadas

# a) ¿Se puede asumir que el número de semillas germinadas por bandeja sigue
#    una distribución binomial con n = 10 y p = 0,3?
# b) Calcule las frecuencias esperadas bajo el modelo binomial.

# ============================================================================


# ============================================================================
# EJEMPLO 8 — Prueba de frecuencias esperadas (bondad de ajuste)
# Trabajo: "Preferencia de hábitat de la rana marsupial (Gastrotheca
# christiani) en las Yungas jujeñas"
# (Datos simulados con fines didácticos)

# Se registraron 120 individuos de Gastrotheca christiani en cuatro
# microhábitats. Bajo la hipótesis nula de ausencia de preferencia, se
# espera que los individuos se distribuyan de manera semejante entre los
# cuatro microhábitats.

microhabitat = c("Bromelias" = 45, "Hojarasca" = 30, "Ramas" = 25, "Suelo" = 20)

microhabitat

# a) ¿Se puede asumir que la rana marsupial no tiene preferencia por ninguno
#    de los cuatro microhábitats?
# b) Calcule las frecuencias esperadas bajo la hipótesis de equiprobabilidad.

# ============================================================================