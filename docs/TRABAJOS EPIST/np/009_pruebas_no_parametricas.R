# =============================================================================
# Bioestadística - Facultad de Ciencias Agrarias - UNJu
# Licenciatura en Ciencias Biológicas - AÑO 2026
# Trabajo Práctico 9: Pruebas No Paramétricas
# Resolución: Parte A (ejemplos guiados) y Parte B (ejercicios 1 a 5)
# Datos simulados con fines didácticos
# =============================================================================

# #############################################################################
# PARTE A - ACTIVIDAD GUIADA
# #############################################################################

# =============================================================================
# EJEMPLO 1 - Riqueza de dieta en lechuza de campanario (U de Mann-Whitney)
# =============================================================================
# PASO 1: Pregunta: ¿la riqueza de presas es mayor en el ambiente periurbano?
#   y: riqueza de presas por egagrópila (cuantitativa discreta)
#   x: ambiente (cualitativa, 2 grupos independientes)

# PASO 2: Datos y exploración
y_urbano     = c(1, 2, 1, 3, 2, 1, 2, 2, 3, 1, 2, 2)
y_periurbano = c(4, 5, 3, 6, 4, 5, 3, 7, 5, 4, 6, 5)

boxplot(y_urbano, y_periurbano, names = c("Urbano", "Periurbano"),
        main = "Riqueza de presas por egagrópila en Tyto alba",
        ylab = "Número de presas distintas (Riqueza)")

mediana_urbano     = median(y_urbano)
mediana_periurbano = median(y_periurbano)
mediana_urbano
mediana_periurbano
IQR(y_urbano)
IQR(y_periurbano)
length(y_urbano)
length(y_periurbano)

# Justificación: conteos discretos, n pequeño (12 por grupo), asimetría
# -> no se asume normalidad -> U de Mann-Whitney

# PASO 3: 
# 
# 1. Hipótesis
# H0: Me_periurbano <= Me_urbano
# H1: Me_periurbano >  Me_urbano
tipo_prueba = "unilateral derecha"
alternativa = "greater"

# 2. Nivel de significación
alfa = 0.05

# 3. Estadístico de prueba: ...

# 4. Regla de desición: "Se rechaza H0 si y solo si ..."

# 5. Cálculos: Prueba (exact = FALSE porque hay empates)
prueba_mann = wilcox.test(y_periurbano, y_urbano,
                          alternative = alternativa, exact = FALSE)
print(prueba_mann)
p_valor = prueba_mann$p.value
p_valor

# 6: Decisión: ...

# 7. Conclusión: ...


# =============================================================================
# EJEMPLO 2 - Muña-muña y malestar estomacal (Wilcoxon apareado)
# =============================================================================
# PASO 1: y: severidad del malestar (Likert 1-5, cualitativa ordinal)
#   Diseño: apareado (mismo poblador antes y después)

# PASO 2: Datos y exploración
y_antes   = c(5, 4, 5, 4, 5, 3, 4, 5, 4, 5)
y_despues = c(2, 1, 2, 2, 3, 1, 2, 1, 2, 2)

diferencias = y_antes - y_despues
plot(table(diferencias), main = "Reducción individual del malestar estomacal",
        xlab = "Poblador (Unidad experimental)",
        ylab = "Disminución en escala (Antes - Después)")

median(y_antes)
median(y_despues)

# Justificación: variable ordinal, sin escala de intervalo ni normalidad
# -> rangos con signo de Wilcoxon

# PASO 3: 

# 1. Hipótesis
# H0: Me_antes <= Me_despues
# H1: Me_antes >  Me_despues
tipo_prueba = "unilateral derecha"
alternativa = "greater"

# 2. Nivel de significación:
alfa = 0.05

# 3. Estadístico de prueba: ...

# 4. Regla de decisión: se rechaza H0 si y solo si ...

# 5: Cálculos
prueba_wilcox_apareada = wilcox.test(y_antes, y_despues, paired = TRUE,
                                     alternative = alternativa, exact = FALSE)
print(prueba_wilcox_apareada)
p_valor_w = prueba_wilcox_apareada$p.value
p_valor_w

# 6: Decisión: ...

# 7: Conclusión: ...


# =============================================================================
# EJEMPLO 3 - Evaluación de la perspectiva de la población de la Puna Jujeña
#             con respecto a la Muña-muña (McNemar)
# =============================================================================
# Contexto: Evaluación de la perspectiva de la población de la Puna Jujeña
#           con respecto a la Muña-muña (Clinopodium gilliesii) y su efectividad
#           en el tratamiento de cuadros de acidez y pesadez estomacal.
# Situación: Registro del estado sintomático de 50 pobladores (40 a 60 años)
#            de la Puna jujeña evaluados Antes y Después de consumir la
#            infusión medicinal de Muña-muña.


# PASO 1 — DEFINICIÓN DE VARIABLE ALEATORIA, DISEÑO Y JUSTIFICACIÓN METODOLÓGICA
# ─────────────────────────────────────────────────────────────────────────────
#
# Justificación metodológica para la selección de la Prueba de McNemar:
# 1. Muestras dependientes / apareadas: Se evalúan las mismas unidades de análisis
#    (los mismos 50 pobladores de la Puna jujeña) en dos momentos temporales
#    distintos (Momento 1: Antes de tomar la infusión vs. Momento 2: Después de tomarla).
# 2. Variable de respuesta cualitativa dicotómica: La presencia de malestar digestivo
#    posee únicamente dos estados mutuamente excluyentes:
#    - Con Malestar / Síntoma presente (-)
#    - Sin Malestar / Síntoma ausente (+)
# 3. Evaluación del cambio: La prueba centra su análisis exclusivamente en los pares
#    discordantes (b y c), evaluando si la proporción de individuos que alivió su
#    síntoma difiere significativamente de la proporción que empeoró.


# PASO 2 — CARGA DE DATOS Y CONSTRUCCIÓN DE LA TABLA DE CONTINGENCIA 2x2 APAREADA
# ─────────────────────────────────────────────────────────────────────────────
# Frecuencias observadas en la muestra de N = 50 pobladores de la Puna jujeña:
#   a = 5  pobladores: Mantiene síntoma (Con malestar Antes -> Con malestar Después)
#   b = 32 pobladores: Alivió síntoma (Con malestar Antes -> Sin malestar Después) [Discordante]
#   c = 3  pobladores: Empeoró síntoma (Sin malestar Antes -> Con malestar Después) [Discordante]
#   d = 10 pobladores: Mantiene sin síntoma (Sin malestar Antes -> Sin malestar Después)

muna_matriz = matrix(c(5, 32, 3, 10), nrow = 2, byrow = TRUE)

colnames(muna_matriz) = c("Después: Con Malestar (-)", "Después: Sin Malestar (+)")
rownames(muna_matriz) = c("Antes: Con Malestar (-)", "Antes: Sin Malestar (+)")

# Visualización de la tabla de contingencia 2x2
muna_matriz


# PASO 3 — PLANTEO DE HIPÓTESIS ESTADÍSTICAS Y NIVEL DE SIGNIFICACIÓN
# ─────────────────────────────────────────────────────────────────────────────
# H0: P(Antes = -, Después = +) = P(Antes = +, Después = -)
#     (La proporción de pobladores que cambia a 'Sin malestar' es igual a la que cambia a 'Con malestar'; la infusión no genera un cambio significativo)
# H1: P(Antes = -, Después = +) ≠ P(Antes = +, Después = -)
#     (La proporción de pobladores que cambia a 'Sin malestar' difiere significativamente de la que cambia a 'Con malestar'; la infusión genera un cambio significativo)
# α  = 0.05
alfa = 0.05

# Estadístico de prueba con corrección de continuidad de Yates:
#
#   χ2_McNemar = (|b - c| - 1)² / (b + c)   con 1 grado de libertad

# Extracción de pares discordantes de la matriz
b = muna_matriz[1, 2] # Frecuencia de pobladores que se aliviaron (b = 32)
c = muna_matriz[2, 1] # Frecuencia de pobladores que empeoraron (c = 3)

# Cálculo del estadístico
chi2_calc = ((abs(b - c) - 1)^2) / (b + c)
chi2_calc

# El p-valor se calcula a partir de la cola derecha de la distribución Chi-cuadrado con 1 g.l. (¿por qué?)
p_val = pchisq(chi2_calc, df = 1, lower.tail = FALSE)
p_val

# Alternativa (¡usar esta versión!)
res_mcnemar = mcnemar.test(muna_matriz, correct = TRUE)
res_mcnemar


# =============================================================================
# EJEMPLO 4 - Ligustrum lucidum y renovales nativos (Spearman)
# =============================================================================
# PASO 1: x: cobertura de Ligustrum (%), y: conteo de renovales nativos

# PASO 2: Datos y exploración
x_cobertura = c(5, 12, 25, 30, 45, 50, 65, 70, 85, 90)
y_renovales = c(48, 40, 32, 28, 15, 18, 8, 6, 2, 1)

plot(x_cobertura, y_renovales, main = "Cobertura de Ligustrum vs Renovales Nativos",
     xlab = "Cobertura de Ligustrum (%)", ylab = "Número de Renovales Nativos",
     pch = 19, col = "darkgreen")

# Justificación: respuesta de conteo, varianza heterogénea, posible falta de
# linealidad estricta -> correlación de rangos de Spearman

# PASO 3: Hipótesis
# H0: rho_s = 0
# H1: rho_s < 0
alfa = 0.05
tipo_prueba = "unilateral izquierda"
alternativa = "less"

# PASO 4: Prueba
prueba_spearman = cor.test(x_cobertura, y_renovales, method = "spearman",
                           alternative = alternativa)
print(prueba_spearman)
rho_calculado = prueba_spearman$estimate
rho_calculado
p_valor_sp = prueba_spearman$p.value
p_valor_sp

# Decisión y conclusión: ...

# =============================================================================
## EJERCICIOS DE APLICACIÓN
# =============================================================================
# EJERCICIO 1 - Germinación de poroto bajo estrés salino (U de Mann-Whitney)
# =============================================================================
# Pregunta estadística: ¿Difiere la mediana de semillas germinadas entre el
# control (0 mM) y el tratamiento salino (100 mM NaCl)?
#
# Consignas:
# 1. Identificar la variable respuesta (y) y la variable de grupo (x), indicando
#    su tipo.
# 2. Realizar un boxplot comparativo entre ambos grupos y calcular medianas e IQR.
# 3. Plantear las hipótesis (H0 y H1) y el nivel de significancia.
# 4. Aplicar la prueba U de Mann-Whitney y concluir con la decisión estadística.

germinadas_control = c(18, 19, 20, 17, 19, 20, 18, 19)
germinadas_sal     = c(12, 10, 14, 11, 9, 13, 10, 12)


# =============================================================================
# EJERCICIO 2 - Calidad de agua en ambientes periglaciares (Wilcoxon apareado)
# =============================================================================
# Pregunta estadística: ¿Difiere la mediana del índice biótico antes y después
# del inicio de la exploración minera en las mismas estaciones?
#
# Consignas:
# 1. Indicar por qué las observaciones son apareadas y qué tipo de variable es
#    el índice biótico.
# 2. Graficar las diferencias (antes - después) por estación y calcular medianas.
# 3. Plantear las hipótesis y el nivel de significancia.
# 4. Aplicar la prueba de rangos con signo de Wilcoxon y concluir.

indice_antes   = c(8, 9, 7, 8, 9, 8, 10, 7, 8)
indice_despues = c(5, 6, 4, 6, 5, 7, 6, 5, 4)


# =============================================================================
# EJERCICIO 3 - Desinfección de patógenos en frutos de naranjo (McNemar)
# =============================================================================
# Pregunta estadística: ¿Cambia la proporción de superficies con patógenos
# presentes antes y después del tratamiento térmico?
#
# Consignas:
# 1. Armar la tabla 2x2 de datos apareados (antes/después) y justificar por qué
#    corresponde McNemar y no Chi-cuadrado de independencia.
# 2. Plantear las hipótesis y el nivel de significancia.
# 3. Aplicar la prueba de McNemar (sin y con corrección de continuidad).
# 4. Interpretar el resultado en función de los pares discordantes.

tabla_patogenos = matrix(c(3, 16, 1, 5), nrow = 2, byrow = TRUE,
                         dimnames = list(Antes = c("Presente", "Ausente"),
                                         Despues = c("Presente", "Ausente")))


# =============================================================================
# EJERCICIO 4 - Altitud y tolerancia al frío en ecotipos de quinua (Spearman)
# =============================================================================
# Pregunta estadística: ¿Existe asociación monotónica positiva entre la altitud
# del sitio de origen y la tolerancia a la germinación en frío?
#
# Consignas:
# 1. Identificar la variable independiente y la dependiente, indicando su tipo.
# 2. Realizar el diagrama de dispersión entre ambas variables.
# 3. Plantear las hipótesis y el nivel de significancia.
# 4. Aplicar la correlación de Spearman e interpretar el coeficiente y la
#    decisión estadística.

altitud         = c(2200, 2500, 2800, 3100, 3400, 3700, 4000, 4200)
tolerancia_frio = c(2, 3, 4, 6, 7, 8, 9, 10)


# =============================================================================
# EJERCICIO 5 - Macroinvertebrados en arroyos puneños (U de Mann-Whitney)
# =============================================================================
# Pregunta estadística: ¿Es mayor la mediana de riqueza taxonómica en tramos
# sin pastoreo que en tramos con pastoreo de camélidos?
#
# Consignas:
# 1. Formular la pregunta de investigación e identificar las variables (y, x).
# 2. Realizar el boxplot comparativo y calcular medianas e IQR.
# 3. Plantear las hipótesis y el nivel de significancia.
# 4. Aplicar la prueba U de Mann-Whitney y concluir sobre el efecto del pastoreo.

con_pastoreo = c(4, 5, 3, 6, 4, 3, 5, 4)
sin_pastoreo = c(9, 11, 8, 12, 10, 9, 11, 10)