# ============================================================
# Primer Parcial de Bioestadística - Jujuy 2026
# Resolución de código
# ============================================================

# ------------------------------------------------------------
# MÓDULO 1: ESTADÍSTICA DESCRIPTIVA
# ------------------------------------------------------------

# Datos
masa = c(20.6, 31.5, 19.2, 22.4, 18.5, 21.0, 19.8, 20.1)

# Variable: masa corporal (g) de adultos de sapo andino
# Tipo: cuantitativa continua
# Escala: razón

media   = mean(masa)      # 21.64
mediana = median(masa)    # 20.35
media
mediana

# media > mediana ∴ asimetría positiva (sesgo a la derecha)
# La mediana describe mejor el centro (robusta ante extremos)
# Nota: si bien no se pedía asentarlo en la hoja, graficar los datos resulta en un gran complemento para responder esta pregunta.

desvio = sd(masa)                      # desvío estándar muestral
CV = (desvio / media) * 100            # CV en %
round(CV, 1)                            # 19.2 %

# Nota: gráfico adecuado (para defensa oral)

# ------------------------------------------------------------
# MÓDULO 2: PROBABILIDAD Y TABLAS DE CONTINGENCIA
# ------------------------------------------------------------

tabla = matrix(c(18, 42,
                  27, 63),
                nrow = 2, byrow = TRUE)
dimnames(tabla) = list(
  "R. rumbolli" = c("presente", "ausente"),
  "G. christiani" = c("presente", "ausente")
)
tabla

N = sum(tabla)   # 150

# A: presencia R. rumbolli ; B: presencia G. christiani
P_A     = sum(tabla["presente", ]) / N        # 60/150
P_B     = sum(tabla[, "presente"]) / N        # 45/150
P_AintB = tabla["presente", "presente"] / N   # 18/150

P_AunB = P_A + P_B - P_AintB
round(P_AunB, 2)   # 0.58

# --- Pregunta 5: P(A | B) ---
P_A_dado_B = P_AintB / P_B
round(P_A_dado_B, 2)   # 0.40
# Interpretación: verificada la presencia de G. christiani, la probabilidad de hallar R. rumbolli = 0.40

P_A * P_B              # 0.12
P_AintB                # 0.12
# Como P(A∩B) = P(A)P(B), A y B son independientes
P_AintB == P_A * P_B   # TRUE


# ------------------------------------------------------------
# MÓDULO 3: DISTRIBUCIONES DE PROBABILIADAD DISCRETAS
# ------------------------------------------------------------

# Parámetros

n = 15; p = .27
dbinom(3, n, p)
1 - pbinom(2, n, p)
n * p 


# ------------------------------------------------------------
# MÓDULO 4: DISTRIBUCIÓN NORMAL
# ------------------------------------------------------------

mu = 145; sigma = 18.5
z120 = (120 - mu) / sigma
z120
z163.5 = (163.5 - mu)/sigma
pnorm(z163.5) - pnorm(z120)

qnorm(0.9, mu, sigma)

