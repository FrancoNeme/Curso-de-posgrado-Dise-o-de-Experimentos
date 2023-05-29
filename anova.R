# CARGA DE LIBRERIAS ----
# = = = = = = = = = = = = 

library(effects)
library(ggeffects)
library(ggplotify)
library(nlme)
library(multcomp)
library(ggplot2)
library(emmeans)
library(car)



# CARGA DE DATOS ----
# = = = = = = = = = = 

archivo_datos = read.csv('datos_total.csv', dec = ",")
archivo_datos$C_B <- as.numeric(as.character(archivo_datos$C_B))
archivo_datos <- archivo_datos[order(archivo_datos$t),]

# Convertir la columna "Temperatura" a tipo factor
archivo_datos$T <- as.factor(archivo_datos$T)

# Convertir la columna "N0" a tipo factor
archivo_datos$N0 <- as.factor(archivo_datos$N0)

# Convertir la columna "Tiempo" a tipo factor
archivo_datos$t <- as.factor(as.character(archivo_datos$t))

# Convertir la columna "C_B" a tipo numérico
archivo_datos$C_B <- as.numeric(archivo_datos$C_B)

# Convertir la columna "Corrida_T" a tipo factor
archivo_datos$Corrida_T <- as.factor(archivo_datos$Corrida_T)

# Convertir la columna "Corrida_N0" a tipo factor
archivo_datos$Corrida_N0 <- as.factor(archivo_datos$Corrida_N0)

# print(archivo_datos)



# PRUEBA DE MODELOS DE ESTRUCTURA DE MATRIZ VAR-COVAR ----
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



# ___ Modelo 0 -----

mlm.modelo.000_C_B_REML <- gls(C_B ~ 1 + T + N0 + t + T:N0 + T:t + N0:t + T:N0:t
                               ,method = "REML"
                               ,na.action = na.omit
                               ,data = archivo_datos)

# Tabla de resumen del modelo

resumen_modelo0 <- summary(mlm.modelo.000_C_B_REML)

print('')
print('---------------------------------------- MODELO 0 ----------------------------------------')
print('------------------------------------------------------------------------------------------')

# Imprimir medidas de ajuste

cat(paste("N:", length(resid(mlm.modelo.000_C_B_REML)), "\n"))
cat(paste("AIC:", AIC(mlm.modelo.000_C_B_REML), "\n"))
cat(paste("BIC:", BIC(mlm.modelo.000_C_B_REML), "\n"))
cat(paste("logLik:", logLik(mlm.modelo.000_C_B_REML), "\n"))
cat(paste("sigma:", summary(mlm.modelo.000_C_B_REML)$sigma, "\n"))



# ___ Modelo 1 -----

mlm.modelo.001_C_B_REML <- gls(C_B ~ 1 + T + N0 + t + T:N0 + T:t + N0:t + T:N0:t
                               ,correlation = corCompSymm(form = ~1 | Corrida_T / Corrida_N0)
                               ,method = "REML"
                               ,na.action = na.omit
                               ,data = archivo_datos)

# Tabla de resumen del modelo

resumen_modelo1 <- summary(mlm.modelo.001_C_B_REML)

print('')
print('---------------------------------------- MODELO 1 ----------------------------------------')
print('------------------------------------------------------------------------------------------')

# Imprimir medidas de ajuste

cat(paste("N:", length(resid(mlm.modelo.001_C_B_REML)), "\n"))
cat(paste("AIC:", AIC(mlm.modelo.001_C_B_REML), "\n"))
cat(paste("BIC:", BIC(mlm.modelo.001_C_B_REML), "\n"))
cat(paste("logLik:", logLik(mlm.modelo.001_C_B_REML), "\n"))
cat(paste("sigma:", summary(mlm.modelo.001_C_B_REML)$sigma, "\n"))


# Matriz de varianzas-covarianzas

VarCovar_M1 = getVarCov(mlm.modelo.001_C_B_REML)
print(VarCovar_M1)



# ___ Modelo 2 ----

mlm.modelo.002_C_B_REML <- gls(C_B ~ 1 + T + N0 + t + T:N0 + T:t + N0:t + T:N0:t
                               ,correlation = corAR1(form = ~as.integer(as.numeric(t)) | Corrida_T / Corrida_N0)
                               ,method = "REML"
                               ,na.action = na.omit
                               ,data = archivo_datos)

# Tabla de resumen del modelo

resumen_modelo2 <- summary(mlm.modelo.002_C_B_REML)

print('')
print('---------------------------------------- MODELO 2 ----------------------------------------')
print('------------------------------------------------------------------------------------------')

# Imprimir medidas de ajuste

cat(paste("N:", length(resid(mlm.modelo.002_C_B_REML)), "\n"))
cat(paste("AIC:", AIC(mlm.modelo.002_C_B_REML), "\n"))
cat(paste("BIC:", BIC(mlm.modelo.002_C_B_REML), "\n"))
cat(paste("logLik:", logLik(mlm.modelo.002_C_B_REML), "\n"))
cat(paste("sigma:", summary(mlm.modelo.002_C_B_REML)$sigma, "\n"))


# Matriz de varianzas-covarianzas

VarCovar_M2 = getVarCov(mlm.modelo.002_C_B_REML)
print(VarCovar_M2)


# ___ Modelo 3 ----

mlm.modelo.003_C_B_REML <- gls(C_B ~ 1 + T + N0 + t + T:N0 + T:t+ N0:t+ T:N0:t
                               ,weights = varComb(varIdent(form = ~1 | t))
                               ,correlation = corAR1(form = ~as.integer(as.numeric(t)) | Corrida_T / Corrida_N0)
                               ,method = "REML"
                               ,na.action = na.omit
                               ,data = archivo_datos
                               ,control = glsControl(msMaxIter = 3000))


# Tabla de resumen del modelo

resumen_modelo3 <- summary(mlm.modelo.003_C_B_REML)

print('')
print('---------------------------------------- MODELO 3 ----------------------------------------')
print('------------------------------------------------------------------------------------------')

# Imprimir medidas de ajuste

cat(paste("N:", length(resid(mlm.modelo.003_C_B_REML)), "\n"))
cat(paste("AIC:", AIC(mlm.modelo.003_C_B_REML), "\n"))
cat(paste("BIC:", BIC(mlm.modelo.003_C_B_REML), "\n"))
cat(paste("logLik:", logLik(mlm.modelo.003_C_B_REML), "\n"))
cat(paste("sigma:", summary(mlm.modelo.003_C_B_REML)$sigma, "\n"))


# Matriz de varianzas-covarianzas

VarCovar_M3 <- getVarCov(mlm.modelo.003_C_B_REML)
print(VarCovar_M3)



# ___ Modelo 4 ----

mlm.modelo.004_C_B_REML <- gls(C_B ~ 1 + T + N0 + t + T:N0 + T:t + N0:t + T:N0:t
                               ,weights = varComb(varIdent(form = ~1 | t))
                               ,correlation = corSymm(form = ~as.integer(as.numeric(t)) | Corrida_T / Corrida_N0)
                               ,method = "REML"
                               ,na.action = na.omit
                               ,data = archivo_datos
                               ,control = glsControl(msMaxIter = 3000))

# Tabla de resumen del modelo

resumen_modelo4 <- summary(mlm.modelo.004_C_B_REML)

print('---------------------------------------- MODELO 4 ----------------------------------------')
print('------------------------------------------------------------------------------------------')

# Imprimir medidas de ajuste

cat(paste("N:", length(resid(mlm.modelo.004_C_B_REML)), "\n"))
cat(paste("AIC:", AIC(mlm.modelo.004_C_B_REML), "\n"))
cat(paste("BIC:", BIC(mlm.modelo.004_C_B_REML), "\n"))
cat(paste("logLik:", logLik(mlm.modelo.004_C_B_REML), "\n"))
cat(paste("sigma:", summary(mlm.modelo.004_C_B_REML)$sigma, "\n"))


# Matriz de varianzas-covarianzas

VarCovar_M4 = getVarCov(mlm.modelo.004_C_B_REML)
print(VarCovar_M4)



# COMPARACION Y SELECCION DE MODELO ----
# = = = = = = = = = = = = = = = = = = = =

comp_2_3 = anova(mlm.modelo.002_C_B_REML, mlm.modelo.003_C_B_REML)
print(comp_2_3)
comp_2_4 = anova(mlm.modelo.002_C_B_REML, mlm.modelo.004_C_B_REML)
print(comp_2_4)
comp_3_4 = anova(mlm.modelo.003_C_B_REML, mlm.modelo.004_C_B_REML)
print(comp_3_4)



# ANOVA MODELO SELECCIONADO ----
# = = = = = = = = = = = = = = = =

anova_modelo_elegido <- anova(mlm.modelo.003_C_B_REML, test = "F")



# PRUEBAS POST HOC ----
# = = = = = = = = = = = =

# Calcular medias ajustadas y prueba de Tukey

medias <- emmeans(mlm.modelo.003_C_B_REML, ~ N0 + T)
tukey <- cld(medias, alpha = 0.05, Letters = letters)

# Convertir medias y letras en un data.frame

medias_df <- as.data.frame(medias)
tukey_df <- as.data.frame(tukey)



# ___ Graficar barras y agregar letras de Tukey ----

ggplot(medias_df, aes(x = factor(N0), y = emmean, fill = factor(T))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = tukey_df, aes(x = factor(N0), y = emmean, label = .group), 
            position = position_dodge(width = 0.9), vjust = 1.5, size = 4) +
  scale_fill_discrete(name = "T", labels = c("13", "18", "23", "28")) +
  labs(x = "N0", y = "C_B") +
  theme_bw()

ggsave("tukey_general.png", plot = last_plot(), dpi = 300, width = 6, height = 4, units = "in")



# ___ Graficos de interaccion ----

t_etiquetas <- c("1","2","3","4","5","6","7")

interacciones <- plot(effect("T*N0*t",mlm.modelo.003_C_B_REML,confidence.level=0.95, xlevels = list(t = t_etiquetas)))

ggp <- as.ggplot(interacciones)

ggsave("interaccion_gral.png", plot = ggp, dpi = 300)#, width = 6, height = 4, units = "in")