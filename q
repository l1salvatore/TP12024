[1mdiff --git a/script_creado.R b/script_creado.R[m
[1mindex 5a04fbe..32b4837 100644[m
[1m--- a/script_creado.R[m
[1m+++ b/script_creado.R[m
[36m@@ -2,6 +2,7 @@[m [mlibrary(dplyr)[m
 library(readxl)[m
 library(ggplot2)[m
 library(tidyr)[m
[32m+[m[32mlibrary(reshape2)[m
 [m
 datos <- read_excel("/home/lsalvatore/Documents/FACULTAD/ProbabilidadYEstadistica/Prob_Datos_LP.xlsx", col_names = FALSE, skip=3)[m
 [m
[36m@@ -14,7 +15,7 @@[m [mcolnames(datos) <- c("OrdenInicial", # Cuantitativa Discreta[m
                       "Provincia", # Cualitativa Nominal[m
                       "TiempoDeResidenciaEnAños", # Cuantitativa Continua[m
                       "FormaObtencionAgua", # Cualitativa Nominal[m
[31m-                      "AguaPotable", # Cualitativa Nominal[m
[32m+[m[32m                      "AguaPotable", # Cualitativa Nominal Dicotómica[m
                       "PresionAgua", # Cualitativa Ordinal[m
                        #TipoDeCalefaccion -> Cualitativa de respuesta múltiple[m
                       "PoseeGasNaturalParaCocina", # Cualitativa Dicotómica[m
[36m@@ -31,12 +32,13 @@[m [mcolnames(datos) <- c("OrdenInicial", # Cuantitativa Discreta[m
                       "PoseeLeñaCarbonParaCalefaccion",# Cualitativa Dicotómica[m
                       "NoTieneParaCalefaccion", # Cualitativa Dicotómica[m
                       "NoNecesitaCalefaccionar",# Cualitativa Dicotómica[m
[32m+[m[41m                     [m
                       "TipoConexionElectrica" # Cualitativa Nominal[m
                      )[m
 datos <- data.frame(datos)[m
 datos_base <- datos |>[m
     mutate([m
[31m-      AguaPotable = ifelse(AguaPotable == 'No' & !is.na(PoseeGasNaturalParaCocina), 'Si', 'No'),[m
[32m+[m[32m      AguaPotable = ifelse(AguaPotable == 'No' & !is.na(AguaPotable), 'Si', 'No'),[m
       PoseeGasNaturalParaCocina = ifelse(PoseeGasNaturalParaCocina == 'Gas natural (red de gas)' & !is.na(PoseeGasNaturalParaCocina), 1, 0),[m
       PoseeGarrafaParaCocina = ifelse(PoseeGarrafaParaCocina == 'Gas natural (red de gas)'& !is.na(PoseeGarrafaParaCocina), 1, 0),[m
       ElectricidadParaCocina = ifelse(ElectricidadParaCocina == 'Electricidad'& !is.na(ElectricidadParaCocina), 1, 0),[m
[36m@@ -50,8 +52,8 @@[m [mdatos_base <- datos |>[m
       NoNecesitaCalefaccionar = ifelse(NoNecesitaCalefaccionar == 'No necesito calefaccionar mi vivienda en ninguna época del año' & !is.na(NoNecesitaCalefaccionar), 1, 0),[m
       # Recodifico las etiquetas de una variable categórica[m
       FormaObtencionAgua = recode(FormaObtencionAgua, "No sabe" = "No sabe",[m
[31m-                       "A través de una conexión con medidor a la red pública" = "Con medidor en red",[m
[31m-                       "A través de una conexión sin medidor, es decir “informalmente”, sea a través de una conexión directa a la red pública o a través de una conexión indirecta a través de un vecinx “informalmente”" = "Sin medidor, informalmente",[m
[32m+[m[32m                       "A través de una conexión con medidor a la red pública" = "Conexión con medidor en red",[m
[32m+[m[32m                       "A través de una conexión sin medidor, es decir “informalmente”, sea a través de una conexión directa a la red pública o a través de una conexión indirecta a través de un vecinx “informalmente”" = "Conexión sin medidor, informalmente",[m
                        "A través de un camión cisterna" = "Camión cisterna",[m
                        "No poseo agua dentro de la vivienda y/o tengo que acarrear desde fuera del terreno en que se ubica mi vivienda"  = "No posee agua, consume agua externa",[m
                        "A través de un pozo" = "Agua de pozo",[m
[36m@@ -67,9 +69,100 @@[m [mdatos_base <- datos_base %>%[m
 [m
 #Tiempo de residencia en años[m
 [m
[31m-ggplot(data.frame(datos_base$TiempoDeResidenciaEnAños), aes(x = datos_base$TiempoDeResidenciaEnAños)) +[m
[31m-  geom_dotplot(binwidth = 1, fill = "blue", color = "black") +[m
[31m-  labs(title = "Gráfico de Puntos",[m
[31m-       x = "Valores",[m
[31m-       y = "Frecuencia") +[m
[31m-  theme_minimal()[m
\ No newline at end of file[m
[32m+[m[32m# Armamos un boxplot, para observar rapidamente un panorama donde se concentra el tiempo de residencia[m
[32m+[m[32mboxplot(datos_base$TiempoDeResidenciaEnAños, main = "Tiempo de Residencia en Años", ylab = "Tiempo de Residencia en Años", col = "lightblue")[m
[32m+[m[32msummary(datos_base$TiempoDeResidenciaEnAños)[m
[32m+[m
[32m+[m[32m#SECCION AGUA[m[41m [m
[32m+[m[32m#Forma de obtención de agua[m
[32m+[m[32magua <- datos_base[m
[32m+[m[32magua$FormaObtencionAgua <- factor(agua$FormaObtencionAgua, levels = names(sort(table(datos_base$FormaObtencionAgua), decreasing = TRUE)))[m
[32m+[m
[32m+[m[32m# --- GRAFICO 1 ------[m
[32m+[m[32mggplot(data.frame(agua), aes(x = FormaObtencionAgua)) +[m
[32m+[m[32m  geom_bar(fill = "lightblue") +[m
[32m+[m[32m  labs(title = "Forma de Obtención del Agua",[m
[32m+[m[32m       x = "",[m
[32m+[m[32m       y = "Cantidad de hogares") +[m
[32m+[m[32m  theme_minimal() +[m
[32m+[m[32m  theme(axis.text.x = element_text(angle = 50, hjust = 1))+[m
[32m+[m[32m  scale_y_continuous(breaks = seq(0, max(table(agua$FormaObtencionAgua)), by = 50))[m
[32m+[m[32m#El agua que consigue, es potable?[m
[32m+[m
[32m+[m[32m# --- GRAFICO 2 ------[m
[32m+[m[32mpie(table(agua$AguaPotable), col = c("salmon", "lightblue"), # Cambia los colores según las categorías[m
[32m+[m[32m    main = "El agua es potable?")[m
[32m+[m
[32m+[m[32m#SECCION CALEFACCION Y COCINA[m
[32m+[m[32mcalefaccion <- data.frame([m
[32m+[m[32m  GasNatural = datos_base$PoseeGasNaturalParaCalefaccion,[m
[32m+[m[32m  Garrafa = datos_base$PoseeGarrafaParaCalefaccion,[m
[32m+[m[32m  Electricidad = datos_base$ElectricidadParaCalefaccion,[m
[32m+[m[32m  "Leña Carbon" = datos_base$PoseeLeñaCarbonParaCalefaccion,[m
[32m+[m[32m  "No Tiene" = datos_base$NoTieneParaCalefaccion,[m
[32m+[m[32m  "No Necesita" = datos_base$NoNecesitaCalefaccionar[m
[32m+[m[32m)[m
[32m+[m[32mcalefaccionfreq <- colSums(calefaccion[, -1])[m
[32m+[m
[32m+[m[32m#barplot(calefaccionfreq , col = "lightblue",[m
[32m+[m[32m#        main = "Modos de Calefaccion en los hogares",[m
[32m+[m[32m#        xlab = "", ylab = "Cantidad de Hogares",[m
[32m+[m[32m#        names.arg = names(calefaccionfreq))[m
[32m+[m
[32m+[m
[32m+[m[32mcocina <- data.frame([m
[32m+[m[32m  GasNatural = datos_base$PoseeGasNaturalParaCocina,[m
[32m+[m[32m  Garrafa = datos_base$PoseeGarrafaParaCocina,[m
[32m+[m[32m  Electricidad = datos_base$ElectricidadParaCocina,[m
[32m+[m[32m  "Leña Carbon" = datos_base$PoseeLeñaCarbonParaCocina,[m
[32m+[m[32m  "No Tiene" = datos_base$NoTieneParaCocina[m
[32m+[m[32m)[m
[32m+[m[32mcocinafreq <- colSums(cocina[, -1])[m
[32m+[m[32m#(cocinafreq , col = "lightblue",[m
[32m+[m[32m#        main = "Modos de cocina en los hogares",[m
[32m+[m[32m#        xlab = "", ylab = "Cantidad de Hogares",[m
[32m+[m[32m#        names.arg = names(cocinafreq))[m
[32m+[m
[32m+[m[32mcalefaccion$ID <- 1:nrow(calefaccion)  # Agregar ID para fusionar correctamente[m
[32m+[m[32mcocina$ID <- 1:nrow(cocina)[m
[32m+[m[41m [m
[32m+[m[32m# Unir tablas por ID[m
[32m+[m[32mdatos_relacion <- merge(calefaccion, cocina, by = "ID", suffixes = c("_Calefaccion", "_Cocina"))[m
[32m+[m
[32m+[m[32m#-----GRAFICO 3: Gas Natural -----[m
[32m+[m[32mtabla_contingencia <- table([m
[32m+[m[32m  calefaccion = datos_relacion$GasNatural_Calefaccion,[m
[32m+[m[32m  cocina = datos_relacion$GasNatural_Cocina[m
[32m+[m[32m)[m
[32m+[m[32mprint(tabla_contingencia)[m
[32m+[m
[32m+[m[32m#-----GRAFICO 3: Garrafa -----[m
[32m+[m[32mtabla_contingencia <- table([m
[32m+[m[32m  calefaccion = factor(datos_relacion$Garrafa_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),[m
[32m+[m[32m  cocina =  factor(datos_relacion$Garrafa_Cocina, levels = c(0,1), labels = c("No", "Sí"))[m
[32m+[m[32m)[m
[32m+[m
[32m+[m[32mprint(tabla_contingencia)[m
[32m+[m
[32m+[m[32m#-----GRAFICO 3: Electricidad -----[m
[32m+[m[32mtabla_contingencia <- table([m
[32m+[m[32m  calefaccion = factor(datos_relacion$Electricidad_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),[m
[32m+[m[32m  cocina = factor(datos_relacion$Electricidad_Cocina, levels = c(0,1), labels = c("No", "Sí"))[m
[32m+[m[32m)[m
[32m+[m
[32m+[m[32mprint(tabla_contingencia)[m
[32m+[m
[32m+[m[32m#-----GRAFICO 3: LeñaCarbon -----[m
[32m+[m[32mtabla_contingencia <- table([m
[32m+[m[32m  calefaccion = factor(datos_relacion$Leña.Carbon_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),[m
[32m+[m[32m  cocina = factor(datos_relacion$Leña.Carbon_Cocina, levels = c(0,1), labels = c("No", "Sí"))[m
[32m+[m[32m)[m
[32m+[m
[32m+[m[32mprint(tabla_contingencia)[m
[32m+[m
[32m+[m[32m#-----GRAFICO 3: No Tiene -----[m
[32m+[m[32mtabla_contingencia <- table([m
[32m+[m[32m  calefaccion = factor(datos_relacion$No.Tiene_Calefaccion, levels = c(0,1), labels = c("No", "Sí")),[m
[32m+[m[32m  cocina = factor(datos_relacion$No.Tiene_Cocina, levels = c(0,1), labels = c("No", "Sí"))[m
[32m+[m[32m)[m
[41m+[m
