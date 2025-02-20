library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)

datos <- read_excel("/home/lsalvatore/Documents/FACULTAD/ProbabilidadYEstadistica/Prob_Datos_LP.xlsx", col_names = FALSE, skip=3)

datos <- datos |>
      select(   # Seleccionar las columnas que quiero conservar
             "...1", "...2","...5", "...24", "...25", "...26", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...50" 
         )

colnames(datos) <- c("OrdenInicial", # Cuantitativa Discreta
                      "Provincia", # Cualitativa Nominal
                      "TiempoDeResidenciaEnAños", # Cuantitativa Continua
                      "FormaObtencionAgua", # Cualitativa Nominal
                      "AguaPotable", # Cualitativa Nominal
                      "PresionAgua", # Cualitativa Ordinal
                       #TipoDeCalefaccion -> Cualitativa de respuesta múltiple
                      "PoseeGasNaturalParaCocina", # Cualitativa Dicotómica
                      "PoseeGarrafaParaCocina", # Cualitativa Dicotómica
                      "ElectricidadParaCocina", # Cualitativa Dicotómica
                      "PoseeLeñaCarbonParaCocina",# Cualitativa Dicotómica
                      "NoTieneParaCocina",# Cualitativa Dicotómica
                     
                       #TipoDeCocina -> Cualitativa de respuesta múltiple
                     
                      "PoseeGasNaturalParaCalefaccion", # Cualitativa Dicotómica
                      "PoseeGarrafaParaCalefaccion", # Cualitativa Dicotómica
                      "ElectricidadParaCalefaccion", # Cualitativa Dicotómica
                      "PoseeLeñaCarbonParaCalefaccion",# Cualitativa Dicotómica
                      "NoTieneParaCalefaccion", # Cualitativa Dicotómica
                      "NoNecesitaCalefaccionar",# Cualitativa Dicotómica
                      "TipoConexionElectrica" # Cualitativa Nominal
                     )
datos <- data.frame(datos)
datos_base <- datos |>
    mutate(
      AguaPotable = ifelse(AguaPotable == 'No' & !is.na(PoseeGasNaturalParaCocina), 'Si', 'No'),
      PoseeGasNaturalParaCocina = ifelse(PoseeGasNaturalParaCocina == 'Gas natural (red de gas)' & !is.na(PoseeGasNaturalParaCocina), 1, 0),
      PoseeGarrafaParaCocina = ifelse(PoseeGarrafaParaCocina == 'Gas natural (red de gas)'& !is.na(PoseeGarrafaParaCocina), 1, 0),
      ElectricidadParaCocina = ifelse(ElectricidadParaCocina == 'Electricidad'& !is.na(ElectricidadParaCocina), 1, 0),
      PoseeLeñaCarbonParaCocina = ifelse(PoseeLeñaCarbonParaCocina == 'Leña/Carbón' & !is.na(PoseeLeñaCarbonParaCocina), 1, 0),
      NoTieneParaCocina = ifelse(NoTieneParaCocina == 'No tengo para cocinar en mi vivienda' & !is.na(NoTieneParaCocina), 1, 0),
      PoseeGasNaturalParaCalefaccion = ifelse(PoseeGasNaturalParaCalefaccion == 'Gas natural (red de gas)' & !is.na(PoseeGasNaturalParaCalefaccion), 1, 0),
      PoseeGarrafaParaCalefaccion = ifelse(PoseeGarrafaParaCalefaccion == 'Gas envasado (garrafa)'& !is.na(PoseeGarrafaParaCalefaccion), 1, 0),
      ElectricidadParaCalefaccion = ifelse(ElectricidadParaCalefaccion == 'Electricidad'& !is.na(ElectricidadParaCalefaccion), 1, 0),
      PoseeLeñaCarbonParaCalefaccion = ifelse(PoseeLeñaCarbonParaCalefaccion == 'Leña/Carbón' & !is.na(PoseeLeñaCarbonParaCalefaccion), 1, 0),
      NoTieneParaCalefaccion = ifelse(NoTieneParaCalefaccion == 'No tengo para calefaccionar mi vivienda' & !is.na(NoTieneParaCalefaccion), 1, 0),
      NoNecesitaCalefaccionar = ifelse(NoNecesitaCalefaccionar == 'No necesito calefaccionar mi vivienda en ninguna época del año' & !is.na(NoNecesitaCalefaccionar), 1, 0),
      # Recodifico las etiquetas de una variable categórica
      FormaObtencionAgua = recode(FormaObtencionAgua, "No sabe" = "No sabe",
                       "A través de una conexión con medidor a la red pública" = "Con medidor en red",
                       "A través de una conexión sin medidor, es decir “informalmente”, sea a través de una conexión directa a la red pública o a través de una conexión indirecta a través de un vecinx “informalmente”" = "Sin medidor, informalmente",
                       "A través de un camión cisterna" = "Camión cisterna",
                       "No poseo agua dentro de la vivienda y/o tengo que acarrear desde fuera del terreno en que se ubica mi vivienda"  = "No posee agua, consume agua externa",
                       "A través de un pozo" = "Agua de pozo",
                       "Conexión a un tanque comunitario" = "Tanque comunitario"),
      TipoConexionElectrica = recode(TipoConexionElectrica, "Conexión a través de un medidor a la red eléctrica" = "Con medidor en red",
                                  "Conexión sin medidor a una red eléctrica (“informal”)" = "Sin medidor, informalmente",
                                  "Conexión a través de un medidor comunitario a la red eléctrica" = "Con medidor comunitario",
                                  "No posee conexión a la red eléctrica en la vivienda" = "No tiene acceso a la red eléctrica")
      
       )
datos_base <- datos_base %>%
  filter(Provincia == "CABA" | Provincia == "Mendoza" | Provincia == "Río Negro" | Provincia == "Santa Cruz" | Provincia == "Jujuy" | Provincia == "Córdoba")

#Tiempo de residencia en años

ggplot(data.frame(datos_base$TiempoDeResidenciaEnAños), aes(x = datos_base$TiempoDeResidenciaEnAños)) +
  geom_dotplot(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Gráfico de Puntos",
       x = "Valores",
       y = "Frecuencia") +
  theme_minimal()