library(dplyr)
library(readxl)
mydata <- read_excel("/home/lsalvatore/Documents/FACULTAD/ProbabilidadYEstadistica2024/Prob_Datos_LP1.xlsx", col_names = FALSE, skip=3)

colnames(mydata) <- c("OrdenInicial", # Cuantitativa Discreta
                      "Provincia", # Cualitativa Nominal
                      "Barrio", # Cualitativa Nominal
                      "TipoVivienda", # Cualitativa Nominal
                      "TieneContratoAlquiler",  # Cualitativa Dicotómica
                      "CostoAlquiler", # Cuantitativa Continua
                      "CuantoAumentoAlquilerUltAño", # Cuantitativa Continua
                      "PorcentajeAumentoAlquiler",  # Cuantitativa Continua
                      "FormaObtencionAgua", # Cualitativa Nominal
                      "SeConsumeAguaEmbotellada", # Cualitativa Dicotómica
                      "PresionAgua", 
                      "LitrosAlmacenamientoAgua", 
                      "PoseeGasNatural", 
                      "PoseeGarrafa", 
                      "CocinaYCalefaccionPorElectricidad", 
                      "PoseeLeñaCarbon",
                      "NoPoseeGas", 
                      "NoNecesitaCalefaccionar",
                      "TipoConexionElectrica", 
                      "EsFrecuenteLosCortesEnVerano", 
                      "EsFrecuenteLosCortesEnInvierno", 
                      "PoseeServicioInternatBandaAncha", 
                      "HayAlMenosUnCelularConInternet",
                      "N_Abonos", 
                      "N_Computadoras", 
                      "N_Celulares", 
                      "FrecuenciaTransporteColectivo", 
                      "FrecuenciaDisparDiaNoche", 
                      "AccesoBicicleta")
mydata <- data.frame(mydata)
mydata_limpia <- mydata |>
    mutate(
      PoseeGasNatural = ifelse(PoseeGasNatural == 'Gas natural (red de gas)' & !is.na(PoseeGasNatural), 1, 0),
      PoseeGarrafa = ifelse(PoseeGarrafa == 'Gas envasado (garrafa)'& !is.na(PoseeGarrafa), 1, 0),
      CocinaYCalefaccionPorElectricidad = ifelse(CocinaYCalefaccionPorElectricidad == 'Electricidad'& !is.na(CocinaYCalefaccionPorElectricidad), 1, 0),
      PoseeLeñaCarbon = ifelse(PoseeLeñaCarbon == 'Leña/Carbón' & !is.na(PoseeLeñaCarbon), 1, 0),
      NoPoseeGas = ifelse(NoPoseeGas == 'No tengo para calefaccionar mi vivienda' & !is.na(NoPoseeGas), 1, 0),
      NoNecesitaCalefaccionar = ifelse(NoNecesitaCalefaccionar == 'No necesito calefaccionar mi vivienda en ninguna época del año' & !is.na(NoNecesitaCalefaccionar), 1, 0)
      )

