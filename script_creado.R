library(dplyr)
library(readxl)
mydata <- read_excel("/home/lsalvatore/Documents/FACULTAD/ProbabilidadYEstadistica2024/Prob_Datos_LP.xlsx", col_names = FALSE, skip=3)

mydata <- mydata |>
      select(   # Seleccionar las columnas que quiero conservar
             "...1", "...2", "...3", "...19", "...20", "...21", "...22", "...23", "...24", "...25", "...26", "...28", "...43", "...44", "...45", "...46", "...47", "...48", "...50",  "...54", "...55", "...56", "...57", "...58", "...59", "...60", "...111", "...112", "...113" 
         )

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
      NoNecesitaCalefaccionar = ifelse(NoNecesitaCalefaccionar == 'No necesito calefaccionar mi vivienda en ninguna época del año' & !is.na(NoNecesitaCalefaccionar), 1, 0),
      # Recodifico las etiquetas de una variable categórica
      FormaObtencionAgua = recode(FormaObtencionAgua, "No sabe" = "No sabe",
                       "A través de una conexión con medidor a la red pública" = "Con medidor en red",
                       "A través de una conexión sin medidor, es decir “informalmente”, sea a través de una conexión directa a la red pública o a través de una conexión indirecta a través de un vecinx “informalmente”" = "Sin medidor, informalmente",
                       "A través de un camión cisterna" = "Camión cisterna",
                       "No poseo agua dentro de la vivienda y/o tengo que acarrear desde fuera del terreno en que se ubica mi vivienda"  = "No posee agua, consume agua externa",
                       "A través de un pozo" = "Agua de pozo",
                       "Conexión a un tanque comunitario" = "Tanque comunitario"),
      PresionAgua = factor(PresionAgua, levels = c("Muy débil", "Débil", "Buena")),
      LitrosAlmacenamientoAgua = factor(LitrosAlmacenamientoAgua, levels = c("Menos de 200 lts", "200 a 500 lts", "Más de 500 lts")),
      TipoConexionElectrica = recode(TipoConexionElectrica, "Conexión a través de un medidor a la red eléctrica" = "Con medidor en red",
                                  "Conexión sin medidor a una red eléctrica (“informal”)" = "Sin medidor, informalmente",
                                  "Conexión a través de un medidor comunitario a la red eléctrica" = "Con medidor comunitario",
                                  "No posee conexión a la red eléctrica en la vivienda" = "No tiene acceso a la red eléctrica"),
      EsFrecuenteLosCortesEnVerano = factor(EsFrecuenteLosCortesEnVerano, levels = c("Más de 4 cortes mensuales", "Por lo menos 3 cortes en el mes",  "Por lo menos 2 cortes en el mes", "Por lo menos 1 corte en el mes", "No son frecuentes")),
      EsFrecuenteLosCortesEnInvierno = factor(EsFrecuenteLosCortesEnInvierno, levels = c("Más de 4 cortes mensuales", "Por lo menos 3 cortes en el mes",  "Por lo menos 2 cortes en el mes", "Por lo menos 1 corte en el mes", "No son frecuentes")),
      PoseeServicioInternatBandaAncha = recode(PoseeServicioInternatBandaAncha, "Si inálambrico/satelital" = "Inalámbrico/Satelital",
                                  "No poseo internet de banda ancha" = "No posee",
                                  "Si a través de cable (coaxial o ADSL)" = "Cableado (Coaxil/ADSL)",
                                  "Si a través de fibra óptica" = "Fibra Optica",
                                  "Sí pero no sé qué tipo de servicio tengo en mi vivienda"  = "No sabe"),
      FrecuenciaTransporteColectivo = factor(FrecuenciaTransporteColectivo, levels = c("1 colectivo cada dos horas", "1 colectivo por hora", "1 colectivo cada 30 minutos", "Menos de 30 minutos entre cada colectivo")),
      AccesoBicicleta = factor(AccesoBicicleta, levels = c("No", "Sí, pero están a más de 1 km de distancia", "Sí, a menos de 1 km")),
      AccesoBicicleta = recode(AccesoBicicleta, "No" = "No tiene",
                                     "Sí, pero están a más de 1 km de distancia" = "A más de 1km de distancia",
                                     "Sí, a menos de 1 km" = "A menos de 1km de distancia"
      
       )
)

mydata_patagonia <-mydata_limpia %>%
  filter(Provincia == "Río Negro" | Provincia == "Santa Cruz")
