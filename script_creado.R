library(dplyr)
library(readxl)
mydata <- read_excel("/home/lsalvatore/Documents/FACULTAD/ProbabilidadYEstadistica2024/Prob_Datos_LP.xlsx", col_names = FALSE, skip=3)

mydata <- mydata |>
      select(   # Seleccionar las columnas que quiero conservar
             "...1", "...2", "...3", "...5", "...24", "...25", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...50",  "...56", "...57", "...58", "...59", "...60", "...111", "...112", "...113" 
         )

colnames(mydata) <- c("OrdenInicial", # Cuantitativa Discreta
                      "Provincia", # Cualitativa Nominal
                      "Barrio", # Cualitativa Nominal
                      "TiempoDeResidenciaEnAños", # Cuantitativa Continua
                      "FormaObtencionAgua", # Cualitativa Nominal
                      "SeConsumeAguaEmbotellada", # Cualitativa Dicotómica
                      "PoseeGasNaturalParaCocina", 
                      "PoseeGarrafaParaCocina", 
                      "ElectricidadParaCocina", 
                      "PoseeLeñaCarbonParaCocina",
                      "NoTieneParaCocina",
                      "PoseeGasNaturalParaCalefaccion", 
                      "PoseeGarrafaParaCalefaccion", 
                      "ElectricidadParaCalefaccion", 
                      "PoseeLeñaCarbonParaCalefaccion",
                      "NoTieneParaCalefaccion", 
                      "NoNecesitaCalefaccionar",
                      "TipoConexionElectrica", 
                      "TipoDeInternetEnElHogar", 
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
                                  "No posee conexión a la red eléctrica en la vivienda" = "No tiene acceso a la red eléctrica"),
      TipoDeInternetEnElHogar = recode(TipoDeInternetEnElHogar, "Si inálambrico/satelital" = "Inalámbrico/Satelital",
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




#GRAFICOS

tableFormaObtencionAguaConteo <- table(mydata_limpia$FormaObtencionAgua)
barplot(tableFormaObtencionAguaConteo, cex.names = 0.45)

tableSinMedidorDeAgua <- mydata_limpia %>% 
  filter(FormaObtencionAgua %in% c('Sin medidor, informalmente'))
barplot(table(tableSinMedidorDeAgua$Provincia), cex.names=0.45)
