library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)

datos <- read_excel("/home/lsalvatore/Documents/FACULTAD/ProbabilidadYEstadistica2024/Prob_Datos_LP.xlsx", col_names = FALSE, skip=3)

datos <- datos |>
      select(   # Seleccionar las columnas que quiero conservar
             "...1", "...2", "...3", "...5", "...24", "...25", "...38", "...39", "...40", "...41", "...42", "...43", "...44", "...45", "...46", "...47", "...48", "...50",  "...56", "...57", "...58", "...59", "...60", "...111", "...112", "...113" 
         )

colnames(datos) <- c("OrdenInicial", # Cuantitativa Discreta
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
datos <- data.frame(datos)
datos_base <- datos |>
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



#Buenos Aires         CABA        Chaco      Córdoba   Corrientes   Entre Ríos      Formosa        Jujuy     La Rioja      Mendoza     Misiones 
#292          235           36           52           51           27           40           56           30           60          109 
#Río Negro   Santa Cruz     Santa Fe     Santiago      Tucumán 
#70           52           40           32           40 

#Debido a que hay una tendencia favorable a Buenos Aires en cuanto a datos, se intenta filtrar y seleccionar el mínimo de hogares por cada provincia
#En este caso, nos adaptamos a Entre rios, que tiene 27 hogares consultados, entonces hacemos un sample de 27 hogares 
filtered <- datos_base[sample( which (datos_base$Provincia == "Buenos Aires"), 27), ]
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "CABA"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Chaco"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Córdoba"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Corrientes"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Entre Ríos"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Formosa"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Jujuy"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "La Rioja"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Mendoza"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Misiones"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Río Negro"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Santa Cruz"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Santa Fe"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Santiago"), 27), ])
filtered <- rbind(filtered, datos_base[sample( which (datos_base$Provincia == "Tucumán"), 27), ])
datos_base <- filtered
#GRAFICOS

##########################################################
#SERVICIO DE AGUA#############################
##########################################################


datos_agua_RedONoRed <-  datos_base |>
         mutate(
           FormaObtencionAgua = recode(FormaObtencionAgua, "No sabe" = "No sabe", 
                                       "Con medidor en red" = "En red",
                                       "Sin medidor, informalmente" = "Fuera de la red",
                                       "Camión cisterna" = "Fuera de la red",
                                       "No posee agua, consume agua externa" = "Fuera de la red",
                                       "Agua de pozo" = "Fuera de la red",
                                       "Tanque comunitario" = "Fuera de la red"
                                       )
         )

#Se grafica primero un panorama de la cantidad de hogares sin medidor en red
RedONoRed_AguaConteo <- table(datos_agua_RedONoRed$FormaObtencionAgua)
barplot(RedONoRed_AguaConteo, cex.names = 0.45)

#Se grafica luego específicamentes las formas que tienen de abastecerse de agua
datos_agua_SinMedidorDeAgua <- datos_base %>% 
  filter(!(FormaObtencionAgua %in% c('Con medidor en red', 'No sabe')))

FormaObtencionAguaConteo <- table(datos_agua_SinMedidorDeAgua$FormaObtencionAgua)
barplot(FormaObtencionAguaConteo, cex.names = 0.45)

#Se divide por provincias las que están fuera de la red
barplot(table(datos_agua_SinMedidorDeAgua$Provincia), cex.names=0.45, main="Hogares sin medidor de agua por provincia")

#Entre las que están fuera de la red, se consume agua embotellada?
pie(table(datos_agua_SinMedidorDeAgua$SeConsumeAguaEmbotellada))

#Entre las que están fuera de la red, cuantos años hace que viven en estas condiciones?
tiempoResidenciaAniosIntervalos <- cut(datos_agua_SinMedidorDeAgua$TiempoDeResidenciaEnAños, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120))
tiempoResidenciaAniosFreq <- table(tiempoResidenciaAniosIntervalos)
barplot(tiempoResidenciaAniosFreq, main="Tiempo de residencia sin contar con medidores de agua", xlab="Tiempo de residencia en años", ylab="Frecuencia", col="blue", cex.names=0.80)


##########################################################
#SERVICIO DE GAS#############################
##########################################################
datos_gas <- datos_base
datos_gas$EnergiaParaCocina = ifelse(datos_gas$PoseeGasNaturalParaCocina, "Gas natural", 
                                     ifelse(datos_gas$NoTieneParaCocina, "No tiene", "Otro abastecimiento"))
datos_gas$EnergiaParaCalefaccion = ifelse(datos_gas$PoseeGasNaturalParaCalefaccion, "Gas natural", 
                                     ifelse(datos_gas$NoTieneParaCalefaccion, "No tiene", "Otro abastecimiento"))
datos_gas$ServicioDeGas = ifelse(datos_gas$PoseeGasNaturalParaCalefaccion | datos_gas$PoseeGasNaturalParaCocina, "Gas natural", 
                                 ifelse(datos_gas$NoTieneParaCalefaccion & datos_gas$NoTieneParaCocina, "No tiene", "Otro abastecimiento"))

#Se grafica primero un panorama de la cantidad de hogares sin gas natural para cocina
RedONoRed_GasConteo <- table(datos_gas$ServicioDeGas)
barplot(RedONoRed_GasConteo, cex.names = 0.45, main='Hogares con gas natural u otro abastecimiento')

#Se divide por provincias las que están fuera de la red
datos_gas_SinMedidorDeGas <- datos_gas %>% 
  filter(ServicioDeGas %in% c('Otro abastecimiento', 'No tiene'))
barplot(table(datos_gas_SinMedidorDeGas$Provincia), cex.names=0.45, main="Hogares sin gas natural con medidor por Provincia")

#Se grafica luego un contraste de cada tipo de abastecimiento de gas por fuera de los que tienen gas natural
tipos_abastecimiento_calor <- c()
tipos_abastecimiento_calor$garrafa = datos_gas$PoseeGarrafaParaCocina | datos_gas$PoseeGarrafaParaCalefaccion
tipos_abastecimiento_calor$electricidad = datos_gas$ElectricidadParaCocina | datos_gas$ElectricidadParaCalefaccion
tipos_abastecimiento_calor$leñacarbon = datos_gas$PoseeLeñaCarbonParaCocina | datos_gas$PoseeLeñaCarbonParaCalefaccion
tipos_abastecimiento_calor$no_tiene = datos_gas$NoTieneParaCalefaccion & datos_gas$NoTieneParaCocina

tipos_abastecimiento_calor_frame <- data.frame(tipos_abastecimiento_calor)
tipos_abastecimiento_calor_long <- pivot_longer(tipos_abastecimiento_calor_frame, cols = everything(), names_to = "Variable", values_to = "Valor")
tipos_abastecimiento_calor_long <- tipos_abastecimiento_calor_long %>%
  mutate(Valor = ifelse(Valor, "Sí", "No"))
ggplot(tipos_abastecimiento_calor_long, aes(x = Variable, fill = Valor)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Sí" = "green", "No" = "red")) +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Distribución de hogares por cada tipo de abastecimiento de gas",
       x = "Columnas",
       y = "Frecuencia",
       fill = "Valor") +
  theme_minimal()

#Entre las que están fuera de la red, cuantos años hace que viven en estas condiciones?
tiempoResidenciaAniosIntervalos <- cut(datos_gas_SinMedidorDeGas$TiempoDeResidenciaEnAños, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120))
tiempoResidenciaAniosFreq <- table(tiempoResidenciaAniosIntervalos)
barplot(tiempoResidenciaAniosFreq, main="Tiempo de residencia sin contar con medidores de gas", xlab="Tiempo de residencia en años", ylab="Frecuencia", col="blue", cex.names=0.80)

##########################################################
#SERVICIO DE GAS#############################
##########################################################
#Dividimos el atributo en 3 tipos de valores:
# En red particular, en red comunitaria, fuera de la red.
datos_luz_RedONoRed <-  datos_base |>
  mutate(
    TipoConexionElectrica = recode(TipoConexionElectrica, "Con medidor en red" = "En red particular", 
                                "Sin medidor, informalmente" = "Fuera de la red",
                                "Con medidor comunitario" = "En red comunitaria",
                                "No tiene acceso a la red eléctrica" = "Fuera de la red"
    )
  )

RedONoRed_LuzConteo <- table(datos_luz_RedONoRed$TipoConexionElectrica)
barplot(RedONoRed_LuzConteo, cex.names = 0.45)

#Se divide por provincias las que están fuera de la red
datos_SinMedidorDeLuz <- datos_luz_RedONoRed %>% 
  filter(TipoConexionElectrica %in% c('Fuera de la red'))
barplot(table(datos_SinMedidorDeLuz$Provincia), cex.names=0.45, main="Hogares sin luz corriente o de medidor por Provincia")

#Entre las que están fuera de la red, cuantos años hace que viven en estas condiciones?
#Descripción gráfica de la variable 'Tiempo de residencia en años' (cuantitativa continua) en relación a la variable categórica 'Tipo de conexión eléctrica'
#Relación entre 'Tiempo de residencia en años' y 'Tipo de conexión eléctrica' (dos variables categorícas). Esta segunda variable filtrada
tiempoResidenciaAniosIntervalos <- cut(datos_SinMedidorDeLuz$TiempoDeResidenciaEnAños, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120))
tiempoResidenciaAniosFreq <- table(tiempoResidenciaAniosIntervalos)
barplot(tiempoResidenciaAniosFreq, main="Tiempo de residencia sin contar con medidores de luz", xlab="Tiempo de residencia en años", ylab="Frecuencia", col="blue", cex.names=0.80)

##########################################################
#SERVICIO DE INTERNET#############################
##########################################################
#Analizamos los diferentes tipos de internet que tiene cada hogar
#Descripción gráfica de la variable 'Tipo de Internet' (Cualitativa en escala nominal)
datos_base_TipoDeInternetEnElHogar_Table <- table(datos_base$TipoDeInternetEnElHogar)
text(barplot(datos_base_TipoDeInternetEnElHogar_Table, main="Tipo de internet en el hogar", ylab="Cantidad de hogares", col="yellow", cex.names=0.80),
     datos_base_TipoDeInternetEnElHogar_Table / 2, labels=datos_base_TipoDeInternetEnElHogar_Table, cex=1.2, col="red")

#Se puede observar que muchos hogares en estos barrios populares no tienen internet. En qué provincias se concentra la mayor cantidad sin conectividad?
#Descripción gráfica de la variable 'Provincia' (categórica en escala nominal) en relación a la variable categórica 'Tipo de internet'
#Relación entre 'Provincia' y 'Tipo de internet' (dos variables categorícas). Esta segunda variable filtrada
datos_SinInternet <- datos_base %>% 
  filter(TipoDeInternetEnElHogar %in% c('No posee'))
datos_SinInternet_Provincia_Table <- table(datos_SinInternet$Provincia)
text(barplot(datos_SinInternet_Provincia_Table, cex.names=0.45, main="Cantidad de hogares sin internet por Provincia"),
     datos_SinInternet_Provincia_Table / 2, labels=datos_SinInternet_Provincia_Table, cex=1.2, col="red")
