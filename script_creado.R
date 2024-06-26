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


#Tabla de distribución de frecuencias de la variable
#Provincia bajo la rla variable Forma Obtención
#Agua. En este caso, necesitamos ver las provincias las
#cuales tienen mayor número de hogares con servicio irregular
#de agua. Relación entre dos categóricas.

datos_Provincia_Agua_Table <- datos_base |>
  select('Provincia', 'FormaObtencionAgua')
datos_Provincia_Agua_Table <- table(datos_Provincia_Agua_Table)
df_tabla <- as.data.frame(datos_Provincia_Agua_Table)
df_suma <- df_tabla %>%
  group_by(FormaObtencionAgua) %>%
  summarise(total = sum(Freq))
df_ordenado <- df_tabla %>%
  left_join(df_suma, by = "FormaObtencionAgua") %>%
  arrange(desc(total), FormaObtencionAgua, Provincia) %>%
  select(-total)  
df_ordenado$FormaObtencionAgua <- factor(df_ordenado$FormaObtencionAgua, levels = unique(df_ordenado$FormaObtencionAgua))
datos_Provincia_Agua_Table <- xtabs(Freq ~ Provincia + FormaObtencionAgua, data = df_ordenado)
datos_Provincia_Agua_Table <- addmargins(datos_Provincia_Agua_Table)
datos_Provincia_Agua_Table

# Gráfico de torta de la variable Se consume agua
# embotellada? bajo la restricción de la variable Forma de
# Obtención Agua. Se necesita ver si de los que consumen agua
# irregular, cuantos de estos hogares consumen agua
# embotellada aparte. Relación entre dos categóricas.
pie(table(datos_agua_SinMedidorDeAgua$SeConsumeAguaEmbotellada))

#Gráfico de barras de la variable Tiempo de Residencia bajo
#la restricción de la variable Forma de Obtención Agua. En
#este caso, queremos mostrar el tiempo en el cual están sin
#contar con un servicio regular de agua.
#Relación entre una cuantitativa y una categórica.

datos_agua_SinMedidorDeAgua <- datos_base |> 
  filter(!(FormaObtencionAgua %in% c('Con medidor en red')))
tiempoResidenciaAniosIntervalos <- cut(datos_agua_SinMedidorDeAgua$TiempoDeResidenciaEnAños, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120))
tiempoResidenciaAniosFreq <- table(tiempoResidenciaAniosIntervalos)
barplot(tiempoResidenciaAniosFreq, main="Tiempo de residencia sin contar con medidores de agua", xlab="Tiempo de residencia en años", ylab="Frecuencia", col="blue", cex.names=0.80)


##########################################################
#SERVICIO DE GAS#############################
##########################################################
datos_gas <- datos_base
datos_gas$ServicioDeGas = ifelse(datos_gas$PoseeGasNaturalParaCalefaccion | datos_gas$PoseeGasNaturalParaCocina, "Gas natural", 
                                 ifelse(datos_gas$NoTieneParaCalefaccion & datos_gas$NoTieneParaCocina, "No tiene", "Otro abastecimiento"))

# Gráfico de barras de las variables Tiene gas natural, Cocina
# o Calefacción por electricidad,Tiene garrafa, Leña o Carbón y
# No tiene gas. En este caso, queremos mostrar si cada hogar
# tiene gas natural o caso contrario qué formas tienen de
# calefaccionar y cocinar.
tipos_abastecimiento_calor <- c()
tipos_abastecimiento_calor$Provincia = datos_gas$Provincia
tipos_abastecimiento_calor$gas_natural = datos_gas$PoseeGasNaturalParaCocina | datos_gas$PoseeGasNaturalParaCalefaccion
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


#Tabla de distribución de frecuencias de la variable
#Provincia bajo la restricción de la variable calculada Servicio
#De Gas. En este caso, necesitamos ver las provincias las cuales
#tienen mayor número de hogares con servicio irregular de gas,
#las que no tienen gas natural Relación entre dos categóricas


datos_gas_SinMedidorDeGas <- datos_gas %>% 
  filter(ServicioDeGas %in% c('Otro abastecimiento', 'No tiene'))
datos_gas_SinMedidorDeGas_Provincia_Table <- datos_gas_SinMedidorDeGas |>
  select('Provincia', 'ServicioDeGas')
datos_gas_SinMedidorDeGas_Provincia_Table <- table(datos_gas_SinMedidorDeGas_Provincia_Table)
df_tabla <- as.data.frame(datos_gas_SinMedidorDeGas_Provincia_Table)
df_suma <- df_tabla %>%
  group_by(ServicioDeGas) %>%
  summarise(total = sum(Freq))
df_ordenado <- df_tabla %>%
  left_join(df_suma, by = "ServicioDeGas") %>%
  arrange(desc(total), ServicioDeGas, Provincia) %>%
  select(-total)  
df_ordenado$ServicioDeGas <- factor(df_ordenado$ServicioDeGas, levels = unique(df_ordenado$ServicioDeGas))
datos_gas_SinMedidorDeGas_Provincia_Table <- xtabs(Freq ~ Provincia + ServicioDeGas, data = df_ordenado)
datos_gas_SinMedidorDeGas_Provincia_Table <- addmargins(datos_gas_SinMedidorDeGas_Provincia_Table)
datos_gas_SinMedidorDeGas_Provincia_Table

#Gráfico de barras de la variable Tiempo de Residencia bajo
#la restricción de la variable valculada Servicio de gas. En este
#caso, queremos mostrar el tiempo en el cual están sin contar
#con un servicio regular de gas.
#Relación entre una cuantitativa y una categórica
tiempoResidenciaAniosIntervalos <- cut(datos_gas_SinMedidorDeGas$TiempoDeResidenciaEnAños, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120))
tiempoResidenciaAniosFreq <- table(tiempoResidenciaAniosIntervalos)
barplot(tiempoResidenciaAniosFreq, main="Tiempo de residencia sin contar con medidores de gas", xlab="Tiempo de residencia en años", ylab="Frecuencia", col="blue", cex.names=0.80)

##########################################################
#SERVICIO DE LUZ#############################
##########################################################
#Dividimos el atributo en 3 tipos de valores:
# En red particular, en red comunitaria, fuera de la red.

#Gráfico de barras de la variable Tipo Conexión Eléctrica.
#Necesitamos ver de forma clara cuantos hogares tienen o no
#servicio corriente de luz, es decir si tienen o no medidor de luz
#particular
datos_luz_RedONoRed <-  datos_base |>
  mutate(
    TipoConexionElectrica = recode(TipoConexionElectrica, "Con medidor en red" = "En red particular", 
                                "Sin medidor, informalmente" = "Fuera de la red",
                                "Con medidor comunitario" = "En red comunitaria",
                                "No tiene acceso a la red eléctrica" = "Fuera de la red"
    )
  )
barplot(table(datos_luz_RedONoRed$TipoConexionElectrica), cex.names = 0.45)

#Tabla de distribución de frecuencias de la variable
#Provincia bajo la restricción de la variable Tipo Conexión
#Eléctrica. En este caso, necesitamos ver las provincias las
#cuales tienen mayor número de hogares con servicio irregular
#de electricidad. Relación entre dos categóricas


datos_SinMedidorDeLuz <- datos_base %>% 
  filter(!(TipoConexionElectrica %in% c('Con medidor en red')))
datos_SinMedidorDeLuz_Provincia_Table <- datos_SinMedidorDeLuz |>
  select('Provincia', 'TipoConexionElectrica')
datos_SinMedidorDeLuz_Provincia_Table <- table(datos_SinMedidorDeLuz_Provincia_Table)
datos_SinMedidorDeLuz_Provincia_Table <- addmargins(datos_SinMedidorDeLuz_Provincia_Table)
datos_SinMedidorDeLuz_Provincia_Table

#Gráfico de barras de la variable Tiempo de Residencia bajo
#la restricción de la variable Tipo Conexión Eléctrica. En este
#caso, queremos mostrar el tiempo en el cual están sin contar
#con un servicio regular de electricidad.
#Relación entre una cuantitativa y una categórica.
tiempoResidenciaAniosIntervalos <- cut(datos_SinMedidorDeLuz$TiempoDeResidenciaEnAños, breaks=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120))
tiempoResidenciaAniosFreq <- table(tiempoResidenciaAniosIntervalos)
barplot(tiempoResidenciaAniosFreq, main="Tiempo de residencia sin contar con medidores de luz", xlab="Tiempo de residencia en años", ylab="Frecuencia", col="blue", cex.names=0.80)

##########################################################
#SERVICIO DE INTERNET#############################
##########################################################
#Gráfico de barras de la variable Tipo de Internet.
#Necesitamos ver de forma clara cuantos hogares tienen o no
#tienen algún tipo de comunicación.

datos_base_TipoDeInternetEnElHogar_Table <- table(datos_base$TipoDeInternetEnElHogar)
text(barplot(datos_base_TipoDeInternetEnElHogar_Table, main="Tipo de internet en el hogar", ylab="Cantidad de hogares", col="yellow", cex.names=0.80),
     datos_base_TipoDeInternetEnElHogar_Table / 2, labels=datos_base_TipoDeInternetEnElHogar_Table, cex=1.2, col="red")

#Tabla de distribución de frecuencias de la variable
#Provincia bajo la restricción de la variable Tipo de Internet.
#En este caso, necesitamos ver las provincias las cuales tienen
#mayor número de hogares sin internet.
#Relación entre dos categóricas.


datos_Internet_Table <- datos_base %>% 
  select('Provincia', 'TipoDeInternetEnElHogar')
datos_Internet_Table <- table(datos_Internet_Table)
datos_Internet_Table <- addmargins(datos_Internet_Table)
datos_Internet_Table

##########################################################
#SERVICIO DE TRANSPORTE#############################
##########################################################

datos_transporte_table <- sort(table(datos_base$FrecuenciaTransporteColectivo), decreasing = TRUE)
text(barplot(datos_transporte_table, main="Cantidad de hogares por frecuencia de transporte", xlab="Frecuencia", col="green", cex.names=0.60),
     datos_transporte_table / 2, labels=datos_transporte_table, cex=1.2, col="red")

#Tabla de distribución de frecuencias de la variable
#Provincia bajo la restricción de la variable Frecuencia Transporte Colectivo.
#En este caso, necesitamos ver las provincias las cuales tienen
#mayor número de hogares sin internet.
#Relación entre dos categóricas.

datos_transporte_table <- datos_base %>% 
  select('Provincia', 'FrecuenciaTransporteColectivo')
datos_transporte_table <- table(datos_transporte_table)
datos_transporte_table <- addmargins(datos_transporte_table)
datos_transporte_table
