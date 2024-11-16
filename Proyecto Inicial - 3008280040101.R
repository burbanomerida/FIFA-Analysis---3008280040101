#*PROYECTO INICIAL*

#0.) PREPARACIÓN DE DATOS:
# Carga de paquetes
install.packages("tidyr")
install.packages("corrplot")
install.packages("summarytools")
install.packages("scales")
library(dplyr)
library(readr)  
library(ggplot2)
library(tidyr)
library(corrplot)
library(summarytools)
library(scales)
# Cargar cada archivo de forma individual
data_15 <- read_delim("D:/fifa2020/players_15.csv", delim = ";")
data_16 <- read_delim("D:/fifa2020/players_16.csv", delim = ";")
data_17 <- read_delim("D:/fifa2020/players_17.csv", delim = ";")
data_18 <- read_delim("D:/fifa2020/players_18.csv", delim = ";")
data_19 <- read_delim("D:/fifa2020/players_19.csv", delim = ";")
data_20 <- read_delim("D:/fifa2020/players_20.csv", delim = ";")

#1.)DISCOVERY: Canvas Data Product

#2.) DATA ANALYSIS:

#2.1) Verificar que los archivos se cargaron correctamente
head(data_15)
head(data_16)
head(data_17)
head(data_18)
head(data_19)
head(data_20)

#2.2) Observar valores y parámetros de los archivos
glimpse(data_15)
glimpse(data_16)
glimpse(data_17)
glimpse(data_18)
glimpse(data_19)
glimpse(data_20)

#2.3) ESstadística Descriptiva Preliminar:
summary(data_15)
summary(data_16)
summary(data_17)
summary(data_18)
summary(data_19)
summary(data_20)

#2.4) Verificación de Tablas Iguales: 

#Compara los nombres de las columnas de los datasets
colnames(data_15) == colnames(data_16)
colnames(data_15) == colnames(data_17)
colnames(data_15) == colnames(data_18)
colnames(data_15) == colnames(data_19)
colnames(data_15) == colnames(data_20)

# Compara las clases de las columnas entre dos dataframes
sapply(data_15, class) == sapply(data_16, class) #IGUAL
sapply(data_16, class) == sapply(data_17, class) #DISTINTO MENTALITY_COMPOSURE
sapply(data_16, class) == sapply(data_18, class) #DISTINTO MENTALITY_COMPOSURE & RELEASE_CLAUSE_EUR
sapply(data_17, class) == sapply(data_18, class) #DISTINTO RELEASE_CLAUSE_EUR
sapply(data_18, class) == sapply(data_19, class) #IGUAL
sapply(data_19, class) == sapply(data_20, class) #DIFERENTES CASI TODAS

#AJUSTAMOS LOS TIPOS DE DATOS DE LOS DATASETS EN EXCEL POWER QUERY PARA QUE SEAN NUMÉRICOS

#2.5) Combinar todos los datos en un solo dataframe
datos_combinados <- bind_rows(data_15, data_16, data_17, data_18, data_19, data_20)

#2.6)Análisis Preliminar

#Ver las primeras filas del dataset
head(datos_combinados)

# Resumen de las variables
summary(datos_combinados)

# Ver los nombres de las columnas
names(datos_combinados)

#2.7) Limpieza Preliminar:
#Eliminar duplicados y sustituir por promedios las variables que son nulol en cada tupla
datos_combinados <- datos_combinados %>%
  group_by(sofifa_id) %>%
  summarize(
    player_url = first(player_url),
    short_name = first(short_name),
    long_name = first(long_name),
    age = first(age),  # Edad de la primera fila
    dob = first(dob),
    height_cm = mean(height_cm, na.rm = TRUE),
    weight_kg = mean(weight_kg, na.rm = TRUE),
    nationality = first(nationality),
    club = first(club),
    preferred_foot = first(preferred_foot),  # Pie preferido del jugador
    overall = mean(overall, na.rm = TRUE),  # Promedio de overall
    potential = mean(potential, na.rm = TRUE),  # Promedio de potential
    value_eur = mean(value_eur, na.rm = TRUE),  # Promedio de value_eur
    wage_eur = mean(wage_eur, na.rm = TRUE),  # Promedio de wage_eur
    pace = mean(pace, na.rm = TRUE),
    shooting = mean(shooting, na.rm = TRUE),
    passing = mean(passing, na.rm = TRUE),
    dribbling = mean(dribbling, na.rm = TRUE),
    defending = mean(defending, na.rm = TRUE),
    physic = mean(physic, na.rm = TRUE),
    gk_diving = mean(gk_diving, na.rm = TRUE),
    gk_handling = mean(gk_handling, na.rm = TRUE),
    gk_kicking = mean(gk_kicking, na.rm = TRUE),
    gk_reflexes = mean(gk_reflexes, na.rm = TRUE),
    gk_speed = mean(gk_speed, na.rm = TRUE),
    gk_positioning = mean(gk_positioning, na.rm = TRUE),
    attacking_crossing = mean(attacking_crossing, na.rm = TRUE),
    attacking_finishing = mean(attacking_finishing, na.rm = TRUE),
    attacking_heading_accuracy = mean(attacking_heading_accuracy, na.rm = TRUE),
    attacking_short_passing = mean(attacking_short_passing, na.rm = TRUE),
    attacking_volleys = mean(attacking_volleys, na.rm = TRUE),
    skill_dribbling = mean(skill_dribbling, na.rm = TRUE),
    skill_curve = mean(skill_curve, na.rm = TRUE),
    skill_fk_accuracy = mean(skill_fk_accuracy, na.rm = TRUE),
    skill_long_passing = mean(skill_long_passing, na.rm = TRUE),
    skill_ball_control = mean(skill_ball_control, na.rm = TRUE),
    movement_acceleration = mean(movement_acceleration, na.rm = TRUE),
    movement_sprint_speed = mean(movement_sprint_speed, na.rm = TRUE),
    movement_agility = mean(movement_agility, na.rm = TRUE),
    movement_reactions = mean(movement_reactions, na.rm = TRUE),
    movement_balance = mean(movement_balance, na.rm = TRUE),
    power_shot_power = mean(power_shot_power, na.rm = TRUE),
    power_jumping = mean(power_jumping, na.rm = TRUE),
    power_stamina = mean(power_stamina, na.rm = TRUE),
    power_strength = mean(power_strength, na.rm = TRUE),
    power_long_shots = mean(power_long_shots, na.rm = TRUE),
    mentality_aggression = mean(mentality_aggression, na.rm = TRUE),
    mentality_interceptions = mean(mentality_interceptions, na.rm = TRUE),
    mentality_positioning = mean(mentality_positioning, na.rm = TRUE),
    mentality_vision = mean(mentality_vision, na.rm = TRUE),
    mentality_penalties = mean(mentality_penalties, na.rm = TRUE),
    mentality_composure = mean(mentality_composure, na.rm = TRUE),
    defending_marking = mean(defending_marking, na.rm = TRUE),
    defending_standing_tackle = mean(defending_standing_tackle, na.rm = TRUE),
    defending_sliding_tackle = mean(defending_sliding_tackle, na.rm = TRUE),
    goalkeeping_diving = mean(goalkeeping_diving, na.rm = TRUE),
    goalkeeping_handling = mean(goalkeeping_handling, na.rm = TRUE),
    goalkeeping_kicking = mean(goalkeeping_kicking, na.rm = TRUE),
    goalkeeping_positioning = mean(goalkeeping_positioning, na.rm = TRUE),
    goalkeeping_reflexes = mean(goalkeeping_reflexes, na.rm = TRUE),
    # Posiciones adicionales
    ls = mean(ls, na.rm = TRUE),
    st = mean(st, na.rm = TRUE),
    rs = mean(rs, na.rm = TRUE),
    lw = mean(lw, na.rm = TRUE),
    lf = mean(lf, na.rm = TRUE),
    cf = mean(cf, na.rm = TRUE),
    rf = mean(rf, na.rm = TRUE),
    rw = mean(rw, na.rm = TRUE),
    lam = mean(lam, na.rm = TRUE),
    cam = mean(cam, na.rm = TRUE),
    ram = mean(ram, na.rm = TRUE),
    lm = mean(lm, na.rm = TRUE),
    lcm = mean(lcm, na.rm = TRUE),
    cm = mean(cm, na.rm = TRUE),
    rcm = mean(rcm, na.rm = TRUE),
    rm = mean(rm, na.rm = TRUE),
    lwb = mean(lwb, na.rm = TRUE),
    ldm = mean(ldm, na.rm = TRUE),
    cdm = mean(cdm, na.rm = TRUE),
    rdm = mean(rdm, na.rm = TRUE),
    rwb = mean(rwb, na.rm = TRUE),
    lb = mean(lb, na.rm = TRUE),
    lcb = mean(lcb, na.rm = TRUE),
    cb = mean(cb, na.rm = TRUE),
    rcb = mean(rcb, na.rm = TRUE),
    rb = mean(rb, na.rm = TRUE),
    .groups = 'drop'  # Elimina la agrupación después de la agregación
  )
#2.8. ESTADISITICA DESCRPTIVA:
summary(datos_combinados)

#2.9. ANÁLISIS DE CORRELACIÓN:
# Calcula la matriz de correlación usando las columnas correctas
cor_matrix <- cor(datos_combinados[, c("age", "height_cm", "weight_kg", "overall", "value_eur", "wage_eur")])
# Graficar la matriz de correlación
corrplot(cor_matrix, method = "circle")

#2.10. ANÁLISIS DE VALORES ATÍPICOS:
# Análisis de valores atípicos con boxplots para las variables 'age', 'height_cm', 'weight_kg'

# Boxplot para la Edad
boxplot(datos_combinados$age, main = "Distribución de la Edad", ylab = "Edad", col = "lightblue")

# Boxplot para la Altura
boxplot(datos_combinados$height_cm, main = "Distribución de la Altura", ylab = "Altura (cm)", col = "lightgreen")

# Boxplot para el Peso
boxplot(datos_combinados$weight_kg, main = "Distribución del Peso", ylab = "Peso (kg)", col = "lightcoral")

# Identificar valores atípicos mediante el cálculo de los valores umbrales de cada variable

# Valores atípicos para la Edad
age_outliers <- boxplot.stats(datos_combinados$age)$out
print("Valores atípicos en Edad:")
print(age_outliers)

# Valores atípicos para la Altura
height_outliers <- boxplot.stats(datos_combinados$height_cm)$out
print("Valores atípicos en Altura:")
print(height_outliers)

# Valores atípicos para el Peso
weight_outliers <- boxplot.stats(datos_combinados$weight_kg)$out
print("Valores atípicos en Peso:")
print(weight_outliers)

#2.11) Distribuciones de Valor de Mercado y Salarios

# Histogramas de Value y Wage
hist(datos_combinados$value_eur, breaks = 30, main = "Distribución de Valor (€)", xlab = "Valor (€)", col = "blue")
hist(datos_combinados$wage_eur, breaks = 30, main = "Distribución de Salarios (€)", xlab = "Salarios (€)", col = "green")


#2.12) Clasificacion de Jugadores Overall comparada con otras Varibables:
# Boxplot de Overall según el Pie Preferido
ggplot(datos_combinados, aes(x = preferred_foot, y = overall, fill = preferred_foot)) +
  geom_boxplot() +
  labs(title = "Overall según el Pie Preferido", x = "Pie Preferido", y = "Overall")

# Comparar el Overall promedio por Año
datos_combinados$year <- substr(datos_combinados$dob, 1, 4)  # Extraer el año de nacimiento
mean_overall_by_year <- datos_combinados %>%
  group_by(year) %>%
  summarize(mean_overall = mean(overall, na.rm = TRUE))

ggplot(mean_overall_by_year, aes(x = year, y = mean_overall)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "red") +
  labs(title = "Promedio de Overall por Año", x = "Año", y = "Overall Promedio")

# Analizar jugadores con valores extremos en Overall
outliers_overall <- datos_combinados %>% filter(overall > quantile(overall, 0.998))
print("Jugadores con Overall excepcional:")
print(outliers_overall)

# Relación entre Edad, Overall y Valor
ggplot(datos_combinados, aes(x = age, y = value_eur, color = overall)) +
  geom_point(alpha = 0.6) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Relación entre Edad, Valor y Overall", x = "Edad", y = "Valor (€)")



#3.) DATA CLEANING:

#Revisión de valores nulos
summary(datos_combinados)  # Ver el resumen de las columnas para encontrar valores nulos

#Imputar valores nulos con la media para cada columna
datos_combinados <- datos_combinados %>%
  mutate(
    overall = ifelse(is.na(overall), mean(overall, na.rm = TRUE), overall),
    potential = ifelse(is.na(potential), mean(potential, na.rm = TRUE), potential),
    value_eur = ifelse(is.na(value_eur), mean(value_eur, na.rm = TRUE), value_eur),
    wage_eur = ifelse(is.na(wage_eur), mean(wage_eur, na.rm = TRUE), wage_eur),
    pace = ifelse(is.na(pace), mean(pace, na.rm = TRUE), pace),
    shooting = ifelse(is.na(shooting), mean(shooting, na.rm = TRUE), shooting),
    passing = ifelse(is.na(passing), mean(passing, na.rm = TRUE), passing),
    dribbling = ifelse(is.na(dribbling), mean(dribbling, na.rm = TRUE), dribbling),
    defending = ifelse(is.na(defending), mean(defending, na.rm = TRUE), defending),
    physic = ifelse(is.na(physic), mean(physic, na.rm = TRUE), physic),
    gk_diving = ifelse(is.na(gk_diving), mean(gk_diving, na.rm = TRUE), gk_diving),
    gk_handling = ifelse(is.na(gk_handling), mean(gk_handling, na.rm = TRUE), gk_handling),
    gk_kicking = ifelse(is.na(gk_kicking), mean(gk_kicking, na.rm = TRUE), gk_kicking),
    gk_reflexes = ifelse(is.na(gk_reflexes), mean(gk_reflexes, na.rm = TRUE), gk_reflexes),
    gk_speed = ifelse(is.na(gk_speed), mean(gk_speed, na.rm = TRUE), gk_speed),
    gk_positioning = ifelse(is.na(gk_positioning), mean(gk_positioning, na.rm = TRUE), gk_positioning),
    attacking_crossing = ifelse(is.na(attacking_crossing), mean(attacking_crossing, na.rm = TRUE), attacking_crossing),
    attacking_finishing = ifelse(is.na(attacking_finishing), mean(attacking_finishing, na.rm = TRUE), attacking_finishing),
    attacking_heading_accuracy = ifelse(is.na(attacking_heading_accuracy), mean(attacking_heading_accuracy, na.rm = TRUE), attacking_heading_accuracy),
    attacking_short_passing = ifelse(is.na(attacking_short_passing), mean(attacking_short_passing, na.rm = TRUE), attacking_short_passing),
    attacking_volleys = ifelse(is.na(attacking_volleys), mean(attacking_volleys, na.rm = TRUE), attacking_volleys),
    skill_dribbling = ifelse(is.na(skill_dribbling), mean(skill_dribbling, na.rm = TRUE), skill_dribbling),
    skill_curve = ifelse(is.na(skill_curve), mean(skill_curve, na.rm = TRUE), skill_curve),
    skill_fk_accuracy = ifelse(is.na(skill_fk_accuracy), mean(skill_fk_accuracy, na.rm = TRUE), skill_fk_accuracy),
    skill_long_passing = ifelse(is.na(skill_long_passing), mean(skill_long_passing, na.rm = TRUE), skill_long_passing),
    skill_ball_control = ifelse(is.na(skill_ball_control), mean(skill_ball_control, na.rm = TRUE), skill_ball_control),
    movement_acceleration = ifelse(is.na(movement_acceleration), mean(movement_acceleration, na.rm = TRUE), movement_acceleration),
    movement_sprint_speed = ifelse(is.na(movement_sprint_speed), mean(movement_sprint_speed, na.rm = TRUE), movement_sprint_speed),
    movement_agility = ifelse(is.na(movement_agility), mean(movement_agility, na.rm = TRUE), movement_agility),
    movement_reactions = ifelse(is.na(movement_reactions), mean(movement_reactions, na.rm = TRUE), movement_reactions),
    movement_balance = ifelse(is.na(movement_balance), mean(movement_balance, na.rm = TRUE), movement_balance),
    power_shot_power = ifelse(is.na(power_shot_power), mean(power_shot_power, na.rm = TRUE), power_shot_power),
    power_jumping = ifelse(is.na(power_jumping), mean(power_jumping, na.rm = TRUE), power_jumping),
    power_stamina = ifelse(is.na(power_stamina), mean(power_stamina, na.rm = TRUE), power_stamina),
    power_strength = ifelse(is.na(power_strength), mean(power_strength, na.rm = TRUE), power_strength),
    power_long_shots = ifelse(is.na(power_long_shots), mean(power_long_shots, na.rm = TRUE), power_long_shots),
    mentality_aggression = ifelse(is.na(mentality_aggression), mean(mentality_aggression, na.rm = TRUE), mentality_aggression),
    mentality_interceptions = ifelse(is.na(mentality_interceptions), mean(mentality_interceptions, na.rm = TRUE), mentality_interceptions),
    mentality_positioning = ifelse(is.na(mentality_positioning), mean(mentality_positioning, na.rm = TRUE), mentality_positioning),
    mentality_vision = ifelse(is.na(mentality_vision), mean(mentality_vision, na.rm = TRUE), mentality_vision),
    mentality_penalties = ifelse(is.na(mentality_penalties), mean(mentality_penalties, na.rm = TRUE), mentality_penalties),
    mentality_composure = ifelse(is.na(mentality_composure), mean(mentality_composure, na.rm = TRUE), mentality_composure),
    defending_marking = ifelse(is.na(defending_marking), mean(defending_marking, na.rm = TRUE), defending_marking),
    defending_standing_tackle = ifelse(is.na(defending_standing_tackle), mean(defending_standing_tackle, na.rm = TRUE), defending_standing_tackle),
    defending_sliding_tackle = ifelse(is.na(defending_sliding_tackle), mean(defending_sliding_tackle, na.rm = TRUE), defending_sliding_tackle),
    goalkeeping_diving = ifelse(is.na(goalkeeping_diving), mean(goalkeeping_diving, na.rm = TRUE), goalkeeping_diving),
    goalkeeping_handling = ifelse(is.na(goalkeeping_handling), mean(goalkeeping_handling, na.rm = TRUE), goalkeeping_handling),
    goalkeeping_kicking = ifelse(is.na(goalkeeping_kicking), mean(goalkeeping_kicking, na.rm = TRUE), goalkeeping_kicking),
    goalkeeping_positioning = ifelse(is.na(goalkeeping_positioning), mean(goalkeeping_positioning, na.rm = TRUE), goalkeeping_positioning),
    goalkeeping_reflexes = ifelse(is.na(goalkeeping_reflexes), mean(goalkeeping_reflexes, na.rm = TRUE), goalkeeping_reflexes),
    # Posiciones adicionales
    ls = ifelse(is.na(ls), mean(ls, na.rm = TRUE), ls),
    st = ifelse(is.na(st), mean(st, na.rm = TRUE), st),
    rs = ifelse(is.na(rs), mean(rs, na.rm = TRUE), rs),
    lw = ifelse(is.na(lw), mean(lw, na.rm = TRUE), lw),
    lf = ifelse(is.na(lf), mean(lf, na.rm = TRUE), lf),
    cf = ifelse(is.na(cf), mean(cf, na.rm = TRUE), cf),
    rf = ifelse(is.na(rf), mean(rf, na.rm = TRUE), rf),
    rw = ifelse(is.na(rw), mean(rw, na.rm = TRUE), rw),
    lam = ifelse(is.na(lam), mean(lam, na.rm = TRUE), lam),
    cam = ifelse(is.na(cam), mean(cam, na.rm = TRUE), cam),
    ram = ifelse(is.na(ram), mean(ram, na.rm = TRUE), ram),
    lm = ifelse(is.na(lm), mean(lm, na.rm = TRUE), lm),
    lcm = ifelse(is.na(lcm), mean(lcm, na.rm = TRUE), lcm),
    cm = ifelse(is.na(cm), mean(cm, na.rm = TRUE), cm),
    rcm = ifelse(is.na(rcm), mean(rcm, na.rm = TRUE), rcm),
    rm = ifelse(is.na(rm), mean(rm, na.rm = TRUE), rm),
    lwb = ifelse(is.na(lwb), mean(lwb, na.rm = TRUE), lwb),
    ldm = ifelse(is.na(ldm), mean(ldm, na.rm = TRUE), ldm),
    cdm = ifelse(is.na(cdm), mean(cdm, na.rm = TRUE), cdm),
    rdm = ifelse(is.na(rdm), mean(rdm, na.rm = TRUE), rdm),
    rwb = ifelse(is.na(rwb), mean(rwb, na.rm = TRUE), rwb),
    lb = ifelse(is.na(lb), mean(lb, na.rm = TRUE), lb),
    lcb = ifelse(is.na(lcb), mean(lcb, na.rm = TRUE), lcb),
    cb = ifelse(is.na(cb), mean(cb, na.rm = TRUE), cb),
    rcb = ifelse(is.na(rcb), mean(rcb, na.rm = TRUE), rcb),
    rb = ifelse(is.na(rb), mean(rb, na.rm = TRUE), rb)
  )


#Identificación de duplicados
# Ver si hay filas duplicadas basadas en la columna 'sofifa_id'
datos_combinados <- datos_combinados %>%
  distinct(sofifa_id, .keep_all = TRUE)  # Elimina duplicados basados en 'sofifa_id'
# Verificación de consistencia de formatos (asegurando que todas las columnas son numéricas)
datos_combinados <- datos_combinados %>%
  mutate(
    # Verificar y convertir columnas numéricas
    overall = as.numeric(overall),
    potential = as.numeric(potential),
    value_eur = as.numeric(value_eur),
    wage_eur = as.numeric(wage_eur),
    pace = as.numeric(pace),
    shooting = as.numeric(shooting),
    passing = as.numeric(passing),
    dribbling = as.numeric(dribbling),
    defending = as.numeric(defending),
    physic = as.numeric(physic),
    gk_diving = as.numeric(gk_diving),
    gk_handling = as.numeric(gk_handling),
    gk_kicking = as.numeric(gk_kicking),
    gk_reflexes = as.numeric(gk_reflexes),
    gk_speed = as.numeric(gk_speed),
    gk_positioning = as.numeric(gk_positioning),
    attacking_crossing = as.numeric(attacking_crossing),
    attacking_finishing = as.numeric(attacking_finishing),
    attacking_heading_accuracy = as.numeric(attacking_heading_accuracy),
    attacking_short_passing = as.numeric(attacking_short_passing),
    attacking_volleys = as.numeric(attacking_volleys),
    skill_dribbling = as.numeric(skill_dribbling),
    skill_curve = as.numeric(skill_curve),
    skill_fk_accuracy = as.numeric(skill_fk_accuracy),
    skill_long_passing = as.numeric(skill_long_passing),
    skill_ball_control = as.numeric(skill_ball_control),
    movement_acceleration = as.numeric(movement_acceleration),
    movement_sprint_speed = as.numeric(movement_sprint_speed),
    movement_agility = as.numeric(movement_agility),
    movement_reactions = as.numeric(movement_reactions),
    movement_balance = as.numeric(movement_balance),
    power_shot_power = as.numeric(power_shot_power),
    power_jumping = as.numeric(power_jumping),
    power_stamina = as.numeric(power_stamina),
    power_strength = as.numeric(power_strength),
    power_long_shots = as.numeric(power_long_shots),
    mentality_aggression = as.numeric(mentality_aggression),
    mentality_interceptions = as.numeric(mentality_interceptions),
    mentality_positioning = as.numeric(mentality_positioning),
    mentality_vision = as.numeric(mentality_vision),
    mentality_penalties = as.numeric(mentality_penalties),
    mentality_composure = as.numeric(mentality_composure),
    defending_marking = as.numeric(defending_marking),
    defending_standing_tackle = as.numeric(defending_standing_tackle),
    defending_sliding_tackle = as.numeric(defending_sliding_tackle),
    goalkeeping_diving = as.numeric(goalkeeping_diving),
    goalkeeping_handling = as.numeric(goalkeeping_handling),
    goalkeeping_kicking = as.numeric(goalkeeping_kicking),
    goalkeeping_positioning = as.numeric(goalkeeping_positioning),
    goalkeeping_reflexes = as.numeric(goalkeeping_reflexes),
    # Posiciones adicionales (verificando el formato numérico)
    ls = as.numeric(ls),
    st = as.numeric(st),
    rs = as.numeric(rs),
    lw = as.numeric(lw),
    lf = as.numeric(lf),
    cf = as.numeric(cf),
    rf = as.numeric(rf),
    rw = as.numeric(rw),
    lam = as.numeric(lam),
    cam = as.numeric(cam),
    ram = as.numeric(ram),
    lm = as.numeric(lm),
    lcm = as.numeric(lcm),
    cm = as.numeric(cm),
    rcm = as.numeric(rcm),
    rm = as.numeric(rm),
    lwb = as.numeric(lwb),
    ldm = as.numeric(ldm),
    cdm = as.numeric(cdm),
    rdm = as.numeric(rdm),
    rwb = as.numeric(rwb),
    lb = as.numeric(lb),
    lcb = as.numeric(lcb),
    cb = as.numeric(cb),
    rcb = as.numeric(rcb),
    rb = as.numeric(rb)
  )

# Lista de las 10 columnas más importantes para aplicar la detección de outliers
columnas_importantes <- c("overall", "potential", "value_eur", "wage_eur", 
                          "pace", "shooting", "passing", "dribbling", 
                          "defending", "physic")

# Función para detectar y reemplazar outliers con NA usando IQR
filtrar_outliers <- function(columna) {
  Q1 <- quantile(columna, 0.25, na.rm = TRUE)
  Q3 <- quantile(columna, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  columna[columna < limite_inferior | columna > limite_superior] <- NA  # Reemplazar outliers con NA
  return(columna)
}

# Aplicar la función de filtrado de outliers solo a las columnas seleccionadas
datos_combinados[columnas_importantes] <- lapply(datos_combinados[columnas_importantes], filtrar_outliers)


#4.) DATA WRANGLING
# Creación de nuevas variables
# Crear una nueva variable para categorizar jugadores según su altura
datos_combinados$altura_categoria <- ifelse(datos_combinados$height_cm > 180, "Alto", "Bajo")

# Crear una variable categórica para el valor del jugador
datos_combinados$valor_categoria <- ifelse(datos_combinados$value_eur > 10^6, "Alto valor", "Bajo valor")

# Restructuración de los datos de ancho a largo
#Usaremos gather para transformar las columnas de las estadísticas (pace, shooting, etc.) a formato largo
datos_largos <- datos_combinados %>%
  select(short_name, overall, pace, shooting, passing, dribbling, defending, physic) %>%
  gather(key = "atributo", value = "valor", -short_name)

#Manejo de valores faltantes
# Reemplazar los valores NA con la media de la columna para las columnas numéricas
datos_combinados <- datos_combinados %>%
  mutate(across(c(overall, potential, value_eur, wage_eur, pace, shooting, passing, dribbling, defending, physic),
                ~replace(., is.na(.), mean(., na.rm = TRUE))))

#Normalización y escalado de datos
# Normalización de las columnas numéricas para que todas tengan la misma escala
datos_combinados$overall_normalizado <- rescale(datos_combinados$overall)
datos_combinados$pace_normalizado <- rescale(datos_combinados$pace)
datos_combinados$shooting_normalizado <- rescale(datos_combinados$shooting)
datos_combinados$passing_normalizado <- rescale(datos_combinados$passing)
datos_combinados$dribbling_normalizado <- rescale(datos_combinados$dribbling)

#Verificación de consistencia en los formatos de las columnas
# Comprobar y asegurar que las columnas 'sofifa_id' y 'height_cm' están en los formatos correctos
datos_combinados$sofifa_id <- as.character(datos_combinados$sofifa_id)
datos_combinados$height_cm <- as.numeric(datos_combinados$height_cm)

#Verificación del conjunto de datos final
# Mostrar las primeras filas para revisar cómo quedó el dataframe
head(datos_combinados)

#Guardar el dataframe limpio en un archivo CSV
write.csv(datos_combinados, 
          "C:/Users/carlo/OneDrive/Escritorio/Usac/2024/Posgrado/Cuarto Trimestre/Introducción al Análisis de Datos/Proyecto Inicial/datos_combinados_limpios.csv", 
          row.names = FALSE)

