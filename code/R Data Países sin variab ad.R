# Cargar las librerías necesarias
library(readxl)   # Para leer archivos Excel
library(dplyr)    # Para manipulación de datos

# -------------------- 1. Cargar los datos --------------------

# Leer la base de datos de países con información general
country_annotation <- read.csv("C:/Users/victo/Downloads/archive/country_annotation.csv", sep=";")

# Leer la base de datos de población mundial
Countries_in_the_world_by_population_2025_ <- read_excel("Countries in the world by population (2025).xlsx")

# -------------------- 2. Preparar y unir las bases de datos --------------------

# Renombrar la columna 'Country' en la base de población para que coincida con 'name'
Countries_in_the_world_by_population_2025_ <- Countries_in_the_world_by_population_2025_ %>%
  rename(name = Country)

# Unir ambas bases de datos por el nombre del país ('name')
country_annotation <- country_annotation %>%
  left_join(Countries_in_the_world_by_population_2025_, by = "name")

# Contar y mostrar cuántos países quedaron sin datos de población
na_count <- sum(is.na(country_annotation$Population))
cat("Cantidad de países sin población tras la unión:", na_count, "\n")

# Identificar los países que no tienen datos de población
countries_with_na <- country_annotation %>%
  filter(is.na(Population)) %>%
  select(name)

# Mostrar los países con NA en Population
View(countries_with_na)

# -------------------- 3. Convertir la columna Population a numérico --------------------

# La columna Population contiene comas como separadores de miles, lo que la hace texto
# Eliminamos las comas y convertimos Population a numérico
country_annotation <- country_annotation %>%
  mutate(Population = as.numeric(gsub(",", "", Population)))

# Verificar que la conversión fue exitosa
str(country_annotation$Population)  # Debe mostrar "num"
summary(country_annotation$Population)  # Revisar estadísticas de los valores

# -------------------- 4. Imputar manualmente la población de ciertos países --------------------

# Lista de países con valores de población corregidos manualmente
corrections <- data.frame(
  name = c("Democratic Republic of the Congo", "Republic of the Congo", "Cape Verde", 
           "Czech Republic", "Faroe Islands", "Myanmar [Burma]", "Palestine", 
           "São Tomé and Príncipe", "East Timor", "Wallis and Futuna", "Ivory Coast"),
  Population = c(109276265, 6332961, 524877, 10735859, 55400, 
                 54500091, 5495443, 235536, 1400638, 11277, 31934230)
)

# Unir la base de datos con las correcciones manuales y reemplazar los valores NA
country_annotation <- country_annotation %>%
  left_join(corrections, by = "name", suffix = c("_original", "_manual")) %>%
  mutate(Population = coalesce(Population_original, Population_manual)) %>%
  select(-Population_original, -Population_manual)

# Verificar que los valores manuales fueron asignados correctamente
country_annotation %>%
  filter(name %in% corrections$name) %>%
  select(name, Population)

# Contar nuevamente los países con NA en Population
na_count_after <- sum(is.na(country_annotation$Population))
cat("Cantidad de países sin población después de la corrección manual:", na_count_after, "\n")

# Ver los países que aún tienen NA en Population
countries_with_na <- country_annotation %>%
  filter(is.na(Population)) %>%
  select(name)

# Mostrar los países con NA en Population
View(countries_with_na)

# -------------------- 5. Eliminar los países sin datos de población --------------------

# Filtrar la base de datos para eliminar los países que aún tienen NA en Population
country_annotation <- country_annotation %>%
  filter(!is.na(Population))

# Verificar que ya no quedan NAs en Population
na_count_final <- sum(is.na(country_annotation$Population))
cat("Cantidad de países sin población después de eliminar los NA:", na_count_final, "\n")

# Ver cuántos países quedaron en la base final
cat("Número final de países en la base de datos:", nrow(country_annotation), "\n")

# -------------------- 6. Guardar la base de datos final --------------------

# Guardar la base de datos final sin países con NA en Population
write.csv(country_annotation, "C:/Users/victo/Downloads/country_annotation_final.csv", row.names = FALSE)

# Mensaje final
cat("La base de datos final ha sido guardada exitosamente.\n")
