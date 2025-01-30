# Cargar las bibliotecas necesarias
library(dplyr)
library(readxl)
library(readr)

# Leer la tabla de las ciudades más pobladas
world.city.listing.table <- read.csv("C:/Users/victo/Downloads/world-city-listing-table.csv", sep=";")
# Leer la tabla de anotaciones de países
country_annotation <- read_delim("C:/Users/victo/Downloads/archive/country_annotation.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Renombrar las columnas para un mejor entendimiento en las uniones
country_annotation <- country_annotation %>%
  rename(country = name)

# Unir las tablas por el nombre del país
final_table <- left_join(world.city.listing.table, country_annotation %>% select(country, continent), by = "country")

# Guardar el resultado en un nuevo archivo CSV
write.csv(final_table, "C:/Users/victo/Downloads/enriched-world-city-listing.csv", row.names = FALSE)

# Mostrar la nueva tabla en RStudio para verificar
View(final_table)

# Verificar la cantidad de valores nulos en la columna de continente
num_nulos <- sum(is.na(final_table$continent))
print(paste("Número de valores nulos en la columna de continente:", num_nulos))

# Ver las filas donde el continente es nulo para saber qué ciudades tienen ese problema
ciudades_con_nulos <- final_table %>% 
  filter(is.na(continent)) %>% 
  select(city, country)

# Mostrar las ciudades con valores nulos
if(nrow(ciudades_con_nulos) > 0) {
  print("Ciudades con valores nulos en la columna de continente:")
  print(ciudades_con_nulos)
} else {
  print("No hay valores nulos en la columna de continente.")
}

# Actualizar manualmente los continentes para los países específicos con valores nulos
final_table <- final_table %>% 
  mutate(continent = case_when(
    country == "DR Congo" & is.na(continent) ~ "Africa",
    country == "Myanmar" & is.na(continent) ~ "Asia",
    country == "Haiti" & is.na(continent) ~ "North America",
    country == "Puerto Rico" & is.na(continent) ~ "North America",
    country == "Cuba" & is.na(continent) ~ "North America",
    TRUE ~ continent
  ))

# Guardar el archivo actualizado con los continentes imputados
write.csv(final_table, "C:/Users/victo/Downloads/enriched-world-city-listing-updated.csv", row.names = FALSE)

# Leer el archivo actualizado y contar nuevamente los valores nulos
final_table_updated <- read.csv("C:/Users/victo/Downloads/enriched-world-city-listing-updated.csv")
num_nulos_post_imputacion <- sum(is.na(final_table_updated$continent))
print(paste("Número de valores nulos en la columna de continente después de la imputación:", num_nulos_post_imputacion))

# Mostrar el DataFrame actualizado
View(final_table_updated)

################################### JUPYTER PARA TRANSLATION Y BACK TRANSLATION ##########################################################}

library(readxl)
df <- read_excel("Back Transalation.xlsx")
View(df)
colnames(df)

# Función para calcular la precisión
calculate_accuracy <- function(original, translated) {
  correct <- sum(tolower(original) == tolower(translated), na.rm = TRUE) # Ignora NA y compara en minúsculas
  total <- length(na.omit(translated)) # Cuenta solo los no-NA
  accuracy <- (correct / total) * 100
  return(accuracy)
}

# Aplicar la función para calcular la precisión de cada idioma
accuracy_spanish <- calculate_accuracy(df[["city"]], df[["city_from_Spanish_to_English"]])
accuracy_chinese <- calculate_accuracy(df[["city"]], df[["city_from_Chinese_to_English"]])
accuracy_arabic <- calculate_accuracy(df[["city"]], df[["city_from_Arabic_to_English"]])
accuracy_indonesian <- calculate_accuracy(df[["city"]], df[["city_from_Indonesian_to_English"]])
accuracy_swahili <- calculate_accuracy(df[["city"]], df[["city_from_Swahili_to_English"]])
accuracy_tamil <- calculate_accuracy(df[["city"]], df[["city_from_Tamil_to_English"]])
accuracy_turkish <- calculate_accuracy(df[["city"]], df[["city_from_Turkish_to_English"]])
accuracy_finnish <- calculate_accuracy(df[["city"]], df[["city_from_Finnish_to_English"]])
accuracy_japanese <- calculate_accuracy(df[["city"]], df[["city_from_Japanese_to_English"]])
accuracy_korean <- calculate_accuracy(df[["city"]], df[["city_from_Korean_to_English"]])
accuracy_vietnamese <- calculate_accuracy(df[["city"]], df[["city_from_Vietnamese_to_English"]])
accuracy_thai <- calculate_accuracy(df[["city"]], df[["city_from_Thai_to_English"]])

# Imprimir los resultados para cada idioma
cat("Accuracy for Spanish:", accuracy_spanish, "%\n")
cat("Accuracy for Chinese:", accuracy_chinese, "%\n")
cat("Accuracy for Arabic:", accuracy_arabic, "%\n")
cat("Accuracy for Indonesian:", accuracy_indonesian, "%\n")
cat("Accuracy for Swahili:", accuracy_swahili, "%\n")
cat("Accuracy for Tamil:", accuracy_tamil, "%\n")
cat("Accuracy for Turkish:", accuracy_turkish, "%\n")
cat("Accuracy for Finnish:", accuracy_finnish, "%\n")
cat("Accuracy for Japanese:", accuracy_japanese, "%\n")
cat("Accuracy for Korean:", accuracy_korean, "%\n")
cat("Accuracy for Vietnamese:", accuracy_vietnamese, "%\n")
cat("Accuracy for Thai:", accuracy_thai, "%\n")



# Función revisada para imprimir todos los desajustes en las traducciones
print_mismatches <- function(df, original_col, translated_col, language) {
  # Convertir todo a minúsculas para la comparación
  original_lower <- tolower(df[[original_col]])
  translated_lower <- tolower(df[[translated_col]])
  
  # Identificar desajustes
  mismatch_indices <- which(original_lower != translated_lower)
  mismatches <- df[mismatch_indices, c(original_col, translated_col)]
  
  cat("Desajustes para", language, ":\n")
  print(mismatches, n = Inf)  # Imprimir todos los desajustes sin truncar
}


# Ejecutar la función para el indonesio
print_mismatches(df, "city", "city_from_Indonesian_to_English", "Indonesian")

# Ejecutar la función para el coreano
print_mismatches(df, "city", "city_from_Korean_to_English", "Korean")

# Ejecutar la función para el tailandés
print_mismatches(df, "city", "city_from_Thai_to_English", "Thai")

# Ejecutar la función para el Español
print_mismatches(df, "city", "city_from_Spanish_to_English", "Spanish")

# Ejecutar la función para el Chino
print_mismatches(df, "city", "city_from_Chinese_to_English", "Chinese")

# Ejecutar la función para el Árabe
print_mismatches(df, "city", "city_from_Arabic_to_English", "Arabic")

# Ejecutar la función para el Suajili
print_mismatches(df, "city", "city_from_Swahili_to_English", "Swahili")

# Ejecutar la función para el Tamil
print_mismatches(df, "city", "city_from_Tamil_to_English", "Tamil")

# Ejecutar la función para el Turco
print_mismatches(df, "city", "city_from_Turkish_to_English", "Turkish")

# Ejecutar la función para el Finés
print_mismatches(df, "city", "city_from_Finnish_to_English", "Finnish")

# Ejecutar la función para el Japonés
print_mismatches(df, "city", "city_from_Japanese_to_English", "Japanese")

# Ejecutar la función para el Vietnamita
print_mismatches(df, "city", "city_from_Vietnamese_to_English", "Vietnamese")


