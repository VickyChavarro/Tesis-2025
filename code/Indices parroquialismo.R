

ciudades_con_posiciones_final <- read.csv("C:/Users/victo/Downloads/ciudades_con_posiciones_final.xls")
View(ciudades_con_posiciones_final)

country_annotation_final_with_trust_gini <- read.csv("C:/Users/victo/Downloads/country_annotation_final_with_trust_gini.csv")
View(country_annotation_final_with_trust_gini)


# Imprimir nombres de las columnas
cat("Columnas de ciudades_con_posiciones_final:\n")
print(colnames(ciudades_con_posiciones_final))

cat("\nColumnas de country_annotation_final_with_trust_gini:\n")
print(colnames(country_annotation_final_with_trust_gini))

ciudades_con_posiciones_final$population <- NULL
idiomas_prioritarios <- c('English', 'Spanish', 'Chinese', 'Arabic', 
                          'Indonesian', 'Swahili', 'Tamil', 'Turkish', 
                          'Finnish', 'Japanese', 'Korean', 'Vietnamese')

library(dplyr)
library(stringr)
library(tidyr)

# Limpiar y seleccionar idioma
country_annotation_final_with_trust_gini <- country_annotation_final_with_trust_gini %>%
  mutate(languages_clean = str_remove_all(languages, "\\[|\\]|'"),
         languages_clean = str_split(languages_clean, ",\\s*")) %>%
  rowwise() %>%
  mutate(language_selected = {
    lang = languages_clean
    # Buscamos si alguno está en la lista prioritaria
    match = lang[lang %in% idiomas_prioritarios]
    if(length(match) > 0) {
      match[1] # Tomamos el primero que aparezca en la lista prioritaria
    } else {
      lang[1] # Si no hay ninguno de los priorizados, tomamos el primero
    }
  }) %>%
  ungroup()


ciudades_final <- ciudades_con_posiciones_final %>%
  left_join(country_annotation_final_with_trust_gini %>% 
              select(country = name, language_selected, population_country = Population), 
            by = "country")

# VER LAS QUE QUEDARON CON NA EN EL LANGUAGE

na_languages_ciudades <- ciudades_final %>%
  filter(is.na(language_selected))

View(na_languages_ciudades)

# CORREGIR ALGUNAS 

ciudades_final <- ciudades_final %>%
  mutate(language_selected = ifelse(country == "DR Congo", "Swahili", language_selected))
ciudades_final %>%
  filter(is.na(language_selected) | country == "DR Congo") %>%
  View()

# VER CANTIDAD DE CIUDADES POR LOS IDIOMAS A TRABAJAR 

# Lista de idiomas a trabajar
idiomas_prioritarios <- c('English', 'Spanish', 'Chinese', 'Arabic', 
                          'Indonesian', 'Swahili', 'Tamil', 'Turkish', 
                          'Finnish', 'Japanese', 'Korean', 'Vietnamese')

# Filtrar las filas con esos idiomas y contarlas
idiomas_encontrados <- ciudades_final %>%
  filter(language_selected %in% idiomas_prioritarios)

# Ver el conteo por idioma
conteo <- idiomas_encontrados %>%
  count(language_selected) %>%
  arrange(desc(n))

View(idiomas_encontrados)
print(conteo)

# Filtrar países donde aparece Tamil en la columna de idiomas original
paises_tamil <- country_annotation_final_with_trust_gini %>%
  mutate(languages_clean = str_remove_all(languages, "\\[|\\]|'")) %>%
  separate_rows(languages_clean, sep = ",\\s*") %>%
  filter(languages_clean == "Tamil") %>%
  distinct(country = name)

print(paises_tamil)


ciudades_tamil <- ciudades_con_posiciones_final %>%
  filter(country %in% c("Sri Lanka", "Singapore"))

View(ciudades_tamil)


ciudades_final <- ciudades_final %>%
  mutate(language_selected = ifelse(country %in% c("Sri Lanka", "Singapore"), "Tamil", language_selected))



# Limpiar y separar la columna original de idiomas
library(tidyr)
library(dplyr)
library(stringr)

# Explosión de la lista original
idiomas_long <- country_annotation_final_with_trust_gini %>%
  mutate(languages_clean = str_remove_all(languages, "\\[|\\]|'")) %>%
  separate_rows(languages_clean, sep = ",\\s*")

# Contar cuántas veces aparecen tus idiomas prioritarios
conteo_original <- idiomas_long %>%
  filter(languages_clean %in% idiomas_prioritarios) %>%
  count(languages_clean) %>%
  arrange(desc(n))

print(conteo_original)



# Obtener el idioma seleccionado por país (sin duplicados)
idiomas_por_pais <- ciudades_final %>%
  select(country, language_selected) %>%
  distinct() %>%
  arrange(country)

View(idiomas_por_pais)

# Filtrar solo países que tienen uno de los 12 idiomas priorizados
ciudades_por_idioma <- ciudades_final %>%
  filter(language_selected %in% idiomas_prioritarios) %>%
  group_by(country, language_selected) %>%
  summarise(n_ciudades = n(), .groups = 'drop') %>%
  arrange(desc(n_ciudades))

View(ciudades_por_idioma)

# Contar ciudades por país con su idioma seleccionado (todos los idiomas)
ciudades_por_pais_all <- ciudades_final %>%
  group_by(country, language_selected) %>%
  summarise(n_ciudades = n(), .groups = 'drop') %>%
  arrange(desc(n_ciudades))

View(ciudades_por_pais_all)


# Contar ciudades por idioma final
ciudades_por_idioma_all <- ciudades_final %>%
  group_by(language_selected) %>%
  summarise(n_ciudades = n(), .groups = 'drop') %>%
  arrange(desc(n_ciudades))

print(ciudades_por_idioma_all)



############################################### PRIMER ÍNDICE ##############################################################

# Lista de idioma -> columna correspondiente
idiomas_cols <- list(
  "English" = "en_posiciones",
  "Spanish" = "sp_posiciones",
  "Chinese" = "ch_posiciones",
  "Swahili" = "sw_posiciones",
  "Tamil" = "ta_posiciones",
  "Turkish" = "tr_posiciones",
  "Finnish" = "fn_posiciones",
  "Japanese" = "jp_posiciones",
  "Korean" = "ko_posiciones",
  "Vietnamese" = "viet_posiciones",
  "Arabic" = "ar_posiciones",
  "Indonesian" = "id_posiciones"
)

# Creamos tabla para guardar resultados
resultados <- data.frame(
  idioma = character(),
  n_ciudades = numeric(),
  promedio_interno = numeric(),
  promedio_externo = numeric(),
  indice = numeric(),
  stringsAsFactors = FALSE
)

# Loop general para cada idioma
for(idioma in names(idiomas_cols)) {
  
  columna <- idiomas_cols[[idioma]]
  
  df_temp <- ciudades_final %>%
    filter(!is.na(.data[[columna]]) & .data[[columna]] > 0) %>%
    mutate(posiciones_num = as.numeric(.data[[columna]]))
  
  internas <- df_temp %>%
    filter(language_selected == idioma)
  
  externas <- df_temp %>%
    filter(language_selected != idioma)
  
  if(nrow(internas) > 0 & nrow(externas) > 0) {
    promedio_interno <- mean(internas$posiciones_num, na.rm = TRUE)
    promedio_externo <- mean(externas$posiciones_num, na.rm = TRUE)
    indice <- promedio_externo / promedio_interno
    n_internas <- nrow(internas)
  } else {
    promedio_interno <- NA
    promedio_externo <- NA
    indice <- NA
    n_internas <- 0
  }
  
  resultados <- resultados %>%
    add_row(idioma = idioma,
            n_ciudades = n_internas,
            promedio_interno = promedio_interno,
            promedio_externo = promedio_externo,
            indice = indice)
}

View(resultados)



######################################## PRIMER ÍNDICE ESTANDARIZADO ######################################################


# Diccionario idioma -> columna
idiomas_cols <- list(
  "English" = "en_posiciones",
  "Spanish" = "sp_posiciones",
  "Chinese" = "ch_posiciones",
  "Swahili" = "sw_posiciones",
  "Tamil" = "ta_posiciones",
  "Turkish" = "tr_posiciones",
  "Finnish" = "fn_posiciones",
  "Japanese" = "jp_posiciones",
  "Korean" = "ko_posiciones",
  "Vietnamese" = "viet_posiciones",
  "Arabic" = "ar_posiciones",
  "Indonesian" = "id_posiciones"
)

# Tabla de resultados
resultados <- data.frame(
  idioma = character(),
  n_ciudades = numeric(),
  promedio_interno = numeric(),
  promedio_externo = numeric(),
  sd_total = numeric(),
  indice_standarizado = numeric(),
  stringsAsFactors = FALSE
)

# Loop para cada idioma
for(idioma in names(idiomas_cols)) {
  
  columna <- idiomas_cols[[idioma]]
  
  df_temp <- ciudades_final %>%
    filter(!is.na(.data[[columna]]) & .data[[columna]] > 0) %>%
    mutate(posiciones_num = as.numeric(.data[[columna]]))
  
  internas <- df_temp %>%
    filter(language_selected == idioma)
  
  externas <- df_temp %>%
    filter(language_selected != idioma)
  
  all_data <- bind_rows(internas, externas)
  
  if(nrow(internas) > 0 & nrow(externas) > 0) {
    promedio_interno <- mean(internas$posiciones_num, na.rm = TRUE)
    promedio_externo <- mean(externas$posiciones_num, na.rm = TRUE)
    sd_total <- sd(all_data$posiciones_num, na.rm = TRUE)
    indice_standarizado <- (promedio_externo - promedio_interno) / sd_total
    n_internas <- nrow(internas)
  } else {
    promedio_interno <- NA
    promedio_externo <- NA
    sd_total <- NA
    indice_standarizado <- NA
    n_internas <- 0
  }
  
  resultados <- resultados %>%
    add_row(idioma = idioma,
            n_ciudades = n_internas,
            promedio_interno = promedio_interno,
            promedio_externo = promedio_externo,
            sd_total = sd_total,
            indice_standarizado = indice_standarizado)
}

View(resultados)


################################################# SEGUNDO ÍNDICE ##############################################################

# Lista idioma -> columna
idiomas_cols <- list(
  "English" = "en_posiciones",
  "Spanish" = "sp_posiciones",
  "Chinese" = "ch_posiciones",
  "Swahili" = "sw_posiciones",
  "Tamil" = "ta_posiciones",
  "Turkish" = "tr_posiciones",
  "Finnish" = "fn_posiciones",
  "Japanese" = "jp_posiciones",
  "Korean" = "ko_posiciones",
  "Vietnamese" = "viet_posiciones",
  "Arabic" = "ar_posiciones",
  "Indonesian" = "id_posiciones"
)

# Lista donde guardaremos rankings por idioma
ranking_paises <- list()

for(idioma in names(idiomas_cols)) {
  
  columna <- idiomas_cols[[idioma]]
  
  df_temp <- ciudades_final %>%
    filter(!is.na(.data[[columna]]) & .data[[columna]] > 0) %>%
    mutate(posiciones_num = as.numeric(.data[[columna]])) %>%
    group_by(country) %>%
    summarise(promedio_posiciones = mean(posiciones_num, na.rm = TRUE), .groups = 'drop') %>%
    arrange(promedio_posiciones) %>%
    mutate(ranking = row_number())
  
  ranking_paises[[idioma]] <- df_temp
}

print(ranking_paises[["Indonesian"]], n = Inf)





library(dplyr)
library(purrr)

# 1️⃣ Generar rankings por idioma
rankings_list <- imap(ranking_paises, function(df, idioma) {
  df %>%
    select(country, ranking) %>%
    rename(!!paste0(idioma, "_rank") := ranking)
})

# 2️⃣ Unir todos los rankings en una sola tabla
ranking_final <- reduce(rankings_list, full_join, by = "country")

# 3️⃣ Contar cuántas ciudades y obtener el language por país
conteo_ciudades_language <- ciudades_final %>%
  group_by(country) %>%
  summarise(
    n_ciudades = n(),
    language_selected = first(language_selected), # asumimos que es el mismo idioma para todas las ciudades de ese país
    .groups = 'drop'
  )

# 4️⃣ Unir conteo y language al ranking
ranking_final <- ranking_final %>%
  left_join(conteo_ciudades_language, by = "country") %>%
  relocate(n_ciudades, language_selected, .after = country)

# 5️⃣ Ver resultado
View(ranking_final)

