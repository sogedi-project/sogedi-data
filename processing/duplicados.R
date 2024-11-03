
# duplicados ola 2

db2 <- rio::import(file = here("input","data", "original", "251024 Edumer Ola2 ID.sav")) %>% 
  as_tibble()

db2$SbjNum_o1[duplicated(db2$SbjNum_o1)]

dupli_db2 <- db2 %>% 
  select(SbjNum, SbjNum_o1,starts_with("T_d"), Colegio_o1, d3_def_o1, everything()) %>% 
  filter(SbjNum_o1 %in% c(dup)) %>% 
  as_tibble()


pacman::p_load(stringi)

repe <- dupli_db2 %>%
  # Crear versiones normalizadas del primer nombre y del apellido
  mutate(
    first_name_norm = str_to_lower(stri_trans_general(word(T_d4_1_o2, 1), "Latin-ASCII")),
    last_name_norm = str_to_lower(stri_trans_general(T_d4_2_o2, "Latin-ASCII"))
  ) %>%
  # Filtrar duplicados por SbjNum_o1, primer nombre y apellido normalizados
  group_by(SbjNum_o1, first_name_norm, last_name_norm) %>%
  filter(n() > 1) %>%
  ungroup()


ids_to_remove <- c(209097622, 207836918, 207833101)
dupli_db2 <- subset(dupli_db2, !(SbjNum %in% ids_to_remove))

dup <- dupli_db2$SbjNum_o1[duplicated(dupli_db2$SbjNum_o1)]

dupli_db2 <- dupli_db2 %>% 
  select(SbjNum, SbjNum_o1, starts_with("T_d4"), Colegio_o1, d3_def_o1) %>% 
  filter(SbjNum_o1 %in% c(dup)) %>% 
  as_tibble()


# nombres ola 1

db1 <- rio::import(file = here("input","data", "original", "310524_BDD_edumer.sav")) %>% 
  as_tibble()

db1 <- db1[,c(1:105)]

db1$SbjNum[duplicated(db1$SbjNum)]

nombre_db1 <- db1 %>% 
  select(SbjNum, T_d4_1, T_d4_2) %>% 
  filter(SbjNum %in% c(dup)) %>% 
  as_tibble() %>%   # Crear versiones normalizadas del primer nombre y del apellido
  mutate(
    first_name_norm = str_to_lower(stri_trans_general(word(T_d4_1, 1), "Latin-ASCII")),
    last_name_norm = str_to_lower(stri_trans_general(T_d4_2, "Latin-ASCII"))
  )



dupli_db2 <- dupli_db2 %>%
  # Crear versiones normalizadas del primer nombre y del apellido
  mutate(
    first_name_norm = str_to_lower(stri_trans_general(word(T_d4_1_o2, 1), "Latin-ASCII")),
    last_name_norm = str_to_lower(stri_trans_general(T_d4_2_o2, "Latin-ASCII"))
  )


dupli_db2 <- dupli_db2[,c(2,7,8)] 
dupli_db2$last_name_norm <- sapply(strsplit(dupli_db2$last_name_norm, " "), `[`, 1)
nombre_db1 <- nombre_db1[,c(1,4,5)]

dupli_db2$match <- as.integer(
  with(dupli_db2, 
       paste(SbjNum_o1, first_name_norm, last_name_norm) %in% 
         paste(nombre_db1$SbjNum, nombre_db1$first_name_norm, nombre_db1$last_name_norm)
  )
)

# rastrear casos cuyo id en la ola 2 está mal digitado en la ola 1 para corregir

casos_mal_id_db2 <- dupli_db2 %>% 
  filter(match == 0) %>% 
  mutate(nombre_completo = paste(first_name_norm, last_name_norm, sep = " "))

db1_names <- db1 %>% 
  select(SbjNum, Date, d2, d3_def, Nivel_def, T_d4_1, T_d4_2,) %>% 
  mutate(
    first_name_norm = str_to_lower(stri_trans_general(word(T_d4_1, 1), "Latin-ASCII")),
    last_name_norm = str_to_lower(stri_trans_general(T_d4_2, "Latin-ASCII")),
    nombre_completo = paste(first_name_norm, last_name_norm, sep = " ")
  )

sum(duplicated(db1_names$SbjNum))

casos_mal_id_db2 <- casos_mal_id_db2 %>%
  mutate(
    match = map_int(
      nombre_completo,
      ~ any(grepl(.x, db1_names$nombre_completo, ignore.case = TRUE))
    )
  )

casos_mal_id_db2 <- casos_mal_id_db2 %>%
  mutate(
    matched_SbjNum = map_chr(
      nombre_completo,
      ~ {
        matched_sbjnums <- db1_names %>%
          filter(grepl(.x, nombre_completo, ignore.case = TRUE)) %>%
          pull(SbjNum)
        if (length(matched_sbjnums) > 0) matched_sbjnums[1] else NA_character_
      }
    ),
    matched_SbjNum = as.numeric(matched_SbjNum)
  )

casos_mal_id_db2 %<>% 
  mutate(aux1 = if_else(SbjNum_o1 == matched_SbjNum, 1, 0))

# reemplazar en ola2

db2 <- db2 %>% 
  mutate(
    first_name_norm = str_to_lower(stri_trans_general(word(T_d4_1_o2, 1), "Latin-ASCII")),
    last_name_norm = str_to_lower(stri_trans_general(T_d4_2_o2, "Latin-ASCII")),
    last_name_norm = sapply(strsplit(last_name_norm, " "), `[`, 1),
    nombre_completo = paste(first_name_norm, last_name_norm, sep = " ")
  )

db2_f <- left_join(db2,
          casos_mal_id_db2[,c(1,4:7)],
          by = c("SbjNum_o1", "nombre_completo"))

db2_f %>% 
  select(c(1,2,6:10), nombre_completo, SbjNum_o1, match, matched_SbjNum) %>% 
  filter(!is.na(match))

db2_f$SbjNum_o1[db2_f$SbjNum == 208083509 & db2_f$SbjNum_o1 == 191617613] <- NA
db2_f$SbjNum_o1[db2_f$SbjNum == 207885416 & db2_f$SbjNum_o1 == 193095150] <- NA


db2_f %>% 
  select(c(1,2,6:10), nombre_completo, SbjNum_o1, match, matched_SbjNum) %>% 
  filter(!is.na(match)) # resuelto

db2_f <- db2_f %>% 
  mutate(match = if_else(is.na(match), 0, match),
         matched_SbjNum = if_else(is.na(matched_SbjNum & match == 0), SbjNum_o1, matched_SbjNum)) 


frq(db2_f$matched_SbjNum)

db2_f$matched_SbjNum[duplicated(db2_f$matched_SbjNum)]

db2_f %>% 
  group_by(matched_SbjNum) %>% 
  filter(n() > 1) %>% 
  ungroup() %>% 
  select(c(1,2,6:10), nombre_completo, SbjNum_o1, match, matched_SbjNum)

db2_f <- subset(db2_f, !(SbjNum %in% ids_to_remove))

db2_f$matched_SbjNum[duplicated(db2_f$matched_SbjNum)]

