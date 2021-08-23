source('C:/rprojects/hasznaltauto/R/functions.R')

cars_data <- map(list.files("C:/rprojects/hasznaltauto/data/cars_data/"), 
                 ~ readRDS(str_c("C:/rprojects/hasznaltauto/data/cars_data/", .))) %>% 
  reduce(rbind) %>% 
  tibble() %>% 
  distinct()

cars_data <- cars_data %>% 
  mutate(
    data = map(data, ~ .[[1]]),
    data = map(data, ~ set_names(., "info", "value")),
    data = map(data, ~ mutate(., info = str_remove_all(info, ":") %>% 
                                stringi::stri_trans_general(id = "Latin-ASCII") %>% 
                                snakecase::to_snake_case(),
    )),
    other_data = map(other_data, ~ stringi::stri_trans_general(., id = "Latin-ASCII") %>% 
                       snakecase::to_snake_case())
  )

variables <- cars_data %>% 
  pull(data) %>% 
  map(~ pull(., info)) %>% 
  reduce(c) %>% 
  unique() %>% 
  keep(~ !(. %in% c("vetelar_eur", "altalanos_adatok", "kotelezo_es_casco_dijak_hirdetes")))

variables_from_table <- map(variables, function(x) {
  transmute(cars_data, info = map_chr(data, ~ str_c(GetFromTable(., x), collapse = ", "))) %>% 
    set_names(x)
}) %>% 
  reduce(cbind) %>% 
  janitor::clean_names()

cars_data <- cbind(cars_data, variables_from_table)

other_features <- cars_data %>% 
  pull(other_data) %>% 
  reduce(c) %>% 
  enframe() %>% 
  count(value, sort = TRUE) %>% 
  head(40) %>% 
  pull(value)

variables_from_other <- map(other_features, function(x) {
  transmute(cars_data, info = map_dbl(other_data, ~ str_detect(str_c(., collapse = ", "), x))) %>% 
    set_names(x)
}) %>% 
  reduce(cbind) %>% 
  janitor::clean_names()

cars_data <- cbind(cars_data, variables_from_other) %>% 
  tibble()

cars_data <- cars_data %>% 
  select(-c(data:atveheto)) %>% 
  select(-terkep, -c(finanszirozas:szavatossagi_garancia), -jarmu_adatok, -abroncs,
         -c(karpit_szine_1:fizetendo_magyarorszagi_forgalomba_helyezes_eseten)) %>% 
  mutate(
    evjarat = gsub("/.*", "", evjarat),
    evjarat = as.integer(evjarat), 
    kilometerora_allasa = str_remove_all(kilometerora_allasa, "\\D"), 
    kilometerora_allasa = as.integer(kilometerora_allasa),
    szallithato_szem_szama = str_remove_all(szallithato_szem_szama, "\\D"), 
    szallithato_szem_szama = as.integer(szallithato_szem_szama),
    ajtok_szama = as.integer(ajtok_szama),
    szin = case_when(
      str_detect(szin, "kék") ~ "kek", 
      str_detect(szin, "ibolya") ~ "kek", 
      str_detect(szin, "türkiz") ~ "kek", 
      str_detect(szin, "piros") ~ "piros", 
      str_detect(szin, "vörös") ~ "piros",
      str_detect(szin, "bordó") ~ "piros",
      str_detect(szin, "fekete") ~ "fekete",
      str_detect(szin, "szürke") ~ "szurke",
      str_detect(szin, "ezüst") ~ "szurke",
      str_detect(szin, "fehér") ~ "feher",
      str_detect(szin, "barna") ~ "barna",
      str_detect(szin, "homok") ~ "barna",
      str_detect(szin, "pezsgő") ~ "barna",
      str_detect(szin, "bézs") ~ "barna",
      str_detect(szin, "vaj") ~ "barna",
      str_detect(szin, "zöld") ~ "zold",
      str_detect(szin, "sárga") ~ "sarga",
      str_detect(szin, "narancs") ~ "sarga",
      str_detect(szin, "lila") ~ "lila",
      TRUE ~ "other"
    ),
    hengerurtartalom = str_remove_all(hengerurtartalom, "\\D"),
    hengerurtartalom = as.integer(hengerurtartalom),
    teljesitmeny = gsub(".*kW, ", "", teljesitmeny),
    teljesitmeny = str_remove_all(teljesitmeny, "\\D"),
    teljesitmeny = as.integer(teljesitmeny),
    muszaki_vizsga_ervenyes = gsub("/.*", "", muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = as.integer(muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = muszaki_vizsga_ervenyes - 2021,
    muszaki_vizsga_ervenyes = ifelse(muszaki_vizsga_ervenyes < 0, "none", muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = as.character(muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = ifelse(is.na(muszaki_vizsga_ervenyes), "none", muszaki_vizsga_ervenyes),
    muszaki_vizsga_ervenyes = factor(muszaki_vizsga_ervenyes, levels = c("none", as.character(0:15)), ordered = TRUE),
    vetelar = str_remove_all(vetelar, "\\D"),
    vetelar = as.integer(vetelar),
    sajat_tomeg = str_remove_all(sajat_tomeg, "\\D"),
    sajat_tomeg = as.integer(sajat_tomeg),
    teljes_tomeg = str_remove_all(teljes_tomeg, "\\D"),
    teljes_tomeg = as.integer(teljes_tomeg),
    csomagtarto = str_remove_all(csomagtarto, "\\D"),
    csomagtarto = as.integer(csomagtarto),
  )

cars_data <- cars_data %>% 
  mutate(
    brand = gsub(".*szemelyauto/", "", url_to_car),
    brand = gsub("/.*", "", brand),
    brand = fct_lump(brand, n = 50),
    nyari_gumi_meret = gsub("/.*",  "", nyari_gumi_meret),
    nyari_gumi_meret = as.integer(nyari_gumi_meret),
    sebessegvalto_fokozatszam = str_remove_all(sebessegvalto_fajtaja, "\\D"),
    sebessegvalto_fokozatszam = as.integer(sebessegvalto_fokozatszam),
    sebessegvalto_fokozatszam = ifelse(is.na(sebessegvalto_fokozatszam), 0, sebessegvalto_fokozatszam),
    sebessegvalto_fajtaja = case_when(
      str_detect(sebessegvalto_fajtaja, "tiptronic") ~ "tiptronic",
      str_detect(sebessegvalto_fajtaja, "zekvenciális") ~ "szekvenciális",
      str_detect(sebessegvalto_fajtaja, "anuális") ~ "manuáis",
      str_detect(sebessegvalto_fajtaja, "utomata") ~ "automata",
      TRUE ~ "egyéb"
    )
  ) %>% 
  mutate_if(is.character, as.factor)


write_rds(cars_data, "C:/rprojects/hasznaltauto/data/cars_data.RDS")

