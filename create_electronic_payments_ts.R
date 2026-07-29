library(tidyverse)
library(lubridate)

elabora_dati_fatturazione <- function(input_dir, output_dir = ".") {
  
  # 1. Elenco dei file e parsing dei metadati dal nome
  tutti_i_file <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
  
  df_files <- tibble(file_path = tutti_i_file) %>%
    mutate(file_name = basename(file_path)) %>%
    # Regex per estrarre: 1=Ambito(IT/EU), 2=StringaData, 3=Tipo(600/601/602)
    filter(str_detect(file_name, "^ATIST?PQ\\.(EU|IT)\\.D([0-9]+)\\.T[0-9]+(600|601|602)\\.csv$")) %>%
    mutate(
      ambito = str_match(file_name, "^ATIST?PQ\\.(EU|IT)\\.")[,2],
      # Estraiamo l'anno prendendo i primi 4 numeri dopo la D
      anno   = as.integer(str_sub(str_match(file_name, "\\.D([0-9]+)\\.")[,2], 1, 4)),
      tipo   = str_match(file_name, "\\.T[0-9]+(600|601|602)\\.csv$")[,2]
    )
  
  if (nrow(df_files) == 0) {
    stop("Nessun file corrispondente ai criteri trovato nella directory specificata.")
  }
  
  # Identifichiamo il range degli anni presenti nei file
  anno_inizio <- min(df_files$anno)
  anno_fine   <- max(df_files$anno)
  
  message(paste("Rilevato intervallo temporale:", anno_inizio, "-", anno_fine))
  
  # Configurazione del parsing numerico italiano (migliaia = . , decimali = ,)
  locale_it <- locale(decimal_mark = ",", grouping_mark = ".")
  
  # 2. Funzione di lettura e standardizzazione di un singolo file
  leggi_file_safely <- function(path, tipo) {
    # Leggiamo tutto inizialmente come stringa (character) eccetto la data,
    # per evitare che readr applichi euristiche errate sui punti delle migliaia.
    # dati <- read_delim(
    #   path, 
    #   delim = ";", 
    #   col_types = cols(.default = "c", data = "D"),
    #   trim_ws = TRUE
    # )
    
    dati <- read_delim(path, delim = ";", col_types = cols(.default = "c"), trim_ws = TRUE) # Added
    
    names(dati) <- tolower(names(dati)) # Added
    
    # Standardizziamo la colonna data nel caso non fosse letta nativamente come Date
    dati <- dati %>% mutate(data = as.Date(data))
    
    # Identifichiamo le colonne da convertire in numerico (tutte tranne i metadati stringa/data)
    colonne_metadati <- c("settimana", "data", "mese", "anno", "zona")
    colonne_numeriche <- setdiff(names(dati), colonne_metadati)
    
    # Pulizia e conversione numerica esplicita
    dati <- dati %>%
      mutate(across(all_of(colonne_numeriche), function(x) {
        # 1. Rimuoviamo i punti usati come separatori delle migliaia (es. 16.577.289,68 -> 16577289,68)
        x_pulito <- str_remove_all(x, "\\.")
        # 2. Sostituiamo la virgola decimale con il punto standard (es. 16577289,68 -> 16577289.68)
        x_pulito <- str_replace(x_pulito, ",", ".")
        # 3. Convertiamo finalmente in double
        as.numeric(x_pulito)
      }))
    
    # Passiamo in formato pivot_longer per i calcoli successivi
    dati_long <- dati %>%
      select(-any_of(c("settimana", "mese", "anno", "zona"))) %>%
      pivot_longer(cols = -data, names_to = "metrica", values_to = "valore") %>%
      mutate(tipo = tipo)
    
    return(dati_long)
  }
  
  # 3. Creazione della griglia completa (Anni x Ambiti x Tipi) per scovare i FILE mancanti
  griglia_completa <- expand_grid(
    anno = anno_inizio:anno_fine,
    ambito = c("IT", "EU"),
    tipo = c("600", "601", "602")
  ) %>%
    left_join(df_files, by = c("anno", "ambito", "tipo"))
  
  # Caricamento effettivo dei dati
  message("Caricamento e parsing dei file in corso...")
  dati_caricati <- griglia_completa %>%
    mutate(dati = map2(file_path, tipo, function(path, t) {
      if (is.na(path)) return(NULL)
      tryCatch(leggi_file_safely(path, t), error = function(e) NULL)
    }))
  
  # Creiamo un set informativo sui file mancanti alla radice
  # file_mancanti_info <- dati_caricati %>%
  #   filter(is.na(file_path) | map_lgl(dati, is.null)) %>%
  #   mutate(nota_file = paste0("manca file ", tipo)) %>%
  #   select(anno, ambito, nota_file)
  # Prima avevi un mutate diretto. Sostituiscilo con questo blocco:
  file_mancanti_info <- dati_caricati %>%
    filter(is.na(file_path) | map_lgl(dati, is.null)) %>%
    mutate(nota_singola = paste0("manca file ", tipo)) %>%
    group_by(anno, ambito) %>%     # Raggruppiamo per anno e ambito per fondere le righe duplicate
    summarise(nota_file = paste(nota_singola, collapse = ", "), .groups = "drop")
  
  
  # Uniamo tutti i dati letti in un unico grande dataframe
  df_long_all <- dati_caricati %>%
    filter(!map_lgl(dati, is.null)) %>%
    select(anno, ambito, dati) %>%
    unnest(dati)
  
  # Trova la data massima complessiva presente nei dati (es. per il 2026 incompleto)
  data_max_assoluta <- max(df_long_all$data, na.rm = TRUE)
  data_min_assoluta <- as.Date(paste0(anno_inizio, "-01-01"))
  
  # Creiamo l'asse temporale definitivo (giornaliero continuo)
  calendario_completo <- tibble(data = seq.Date(from = data_min_assoluta, to = data_max_assoluta, by = "day")) %>%
    mutate(
      settimana = as.integer(format(data, "%V")), # Numero settimana standard ISO
      mese = month(data),
      anno = year(data)
    )
  
  # 4. Funzione di elaborazione per singolo ambito (IT o EU)
  elabora_ambito <- function(target_ambito) {
    message(paste("Elaborazione ambito:", target_ambito))
    
    # Generiamo la struttura completa Date x Metriche per questo ambito
    metriche_distinte <- unique(df_long_all$metrica)
    
    base_ambito <- expand_grid(
      data = calendario_completo$data,
      metrica = metriche_distinte
    )
    
    # Filtriamo i dati correnti dell'ambito
    dati_ambito <- df_long_all %>% filter(ambito == target_ambito)
    
    # Ricostruiamo i dati inserendo gli zeri dove mancano le date
    dati_allineati <- base_ambito %>%
      left_join(dati_ambito, by = c("data", "metrica")) %>%
      mutate(valore = coalesce(valore, 0)) # Sostituzione con 0 se la data/metrica manca nel file
    
    # Controlliamo la presenza delle date nei singoli file per popolare i DETTAGLI_AGGR
    presenza_date <- dati_ambito %>%
      distinct(data, tipo) %>%
      mutate(presente = TRUE) %>%
      pivot_wider(names_from = tipo, values_from = presente, values_fill = FALSE, names_prefix = "has_")
    
    # Assicuriamo la presenza delle colonne anche se un intero tipo di file non esistesse
    for(t in c("600", "601", "602")) {
      col_name <- paste0("has_", t)
      if(!col_name %in% names(presenza_date)) presenza_date[[col_name]] <- FALSE
    }
    
    # Calcolo della formula: 600 - 601 - 602
    dati_concorrenti <- dati_allineati %>%
      select(data, metrica, tipo, valore) %>%
      pivot_wider(names_from = tipo, values_from = valore, values_fill = 0)
    
    # Assicuriamo la presenza dei tre tipi di colonne numeriche
    for(t in c("600", "601", "602")) {
      if(!t %in% names(dati_concorrenti)) dati_concorrenti[[t]] <- 0
    }
    
    dati_calcolati <- dati_concorrenti %>%
      mutate(valore_corretto = `600` - `601` - `602`) %>%
      select(data, metrica, valore_corretto) %>%
      pivot_wider(names_from = metrica, values_from = valore_corretto)
    
    # Costruzione del flag di anomalia DETTAGLI_AGGR
    note_date <- calendario_completo %>%
      left_join(presenza_date, by = "data") %>%
      left_join(file_mancanti_info %>% filter(ambito == target_ambito), by = "anno") %>%
      mutate(
        # Anomalie sui singoli giorni
        manca_giorno_600 = if_else(!has_600 & is.na(nota_file), "manca in 600", NA_character_),
        manca_giorno_601 = if_else(!has_601 & is.na(nota_file), "manca in 601", NA_character_),
        manca_giorno_602 = if_else(!has_602 & is.na(nota_file), "manca in 602", NA_character_)
      ) %>%
      unite("dettagli_giorno", manca_giorno_600, manca_giorno_601, manca_giorno_602, nota_file, 
            sep = ", ", na.rm = TRUE) %>%
      mutate(DETTAGLI_AGGR = if_else(dettagli_giorno == "", "OK", dettagli_giorno)) %>%
      select(data, DETTAGLI_AGGR)
    
    # Unione finale con l'anagrafica del calendario e i metadati richiesti
    risultato_finale <- calendario_completo %>%
      left_join(dati_calcolati, by = "data") %>%
      left_join(note_date, by = "data") %>%
      mutate(zona = target_ambito) %>%
      # Riordino colonne inserendo i metadati all'inizio
      select(settimana, data, mese, anno, zona, DETTAGLI_AGGR, everything())
    
    return(risultato_finale)
  }
  
  # 5. Generazione dei dataset IT ed EU
  df_output_IT <- elabora_ambito("IT")
  df_output_EU <- elabora_ambito("EU")
  
  # 6. Generazione del dataset SUM (Somma di IT e EU)
  message("Generazione dataset complessivo (Somma di IT ed EU)...")
  
  # Identifichiamo le colonne numeriche da sommare (tutte tranne i metadati iniziali)
  colonne_da_sommare <- setdiff(names(df_output_IT), c("settimana", "data", "mese", "anno", "zona", "DETTAGLI_AGGR"))
  
  df_output_SUM <- full_join(
    df_output_IT %>% select(-zona),
    df_output_EU %>% select(-zona),
    by = c("settimana", "data", "mese", "anno"),
    suffix = c("_IT", "_EU")
  )
  
  # Eseguiamo la somma dinamica per ogni colonna metrica
  for(col in colonne_da_sommare) {
    col_it <- paste0(col, "_IT")
    col_eu <- paste0(col, "_EU")
    df_output_SUM[[col]] <- coalesce(df_output_SUM[[col_it]], 0) + coalesce(df_output_SUM[[col_eu]], 0)
  }
  
  # Uniamo le note di tracciamento per il file SUM
  df_output_SUM <- df_output_SUM %>%
    mutate(
      zona = "TOTAL_SUM",
      DETTAGLI_AGGR = case_when(
        DETTAGLI_AGGR_IT == "OK" & DETTAGLI_AGGR_EU == "OK" ~ "OK",
        TRUE ~ paste0("IT: (", DETTAGLI_AGGR_IT, ") | EU: (", DETTAGLI_AGGR_EU, ")")
      )
    ) %>%
    select(settimana, data, mese, anno, zona, DETTAGLI_AGGR, all_of(colonne_da_sommare))
  
  # 7. Scrittura dei file di output in CSV (con lo standard desiderato, es. punto e virgola)
  message("Scrittura dei file di output...")
  write_delim(df_output_IT,  file.path(output_dir, "output_serie_storica_IT.csv"), delim = ";")
  write_delim(df_output_EU,  file.path(output_dir, "output_serie_storica_EU.csv"), delim = ";")
  write_delim(df_output_SUM, file.path(output_dir, "output_serie_storica_SUM.csv"), delim = ";")
  
  message("Processo completato con successo!")
}

# --- Esempio di utilizzo ---
# elabora_dati_fatturazione(input_dir = "C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\NTTS_high_freq\\NTTS_high_freq\\fatturazione_elettronica\\ATISTPQ_2026152_020626/", output_dir = ".") # output nella cartella corrente (.)
elabora_dati_fatturazione(input_dir = "C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\NTTS_high_freq_git\\NTTS_high_freq\\fatturazione_elettronica\\ATISTPQ_2026187_070726\\", output_dir = "C:\\Users\\UTENTE\\OneDrive - ISTAT\\Desktop\\NTTS_high_freq_git\\NTTS_high_freq\\fatturazione_elettronica\\") # output nella cartella corrente (.)

