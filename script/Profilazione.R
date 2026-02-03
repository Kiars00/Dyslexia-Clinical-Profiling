library(tidyverse)
library(stringr)
library(car)
library(readr)
library(MuMIn)
library(car)
library(lme4)
library(lmerTest)
library(sjPlot)
library(MASS)
library(janitor)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales) # Serve per formattare i numeri dell'asse Y
library(ggrepel)
library(broom)
library(knitr) # Per una visualizzazione pulita in console

# 1. Carichiamo i dati grezzi (o usa il df che hai già caricato)
df_raw <- read_csv("Controlli.csv") 
df_raw <- df_raw %>%
  # A. Rinomino le colonne spagnole con i nomi italiani del tuo studio clinico
  rename(
    id_frase       = item,         # item -> id_frase 
    parola_target  = nombre_rol,   # nombre_rol -> parola_target
    rt_nome        = rt_nombre,    # rt_nombre -> rt_nome
    rt_pronome     = rt_pronombre,
    frase_completa = oracion,
    rt_word_0      = palabra_0,
    rt_word_1      = palabra_1,
    rt_word_2      = palabra_2,
    rt_word_3      = palabra_3,
    rt_word_4      = palabra_4,
    rt_word_5      = palabra_5,
    rt_word_6      = palabra_6,
    rt_word_7      = palabra_7,
    rt_word_8      = palabra_8,
    rt_word_9      = palabra_9,
    rt_word_10     = palabra_10,
    rt_word_11     = palabra_11,
    rt_word_12     = palabra_12,
    rt_word_13     = palabra_13,
    rt_word_14     = palabra_14,
    nazionalita    = nacionalidad,
    id_condizione  = item_exp,
    bias_di_genere = bias,
    id             = ID,
    eta            = edad,         # edad -> eta
    part           = part          # questa di solito resta uguale
  ) %>%
  # B. Creo variabili necessarie e inverto la logica errori
  mutate(
    # Creo la colonna che manca nei controlli (perché nei clinici filtriamo su questa)
    condizione_linguistica = "gender",
    
    # Conversione tipi (Categorici e Numerici)
    across(c(part, id_frase), as.factor),
    rt_nome = as.numeric(rt_nome),
    tiempo_total = as.numeric(as.character(tiempo_total)),
    
    # Inversione Logica Errori: 
    # Spagnolo: errores="no" -> Italiano: is_corretto=TRUE
    is_corretto = case_when(
      tolower(as.character(errores)) == "no" ~ TRUE, 
      errores == 0 ~ TRUE,
      TRUE ~ FALSE # Se c'è "si" o 1, è un errore (FALSE)
    ),
    
    # Aggiungo etichetta nazione (come facevi tu)
    muestra = "italia"
  )

df_raw <- df_raw %>% 
  filter(part != "3") %>% # Gloria
  filter(part != "4") %>% # Gloria
  filter(part != "5") %>% # Gloria
  filter(part != "6") %>% # Gloria
  filter(part != "8") %>% # Gabi
  filter(part != "9") %>% # Gabi
  filter(part != "11") %>% # Gabi
  filter(part != "12") %>% # Gabi
  filter(part != "24") %>% # completó mal los datos sociodemográficos
  filter(part != "47") %>% # es argentina
  filter(part != "60") %>% # tardó más de 150 min
  filter(part != "15") %>% # cometió 7 errores
  filter(part != "22") %>% # cometió 7 errores
  filter(part != "27") %>% # cometió 7 errores
  filter(part != "57") %>% # cometió 8 errores
  filter(part != "87") %>% # cometió 8 errores
  filter(part != "25") %>% # cometió 9 errores
  filter(part != "101") %>% # cometió 10 errores
  filter(part != "42") %>% # cometió 11 errores
  filter(part != "67") %>% # cometió 11 errores
  filter(!is.na(eta) & eta <= 40) %>%
  droplevels()



df_controlli_intero <- df_raw #conservo il dataset per comparare


print(paste("1. TOTALE PARTECIPANTI NEL FILE:", n_distinct(df_raw$part)))

# 2. Vediamo cosa c'è nelle colonne CHIAVE (senza filtri)
print("--- CAMPIONE DI VALORI 'cond' ---")
print(head(unique(df_raw$cond), 20)) # Vediamo i primi 20 valori unici

print("--- CAMPIONE DI VALORI 'bias_di_genere' ---")
print(head(unique(df_raw$bias_di_genere), 20))

# 3. Controlliamo l'accoppiamento
# Creiamo una tabella temporanea pulendo al minimo
df_check <- df_raw %>%
  mutate(
    # Pulizia minima: minuscolo e via gli spazi
    c_str = str_trim(tolower(as.character(cond))),
    b_str = str_trim(tolower(as.character(bias_di_genere))),
    
    # Creiamo un'etichetta combinata per vedere cosa esce
    Combo = paste(c_str, b_str, sep = " | ")
  )

print("--- COMBINAZIONI TROVATE (Top 20) ---")
print(head(sort(table(df_check$Combo), decreasing = TRUE), 20))

# 4. Vediamo quanti hanno "Match" e "Mismatch" (logica g_masc vs masc)

df_test_logic <- df_check %>%
  mutate(
    # Togliamo "g_" da ENTRAMBE le parti per sicurezza
    c_clean = str_remove(c_str, "g_"),
    b_clean = str_remove(b_str, "g_"),
    
    Tipo = case_when(
      c_clean == b_clean ~ "Match",
      c_clean != b_clean ~ "Mismatch",
      TRUE ~ "Boh"
    )
  )

print("--- CONTEGGIO TIPO PROVA (Match/Mismatch) ---")
print(table(df_test_logic$Tipo))

# 5. Quanti soggetti hanno ENTRAMBE le condizioni?
soggetti_completi <- df_test_logic %>%
  filter(Tipo %in% c("Match", "Mismatch")) %>%
  distinct(part, Tipo) %>%
  count(part) %>%
  filter(n >= 2) # Deve avere almeno 2 tipi di prove diverse

print(paste("SOGGETTI CON DATI COMPLETI (Match + Mismatch):", nrow(soggetti_completi)))


df_ita_controlli <- df_controlli_intero %>%
  mutate(id_genero = case_when(
    id_genero %in% c("F", "Donna", "femmina", "f", "donna cis", "donna", "Femmina", 
                     "DONNA", "Femminile", "FEMMINA") ~ "donna",
    id_genero %in% c("MASCHIO", "maschio", "Maschio", "uomo", "M", "MASCHILE", "UOMO",
                     "Maschile", "Uomo") ~ "uomo", )) %>%
  mutate(id_genero = factor(id_genero, levels = c("donna", "uomo")))

partecipanti_ita_controlli <- df_ita_controlli %>% 
  distinct(part, eta, id, id_genero, educacion, lengua, nazionalita, residencia, mail, tiempo_total, lista)
summary(partecipanti_ita_controlli) # 58 donne e 26 uomini

# 4. CALCOLO STATISTICHE NORMATIVE (SUI 67 RIMASTI)
df_soggetti_norma <- df_controlli_intero %>% 
  distinct(part, .keep_all = TRUE)

stats_norma <- df_soggetti_norma %>%
  summarise(
    Totale_N = n(),
    # Qui uso as.numeric ancora una volta per sicurezza estrema
    Eta_Media = mean(as.numeric(eta), na.rm = TRUE),
    Eta_SD = sd(as.numeric(eta), na.rm = TRUE),
    Eta_Min = min(eta, na.rm = TRUE), 
    Eta_Max = max(eta, na.rm = TRUE)
  )

print("--- STATISTICHE NORMA (67 Partecipanti) ---")
print(stats_norma)
#Totale: 67
#Media: 25.9
#SD: 4.78
#min: 19 anni; max: 38 anni
#53 femmine e 14 maschi

# ==============================================================================
# 1. CALCOLO AUTOMATICO DELLA NORMA (CONTROLLI) - LOGICA HARD CONFLICT
# ==============================================================================

cat("\n--- CALCOLO NORMA IES (CAMPIONE NORMATIVO) ---\n")
n_prima <- nrow(df_raw)

# 1. PRE-PROCESSING E PULIZIA OUTLIERS
df_norma <- df_raw %>%
  mutate(
    rt_determinante = as.numeric(rt_determinante),
    rt_nome         = as.numeric(rt_nome),
    rt_verbo1       = as.numeric(rt_verbo1),
    rt_pronome      = as.numeric(rt_pronome),
    rt_verbo2       = as.numeric(rt_verbo2),
    
    # Pulizia stringhe per la logica a 4 condizioni (fondamentale per differenziare Hard/Soft)
    c_clean = str_trim(str_remove(tolower(as.character(cond)), "g_")),
    b_clean = str_trim(tolower(as.character(bias_di_genere))),
    
    # Definiamo le condizioni esattamente come nei clinici
    Tipo = case_when(
      c_clean == "masc" & b_clean == "masc" ~ "Match_Masc",
      c_clean == "fem"  & b_clean == "masc" ~ "Mismatch_Hard",
      c_clean == "masc" & b_clean == "fem"  ~ "Mismatch_Soft",
      c_clean == "fem"  & b_clean == "fem"  ~ "Match_Fem",
      TRUE ~ NA_character_
    ),
    Is_Corretto = if_else(str_detect(tolower(errores), "no"), 1, 0)
  ) %>%
  filter(rt_nome > 100) %>%
  filter(rt_determinante < 3000) %>%
  filter(rt_nome < 3500) %>%
  filter(rt_verbo1 < 3500) %>%
  filter(rt_pronome < 3000) %>%
  filter(rt_verbo2 < 3500) %>%
  drop_na(rt_nome, Tipo)

# Conteggio outlier rimossi
n_dopo <- nrow(df_norma)
cat("Percentuale dati persi:", round(((n_prima - n_dopo) / n_prima) * 100, 2), "%\n")
#4.48%

# 2. CALCOLO IES PER SOGGETTO (Individuale)
ies_per_soggetto <- df_norma %>%
  group_by(part, Tipo) %>%
  summarise(
    RT_medio = mean(rt_nome[Is_Corretto == 1], na.rm = TRUE),
    ACC      = mean(Is_Corretto, na.rm = TRUE),
    log_IES  = log(RT_medio / ACC), 
    .groups = "drop"
  )

# 3. PARAMETRI DELLA NORMA (stats_campione)
stats_campione <- ies_per_soggetto %>%
  group_by(Tipo) %>%
  summarise(
    IES      = mean(log_IES, na.rm = TRUE),
    SD_IES   = sd(log_IES, na.rm = TRUE),
    N        = n(),
    .groups = "drop"
  )

SD_DELTA_HARD_NORMA <- ies_per_soggetto %>%
  pivot_wider(id_cols = part, names_from = Tipo, values_from = log_IES) %>%
  mutate(Delta_H = Mismatch_Hard - Match_Masc) %>%
  summarise(sd = sd(Delta_H, na.rm = TRUE)) %>%
  pull(sd)

# 1. Calcola il Delta individuale per ogni controllo (Soggetti Sani)
delta_individuali_controlli <- ies_per_soggetto %>%
  pivot_wider(id_cols = part, names_from = Tipo, values_from = log_IES) %>%
  mutate(
    Delta_H = Mismatch_Hard - Match_Masc,
    Delta_S = Mismatch_Soft - Match_Fem
  )

# 2. Estrai la SD corretta del Delta Soft
SD_DELTA_SOFT_NORMA <- sd(delta_individuali_controlli$Delta_S, na.rm = TRUE)

# --- ESTRAZIONE HARD ---
IES_Match_M    <- stats_campione$IES[stats_campione$Tipo == "Match_Masc"]
IES_Mismatch_H <- stats_campione$IES[stats_campione$Tipo == "Mismatch_Hard"]
SD_Match_M     <- stats_campione$SD_IES[stats_campione$Tipo == "Match_Masc"]
SD_Mismatch_H  <- stats_campione$SD_IES[stats_campione$Tipo == "Mismatch_Hard"]

DELTA_HARD <- IES_Mismatch_H - IES_Match_M

#SD_POOLED_HARD <- sqrt((SD_Match_M^2 + SD_Mismatch_H^2) / 2)

# --- ESTRAZIONE SOFT ---
IES_Match_F    <- stats_campione$IES[stats_campione$Tipo == "Match_Fem"]
IES_Mismatch_S <- stats_campione$IES[stats_campione$Tipo == "Mismatch_Soft"]
SD_Match_F     <- stats_campione$SD_IES[stats_campione$Tipo == "Match_Fem"]
SD_Mismatch_S  <- stats_campione$SD_IES[stats_campione$Tipo == "Mismatch_Soft"]

DELTA_SOFT <- IES_Mismatch_S - IES_Match_F
#SD_POOLED_SOFT <- sqrt((SD_Match_F^2 + SD_Mismatch_S^2) / 2)

# ==============================
# TABELLA NORMA COMPLETA
# ==============================
norma_vals <- tibble(
  Condizione = c("HARD (Masc-Bias)", "HARD (Masc-Bias)", "SOFT (Fem-Bias)", "SOFT (Fem-Bias)"),
  Parametro  = c("Media Delta", "SD Norma", "Media Delta", "SD Norma"),
  Valore     = c(DELTA_HARD, SD_DELTA_HARD_NORMA, DELTA_SOFT, SD_DELTA_SOFT_NORMA)
)

print(norma_vals)

cat("\n--- CALCOLO NORMA IES (CAMPIONE NORMATIVO) ---\n")
# 2. CALCOLO IES PER SOGGETTO (Necessario per Crawford/Z-score)
ies_per_soggetto <- df_norma %>%
  group_by(part, Tipo) %>%
  summarise(
    RT_medio = mean(rt_nome[Is_Corretto == 1], na.rm = TRUE),
    ACC      = mean(Is_Corretto, na.rm = TRUE),
    # Calcolo IES e applico subito il LOG (visti i risultati dei Q-Q plot di prima)
    log_IES  = log(RT_medio / ACC), 
    .groups = "drop"
  )

# 3. CALCOLO PARAMETRI DELLA NORMA (Media e SD dei log-IES)
tabella_norma_finale <- ies_per_soggetto %>%
  group_by(Tipo) %>%
  summarise(
    Media_Norma = mean(log_IES, na.rm = TRUE),
    SD_Norma    = sd(log_IES, na.rm = TRUE),
    N_Controlli = n()
  )

print(tabella_norma_finale)


# Creiamo un dataframe pulito per il grafico
ies_plot_data <- ies_per_soggetto %>% 
  filter(is.finite(log_IES))

plot(density(ies_plot_data$log_IES), 
     main="Distribuzione Log-IES (Controlli)", 
     xlab="Log-IES", col="blue", lwd=2)
# Salva il Density Plot del Log-IES
png("Density_LogIES_Controlli.png", width = 2400, height = 1800, res = 300)
plot(density(ies_plot_data$log_IES), 
     main="Distribuzione Log-IES (Controlli)", 
     xlab="Log-IES", col="blue", lwd=2)
dev.off()

png("QQPlot_LogIES_Controlli.png", width = 2400, height = 1800, res = 300)
qqnorm(ies_plot_data$log_IES, main="Q-Q Plot Log-IES")
qqline(ies_plot_data$log_IES, col="red")
dev.off()
graphics.off()
# lo stesso per il Q-Q plot
qqnorm(ies_plot_data$log_IES, main="Q-Q Plot Log-IES")
qqline(ies_plot_data$log_IES, col="red")
plot(density(ies_per_soggetto$log_IES, na.rm = TRUE), 
     main="Distribuzione Log-IES (Controlli)", 
     xlab="Log-IES", col="blue", lwd=2)

# 2. Q-Q Plot
qqnorm(ies_per_soggetto$log_IES, main="Q-Q Plot Log-IES")
qqline(ies_per_soggetto$log_IES, col="red")

# 3. Test statistico formale (Shapiro-Wilk)
# Se p > 0.05, i dati sono considerati normali
shapiro.test(ies_per_soggetto$log_IES)

# ==============================
# 2. IES DI CAMPIONE (MATCH vs MISMATCH)
# ==============================

# STEP 2: Creiamo stats_campione con Media e SD dei valori individuali
stats_campione <- ies_per_soggetto  %>%
  group_by(Tipo) %>%
  summarise(
    IES      = mean(log_IES, na.rm = TRUE), # Media dei log-IES
    SD_IES   = sd(log_IES, na.rm = TRUE),   # SD dei log-IES (fondamentale per Crawford)
    N        = n(),                         # Numero di partecipanti
    .groups = "drop"
  )

print(stats_campione)


# 1. Recuperiamo i Delta Medi (già calcolati prima)
# Usiamo i nomi HARD e SOFT per non confonderci
DELTA_MEDIO_HARD <- DELTA_HARD  # Differenza media nei sani (Masc-Bias)
DELTA_MEDIO_SOFT <- DELTA_SOFT  # Differenza media nei sani (Fem-Bias)

# 2. Usiamo le SD dei Delta (quelle che abbiamo ricalcolato per Crawford)
SD_RIFERIMENTO_HARD <- SD_DELTA_HARD_NORMA 
SD_RIFERIMENTO_SOFT <- SD_DELTA_SOFT_NORMA

# 3. Calcolo delle soglie cliniche (Basate sulla variabilità del Delta)
SOGLIA_RISCHIO_HARD <- DELTA_MEDIO_HARD + SD_RIFERIMENTO_HARD
SOGLIA_CLINICA_HARD <- DELTA_MEDIO_HARD + (2 * SD_RIFERIMENTO_HARD)

# 3. TABELLA NORMA AGGIORNATA
norma_vals <- tibble(
  Condizione = c("HARD (Conflitto Maschile)", "SOFT (Conflitto Femminile)"),
  `Media Delta (log-IES)` = c(DELTA_MEDIO_HARD, DELTA_MEDIO_SOFT),
  `SD della Differenza` = c(SD_RIFERIMENTO_HARD, SD_RIFERIMENTO_SOFT),
  `Soglia Rischio (+1 SD)` = c(SOGLIA_RISCHIO_HARD, DELTA_MEDIO_SOFT + SD_RIFERIMENTO_SOFT),
  `Soglia Clinica (+2 SD)` = c(SOGLIA_CLINICA_HARD, DELTA_MEDIO_SOFT + (2 * SD_RIFERIMENTO_SOFT))
)

print(norma_vals)

table(df_raw$errores)

# ==============================
# 6. NUMEROSITÀ CAMPIONE
# ==============================

n_soggetti <- n_distinct(df_norma$part)

cat("\nSoggetti unici che compongono la norma:", n_soggetti, "\n")


# ==============================================================================
# 2. ANALISI DISLESSICI (df_intero) CON GESTIONE ERRORI (500.000)
# ==============================================================================
df_intero <- read_delim("DATI_dislessici_PULITI.csv", delim = ";", show_col_types = FALSE) %>%
  clean_names()

df_intero <- df_intero %>%
  filter(str_detect(tolower(id_condizione), "gender"))

df_dislessici_out <- df_intero
df_intero_dislessici <- df_dislessici_out #conservo il dataset per comparare



df_dislessici_out[which(df_dislessici_out[,9]<100),] # rt_determinante
df_dislessici_out[which(df_dislessici_out[,10]<100),] # rt_nombre: 1 punto de dato
df_dislessici_out[which(df_dislessici_out[,11]<100),] # rt_verbo1
df_dislessici_out[which(df_dislessici_out[,12]<100),] # rt_pronombre
df_dislessici_out[which(df_dislessici_out[,13]<100),] # rt_verbo2

df_dislessici_out <- df_dislessici_out %>% 
  filter(rt_nome > 100)

#=============================================


## Dati massimi (3.000/3.500 ms per parola) ####

df_dislessici_out[which(df_dislessici_out[,9]>6000),] 
df_dislessici_out[which(df_dislessici_out[,10]>7000),] 
df_dislessici_out[which(df_dislessici_out[,11]>6500),] 
df_dislessici_out[which(df_dislessici_out[,12]>6000),] 
df_dislessici_out[which(df_dislessici_out[,13]>6500),] 

df_dislessici_out <- df_dislessici_out %>% 
  filter(rt_determinante < 6000) %>%  
  filter(rt_nome < 7000) %>%
  filter(rt_verbo1 < 6500) %>%
  filter(rt_pronome < 6000) %>%
  filter(rt_verbo2 < 6500)

#calcolo percentuale dei dati toatli tolti: 75 datos
((dim(df_intero_dislessici)[1] - dim(df_dislessici_out)[1])/ dim(df_intero_dislessici)[1])*100 ## 4.55%



# ==============================================================================
# DIAGNOSI CLINICA: CALCOLO Z-SCORE DI CRAWFORD SUI DISLESSICI
# ==============================================================================

df_diagnosi_final <- df_dislessici_out %>%
  mutate(
    p_id = factor(part),
    # Pulizia etichette per allineamento con la Norma
    c_clean = str_trim(str_remove(tolower(as.character(cond)), "g_")),
    b_clean = str_trim(tolower(as.character(bias_di_genere))),
    
    Cond_IES = case_when(
      c_clean == "masc" & b_clean == "masc" ~ "Match_Masc",
      c_clean == "fem"  & b_clean == "masc" ~ "Mismatch_Hard",
      c_clean == "masc" & b_clean == "fem"  ~ "Mismatch_Soft",
      c_clean == "fem"  & b_clean == "fem"  ~ "Match_Fem",
      TRUE ~ NA_character_
    ),
    is_corretto = if_else(tolower(as.character(corretto)) %in% c("si", "yes", "1"), 1, 0),
    rt_val = as.numeric(rt_nome)
  ) %>%
  filter(!is.na(Cond_IES)) %>%
  
  # Aggregazione trial-by-trial
  group_by(p_id, Cond_IES) %>%
  summarise(
    Accuracy = mean(is_corretto, na.rm=TRUE),
    RT_Corretti = mean(rt_val[is_corretto == 1], na.rm=TRUE),
    .groups = "drop"
  ) %>%
  
  # Calcolo Log-IES (con gestione dei crash)
  mutate(
    Log_IES = log(ifelse(Accuracy == 0 | is.na(RT_Corretti), 500000, RT_Corretti / max(Accuracy, 0.01)))
  ) %>%
  
  # Formattazione Wide per calcolo Delta
  pivot_wider(id_cols = p_id, names_from = Cond_IES, values_from = Log_IES) %>%
  
  mutate(
    # 1. Calcolo Delta individuale (Costo dello Stereotipo)
    Delta_Paziente = Mismatch_Hard - Match_Masc,
    
    # 2. CALCOLO Z-Vero (Test di Crawford-Howell)
    # Usiamo DELTA_HARD (Media sani) e SD_DELTA_HARD_NORMA (Variabilità differenze sani)
    # La formula include la correzione sqrt((n+1)/n) con n=67
    Z_Vero = (Delta_Paziente - DELTA_HARD) / (SD_DELTA_HARD_NORMA * sqrt((67 + 1) / 67)),
    
    # 3. Classificazione Diagnostica basata sullo Z_Vero
    Diagnosi = case_when(
      Match_Masc >= 13 & Mismatch_Hard >= 13 ~ "TASK FAILURE",
      Match_Masc >= 13                      ~ "BASELINE CRASH",
      Mismatch_Hard >= 13                   ~ "MISMATCH COLLAPSE",
      Z_Vero > 1.96                         ~ "DEFICIT MARCATO",
      Z_Vero > 1.65                         ~ "DEFICIT LIEVE",
      Z_Vero > 1.00                         ~ "RISCHIO MODERATO",
      TRUE                                  ~ "NELLA NORMA"
    ),
    
    # Variabile per i grafici (cap ai valori estremi per leggibilità)
    Z_Grafico = case_when(
      Z_Vero > 4 ~ 4,
      Z_Vero < -2 ~ -2,
      TRUE ~ Z_Vero
    )
  )

# Verifica il risultato finale
print(df_diagnosi_final)



# =============================================================
# 1. PREPARAZIONE DATI PER IL GRAFICO (FORMATO LONG)
# =============================================================

# Creiamo un dataset apposta per il plot partendo dai tuoi dati puliti
df_per_grafico <- df_dislessici_out %>%
  mutate(
    c_clean = str_trim(str_remove(tolower(as.character(cond)), "g_")),
    b_clean = str_trim(tolower(as.character(bias_di_genere))),
    is_corretto = if_else(tolower(as.character(corretto)) %in% c("si", "yes", "1"), 1, 0)
  ) %>%
  group_by(part, c_clean, b_clean) %>%
  summarise(
    Accuracy = mean(is_corretto, na.rm=TRUE),
    RT_Corretti = mean(rt_nome[is_corretto == 1], na.rm=TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Calcoliamo IES grezzo e Log_IES
    Raw_IES = RT_Corretti / Accuracy,
    Log_IES = log(Raw_IES)
  ) %>%
  # Filtriamo i crash infiniti per non rompere il grafico
  filter(is.finite(Log_IES))

# =============================================================
# 2. GENERAZIONE GRAFICO 2x2 (DENSITÀ E RETTA)
# =============================================================

graphics.off()
png("Normalita_Dislessici_Confronto.png", width = 2800, height = 2400, res = 300)
par(mfrow=c(2,2), mar=c(5,5,4,2))

# --- PRIMA: DATI GREZZI ---
# Densità
plot(density(df_per_grafico$Raw_IES, na.rm=TRUE), 
     main="A. Densità IES Grezzi", 
     xlab="IES (ms)", col="firebrick", lwd=2)

# Q-Q Plot con Retta
qqnorm(df_per_grafico$Raw_IES, main="B. Q-Q Plot IES Grezzi", col="firebrick")
qqline(df_per_grafico$Raw_IES, col="black", lwd=2)

# --- DOPO: TRASFORMAZIONE LOG ---
# Densità
plot(density(df_per_grafico$Log_IES, na.rm=TRUE), 
     main="C. Densità Log-IES", 
     xlab="Log-IES", col="dodgerblue4", lwd=2)

# Q-Q Plot con Retta
qqnorm(df_per_grafico$Log_IES, main="D. Q-Q Plot Log-IES", col="dodgerblue4")
qqline(df_per_grafico$Log_IES, col="black", lwd=2)

dev.off()

cat("Grafico salvato con successo!")
# ==============================================================================
# DIAGNOSI CLINICA DEFINITIVA (CORREZIONE SOGGETTO 8)
# ==============================================================================
#IES
# ==============================================================================
# 1. PREPARAZIONE DATI DA 'df_intero'
# ==============================================================================

# Usiamo df_intero come indicato
df_ies_prep <- df_dislessici_out %>%
  mutate(
    # 1. LOGICA CORRETTEZZA
    is_corretto = case_when(
      tolower(as.character(corretto)) %in% c("yes") ~ 1, # 1 = Giusto
      TRUE ~ 0 # 0 = Sbagliato
    ),
    
    # 2. PULIZIA ETICHETTE (Standardizzazione Match/Mismatch)
    c_clean = str_trim(str_remove(tolower(as.character(cond)), "g_")),
    b_clean = str_trim(tolower(as.character(bias_di_genere))),
    
    cond_cruzada = case_when(
      c_clean == "fem" & b_clean == "masc" ~ "Mismatch_Hard",
      c_clean == "masc" & b_clean == "fem" ~ "Mismatch_Soft",
      c_clean == "fem" & b_clean == "fem"  ~ "Match_Fem",
      c_clean == "masc" & b_clean == "masc" ~ "Match_Masc",
      TRUE ~ NA_character_
    )
  ) %>%
  # Filtriamo solo le righe che hanno una condizione valida
  filter(!is.na(cond_cruzada))

# ==============================================================================
# 2. CALCOLO ACCURATEZZA E RT (Separati)
# ==============================================================================

# A. ACCURATEZZA (Calcolata su TUTTE le prove presenti in df_intero)
acc_stats <- df_ies_prep %>%
  group_by(part, cond_cruzada) %>%
  summarise(
    Accuracy = mean(is_corretto), 
    .groups = "drop"
  )

# B. TEMPI DI REAZIONE (Calcolati SOLO sulle risposte CORRETTE)
# Nota: Se uno ha 0% accuratezza, qui avrà NaN, ma lo gestiamo dopo.
rt_stats <- df_ies_prep %>%
  filter(is_corretto == 1) %>% 
  group_by(part, cond_cruzada) %>%
  summarise(
    Mean_RT = mean(rt_nome, na.rm = TRUE), 
    .groups = "drop"
  )

# ==============================================================================
# 3. UNIONE E CALCOLO IES
# ==============================================================================


df_ies_final <- full_join(acc_stats, rt_stats, by = c("part", "cond_cruzada")) %>%
  mutate(
    # Gestione divisione per zero o NaN
    # Se Accuracy è 0, mettiamo 0.01 per far esplodere il punteggio (come deve essere)
    Accuracy_Adj = ifelse(Accuracy == 0 | is.na(Accuracy), 0.01, Accuracy),
    
    # Se Mean_RT è mancante (perché ha sbagliato tutto), stimiamo un tempo "penalizzato"
    # (es. 5000ms) oppure teniamo NA. Per ora teniamo NA, ma col fix sopra non dovrebbe servire.
    RT_Adj = ifelse(is.na(Mean_RT), 5000, Mean_RT), 
    
    # FORMULA IES: Tempo / Accuratezza
    IES = RT_Adj / Accuracy_Adj
  )
df_ies_final <- df_ies_final %>%
  mutate(
    # Se accuratezza è 0, l'IES diventa 500.000 (segnala il fallimento)
    IES = ifelse(Accuracy == 0, 500000, Mean_RT / Accuracy)
  )

print("--- ANTEPRIMA IES CALCOLATO ---")
print(df_ies_final)

# --- DIAGNOSI DESCRITTIVA (MS GREZZI) ---
profilo_ies_raw <- df_ies_final %>%
  # Nota: assicurati che df_ies_final qui contenga l'IES non logaritmico
  dplyr::select(part, cond_cruzada, IES) %>%
  pivot_wider(names_from = cond_cruzada, values_from = IES) %>%
  
  mutate(
    # 1. Delta in millisecondi (più facile da spiegare a voce)
    Delta_Ms_Hard = Mismatch_Hard - Match_Masc,
    Delta_Ms_Soft = Mismatch_Soft - Match_Fem,
    
    # 2. SOGLIA DI CROLLO (10.000 ms = 10 secondi di IES sono un fallimento)
    Soglia_Crash = 10000, 
    
    Diagnosi_Raw = case_when(
      Match_Masc > Soglia_Crash ~ "CROLLO BASELINE (Saturazione)",
      Mismatch_Hard > Soglia_Crash ~ "CROLLO AL CONFLITTO (Saturazione)",
      Delta_Ms_Hard > 2000 ~ "VULNERABILITÀ ESTREMA (>2s)",
      Delta_Ms_Hard > 500  ~ "VULNERABILITÀ MODERATA",
      Delta_Ms_Hard < 0    ~ "RESILIENZA / PARADOSSO",
      TRUE                 ~ "NELLA NORMA (RT stabili)"
    )
  ) %>%
  dplyr::select(part, Diagnosi_Raw, Delta_Ms_Hard, Match_Masc, Mismatch_Hard)

print(profilo_ies_raw)

#IES


cat("\n--- Parametri Normativi Applicati ---\n")
cat("Media:", media_normativa, " SD:", sd_normativa, " N:", n_normativa, "\n")


# 1. Definizione funzione di Crawford (Nomi variabili allineati)
test_crawford <- function(patient_val, control_mean, control_sd, control_n) {
  # t-statistica di Crawford
  t_val <- (patient_val - control_mean) / (control_sd * sqrt((control_n + 1) / control_n))
  
  # Percentile clinico: se il soggetto è molto lento, il percentile sarà alto (es. 98%)
  p_clinico <- pt(t_val, df = control_n - 1, lower.tail = TRUE)
  
  return(list(t = t_val, p = p_clinico))
}

tabella_diagnostica_finalissima <- df_diagnosi_final %>%
  rowwise() %>%
  mutate(
    # 1. Calcolo del Delta individuale (Log-IES Mismatch - Log-IES Match)
    Delta_Paziente = Mismatch_Hard - Match_Masc,
    
    # 2. ESECUZIONE TEST DI CRAWFORD
    # patient_val  = Delta_Paziente (Il rallentamento del dislessico)
    # control_mean = DELTA_HARD     (Il rallentamento medio dei sani)
    # control_sd   = SD_DELTA_HARD_NORMA (Variabilità del rallentamento dei sani)
    # control_n    = 67             (Il numero dei controlli)
    
    Risultato_C = list(test_crawford(Delta_Paziente, DELTA_HARD, SD_DELTA_HARD_NORMA, 67)),
    
    Z_Vero = Risultato_C$t,
    Percentile_Clinico = Risultato_C$p * 100,
    
    # 3. DIAGNOSI CLINICA (Mappatura dei Fenotipi)
    Stato_Clinico = case_when(
      Match_Masc >= 13 & Mismatch_Hard >= 13 ~ "TASK FAILURE",
      Match_Masc >= 13                      ~ "BASELINE CRASH",
      Mismatch_Hard >= 13                   ~ "MISMATCH COLLAPSE",
      Z_Vero > 1.96                         ~ "DEFICIT MARCATO",
      Z_Vero > 1.65                         ~ "DEFICIT LIEVE",
      Z_Vero > 1.00                         ~ "RISCHIO MODERATO",
      TRUE                                  ~ "NELLA NORMA"
    ),
    
    # 4. Formattazione per la Tesi
    Percentile_Tesi = case_when(
      Stato_Clinico %in% c("TASK FAILURE", "BASELINE CRASH") ~ "N.A.",
      Percentile_Clinico > 99.9 ~ "> 99.9",
      Percentile_Clinico < 0.1  ~ "< 0.1",
      TRUE ~ as.character(round(Percentile_Clinico, 2))
    )
  ) %>%
  # Selezioniamo le colonne per il capitolo finale dei risultati
  dplyr::select(p_id, Delta_Paziente, Z_Vero, Percentile_Tesi, Stato_Clinico)

# ==============================================================================
# 3. VISUALIZZAZIONE RISULTATI
# ==============================================================================
print("--- TABELLA DIAGNOSTICA DEFINITIVA (CRAWFORD TEST SUI DELTA) ---")
print(tabella_diagnostica_finalissima)


# ==============================================================================
# 0. SOGLIA CLINICA (Z = 1.65 → ms)
# ==============================================================================
soglia_clinica_ms <- media_sani + (1.65 * sd_sani)

# ==============================================================================
# 2. GRAFICO CLINICO FINALE
# ==============================================================================
# ==============================================================================
# GRAFICO DEFINITIVO: GRAVITÀ DELLA DISFUNZIONE (Z-SCORE)
# ==============================================================================

# 1. Preparazione dati con scala "Gravità"
df_finale_plot <- tabella_diagnostica_finalissima %>%
  mutate(
    # Fissiamo l'altezza visiva (Z_Plot): 
    # Crash e Failure devono stare in alto (quota 4) per mostrare la gravità.
    # Gli altri usano il valore assoluto dello Z-score per stare sopra lo zero.
    Z_Plot = case_when(
      Stato_Clinico == "TASK FAILURE"      ~ 4.0,
      Stato_Clinico == "BASELINE CRASH"    ~ 4.0,
      Stato_Clinico == "MISMATCH COLLAPSE" ~ 4.0,
      TRUE                                 ~ abs(Z_Vero) 
    ),
    # Etichetta con il valore Z originale (per non perdere il dato scientifico)
    Etichetta_Z = round(Z_Vero, 2)
  )

# 2. Generazione del Grafico (Stile Immagine 2)
ggplot(df_finale_plot, aes(x = factor(p_id), y = Z_Plot, fill = Stato_Clinico)) +
  
  # --- BARRE ---
  geom_col(color = "black", width = 0.7, size = 0.4) +
  
  # --- LINEA SOGLIA (Z = 1.65) ---
  geom_hline(yintercept = 1.65, linetype = "dashed", color = "black", linewidth = 0.8) +
  
  # --- ETICHETTE VALORI ---
  geom_text(aes(label = Etichetta_Z), vjust = -0.8, size = 4.5, fontface = "bold") +
  
  # --- COLORI RICHIESTI ---
  scale_fill_manual(values = c(
    "TASK FAILURE"      = "black",   # Nero
    "BASELINE CRASH"    = "red",     # Rosso
    "MISMATCH COLLAPSE" = "#8B1A1A",  # Amaranto/Maroon
    "RISCHIO MODERATO"     = "blue",    # Blu
    "NELLA NORMA"       = "#008B45"   # Verde
  )) +
  
  # --- ASSE Y CATEGORICO ---
  scale_y_continuous(
    limits = c(0, 4.8),
    breaks = c(0, 1.65, 3, 4),
    labels = c("0", "Soglia (1.65)", "3", "Crash/Deficit")
  ) +
  
  # --- TESTO SOGLIA ---
  annotate("text", x = 0.6, y = 1.85, label = "Soglia Deficit (Z=1.65)", 
           color = "black", fontface = "italic", size = 3.5, hjust = 0) +
  
  # --- LABELS ---
  labs(
    title = "Profilo Clinico Individuale: Saturazione delle Risorse",
    subtitle = "Z-Score basati su Crawford Test (Norma N=67)",
    x = "Partecipante (ID)",
    y = "Gravità della Disfunzione (Z-Score)",
    fill = "Stato Clinico"
  ) +
  
  # --- TEMA ---
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12, color = "black", face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    plot.title = element_text(size = 16, face = "bold")
  )
ggsave("IES_Profilazione_Clinica_Final.png",
       width = 13,
       height = 8,
       dpi = 300)

#=======================ANALISI DEGLI ERRORI====================================

error_analysis_match_mismatch <- df_ies_prep %>%
  mutate(
    is_errore = 1 - is_corretto,
    congruenza = case_when(
      str_detect(cond_cruzada, "Match") ~ "Congruente",
      str_detect(cond_cruzada, "Mismatch") ~ "Incongruente"
    )
  ) %>%
  group_by(part, congruenza) %>%
  summarise(
    N_trial = n(),
    Tot_Errori = sum(is_errore),
    Accuracy = mean(is_corretto) * 100,
    .groups = "drop"
  )

ggplot(error_analysis_match_mismatch,
       aes(x = congruenza, y = Accuracy, fill = congruenza)) +
  geom_col(color = "black", width = 0.6) +
  facet_wrap(~ part) +
  scale_fill_manual(values = c(
    "Congruente" = "#9E9E9E",
    "Incongruente" = "#D32F2F"
  )) +
  labs(
    title = "Distribuzione degli errori per congruenza stereotipica",
    subtitle = "Confronto entro-soggetto",
    x = "Condizione",
    y = "Accuracy"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )
ggsave("Analisi_errori_DG.png",
       width = 13,
       height = 8,
       dpi = 300)

errori_mm_partecipante <- df_ies_prep %>%
  mutate(
    tipo_condizione = case_when(
      cond_cruzada %in% c("Mismatch_Hard", "Mismatch_Soft") ~ "Mismatch",
      cond_cruzada %in% c("Match_Fem", "Match_Masc") ~ "Match",
      TRUE ~ NA_character_
    ),
    is_errore = if_else(is_corretto == 0, 1, 0)
  ) %>%
  filter(!is.na(tipo_condizione)) %>%
  group_by(part, tipo_condizione) %>%
  summarise(
    Tot_Errori = sum(is_errore),
    Accuracy = mean(is_corretto) * 100,
    .groups = "drop"
  )



indice_bias_mm <- errori_mm_partecipante %>%
  dplyr::select(part, tipo_condizione, Tot_Errori) %>%
  tidyr::pivot_wider(
    names_from = tipo_condizione,
    values_from = Tot_Errori,
    values_fill = 0
  ) %>%
  mutate(
    Indice_Bias_MM = Mismatch - Match,
    Direzione_Bias_MM = case_when(
      Indice_Bias_MM > 0 ~ "Mismatch > Match",
      Indice_Bias_MM < 0 ~ "Match > Mismatch",
      TRUE ~ "Nessuna differenza"
    )
  )

df_errori_wide <- error_analysis_match_mismatch %>%
  pivot_wider(
    id_cols = part,                     
    names_from = congruenza,            # Prende "Congruente" e "Incongruente"
    values_from = c(Tot_Errori, Accuracy) # Prende i valori calcolati
  )

# ==============================================================================
# UNIONE FINALE: DIAGNOSI CRONOMETRICA + ANALISI ERRORI
# ==============================================================================

tabella_finale_completa <- tabella_diagnostica_finalissima %>%
  # 1. Selezioniamo le colonne chiave dalla diagnostica blindata
  # Usiamo Delta_Paziente
  dplyr::select(p_id, 
                Stato_Clinico, 
                Delta_Paziente, 
                Z_Vero) %>%
  
  # 2. Prepariamo l'ID per il join
  mutate(part = as.numeric(as.character(p_id))) %>%
  
  # 3. Uniamo i dati degli errori (Tot_Errori_Congruente, Tot_Errori_Incongruente, etc.)
  left_join(df_errori_wide, by = "part") %>%
  
  # 4. Pulizia estetica: teniamo p_id come riferimento e ordiniamo
  dplyr::select(p_id, Stato_Clinico, Z_Vero, Delta_Paziente, 
                starts_with("Tot_Errori"), starts_with("Accuracy")) %>%
  arrange(as.numeric(as.character(p_id)))

# 5. VISUALIZZAZIONE FINALE
print("--- TABELLA INTEGRATA (TEMPI + ERRORI) ---")
print(tabella_finale_completa)



#ANALISI CORRELAZIONE

correlazione_clinica <- cor.test(df_diagnosi_final$Match_Masc, 
                                 df_diagnosi_final$Delta_Paziente, 
                                 method = "spearman")

print(correlazione_clinica)




# ==============================================================================
# PREPARAZIONE DATI PER INTERACTION PLOT (LOG-IES)
# ==============================================================================

df_plot_diagnostico <- df_diagnosi_final %>%
  # Prendiamo i tempi logaritmici e la diagnosi
  dplyr::select(p_id, Match_Masc, Mismatch_Hard, Diagnosi) %>%
  
  # Trasformiamo in formato lungo per ggplot
  pivot_longer(cols = c(Match_Masc, Mismatch_Hard), 
               names_to = "Condizione", 
               values_to = "Log_IES") %>%
  
  mutate(
    # Ordiniamo le condizioni: prima la base, poi il conflitto
    Condizione = factor(Condizione, levels = c("Match_Masc", "Mismatch_Hard")),
    Cond_Lab = ifelse(Condizione == "Match_Masc", "Base\n(Match)", "Conflitto\n(Hard)")
  )

print(df_plot_diagnostico)



ggplot(df_plot_diagnostico,
       aes(x = Condizione,
           y = Log_IES,
           group = p_id,
           color = Diagnosi)) +
  
  # Linee che collegano i punti per ogni soggetto
  geom_line(linewidth = 1.2, alpha = 0.8) +
  
  # Punti sulle condizioni
  geom_point(size = 4) +
  
  # Colori per diagnosi clinica
  scale_color_manual(values = c(
    "MISMATCH COLLAPSE" = "#8B1A1A",  # Rosso scuro
    "BASELINE CRASH"    = "red",      # Rosso
    "TASK FAILURE"      = "black",    # Nero
    "RISCHIO MODERATO"  = "blue",     # Blu
    "NELLA NORMA"       = "#008B45"   # Verde
  )) +
  
  # Etichette ID (solo sulla condizione di conflitto)
  geom_text_repel(
    data = df_plot_diagnostico %>% 
      filter(Condizione == "Mismatch_Hard"),
    aes(label = p_id),
    fontface = "bold",
    size = 4,
    nudge_x = 0.15,
    direction = "y",
    segment.color = "grey50",
    segment.size = 0.4,
    show.legend = FALSE
  ) +
  
  # Scale
  scale_y_continuous(
    limits = c(6, 14),
    breaks = seq(6, 14, 2)
  ) +
  
  scale_x_discrete(
    labels = c(
      "Match_Masc"   = "Base (Match)",
      "Mismatch_Hard" = "Conflitto (Hard)"
    ),
    expand = expansion(add = c(0.2, 0.6))
  ) +
  
  # Etichette e titoli
  labs(
    title = "Effetto del conflitto di genere sui casi singoli",
    subtitle = "Interazione tra efficienza basale e carico stereotipico",
    x = "Condizione sperimentale",
    y = "Costo cognitivo (Log-IES)",
    color = "Diagnosi clinica"
  ) +
  
  # Tema
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("Interazione_cond_DG.png",
       width = 13,
       height = 8,
       dpi = 300)


# ==============================================================================
# PREPARAZIONE DATI 
# ==============================================================================

df_plot_individuale <- profilo_ies_corretto %>%
  dplyr::select(part, Match_Masc, Mismatch_Hard) %>%
  pivot_longer(cols = c(Match_Masc, Mismatch_Hard), 
               names_to = "Condizione", 
               values_to = "IES") %>%
  mutate(
    part = as.factor(part),
    Condizione = factor(Condizione, levels = c("Match_Masc", "Mismatch_Hard"))
  )


# ==============================================================================
# CONFRONTO TRA GRUPPI: SANI vs DISLESSICI (T-TEST SUL DELTA)
# ==============================================================================

# 1. Prepariamo i Delta dei Sani (Controllo)
delta_sani <- ies_per_soggetto %>%
  filter(Tipo %in% c("Match_Masc", "Mismatch_Hard")) %>%
  pivot_wider(id_cols = part, names_from = Tipo, values_from = log_IES) %>%
  mutate(Delta = Mismatch_Hard - Match_Masc, Gruppo = "Controllo") %>%
  dplyr::select(Gruppo, Delta)

# 2. Prepariamo i Delta degli 8 Dislessici 
delta_dislessici <- tabella_diagnostica_finalissima %>%
  mutate(Delta = Delta_Paziente, Gruppo = "Dislessici") %>%
  dplyr::select(Gruppo, Delta)

# 3. Uniamo i database e lanciamo il test
df_confronto <- bind_rows(delta_sani, delta_dislessici)

# Usiamo il Welch T-Test che non assume varianze uguali (Fondamentale visto che i dislessici variano molto di più)
risultato_t <- t.test(Delta ~ Gruppo, data = df_confronto)

print(risultato_t)

# 4. Grafico Definitivo
ggplot(df_confronto, aes(x = Gruppo, y = Delta, fill = Gruppo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, aes(color = Gruppo)) +
  
  scale_fill_manual(values = c("Controllo" = "gray80", "Dislessici" = "#ffcdd2")) +
  scale_color_manual(values = c("Controllo" = "gray30", "Dislessici" = "#b71c1c")) +
  
  labs(
    title = "Confronto Gruppi: Impatto del Bias (Hard)",
    subtitle = paste("T-test su Log-IES, p =", round(risultato_t$p.value, 4)),
    y = "Costo dell'Integrazione (Delta Log-IES)",
    x = ""
  ) +
  theme_classic()

ggsave("Boxplot_t_test_medie.png", width = 9, height = 7)


# ==============================================================================
# 2. CALCOLO STATISTICO: F-TEST SULLA VARIANZA (Eterogeneità)
# ==============================================================================

# 1. UNIONE DATI
df_confronto_totale <- bind_rows(delta_sani, delta_dislessici)

# 2. CALCOLO F-TEST SULLA VARIANZA
test_var <- var.test(Delta ~ Gruppo, data = df_confronto_totale)

p_val_f <- test_var$p.value
f_stat  <- test_var$statistic

# Formattazione etichetta per il grafico
label_p_f <- ifelse(p_val_f < 0.001, "p < .001 ***", paste("p =", round(p_val_f, 3)))

print("--- RISULTATO F-TEST SULLA VARIANZA ---")
print(test_var)

# 3. GRAFICO 
ggplot(df_confronto_totale, aes(x = Gruppo, y = Delta, fill = Gruppo)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = Gruppo), width = 0.15, size = 3, alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  coord_cartesian(ylim = c(min(df_confronto_totale$Delta) - 1,
                           max(df_confronto_totale$Delta) + 1)) + 
  
  # Barra della significatività
  annotate("segment", x = 1, xend = 2, y = 6.5, yend = 6.5, size = 0.6) +
  annotate("text", x = 1.5, y = 7.2, label = paste("F-test (Varianza):", label_p_f), 
           size = 4.5, fontface = "bold") +
  
  scale_fill_manual(values = c("Controllo" = "gray80", "Dislessici" = "#ffcdd2")) +
  scale_color_manual(values = c("Controllo" = "gray30", "Dislessici" = "#b71c1c")) +
  
  labs(
    title = "Eterogeneità della Risposta: Sani vs Dislessici",
    subtitle = "Confronto della dispersione del costo cognitivo (Delta Log-IES)",
    y = "Costo di Integrazione (Delta Log-IES)",
    x = ""
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggsave("F_test_varianza_completo.png", width = 9, height = 7)


# 1. Preparazione dati per TUTTI gli 8 soggetti: FENOTIPI
df_fenotipi_plot <- tabella_finale_completa %>%
  mutate(
    Delta_Errori_Base = Tot_Errori_Incongruente - Tot_Errori_Congruente,
    
    # FIX LOGICO PER S8 E TASK FAILURE:
    # Se il soggetto ha accuratezza 0 ovunque, gli assegnamo un valore convenzionale 
    # sulla X (es. 5) per portarlo nell'area di "Collasso"
    Delta_Errori = case_when(
      Stato_Clinico == "TASK FAILURE" ~ 5.0, 
      Stato_Clinico == "BASELINE CRASH" & Delta_Errori_Base == 0 ~ 4.5,
      TRUE ~ Delta_Errori_Base
    ),
    
    Z_Abs = abs(Z_Vero),
    Label = paste0("S", p_id)
  )


# 2. Creazione della Mappa Fenotipica
ggplot(df_fenotipi_plot, aes(x = Delta_Errori, y = Z_Abs)) +
  annotate("rect", xmin = 0.5, xmax = 5, ymin = 2, ymax = 15, alpha = .1, fill = "red") +   # Area di Collasso
  annotate("rect", xmin = -3, xmax = 0.5, ymin = -0.5, ymax = 2, alpha = .1, fill = "green") + # Area di Resilienza
  
  # Linee di riferimento (Zero errori e Soglia Z=2)
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 1.96, linetype = "dotted", color = "red", alpha = 0.5) +
  
  # Punti e Etichette
  geom_point(aes(color = Stato_Clinico), size = 6, alpha = 0.8) +
  geom_text(aes(label = Label), vjust = -1.2, fontface = "bold", size = 4.5) +
  
  scale_color_manual(values = c(
    "MISMATCH COLLAPSE" = "#8B1A1A", 
    "BASELINE CRASH"    = "#D32F2F",
    "TASK FAILURE"      = "black",
    "RISCHIO MODERATO"  = "#1976D2",
    "NELLA NORMA"       = "#388E3C"
  )) +
  
  labs(
    title = "Mappa Fenotipica della Risposta al Bias",
    subtitle = "Sintesi tra Rallentamento Esecutivo (Z) e Caduta dell'Accuratezza (Δ Errori)",
    x = "Incremento degli Errori (Incongruente - Congruente)",
    y = "Distanza statistica dalla Norma (|Z-Score|)",
    color = "Profilo Clinico"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16)
  ) +

  scale_y_continuous(limits = c(-0.5, 14)) +
  scale_x_continuous(limits = c(-3, 5))

ggsave("Mappa_Fenotipica_Clinica.png", width = 10, height = 7)

# ==============================================================================
# ANALISI DELL'EFFETTO INDIVIDUALE (COHEN'S D)
# ==============================================================================

# 1. Assicuriamoci che i criteri di crash abbiano UNA SOLA riga per soggetto
criteri_crash <- tabella_diagnostica_finalissima %>%
  mutate(part = as.numeric(as.character(p_id))) %>%
  dplyr::select(part, Z_Vero) %>%
  distinct(part, .keep_all = TRUE) # Fondamentale per evitare duplicati

# 2. Calcoliamo le medie (una riga per soggetto)
analisi_riassunto <- df_dislessici_out %>%
  mutate(
    part = as.numeric(as.character(part)),
    log_RT = log(as.numeric(rt_nome)),
    cond_cruzada = case_when(
      str_detect(cond, "fem")  & bias_di_genere == "masc" ~ "Mismatch_Hard",
      str_detect(cond, "masc") & bias_di_genere == "fem"  ~ "Mismatch_Soft",
      str_detect(cond, "fem")  & bias_di_genere == "fem"  ~ "Match_Fem",
      str_detect(cond, "masc") & bias_di_genere == "masc" ~ "Match_Masc",
      TRUE ~ NA_character_
    ),
    Cond_Num = ifelse(str_detect(cond_cruzada, "Match"), 0, 1)
  ) %>%
  filter(corretto == "yes") %>% 
  group_by(part) %>%
  summarise(
    N_Match = sum(Cond_Num == 0),
    N_Mismatch = sum(Cond_Num == 1),
    Media_Match = mean(log_RT[Cond_Num == 0], na.rm = TRUE),
    Media_Mismatch = mean(log_RT[Cond_Num == 1], na.rm = TRUE),
    SD_Match = sd(log_RT[Cond_Num == 0], na.rm = TRUE),
    SD_Mismatch = sd(log_RT[Cond_Num == 1], na.rm = TRUE),
    .groups = "drop"
  )

# 3. UNIAMO i dati e applichiamo la regola clinica
analisi_tutti_effetti <- analisi_riassunto %>%
  left_join(criteri_crash, by = "part") %>%
  mutate(
    Pooled_SD = sqrt((SD_Match^2 + SD_Mismatch^2) / 2),
    # REGOLA: Se Z <= -8, l'effetto non si calcola (Rumore)
    Stereotype_Effect_Size = ifelse(Z_Vero <= -8, NA, (Media_Mismatch - Media_Match) / Pooled_SD)
  ) %>%
  mutate(
    Magnitudo = case_when(
      Z_Vero <= -8 ~ "BASELINE CRASH",
      is.na(Stereotype_Effect_Size) ~ "Incalcolabile/Crash",
      Stereotype_Effect_Size > 0.8  ~ "EFFETTO GRANDE",
      Stereotype_Effect_Size > 0.5  ~ "EFFETTO MEDIO",
      Stereotype_Effect_Size > 0.2  ~ "EFFETTO PICCOLO",
      Stereotype_Effect_Size < 0    ~ "Inversione/Rumore",
      TRUE ~ "Trascurabile"
    )
  )

print(analisi_tutti_effetti)



# --- 1. PREPARAZIONE DATI PER IL GRAFICO ---
grafico_data <- analisi_tutti_effetti %>%
  mutate(
    part = factor(part),
    # Per i crash (NA) mettiamo un'altezza fissa di 1.2 per renderli visibili
    Plot_Size = ifelse(is.na(Stereotype_Effect_Size), 1.2, Stereotype_Effect_Size)
  )

# --- 2. GENERAZIONE GRAFICO ---
ggplot(grafico_data, aes(x = part, y = Plot_Size, fill = Magnitudo)) +
  
  # LIVELLO 1: Barre per tutti tranne S7
  geom_bar(data = filter(grafico_data, part != 7), 
           stat = "identity", width = 0.7, color = "black", size = 0.3) +
  
  # LIVELLO 2: Barra speciale per S7 (Trasparente e Tratteggiata)
  geom_bar(data = filter(grafico_data, part == 7), 
           stat = "identity", width = 0.7, color = "black", fill = NA, 
           linetype = "dashed", size = 0.5) +
  
  # Linee di riferimento (Soglie)
  geom_hline(yintercept = c(0.2, 0.5, 0.8), linetype = "dotted", color = "gray40", alpha = 0.6) +
  

  scale_fill_manual(values = c(
    "BASELINE CRASH"    = "#000000", # Nero pieno
    "EFFETTO GRANDE"    = "#d73027", # Rosso
    "EFFETTO MEDIO"     = "#fc8d59", # Arancio
    "EFFETTO PICCOLO"   = "#fee090", # Giallo
    "Inversione/Rumore" = "#2ca02c", # Verde
    "Trascurabile"      = "#e0f3f8"  # Azzurrino
  ), na.translate = FALSE) + 
  
  # Numeri sopra le barre
  geom_text(data = filter(grafico_data, !is.na(Stereotype_Effect_Size)),
            aes(label = round(Stereotype_Effect_Size, 2)), 
            vjust = ifelse(filter(grafico_data, !is.na(Stereotype_Effect_Size))$Plot_Size > 0, -0.6, 1.6), 
            size = 3.8, fontface = "bold") +
  
  # ETICHETTA SOGLIA
  annotate("label", x = 0.5, y = 0.8, label = "Soglia Effetto Grande (d > 0.8)", 
           hjust = 0, size = 3, color = "gray20", fill = "white", label.size = NA, fontface = "italic") +
  
  # Titoli e Label
  labs(
    title = "Impatto Individuale dello Stereotipo (Cohen's d)",
    subtitle = "S7: Incalcolabile (Mismatch Collapse) | S2, S4: Baseline Crash",
    x = "Soggetto (ID)",
    y = "Effect Size (d di Cohen)",
    fill = "Interpretazione Clinica"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  ) +
  coord_cartesian(ylim = c(-1, 3.8))

ggsave("Magnitudo_Stereotipo.png", width = 10, height = 7)


df_dislessici_out %>% 
  filter(part == 3) %>% 
  group_by(cond, bias_di_genere) %>% 
  summarise(
    Totale_Prove = n(),
    Corrette = sum(corretto == "yes"),
    Errori = sum(corretto == "no"),
    Accuratezza = mean(corretto == "yes") * 100
  )


# 1. Prepariamo la tabella diagnostica
diagnostica_clean <- tabella_diagnostica_finalissima %>%
  mutate(p_id = as.numeric(as.character(p_id))) %>%
  dplyr::select(p_id, Z_Vero, Stato_Clinico)

# 2. Flusso unico: Calcolo medie -> Join -> Labeling Clinico
tabella_unificata <- df_dislessici_out %>%
  mutate(
    p_id = as.numeric(as.character(part)),
    log_RT = log(as.numeric(rt_nome)),
    cond_cruzada = case_when(
      str_detect(cond, "fem")  & bias_di_genere == "masc" ~ "Mismatch_Hard",
      str_detect(cond, "masc") & bias_di_genere == "fem"  ~ "Mismatch_Soft",
      str_detect(cond, "fem")  & bias_di_genere == "fem"  ~ "Match_Fem",
      str_detect(cond, "masc") & bias_di_genere == "masc" ~ "Match_Masc"
    ),
    is_match = str_detect(cond_cruzada, "Match")
  ) %>%
  # Rimuoviamo il filtro corretto == "yes" qui per non perdere S8, 
  # lo useremo dentro il summarise
  group_by(p_id) %>%
  summarise(
    Media_Match = mean(log_RT[is_match & corretto == "yes"], na.rm = TRUE),
    Media_Mismatch = mean(log_RT[!is_match & corretto == "yes"], na.rm = TRUE),
    SD_Match = sd(log_RT[is_match & corretto == "yes"], na.rm = TRUE),
    SD_Mismatch = sd(log_RT[!is_match & corretto == "yes"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(diagnostica_clean, by = "p_id") %>%
  mutate(
    Pooled_SD = sqrt((SD_Match^2 + SD_Mismatch^2) / 2),
    
    # Calcolo d di Cohen
    Stereotype_Effect_Size = ifelse(Stato_Clinico %in% c("BASELINE CRASH", "TASK FAILURE", "MISMATCH COLLAPSE"), 
                                    NA, (Media_Mismatch - Media_Match) / Pooled_SD),
    
    # Magnitudo D corretta
    Magnitudo_D = case_when(
      Stato_Clinico == "TASK FAILURE"     ~ "TASK FAILURE (Acc. 0%)",
      Stato_Clinico == "BASELINE CRASH"   ~ "BASELINE CRASH",
      Stato_Clinico == "MISMATCH COLLAPSE" ~ "MISMATCH COLLAPSE",
      is.na(Stereotype_Effect_Size)       ~ "Dati Insufficienti",
      Stereotype_Effect_Size > 0.8        ~ "EFFETTO GRANDE",
      Stereotype_Effect_Size > 0.5        ~ "EFFETTO MEDIO",
      Stereotype_Effect_Size > 0.2        ~ "EFFETTO PICCOLO",
      Stereotype_Effect_Size < 0          ~ "INVERSIONE (Resilienza)",
      TRUE                                ~ "Trascurabile"
    ),
    
    # Sintesi Diagnostica per le Conclusioni
    Sintesi_Diagnostica = case_when(
      Stato_Clinico == "TASK FAILURE"      ~ "Collasso sistemico (Accuratezza nulla)",
      Stato_Clinico == "BASELINE CRASH"    ~ "Saturazione primaria delle risorse",
      Stato_Clinico == "MISMATCH COLLAPSE" ~ "Collasso selettivo al conflitto sociale",
      Stereotype_Effect_Size > 1.5         ~ "Vulnerabilità Esecutiva Massima (High Effort)",
      Stereotype_Effect_Size > 0.5         ~ "Vulnerabilità Esecutiva Moderata",
      TRUE                                 ~ "Profilo Resiliente o Compensato"
    )
  ) %>%
  dplyr::select(p_id, Z_Vero, Stato_Clinico, Stereotype_Effect_Size, Magnitudo_D, Sintesi_Diagnostica)
print(tabella_unificata)
