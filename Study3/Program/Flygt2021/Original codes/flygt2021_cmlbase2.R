# Set working directory to where this R program is
if (require("rstudioapi") && isAvailable()) {
  original_wd <- getwd()  # Store the original working directory
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)}
setwd(wd)

suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

setwd("/Users/yitche/SweCan_CML/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelsen")

# Kvalitetsregistret -----------------------------------------------------------
dfAll <- haven::read_sas("sos_kml1_au_21nov19.sas7bdat") %>% 
  dplyr::rename_all(tolower)

# Anmälan
dfKR <- dfAll %>%
  select(
    lopnr,
    kon_value,
    region_namn,
    d0_date,
    d0_year,
    d0_age,
    d0_sokalscoregrp,
    d0_eltsscoregrp,
    vitalstatus,
    vitalstatusdatum_estimat,
    a_tki
  ) %>% 
  arrange(lopnr, d0_date) %>% 
  distinct(lopnr, .keep_all = TRUE)

# Första utsättning från uppföljningsformuläret
dfKRFirstUtsatt <- dfAll %>%
  filter(u_behandtki %in% 1) %>%
  mutate(
    u0_tki_utsatt_rappdat = if_else(
      !is.na(u_datbedom),
      u_datbedom,
      u_inrappdtm
    )
  ) %>%
  select(
    lopnr,
    u0_tki_utsatt_rappdat,
    u0_tki_utsatt = u_behandtki,
    u0_tki_utsatt_date = u_behandtkidtm,
    u0_tki_utsatt_orsak = u_behandtkiorsak
  ) %>%
  arrange(
    lopnr,
    u0_tki_utsatt_date,
    u0_tki_utsatt_rappdat
  ) %>%
  distinct(lopnr, .keep_all = TRUE)

# Senaste uppföljningsformuläret där uppgift om utsättning är besvarad
dfKRFirstUtsattLatestUppf <- dfAll %>%
  filter(u_behandtki %in% 0:1) %>%
  mutate(
    u0_tki_utsatt_latest_rappdat = if_else(
      !is.na(u_datbedom),
      u_datbedom,
      u_inrappdtm
    )
  ) %>%
  select(
    lopnr,
    u0_tki_utsatt_latest_rappdat
  ) %>%
  arrange(
    lopnr,
    desc(u0_tki_utsatt_latest_rappdat)
  ) %>%
  distinct(lopnr, .keep_all = TRUE)

# Utsättningsformuläret --------------------------------------------------------
df <- haven::read_sas("sos_kml2_utsatt_21nov19.sas7bdat")

suppressMessages(
  dfUtsattning <- as_tibble(df) %>%
    
    # Variabelnamn till lowercase
    dplyr::rename_all(iconv) %>%
    dplyr::rename_all(tolower) %>%
    
    # Ta bort _värde-suffix
    rename_at(
      .vars = vars(ends_with("_värde")),
      .funs = list(~gsub(pattern = "_värde", replacement = "", x = .))
    )  %>%
    
    # factor till character
    mutate_if(is.factor, as.character) %>%
    
    # Variabelformat
    type_convert() %>%
    
    # Encoding
    dplyr::mutate_if(is.character, iconv) %>%
    
    filter(!is.na(ut_inr_inrappdtm))
)

# Första utsättning från STOP-formuläret
dfSTOPFirstUtsatt <- dfUtsattning %>%
  mutate(
    stop0_tki_utsatt_orsak = case_when(
      ut_tki_behbrasvar %in% 1 ~ 1,
      ut_tki_behallogen %in% 1 ~ 5,
      ut_tki_behbiverkan %in% 1 ~ 2,
      ut_tki_behdaligfolj %in% 1 ~ 3,
      ut_tki_behonskan %in% 1 ~ 6,
      ut_tki_behgraviditet %in% 1 ~ 7,
      ut_tki_behannat %in% 1 ~ 4
    )
  ) %>%
  select(
    lopnr,
    stop0_tki_utsatt_rappdat = ut_inr_inrappdtm,
    stop0_tki_utsatt = ut_tki_utsatt,
    stop0_tki_utsatt_date = ut_tki_utsattdtm,
    stop0_tki_utsatt_orsak
  ) %>%
  arrange(lopnr, desc(stop0_tki_utsatt), stop0_tki_utsatt_date) %>%
  distinct(lopnr, .keep_all = TRUE)

# Senaste STOP-formuläret där uppgift om utsättning är besvarad
dfSTOPFirstUtsattLatestUppf <- dfUtsattning %>%
  filter(ut_tki_utsatt %in% 0:1) %>%
  select(
    lopnr,
    stop0_tki_utsatt_latest_rappdat = ut_inr_inrappdtm
  ) %>%
  arrange(lopnr, desc(stop0_tki_utsatt_latest_rappdat)) %>%
  distinct(lopnr, .keep_all = TRUE)

# Merge ------------------------------------------------------------------------
df <- dfKR %>%
  left_join(
    dfSTOPFirstUtsatt,
    by = "lopnr"
  ) %>%
  left_join(
    dfSTOPFirstUtsattLatestUppf,
    by = "lopnr"
  ) %>%
  left_join(
    dfKRFirstUtsatt,
    by = "lopnr"
  ) %>%
  left_join(
    dfKRFirstUtsattLatestUppf,
    by = "lopnr"
  ) %>%
  mutate(
    final_tki_utsatt = case_when(
      stop0_tki_utsatt %in% 1 | u0_tki_utsatt %in% 1 ~ 1,
      stop0_tki_utsatt %in% 0 | !is.na(u0_tki_utsatt_latest_rappdat) ~ 0
    ),
    final_tki_utsatt_date = case_when(
      stop0_tki_utsatt %in% 1 ~ stop0_tki_utsatt_date,
      u0_tki_utsatt %in% 1 ~ u0_tki_utsatt_date
    ),
    final_tki_utsatt_orsak = case_when(
      stop0_tki_utsatt %in% 1 ~ stop0_tki_utsatt_orsak,
      u0_tki_utsatt %in% 1 ~ u0_tki_utsatt_orsak
    ),
    final_tki_utsatt_orsak = factor(
      final_tki_utsatt_orsak,
      levels = c(1, 5, 2, 3, 6, 7, 4, NA),
      exclude = NULL,
      labels = c(
        "Deep molecular response",
        "Before allo-SCT",
        "Adverse events",
        "Poor compliance",
        "Patient's wishes",
        "Pregnancy/planned pregnancy",
        "Other",
        "Missing"
      )
    )
  )

# Urval ------------------------------------------------------------------------
df <- df %>%
  filter(d0_year %in% 2007:2012)

addmargins(table(df$final_tki_utsatt_orsak, df$final_tki_utsatt, useNA = "ifany"))

# CMLBaSe2:
# ---------
# Totalt 582 fall med diagnosår 2007-2012,
# varav 231 st har rapporterad TKI-utsättning på antingen uppföljnings- eller STOP-formuläret.
# 129 av dessa har orsak "Deep molecular response".

dfSTOPDMR <- df %>%
  filter(
    final_tki_utsatt %in% 1,
    final_tki_utsatt_orsak %in% "Deep molecular response"
  )

setwd(wd)
saveRDS(dfSTOPDMR, "../Data/flygt2021_cmlbase2.rds")
