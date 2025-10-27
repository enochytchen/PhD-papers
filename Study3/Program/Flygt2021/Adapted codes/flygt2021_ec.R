suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(dplyr))

# Kvalitetsregistret -----------------------------------------------------------
os = Sys.info()["sysname"]
wd_data = if ( os == "Darwin") "/Volumes/projects/SweCan_CML/Enoch_PhD/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelsen" else # macOS
  if ( os == "Windows") "P:/SweCan_CML/Enoch_PhD/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelse" else # Windows
    stop("Unknown system")
setwd(wd_data)

# Kvalitetsregistret -----------------------------------------------------------
df <- haven::read_sas("sos_kml1_au_21nov19.sas7bdat") %>% 
  dplyr::rename_all(tolower)

# Anmälan
dfKR <- df %>%
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
  )

# Utsättningsformuläret --------------------------------------------------------
load("data/kml-export-rdata-utsattning.RData")

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
    persnr,
    stop0_tki_utsatt_rappdat = ut_inr_inrappdtm,
    stop0_tki_utsatt = ut_tki_utsatt,
    stop0_tki_utsatt_date = ut_tki_utsattdtm,
    stop0_tki_utsatt_orsak
  ) %>%
  arrange(persnr, desc(stop0_tki_utsatt), stop0_tki_utsatt_date) %>%
  distinct(persnr, .keep_all = TRUE)

# Senaste STOP-formuläret där uppgift om utsättning är besvarad
dfSTOPFirstUtsattLatestUppf <- dfUtsattning %>%
  filter(ut_tki_utsatt %in% 0:1) %>%
  select(
    persnr,
    stop0_tki_utsatt_latest_rappdat = ut_inr_inrappdtm
  ) %>%
  arrange(persnr, desc(stop0_tki_utsatt_latest_rappdat)) %>%
  distinct(persnr, .keep_all = TRUE)

# Merge ------------------------------------------------------------------------
df <- dfKR %>%
  left_join(
    dfSTOPFirstUtsatt,
    by = "persnr"
  ) %>%
  left_join(
    dfSTOPFirstUtsattLatestUppf,
    by = "persnr"
  ) %>%
  mutate(
    final_tki_utsatt = stop0_tki_utsatt,
    final_tki_utsatt_date = stop0_tki_utsatt_date,
    final_tki_utsatt_orsak = stop0_tki_utsatt_orsak,
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

dfSTOPDMR <- df %>%
  filter(
    final_tki_utsatt %in% 1,
    final_tki_utsatt_orsak %in% "Deep molecular response"
  )
