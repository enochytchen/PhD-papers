library(haven)
# readsas

setwd("/Users/yitche/SweCan_CML/Data/Raw_Data/20200603_cmlbase2/2019-11-29 - Från Socialstyrelsen")
Outpatient <- read_sas("r_par_ov_kml_11731_2018.sas7bdat") 
Outpatient <- Outpatient |>
              rename(idnr = LopNr)


studypop <- readRDS("/Users/yitche/Library/CloudStorage/OneDrive-KarolinskaInstitutet/ec_phd/Study3_QALY/Data/01a_studypop.rds")

## Keep if date_tfr_start is not missing
studypop_tfr <- subset(studypop, !is.na(date_tfr_start)) |>
                select(idnr, date_item, item, date_tfr_start, tfr_end)

## Generate date_tfr_end if there is any date_item after date_tfr_start
studypop_tfr <- studypop_tfr |>
                group_by(idnr) |>
                mutate(date_tfr_end = ifelse(any(date_item > date_tfr_start), 
                                             as.Date(min(date_item[date_item > date_tfr_start])), 
                                             NA)) |>
                ungroup()

studypop_tfr$date_tfr_end <- as.Date(studypop_tfr$date_tfr_end)

## Keep one idnr
studypop_tfr2 <- studypop_tfr |>
                 distinct(idnr, .keep_all = TRUE) |>
                 select(idnr, date_tfr_start, date_tfr_end)

## Merge Outpatient and study_tfr2, and keep only those indr exists in studypop_tfr2
Outpatient_tfr <- merge(Outpatient, studypop_tfr2, by = "idnr", all.x = TRUE) |>
                  filter(!is.na(date_tfr_start)) |>
                  select(idnr, date_tfr_start, date_tfr_end, everything())

## Only keep records INDATUM betwwen date_tfr_start and date_tfr_end
Outpatient_tfr <- Outpatient_tfr |>
                  filter(INDATUM >= date_tfr_start & INDATUM <= date_tfr_end) |>
                  filter(HDIA == "C921")

## Tabulate Outpatient_tfr$OP, and show missing
nrow(Outpatient_tfr)
print(table(Outpatient_tfr$OP, useNA = "always"))
