# Additional variables from NPR -------------------------------------------

load(file = paste0(shfdbpath, "/data/", datadate, "/patregrsdata.RData"))

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "counthosphf",
  noof = TRUE,
  stoptime = global_followup,
  diakod = global_icdhf,
  censdate = censdtm,
  warnings = FALSE,
  meta_reg = "NPR (in)",
  valsclass = "fac"
)

rsdata <- create_sosvar(
  sosdata = patregrsdata %>% filter(sos_source == "sv"),
  cohortdata = rsdata,
  patid = lopnr,
  indexdate = shf_indexdtm,
  sosdate = INDATUM,
  diavar = HDIA,
  type = "out",
  name = "counthospany",
  noof = TRUE,
  stoptime = global_followup,
  diakod = " ",
  censdate = censdtm,
  warnings = FALSE,
  meta_reg = "NPR (in)",
  valsclass = "fac"
)

# Repeated outcome for MCF figure --------------------------------------------

svpatreg <- patregrsdata %>%
  filter(sos_source == "sv")

svpatreg <- left_join(
  rsdata %>%
    select(
      lopnr, shf_indexdtm, sos_com_charlsonciage_cat, shf_ef_cat, censdtm
    ),
  svpatreg %>%
    select(lopnr, INDATUM, HDIA),
  by = "lopnr"
) %>%
  mutate(sos_outtime = as.numeric(INDATUM - shf_indexdtm)) %>%
  filter(sos_outtime > 0 & sos_outtime <= global_followup & INDATUM <= censdtm)

# hf
svpatreghf <- svpatreg %>%
  mutate(sos_out_hosp = stringr::str_detect(HDIA, global_icdhf)) %>%
  filter(sos_out_hosp) %>%
  select(-INDATUM, -HDIA)

rsdatarephf <- bind_rows(
  rsdata %>%
    select(
      lopnr, shf_indexdtm, sos_com_charlsonciage_cat, shf_ef_cat, censdtm
    ),
  svpatreghf
) %>%
  mutate(
    sos_out_hosp = if_else(is.na(sos_out_hosp), 0, 1),
    sos_outtime = as.numeric(if_else(is.na(sos_outtime), as.numeric(censdtm - shf_indexdtm), sos_outtime))
  )

rsdatarephf <- rsdatarephf %>%
  group_by(lopnr, shf_indexdtm, sos_outtime) %>%
  arrange(desc(sos_out_hosp)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(lopnr, shf_indexdtm)

rsdatarephf <- rsdatarephf %>%
  mutate(
    extra = 0
  )

extrarsdatarephf <- rsdatarephf %>%
  group_by(lopnr) %>%
  arrange(sos_outtime) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(sos_out_hosp == 1) %>%
  mutate(
    sos_out_hosp = 0,
    extra = 1
  )

rsdatarephf <- bind_rows(rsdatarephf, extrarsdatarephf) %>%
  arrange(lopnr, sos_outtime, extra) %>%
  mutate(sos_out_hosp = factor(sos_out_hosp, levels = 0:1, labels = c("No", "Yes")))


# ac
rsdatarepany <- bind_rows(
  rsdata %>%
    select(
      lopnr, shf_indexdtm, sos_com_charlsonciage_cat, shf_ef_cat, censdtm
    ),
  svpatreg %>% 
    select(-INDATUM, -HDIA) %>%
    mutate(sos_out_hosp = TRUE)
) %>%
  mutate(
    sos_out_hosp = if_else(is.na(sos_out_hosp), 0, 1),
    sos_outtime = as.numeric(if_else(is.na(sos_outtime), as.numeric(censdtm - shf_indexdtm), sos_outtime))
  )

rsdatarepany <- rsdatarepany %>%
  group_by(lopnr, shf_indexdtm, sos_outtime) %>%
  arrange(desc(sos_out_hosp)) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(lopnr, shf_indexdtm)

rsdatarepany <- rsdatarepany %>%
  mutate(
    extra = 0
  )

extrarsdatarepany <- rsdatarepany %>%
  group_by(lopnr) %>%
  arrange(sos_outtime) %>%
  slice(n()) %>%
  ungroup() %>%
  filter(sos_out_hosp == 1) %>%
  mutate(
    sos_out_hosp = 0,
    extra = 1
  )

rsdatarepany <- bind_rows(rsdatarepany, extrarsdatarepany) %>%
  arrange(lopnr, sos_outtime, extra) %>%
  mutate(sos_out_hosp = factor(sos_out_hosp, levels = 0:1, labels = c("No", "Yes")))

rm(patregrsdata)
gc()
