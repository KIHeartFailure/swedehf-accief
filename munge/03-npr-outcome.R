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
#
# # Repeated outcome for MCF figure --------------------------------------------
#
# svpatreg <- patregrsdata %>%
#   filter(sos_source == "sv")
#
# svpatreg <- left_join(
#   rsdata %>%
#     select(
#       lopnr, shf_indexdtm, cancer, shf_ef_cat, censdtm
#     ),
#   svpatreg %>%
#     select(lopnr, INDATUM, HDIA),
#   by = "lopnr"
# ) %>%
#   mutate(sos_outtime = as.numeric(INDATUM - shf_indexdtm)) %>%
#   filter(sos_outtime > 0 & sos_outtime <= global_followup & INDATUM <= censdtm)
#
# svpatreg <- svpatreg %>%
#   mutate(sos_out_hosphf = stringr::str_detect(HDIA, global_icdhf)) %>%
#   filter(sos_out_hosphf) %>%
#   select(-INDATUM, -HDIA)
#
# rsdatarep <- bind_rows(
#   rsdata %>%
#     select(
#       lopnr, shf_indexdtm, cancer, shf_ef_cat, censdtm
#     ),
#   svpatreg
# ) %>%
#   mutate(
#     sos_out_hosphf = if_else(is.na(sos_out_hosphf), 0, 1),
#     sos_outtime = as.numeric(if_else(is.na(sos_outtime), as.numeric(censdtm - shf_indexdtm), sos_outtime))
#   )
#
# rsdatarep <- rsdatarep %>%
#   group_by(lopnr, shf_indexdtm, sos_outtime) %>%
#   arrange(desc(sos_out_hosphf)) %>%
#   slice(1) %>%
#   ungroup() %>%
#   arrange(lopnr, shf_indexdtm)
#
# rsdatarep <- rsdatarep %>%
#   mutate(
#     extra = 0
#   )
#
# extrarsdatarep <- rsdatarep %>%
#   group_by(lopnr) %>%
#   arrange(sos_outtime) %>%
#   slice(n()) %>%
#   ungroup() %>%
#   filter(sos_out_hosphf == 1) %>%
#   mutate(
#     sos_out_hosphf = 0,
#     extra = 1
#   )
#
# rsdatarep <- bind_rows(rsdatarep, extrarsdatarep) %>%
#   arrange(lopnr, sos_outtime, extra) %>%
#   mutate(sos_out_hosphf = factor(sos_out_hosphf, levels = 0:1, labels = c("No", "Yes")))

rm(patregrsdata)
gc()
