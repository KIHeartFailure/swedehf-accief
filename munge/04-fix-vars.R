# Cut outcomes at x years

rsdata <- cut_surv(rsdata, sos_out_deathcvhosphf, sos_outtime_hosphf, global_followup, cuttime = FALSE, censval = "No")
rsdata <- cut_surv(rsdata, sos_out_hosphf, sos_outtime_hosphf, global_followup, cuttime = TRUE, censval = "No")
rsdata <- cut_surv(rsdata, sos_out_hospany, sos_outtime_hospany, global_followup, cuttime = TRUE, censval = "No")
rsdata <- cut_surv(rsdata, sos_out_deathcv, sos_outtime_death, global_followup, cuttime = FALSE, censval = "No")
rsdata <- cut_surv(rsdata, sos_out_deathnoncv, sos_outtime_death, global_followup, cuttime = FALSE, censval = "No")
rsdata <- cut_surv(rsdata, sos_out_death, sos_outtime_death, global_followup, cuttime = TRUE, censval = "No")

rsdata <- rsdata %>%
  mutate(
    censdtm = pmin(shf_indexdtm + global_followup, censdtm),
    shf_arbdosetg = case_when(
      shf_arbsub == "Candesartan" ~ shf_arbdose / 32,
      shf_arbsub == "Losartan" ~ shf_arbdose / 150,
      shf_arbsub == "Valsartan" ~ shf_arbdose / 320
    ),
    shf_aceidosetg = case_when(
      shf_aceisub == "Ramipril" ~ shf_aceidose / 10,
      shf_aceisub == "Captopril" ~ shf_aceidose / 150,
      shf_aceisub == "Enalapril" ~ shf_aceidose / 20,
      shf_aceisub == "Lisinopril" ~ shf_aceidose / 35
    ),
    shf_arnidosetg = case_when(
      shf_arnidose %in% c("194/206") ~ 1,
      shf_arnidose %in% c("97/103", "98/102", "146/154") ~ 0.5,
      shf_arnidose %in% c("49/51", "48/52", "73/77") ~ 0.25,
      shf_arnidose %in% c("24/26") ~ 0.125,
    ),
    shf_rasiarnidosetg = pmax(shf_arbdosetg, shf_aceidosetg, na.rm = T),
    shf_rasiarnidosetg = pmax(shf_rasiarnidosetg, shf_arnidosetg, na.rm = T),
    shf_rasiarnidosetg_cat = factor(
      case_when(
        shf_rasiarni == "No" | is.na(shf_rasiarni) ~ 0,
        shf_rasiarnidosetg < .5 ~ 1,
        shf_rasiarnidosetg < 1 ~ 2,
        shf_rasiarnidosetg >= 1 ~ 3
      ),
      levels = 0:3, labels = c("No/Missing ACEi/ARB/ARNi", "1-49", "50-99", ">=100")
    ),
    shf_bbldosetg = case_when(
      shf_bblsub == "Bisoprolol" ~ shf_bbldose / 10,
      shf_bblsub == "Carvedilol" ~ shf_bbldose / 50,
      shf_bblsub == "Metoprolol" ~ shf_bbldose / 200
    ),
    shf_bbldosetg_cat = factor(
      case_when(
        shf_bbl == "No" | is.na(shf_bbl) ~ 0,
        shf_bbldosetg < .5 ~ 1,
        shf_bbldosetg < 1 ~ 2,
        shf_bbldosetg >= 1 ~ 3
      ),
      levels = 0:3, labels = c("No/Missing Beta-blocker", "1-49", "50-99", ">=100")
    ),
    shf_mradosetg = shf_mradose / 50,
    shf_mradosetg_cat = factor(
      case_when(
        shf_mra == "No" | is.na(shf_mra) ~ 0,
        shf_mradosetg < .5 ~ 1,
        shf_mradosetg < 1 ~ 2,
        shf_mradosetg >= 1 ~ 3
      ),
      levels = 0:3, labels = c("No/Missing MRA", "1-49", "50-99", ">=100")
    ),
    shf_indexyear_cat = factor(case_when(
      shf_indexyear <= 2010 ~ "2000-2010",
      shf_indexyear <= 2015 ~ "2011-2015",
      shf_indexyear <= 2021 ~ "2016-2020",
      shf_indexyear <= 2023 ~ "2021-2023"
    )),
    shf_bpsys_cat = factor(
      case_when(
        shf_bpsys < 140 ~ 1,
        shf_bpsys >= 140 ~ 2
      ),
      levels = 1:2, labels = c("<140", ">=140")
    ),
    sos_com_charlsonciage_cat = factor(
      case_when(
        sos_com_charlsonciage <= 3 ~ 1,
        sos_com_charlsonciage <= 7 ~ 2,
        sos_com_charlsonciage >= 8 ~ 3
      ),
      levels = 1:3,
      labels = c(
        "1-3",
        "4-7",
        ">=8"
      )
    )
  )

# income
inc <- rsdata %>%
  reframe(incsum = list(enframe(quantile(scb_dispincome,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  ))), .by = shf_indexyear) %>%
  unnest(cols = c(incsum)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- left_join(
  rsdata,
  inc,
  by = "shf_indexyear"
) %>%
  mutate(
    scb_dispincome_cat = factor(
      case_when(
        scb_dispincome < `33%` ~ 1,
        scb_dispincome < `66%` ~ 2,
        scb_dispincome >= `66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile within year", "2nd tertile within year", "3rd tertile within year")
    )
  ) %>%
  select(-`33%`, -`66%`)

# ntprobnp

nt <- rsdata %>%
  reframe(ntmed = list(enframe(quantile(shf_ntprobnp,
    probs = c(0.33, 0.66),
    na.rm = TRUE
  ))), .by = shf_ef_cat) %>%
  unnest(cols = c(ntmed)) %>%
  pivot_wider(names_from = name, values_from = value)

rsdata <- left_join(
  rsdata,
  nt,
  by = c("shf_ef_cat")
) %>%
  mutate(
    shf_ntprobnp_cat = factor(
      case_when(
        shf_ntprobnp < `33%` ~ 1,
        shf_ntprobnp < `66%` ~ 2,
        shf_ntprobnp >= `66%` ~ 3
      ),
      levels = 1:3,
      labels = c("1st tertile within EF", "2nd tertile within EF", "3rd tertile within EF")
    )
  ) %>%
  select(-`33%`, -`66%`)

rsdata <- rsdata %>%
  mutate(across(where(is_character), factor))
