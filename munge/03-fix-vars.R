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
      shf_arbsub == "Eprosartan" ~ shf_arbdose / 800,
      shf_arbsub == "Irbesartan" ~ shf_arbdose / 300,
      shf_arbsub == "Losartan" ~ shf_arbdose / 150,
      shf_arbsub == "Telmisartan" ~ shf_arbdose / 80,
      shf_arbsub == "Valsartan" ~ shf_arbdose / 320
    ),
    shf_aceidosetg = case_when(
      shf_aceisub == "Captopril" ~ shf_aceidose / 150,
      shf_aceisub == "Cilazapril" ~ shf_aceidose / 5,
      shf_aceisub == "Enalapril" ~ shf_aceidose / 40,
      shf_aceisub == "Fosinopril" ~ shf_aceidose / 40,
      shf_aceisub == "Kinapril" ~ shf_aceidose / 40,
      shf_aceisub == "Lisinopril" ~ shf_aceidose / 35,
      shf_aceisub == "Perindopril" ~ shf_aceidose / 8,
      shf_aceisub == "Ramipril" ~ shf_aceidose / 10,
      shf_aceisub == "Trandolapril" ~ shf_aceidose / 4
    ),
    shf_arnidosetg = case_when(
      shf_arnidose %in% c("194/206") ~ 1,
      shf_arnidose %in% c("97/103", "98/102", "146/154") ~ 0.5,
      shf_arnidose %in% c("49/51", "48/52", "73/77") ~ 0.25,
      shf_arnidose %in% c("24/26") ~ 0.125
    ),
    shf_rasiarnidosetg = pmax(shf_arbdosetg, shf_aceidosetg, na.rm = T),
    shf_rasiarnidosetg = pmax(shf_rasiarnidosetg, shf_arnidosetg, na.rm = T) * 100,
    shf_rasiarnidosetg = if_else(shf_rasiarni == "No" | is.na(shf_rasiarni), NA_real_, shf_rasiarnidosetg),
    shf_rasiarnidosetg_cat = factor(
      case_when(
        shf_rasiarnidosetg < 50 ~ 1,
        shf_rasiarnidosetg < 100 ~ 2,
        shf_rasiarnidosetg >= 100 ~ 3
      ),
      levels = 1:3, labels = c("1-49", "50-99", ">=100")
    ),
    shf_bbldosetg = case_when(
      shf_bblsub == "Atenolol" ~ shf_bbldose / 100,
      shf_bblsub == "Bisoprolol" ~ shf_bbldose / 10,
      shf_bblsub == "Carvedilol" ~ shf_bbldose / 50,
      shf_bblsub == "Labetalol" ~ shf_bbldose / 400,
      shf_bblsub == "Metoprolol" ~ shf_bbldose / 200,
      shf_bblsub == "Pindolol" ~ shf_bbldose / 15,
      shf_bblsub == "Propanolol" ~ shf_bbldose / 160,
      shf_bblsub == "Sotalol" ~ shf_bbldose / 320
    ) * 100,
    shf_bbldosetg = if_else(shf_bbl == "No" | is.na(shf_bbl), NA_real_, shf_bbldosetg),
    shf_bbldosetg_cat = factor(
      case_when(
        shf_bbldosetg < 50 ~ 1,
        shf_bbldosetg < 100 ~ 2,
        shf_bbldosetg >= 100 ~ 3
      ),
      levels = 1:3, labels = c("1-49", "50-99", ">=100")
    ),
    shf_mradosetg = shf_mradose / 50 * 100,
    shf_mradosetg = if_else(shf_mra == "No" | is.na(shf_mra), NA_real_, shf_mradosetg),
    shf_mradosetg_cat = factor(
      case_when(
        shf_mradosetg < .5 ~ 1,
        shf_mradosetg < 1 ~ 2,
        shf_mradosetg >= 1 ~ 3
      ),
      levels = 1:3, labels = c("1-49", "50-99", ">=100")
    ),
    shf_sglt2 = case_when(
      shf_indexdtm < ymd("2021-11-01") ~ NA_character_,
      TRUE ~ shf_sglt2
    ),
    shf_sglt2dosetg = case_when(
      shf_sglt2sub == "Dapagliflozin" ~ shf_sglt2dose / 10,
      shf_sglt2sub == "Empagliflozin" ~ shf_sglt2dose / 10,
      shf_sglt2sub == "Ertugliflozin" ~ shf_sglt2dose / 15,
      shf_sglt2sub == "Kanagliflozin" ~ shf_sglt2dose / 300
    ) * 100,
    shf_sglt2dosetg = if_else(shf_sglt2 == "No" | is.na(shf_sglt2), NA_real_, shf_sglt2dosetg),
    shf_sglt2dosetg_cat = factor(
      case_when(
        shf_sglt2dosetg < 50 ~ 1,
        shf_sglt2dosetg < 100 ~ 2,
        shf_sglt2dosetg >= 100 ~ 3
      ),
      levels = 1:3, labels = c("1-49", "50-99", ">=100")
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
        sos_com_charlsonciage <= 6 ~ 2,
        sos_com_charlsonciage >= 7 ~ 3
      ),
      levels = 1:3,
      labels = c(
        "1-3",
        "4-6",
        ">=7"
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
