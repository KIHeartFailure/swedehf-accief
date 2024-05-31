rsdata <- rsdata %>%
  mutate(
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
        sos_com_charlsonciage <= 1 ~ 1,
        sos_com_charlsonciage <= 3 ~ 2,
        sos_com_charlsonciage <= 7 ~ 3,
        sos_com_charlsonciage >= 8 ~ 4
      ),
      levels = 1:4,
      labels = c(
        "1",
        "2-3",
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
