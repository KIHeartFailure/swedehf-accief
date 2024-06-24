# Impute missing values ---------------------------------------------------

modvarstmp <- modvars

rsdatauseforimp <- rsdata %>%
  select(lopnr, shf_indexdtm, !!!syms(modvarstmp), contains(outvars$var), !!!syms(outvars$time), contains("sos_com_charlsonciage"))

noimpvars <- names(rsdatauseforimp)[!names(rsdatauseforimp) %in% modvars]

# Nelson-Aalen estimator
na <- basehaz(coxph(Surv(sos_outtime_hosphf, sos_out_deathcvhosphf == "Yes") ~ 1,
  data = rsdata, method = "breslow"
))

rsdatauseforimp <- left_join(rsdatauseforimp, na, by = c("sos_outtime_death" = "time"))

ini <- mice(rsdatauseforimp, maxit = 0, print = F)

pred <- ini$pred
pred[, noimpvars] <- 0
pred[noimpvars, ] <- 0 # redundant

# change method used in imputation to prop odds model
meth <- ini$method
meth[c("scb_education", "shf_indexyear_cat", "shf_ntprobnp_cat", "scb_dispincome_cat")] <- "polr"
meth[noimpvars] <- ""

## check no cores
cores_2_use <- detectCores() - 1
if (cores_2_use >= 10) {
  cores_2_use <- 10
  m_2_use <- 1
} else if (cores_2_use >= 5) {
  cores_2_use <- 5
  m_2_use <- 2
} else {
  stop("Need >= 5 cores for this computation")
}

cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 49956)
registerDoParallel(cl)

imprsdata <-
  foreach(
    no = 1:cores_2_use,
    .combine = ibind,
    .export = c("meth", "pred", "rsdatauseforimp"),
    .packages = "mice"
  ) %dopar% {
    mice(rsdatauseforimp,
      m = m_2_use, maxit = 10, method = meth,
      predictorMatrix = pred,
      printFlag = FALSE
    )
  }
stopImplicitCluster()

# Check if all variables have been fully imputed --------------------------

datacheck <- mice::complete(imprsdata, 1)

for (i in seq_along(modvarstmp)) {
  if (any(is.na(datacheck[, modvarstmp[i]]))) stop("Missing for imp vars")
}
for (i in seq_along(modvarstmp)) {
  if (any(is.na(datacheck[, modvarstmp[i]]))) print(paste0("Missing for ", modvarstmp[i]))
}


# EF subgroups ------------------------------------------------------------

imprsdataref <- mice::filter(imprsdata, rsdata$shf_ef_cat == "HFrEF")
imprsdatamref <- mice::filter(imprsdata, rsdata$shf_ef_cat == "HFmrEF")
imprsdatapef <- mice::filter(imprsdata, rsdata$shf_ef_cat == "HFpEF")
