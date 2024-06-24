# default is to use tidyverse functions
select <- dplyr::select
rename <- dplyr::rename
filter <- dplyr::filter
mutate <- dplyr::mutate
complete <- tidyr::complete
fixed <- stringr::fixed

# used for calculation of ci
global_z05 <- qnorm(1 - 0.025)

shfdbpath <- "F:/STATISTIK/Projects/20210525_shfdb4/dm/"
datadate <- "20240423"

global_cols <- RColorBrewer::brewer.pal(7, "Dark2")

global_followup_year <- 3
global_followup <- 3 * 365.25

global_icdhf <- " I110| I130| I132| I255| I420| I423| I425| I426| I427| I428| I429| I43| I50| J81| K761| R570"
