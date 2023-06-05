# Script to assign extinction score to birds

# Packages ---------------------------------------------------------------------

library("dplyr")
library("ggplot2")


# Functions --------------------------------------------------------------------
#' Extinction probablity based on IUCN status
#' 
#' This function returns an extinction probability based on a IUCN Red List
#' status. The extinction probability it returns is based on the EDGE2 metric as
#' defined by Gumbs et al. (2023) <doi:10.1371/journal.pbio.3001991>.
#' In particular, it returns the Global Endangerment 2 score (GE2).
#' See their Figure 1, Supplementary Info S1 for more information and the
#' underlying data with data sheet S4.
#' 
#' The idea is that each IUCN status is associated with a median extinction risk
#' with the risk doubling as going from least to most endangered species.
#' The function is than completed to be continuous and curved from 0.0001 to
#' 0.9999 (never assigning a extinction risk of 1).
#' To convey uncertainty, species are randomly assigned an extinction risk
#' within each status category in a way that the median probability is the one
#' assigned to the Red List status.
#' 
#' @param redlist_status `character(1)` defining the IUCN Red List status
#'   it can be:
#'     * `"LC"`, for Least Concern,
#'     * `"NT"`, for Near Threatened,
#'     * `"VU"`, for Vulnerable,
#'     * `"EN"`, for Endangered,
#'     * `"CR"`, for Critically Endangered,
#'     * `"EW"`, for Extinct in the Wild,
#'     * `"EX"`, for Extinct,
#'     * `"DD"`, for Data Deficient,
#'     * `"NE"` or `NA`, for Not evaluated species
predict_extinction_risk_iucn_status = function(
    redlist_status = c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD", "NE", NA)
) {
  
  redlist_status = match.arg(redlist_status)
  
  switch(
    redlist_status,
    `NA` = runif(1, min = 0.0001,    max = 0.9999),
    "DD" = runif(1, min = 0.0001,    max = 0.9999),
    "NE" = runif(1, min = 0.0001,    max = 0.9999),
    "LC" = runif(1, min = 0.0001,    max = 0.0909375),
    "NT" = runif(1, min = 0.0909375, max = 0.181875),
    "VU" = runif(1, min = 0.181875,  max = 0.36375),
    "EN" = runif(1, min = 0.36375,   max = 0.60625),
    "CR" = runif(1, min = 0.60625,   max = 0.9999),
    "EW" = runif(1, min = 0.60625,   max = 0.9999),
    "EX" = 1,
    NA_real_
  )
  
}


# Data load --------------------------------------------------------------------

# IUCN status were extracted June 1st 2023
bird_status_funrar = readRDS("data/bird_di_status_risk.Rds")

# Extinction risk from Gumbs et al. 2023 on EDGE2
extinction_risk_sheet = readxl::read_xlsx(
  "data/journal.pbio.3001991.s004.xlsx", sheet = 1, range = "A2:B20002"
) %>%
  mutate(
    iucn_status = case_when(
      Rank <=  4000 ~ "LC",
      Rank <=  8000 ~ "NT",
      Rank <= 12000 ~ "VU",
      Rank <= 16000 ~ "EN",
      TRUE          ~ "CR"
    )
  )


# Assign extinction risk -------------------------------------------------------
# We assign extinction risk from IUCN status based on the method developed for
# the EDGE2 metrics as specified in S1 of
# Gumbs et al. 2023 https://doi.org/10.1371/journal.pbio.3001991.

# Get bounds for probability of extinctions
min_max_pext = extinction_risk_sheet %>%
  group_by(iucn_status) %>%
  summarise(min_pext = min(pext), max_pext = max(pext))

additional_status = tribble(
  ~iucn_status, ~min_pext, ~max_pext,
  "DE", 0.0001, 0.9999,
   NA,  0.0001, 0.9999,
  "EW", 0.698253, 0.9999,
  "EX", 1, 1
)

min_max_pext = min_max_pext %>%
  bind_rows(additional_status) %>%
  mutate(
    expected_median = 1/2 * (min_pext + max_pext)
  )


plot_extinction_risk_gumbs_2023 = extinction_risk_sheet %>%
  ggplot(aes(Rank, pext, color = iucn_status, fill = iucn_status)) +
  geom_line(linewidth = 1) +
  stat_summary_bin(
    fun = \(x) {median(x)},
    breaks = c(0, 4000, 8000, 12000, 16000, 20000),
    geom = "point", shape = 21, color = "#333333", size = 3, 
  ) +
  scale_color_manual(
    values = c(
      LC = "#60c659", NT = "#cce227", VU = "#f9e814", EN = "#fc7f3f",
      CR = "#db1d02"
    )
  ) +
  scale_fill_manual(
    values = c(
      LC = "#60c659", NT = "#cce227", VU = "#f9e814", EN = "#fc7f3f",
      CR = "#db1d02"
    )
  ) +
  labs(x = "Rank (as in Excel sheet)", y = "Probablity of Extinction") +
  theme_bw()

plot_extinction_risk_gumbs_2023


# Actual extinction risk assignation -------------------------------------------
set.seed(20230601)

bird_status_funrar$extinction_risk = lapply(
  bird_status_funrar$iucn_status, predict_extinction_risk_iucn_status
) %>%
  unlist()

saveRDS(bird_status_funrar, "data/bird_status_funrar_risk.Rds")


# Final plot
bird_status_funrar %>%
  filter(!is.na(iucn_status)) %>% 
  ggplot(aes(din, extinction_risk)) +
  geom_point(shape = ".") +
  geom_smooth()

bird_status_funrar
