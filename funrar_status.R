# Script to relate IUCN status and functional distinctiveness of global
# Crcodilians

# Packages ---------------------------------------------------------------------
library("dplyr")
library("ggplot2")


# Functions --------------------------------------------------------------------
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


# Load data --------------------------------------------------------------------
crocodiles_traits = readxl::read_excel(
  "data/Crocodylian Functional Trait Database.xlsx", skip = 1
)


# Clean Trait Data -------------------------------------------------------------
cleaned_traits = crocodiles_traits %>%
  # Remove all non-trait rows
  filter(is.na(References)) %>%
  # Manually remove other non-trait rows
  filter(!is.na(Species) | !is.na(Alligator_sinensis)) %>%
  filter(!grepl("Zhang", Alligator_sinensis, fixed = TRUE)) %>%
  select(-References) %>%
  mutate(Species = ifelse(is.na(Species), "Skull_2", Species)) %>%
  tidyr::pivot_longer(cols = -Species) %>%
  tidyr::pivot_wider(names_from = Species, values_from = value) %>%
  rename(species = name) %>% 
  mutate(
    across(
      c(`Relative Clutch Mass`, `Bite Force (size standardised)`,
        `Skull (combined as single trait)`, Skull_2,
        `Highest Latitude in Range`
        ), \(x) as.numeric(x)),
    across(
      c(`Female size at maturity`, `Male maximum size`), \(x) as.integer(x)
    ),
    across(
      c(`Nesting Type`:`Brumation Ability`,
        `Salt Tolerance`:`Habitat Generalism`), \(x) as.character(x)
    )
  ) %>%
  janitor::clean_names()


# Compute functional dissimilarity ---------------------------------------------

quanti_traits = cleaned_traits[
  , c("species", "relative_clutch_mass", "female_size_at_maturity",
      "male_maximum_size", "bite_force_size_standardised",
      "skull_combined_as_single_trait", "skull_2", "highest_latitude_in_range",
      "habitat_generalism")
] %>%
  mutate(
    habitat_generalism = as.integer(habitat_generalism),
    across(where(is.numeric), scale)
  ) %>%
  as.data.frame() %>%
  tibble::column_to_rownames("species")

quali_traits = cleaned_traits[, c("species", "nesting_type")] %>%
  as.data.frame() %>%
  tibble::column_to_rownames("species")

bin_traits = cleaned_traits[
  , c("species", "communal_nesting", "burrowing_ability", "aestivation_ability",
      "brumation_ability")
] %>%
  mutate(
    across(
      -species,
      \(x) case_when(
        x == "Not Recorded" ~ FALSE,
        x == "No"           ~ FALSE,
        x == "Yes"          ~ TRUE
      )
    )
  ) %>%
  as.data.frame() %>%
  tibble::column_to_rownames("species")

ord_traits = cleaned_traits[
  , c("species", "terrestriality", "salt_tolerance")
] %>% 
  mutate(across(-species, \(x) ordered(x))) %>%
  as.data.frame() %>%
  tibble::column_to_rownames("species")

trait_dissim = ade4::dist.ktab(
  ade4::ktab.list.df(
    list(quanti_traits, quali_traits, bin_traits, ord_traits),
  ), type = c("Q", "N", "D", "O")
)


# Compute functional distinctiveness -------------------------------------------

crocodiles_funrar = funrar::distinctiveness_global(trait_dissim)

# Get IUCN status --------------------------------------------------------------

crocodiles_status = cleaned_traits %>%
  select(species) %>%
  mutate(species = gsub("_", " ", species, fixed = TRUE)) %>%
  pull(species) %>%
  taxize::iucn_summary(
    key = "efa0af4a4187b822b6a2eaba8e3d1d14a52d837b9ecfb9f99e512beb6bffd2bb"
  ) %>%
  taxize::iucn_status() %>%
  tibble::enframe("species", "iucn_status")


crocodile_total = crocodiles_status %>%
  mutate(species = gsub(" ", "_", species, fixed = TRUE)) %>% 
  inner_join(crocodiles_funrar, by = "species") %>%
  mutate(
    iucn_status = case_when(
      iucn_status == "LR/cd" ~ "NT",
      is.na(iucn_status)     ~ "NE",
      TRUE ~ iucn_status
    ) %>%
      ordered(levels = c("LC", "NT", "VU", "CR", "NE"))
  )


set.seed(20230601)

# Predict Extinction Risk
crocodile_total$extinction_risk = lapply(
  as.character(crocodile_total$iucn_status), predict_extinction_risk_iucn_status
) %>%
  unlist()

# Plot figure ------------------------------------------------------------------

plot_funrar_status = crocodile_total %>%
  filter(iucn_status != "NE") %>% 
  ggplot(aes(global_di, iucn_status)) +
  geom_point(position = position_jitter(height = 0.05)) +
  labs(x = "Functional Distinctiveness", y = "IUCN Status") +
  scale_y_discrete(
    labels = c(
      NE = "Not Evaluated", CR = "Critically Endangered", VU = "Vulnerable",
      NT = "Near Threatened", LC = "Least Concern"
    )
  ) +
  theme_bw()

plot_funrar_status

plot_funrar_risk = crocodile_total %>%
  filter(iucn_status != "NE") %>%
  ggplot(aes(global_di, extinction_risk)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Functional Distinctiveness", y = "Extinction Risk") +
  theme_bw() +
  theme(aspect.ratio = 1)

plot_funrar_risk
