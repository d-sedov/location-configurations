################################################################################
#
# FILE: naics-effects-on-cbg-visits.R
#
# BY: Dmitry Sedov 
#
# CREATED: Tue Nov 19 2019 
#
# DESC: This file creates a descriptive statistics of how the presence of POIs 
#       with different NAICS codes 'affect' the visits to the respective CBGs.
#
################################################################################

# Import libraries
library(readr) # Importing from csv and other sources
library(dplyr) # Chained sql-style table operations
library(tidyr) # Reshaping tables
library(jtools) # Plotting coefficients
library(haven)

# Set the data path
data_path <- '/Users/muser/dfolder/Research/urban/data/'

# Set the cbg patterns path
cbg_patterns_path <- file.path(data_path, 'cbg_patterns.csv')
# Import the cbg_patterns data
cbg_patterns <- read_csv(cbg_patterns_path, 
                         col_types = cols_only(census_block_group = col_guess(),
                                               raw_visit_count = col_guess()
                                               )
                         )
# Set the pois path
pois_full_path <- file.path(data_path, 'pois_full.csv')
# Import the pois_full data
pois_full <- read_csv(pois_full_path,
                      col_types = cols_only(cbg = col_guess(),
                                            naics_first2 = col_guess()
                                            )
                      )

# Set the cbgs path
cbgs_path <- file.path(data_path, 'cbgs.csv')
# Import the cbgs data
cbgs <- read_csv(cbgs_path,
                 col_types = cols_only(area_m2 = col_guess(),
                                       cbg = col_guess(), devices = col_guess()
                                       )
                 )

# Grouping by cbg, count POIs in different NAICS 2 codes:
cbg_pois <- pois_full %>% 
    group_by(cbg, naics_first2) %>%
    summarise(n = n()) %>% 
    pivot_wider(names_from = naics_first2,
                values_from = n,
                names_prefix = 'naics',
                values_fill = c('n' = 0))

# Join the datasets, replace missing pois counts by 0s, keep complete cases only
cbgs <- cbgs %>% 
    left_join(cbg_patterns, by = c('cbg' = 'census_block_group')) %>%
    left_join(cbg_pois) %>%
    mutate_at(vars(starts_with('naics')), 
              list(~ ifelse(is.na(.), 0, .))
              ) %>%
    drop_na() 

cbgs <- cbgs %>%
    mutate(tract = str_sub(cbg, 1, -4))
    
cbgs_out_path <- file.path(data_path, 'cbgs_naics.csv')
write_csv(cbgs, path = cbgs_out_path)
# Standardize the columns:
# OR MAYBE NOT:
# cbgs <- cbgs %>% 
#    mutate_at(vars(-one_of(c('cbg'))), list(~scale(.) %>% as.vector))

# Run the regression
fit <- lm(data = cbgs, raw_visit_count ~ . - cbg)
# Get the sorted coefficients
coefs_sorted <- sort(fit$coefficients)

# Present the coefficients:
# NAICs industry names data frame
naics_codes <- paste0('naics', 
                      c(11, 21, 22, 23,
                        31, 32, 33,
                        42, 44, 45,
                        48, 49,
                        51, 52, 53,
                        54, 55, 56,
                        61, 62,
                        71, 72,
                        81, 92, NA)
                      )
naics_names <- c("Agriculture", "Mining", "Utilities", "Construction",
                 "Manufacturing (1)", "Manufacturing (2)", "Manufacturing (3)",
                 "Wholesale", "Retail (1)", "Retail (2)",
                 "Transportation", "Warehousing",
                 "Information", "Finance", "R. Estate",
                 "Professional", "Management", "Administrative",
                 "Education", "Health",
                 "Entertainment","Food + Accom.",
                 "Services", "Pub. Adm.", "NA"
)
naics_codes_named <- setNames(naics_codes, naics_names)

naics_names_named <- setNames(naics_names, naics_codes)

# Write variable labels for export to Stata
vars_labels_data <- data.frame(var = naics_codes_named, names = names(naics_codes_named))
write.table(vars_labels_data, sep = " ", file = file.path(data_path, 'var_labels.csv'), row.names = F)

plot_summs(fit, coefs = naics_codes, robust = TRUE)
    