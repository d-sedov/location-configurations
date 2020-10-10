###############################################################################
#
# FILE: cbsa_description.R
#
# BY: Dmitry Sedov 
#
# DATE: Sun Sep 13 2020
#
# DESC: This code contains the code to describe the CBSA market areas.
#
###############################################################################


################################ Libraries ####################################

library(readr)
library(dplyr)
library(lfe)
library(dplyr)
library(ggplot2)
library(gsubfn)
library(data.table)
library(stargazer)
library(kableExtra)

###############################################################################


################################ Constants ####################################

descriptive_folder_path <- '/home/quser/project_dir/urban/data/output/descriptive'
markups_folder_path <- '/home/quser/project_dir/urban/data/output/entry/markups'

tables_folder_path <- file.path('/home/quser/project_dir/urban/output', 
                                'tables/descriptive/')

# This function computes the summary statistics for a variable
simple_summary <- function(data, var) {
  require(dplyr)
  require(tibble)
  data %>% 
    select(!!sym(var)) %>% 
    summarise_all(list(q10 = ~quantile(., 0.1, na.rm = T),
                       q25 = ~quantile(., 0.25, na.rm = T), 
                       median = ~median(as.numeric(.), na.rm = T), 
                       q75 = ~quantile(., 0.75, na.rm = T), 
                       q90 = ~quantile(., 0.9, na.rm = T),
                       mean = ~mean(., na.rm = T), 
                       sd = ~sd(., na.rm = T)
    )
    ) %>%
    add_column(v = var, .before = 1)
}

###############################################################################


################################ Main code ####################################

# Import markups - restaurants in the estimation sample 
estimated_markups <- read_csv(file.path(markups_folder_path, 
                                        'restaurants_estimated_markups.csv'))

estimated_markups <- estimated_markups %>% 
  mutate(price = ifelse(price == -1, NA, price))

# Import cbgs and population
cbgs <- read_csv(file.path(descriptive_folder_path, 'data_cbg.csv'))
census_population <- read_csv(file.path(descriptive_folder_path,
                                        'cbg_population.csv')) %>%
  rename(cbg = home_cbg)

cbgs_by_cbsa <- cbgs %>%
  left_join(census_population) %>%
  group_by(cbsa) %>%
  summarize(area_sqkm = sum(area_m2) / 1e6, 
            n_cbg = n(), 
            total_pop = sum(total_pop)) 
  
rest_by_cbsa <- estimated_markups %>%
  group_by(r_cbsa) %>% 
  summarize(n_rest = n(),
            price = mean(price, na.rm = T), 
            rating = mean(rating, na.rm = T)) %>%
  rename(cbsa = r_cbsa)

rest_by_cbsa <- rest_by_cbsa %>% 
  left_join(cbgs_by_cbsa) %>%
  mutate(total_pop = total_pop / 1000)

# Select variables for summary statistics construction
vars <- list('area_sqkm',
             'total_pop',
             'n_cbg',
             'n_rest',
             'rating',
             'price'
             )

# Summarize all deviations
markets_summary <- bind_rows(lapply(vars,
                                    function(x) {
                                      simple_summary(rest_by_cbsa, x)
                                      }
                                    )
                             )
# Rename
markets_summary$v <- c('Area (sq.km.)', 'Pop. (in 1000s)', 'CBG count', 
                       'Rest. count', 'Mean price', 'Mean rating')
colnames(markets_summary) <- str_to_title(colnames(markets_summary))
colnames(markets_summary)[colnames(markets_summary) == 'Sd'] <- 'SD'
colnames(markets_summary)[1] <- ''

kable(markets_summary, format = 'latex', digits = 2, 
      caption = paste('Summary statistics for market areas (CBSAs) used in estimation.'),
      label = 'cbsa_summary_stats', booktabs = T, linesep = '') %>%   
  row_spec(0, bold = T, italic = T) %>%
  cat(., file = file.path(tables_folder_path, 'cbsas_summary.tex'))

###############################################################################
  