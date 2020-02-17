################################################################################
################################################################################
#
# FILE: establishments-summary.R
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Feb 13 2020
#
# DESC: This file creates a table with establishment summary statistics.
#
################################################################################
################################################################################


################################ Libraries #####################################

library(readr)
library(dplyr)
library(xtable)
library(kableExtra)
library(jsonlite)
library(tidyr)
library(ggplot2)
library(scales)

################################################################################

################################# Options ######################################

# Set the tables properties
# Footnote font size
options(xtable.size = 'footnotesize')
# Page top placement
options(xtable.table.placement = 't')

# Input folder
input_folder_path = '/Users/muser/dfolder/Research/urban/data/output/descriptive'

# Output folders
tables_folder_path = '/Users/muser/dfolder/Research/urban/output/tables/descriptive'
plots_folder_path = '/Users/muser/dfolder/Research/urban/output/plots/descriptive'

# Visits data vintage
year = '2018'
month = 'October'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 14),
                  legend.title = element_text(size= 16),
                  plot.title = element_text(hjust = 0.5, size = 18)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

################################################################################

################################# Functions ####################################

# This function computes the summary statistics for a variable
simple_summary <- function(data, var) {
    require(dplyr)
    require(tibble)
    data %>% 
        select(!!sym(var)) %>% 
        summarise_all(list(perc_na = function(x){100 * 
                sum(is.na(x)) / length(x)},
                q10 = ~quantile(., 0.1, na.rm = T),
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

################################################################################

############################## Prepare data ####################################

# Import data
establishments <- read_csv(file.path(input_folder_path, 'data_establishments.csv'))

# Transform the total minutes open variable
establishments <- establishments %>%
    mutate(perc_time_open = 100 * total_minutes_open / (24 * 60 * 31)) %>%
    mutate(perc_time_open = replace(perc_time_open,  
                                    which(perc_time_open <= 0.1 | perc_time_open >= 100.1), 
                                    NA))
# Encode 'urban' establishments
establishments <- establishments %>%
    mutate(urban = !is.na(cbsa)) 

# Extract the first two digits of NAICS codes:
establishments <- establishments %>%
    mutate(naics_first2 = as.integer(substr(as.character(naics_code), 1, 2)))

# NAICS industry names data frame
naics_codes <- c(11, 21, 22, 23, 
                 31, 32, 33, 
                 42, 44, 45, 
                 48, 49, 
                 51, 52, 53,
                 54, 55, 56, 
                 61, 62, 
                 71, 72, 
                 81, 92)
naics_names <- c("Agriculture", "Mining", "Utilities", "Construction",
                 "Manufacturing (1)", "Manufacturing (2)", "Manufacturing (3)",
                 "Wholesale", "Retail (1)", "Retail (2)",
                 "Transportation", "Warehousing",
                 "Information", "Finance", "R. Estate",
                 "Professional", "Management", "Administrative",
                 "Education", "Health",
                 "Entertainment","Food + Accom.",
                 "Services", "Pub. Adm."
)
naics_codes_names <- data.frame(naics_first2 = naics_codes, 
                                Industry = naics_names)

################################################################################

############################## Summary table ###################################

# Variables to summarise
vars <- c('urban',
          'area_m2',
          'perc_time_open',
          'raw_visit_counts'
)

# Compute total POI count by NAICS categories
naics_basic <- establishments %>% 
    group_by(naics_first2) %>%
    summarise(Quantity = n() / 1000)

# Compute averages by NAICS categories, transform urban variable to percentages
naics_extended <- establishments %>% 
    group_by(naics_first2) %>% 
    summarise_at(vars, mean, na.rm = T) %>%
    mutate(urban = 100 * urban)

# Join summaries
establishments_summary <- right_join(naics_codes_names, naics_basic) %>%
    inner_join(naics_extended) %>%
    arrange(-Quantity) %>%
    select(-naics_first2)

# Column names change
cols <- c(`% Time open` = 'perc_time_open',
          `% Urban` = 'urban',
          `Area (sq. m)` = 'area_m2',
          `Visits` = 'raw_visit_counts'
          )
establishments_summary <- establishments_summary %>%
    rename(!!cols)

# Produce the LaTeX table
outfile = file.path(tables_folder_path, 'establishments_summary_table.tex')
xtable(establishments_summary,
       caption = 'Establishments dataset codes summary statistics.',
       label = 'establishments_summary_table',
       digits = c(0,0,1,0,1,0,1)) %>% 
    xtable2kable(include.rownames = FALSE,
                 booktabs = TRUE) %>%
    add_header_above(c(' ', ' ', 'Averages' = 4), bold = TRUE, italic = TRUE) %>%
    row_spec(row = 0, bold = TRUE) %>%
    add_footnote(paste('\\scriptsize{\\emph{Note}: Subset of data for',
                       month,
                       paste0(year, '.}')
                       ),
                 notation = 'none',
                 escape = FALSE
                 ) %>%
    cat(file = outfile)

################################################################################
