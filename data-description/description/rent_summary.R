################################################################################
################################################################################
#
# FILE: rent_summary.R
#
# BY: Dmitry Sedov 
#
# CREATED: Wed May 13 2020
#
# DESC: This file creates summary tables / plots for the commercial rent data.
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
library(extrafont)
library(ggpubr)
library(stringr)
library(stargazer)

################################################################################

################################# Options ######################################

# Set the tables properties
# Footnote font size
options(xtable.size = 'footnotesize')
# Page top placement
options(xtable.table.placement = 't')

# Input folder
input_folder_path = '/Users/muser/dfolder/Research/urban/data/output/rent'

# Output folders
tables_folder_path = '/Users/muser/dfolder/Research/urban/output/tables/descriptive'
plots_folder_path = '/Users/muser/dfolder/Research/urban/output/plots/descriptive'

# Visits data vintage
year = '2019'
month = 'July'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(size = 6),
                  axis.title = element_text(size = 8)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#ffe200', '#722ab5')

################################################################################


################################ Functions #####################################

sumarise_rent <- function(data) {
    part1 <- data %>% summarise(source = str_to_sentence(first(source)),
                                n = n(),
                                n_zip = n_distinct(zip_code), 
                                rate_q25 = quantile(rate, 0.25, na.rm = T), 
                                rate_q50 = quantile(rate, 0.50, na.rm = T), 
                                rate_q75 = quantile(rate, 0.75, na.rm = T), 
                                footage_mean = mean(footage, na.rm = T))
    part2 <- data %>% 
        group_by(zip_code) %>%
        summarise(count = n()) %>%
        ungroup() %>% 
        summarise(count_mean = mean(count, na.rm = T))
    
    value <- part1 %>% 
        bind_cols(part2) %>%
        select(source,
               n, n_zip, 
               count_mean, footage_mean,
               rate_q25, rate_q50, rate_q75)
    return(value)
}

remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

################################################################################


############################## Prepare data ####################################

# Import data
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv'))

loopnet_data <- read_csv(file.path(input_folder_path, 
                                   'loopnet_listings.csv'))
crexi_data <- read_csv(file.path(input_folder_path, 
                                 'crexi_listings.csv')) %>% 
    select(-X1) %>% mutate(source = 'Crexi')
commercialexchange_data <- read_csv(file.path(input_folder_path, 
                                              'commercialexchange_listings.csv')) %>% 
    select(-X1) %>% mutate(source = 'CommercialExchange')
commercialcafe_data <- read_csv(file.path(input_folder_path, 
                                          'commercialcafe_listings.csv')) %>% 
    select(-X1) %>% mutate(source = 'CommercialCafe')

all_rent_data <- loopnet_data %>% 
    select(-subtype) %>% 
    bind_rows(crexi_data) %>%
    bind_rows(commercialexchange_data) %>%
    mutate(source = 'All w/o CommercialCafe')

# Aggregate data by zip_code
restaurants_by_zip <- restaurants %>% 
    filter(!is.na(cbsa)) %>%
    group_by(zip_code) %>% 
    summarise(cbsa = first(cbsa), 
              visits = mean(raw_visit_counts, na.rm = TRUE))
rent_by_zip <- all_rent_data %>% 
    group_by(zip_code) %>%
    summarise(mean_rate = mean(rate, na.rm = TRUE), 
              mean_footage = mean(footage, na.rm = TRUE))
restaurants_by_zip <- restaurants_by_zip %>% left_join(rent_by_zip)
# Remove outliers
restaurants_by_zip <- restaurants_by_zip %>%
    mutate_at(vars(visits, 
                   mean_rate,
                   mean_footage), 
              funs(remove_outliers))
################################################################################


########################### Create plots and tables ############################

# Summary stats
loopnet_summary <- sumarise_rent(loopnet_data)
crexi_summary <- sumarise_rent(crexi_data)
commercialexchange_summary <- sumarise_rent(commercialexchange_data)
commercialcafe_summary <- sumarise_rent(commercialcafe_data)
all_rent_summary <- sumarise_rent(all_rent_data)

rent_summary_table <- bind_rows(loopnet_summary, 
                                crexi_summary, 
                                commercialexchange_summary, 
                                commercialcafe_summary, 
                                all_rent_summary) %>%
    rename(Source = source, 
           Listings = n, 
           `ZIP codes` = n_zip, 
           `Listings per ZIP` = count_mean, 
           `Sq. footage` = footage_mean, 
           `Q25` = rate_q25, 
           `Q50` = rate_q50, 
           `Q75` = rate_q75)

outfile <- file.path(tables_folder_path, 'rent_summary.tex')
kable(rent_summary_table, 'latex', booktabs = T, digits = 3) %>%
    add_header_above(c("", "N" = 2 , "Means" = 2, "Rent quantiles" = 3)) %>%
    cat(file = outfile)

# Visits - rental rate relationship
p1 <- ggplot(data = restaurants_by_zip, 
             aes(x = visits, 
                 y = mean_rate)) + 
    geom_smooth(size = 0.3, 
                color = mycolorscheme3[2], 
                fill = mycolorscheme3[2]) + 
    stat_summary_bin(fun.y = 'mean',
                     bins = 30, 
                     size = 0.15, 
                     geom = 'point') +
    theme_bw(base_family = 'Times') +
    my_theme +
    xlab('Ave. rest. visits in ZIP code') +
    ylab('Ave. rate per sq. ft.')

ggsave(filename = file.path(plots_folder_path, 'rent_visits.pdf'),
       device = cairo_pdf,
       plot = p1,
       width = 2.5,
       height = 1.75)
embed_fonts(file.path(plots_folder_path, 'rent_visits.pdf'))


fit1 <- felm(data = restaurants_by_zip, mean_rate ~ visits | 0 | 0 | cbsa)
fit2 <- felm(data = restaurants_by_zip, mean_rate ~ visits | cbsa | 0 | cbsa)
fit3 <- felm(data = restaurants_by_zip, mean_footage ~ visits | 0 | 0 | cbsa)
fit4 <- felm(data = restaurants_by_zip, mean_footage ~ visits | cbsa | 0 | cbsa)

rent_regression_table_file_path <- file.path(tables_folder_path, 
                                             'rent_regression_table.tex')
stargazer(fit1, fit2, fit3, fit4, 
          type = 'latex', 
          digits = 4, 
          align = TRUE,
          omit = 'Constant', 
          omit.stat = c('adj.rsq', 'ser'),
          add.lines = list(`CBSA FE` = c('CBSA FE', 'No', 'Yes', 'No', 'Yes'),
                           `SE` = c('SE', 'cl(CBSA)', 'cl(CBSA)', 'cl(CBSA)', 'cl(CBSA)')
                           ),
          dep.var.labels = c('Ave. rate per sq.ft.', 'Ave. listing footage'), 
          covariate.labels = c('Ave. visits'), 
          out = rent_regression_table_file_path)

################################################################################