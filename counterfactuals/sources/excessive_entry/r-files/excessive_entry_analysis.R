###############################################################################
#
# FILE: excessive_entry_analysis.R
#
# BY: Dmitry Sedov 
#
# DATE: Thu Sep 10 2020
#
# DESC: This code contains the code to analyze the excessive entry.
#
# COMMENT: 
#
###############################################################################


################################ Libraries ####################################

library(dplyr)
library(readr)
library(extrafont)
library(extrafontdb)
library(scales)
library(ggplot2)
library(lfe)
library(stargazer)
library(sandwich)
library(ggpubr)

###############################################################################


################################# Functions ###################################

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


################################# Options ######################################

fc_estimates <- data.frame(price = c(1, 2), 
                           fc = c(328688.8, 642701.1), 
                           se = c(7981.7, 11135.8))

output_file_path <- file.path('/home/quser/project_dir/urban/data/output', 
                              'counterfactuals',
                              'excessive_entry',
                              'deleted_firms.csv')

input_folder_path <- file.path('/home/quser/project_dir', 
                               'urban/data/output/descriptive')

plots_folder_path <- file.path('/home/quser/project_dir/urban/output', 
                               'plots/model')
tables_folder_path <- file.path('/home/quser/project_dir/urban/output', 
                                'tables/model')

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(size = 6),
                  axis.title = element_text(size = 8)
)
my_theme_large <- theme(legend.text = element_text(size = 10),
                        legend.title = element_text(size = 12),
                        plot.title = element_text(hjust = 0.5, size = 20),
                        axis.text = element_text(size = 10),
                        axis.title = element_text(size = 12)
)

mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

###############################################################################



################################## Main code ##################################

# Read in the data
excessive_entry <- read_csv(output_file_path)
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv')) %>%
  group_by(cbsa) %>% summarize(n = n())

excessive_entry_cbsa_total <- excessive_entry %>%
  group_by(cbsa) %>% 
  summarize(total_deleted = sum(n)) %>% 
  left_join(restaurants) %>%
  mutate(share_deleted = total_deleted / n)

min(excessive_entry_cbsa_total$share_deleted)
max(excessive_entry_cbsa_total$share_deleted)

pic_excessive_percent <- ggplot(data = excessive_entry_cbsa_total,
                                aes(x = share_deleted)) + 
  geom_histogram(bins = 40,
                 color = 'white',
                 fill = mycolorscheme3[2]) +
  geom_vline(aes(xintercept = median(share_deleted)), 
             col = 'black',
             size = 0.25,
             linetype = 'dashed') +
  theme_bw(base_family = 'Times') + 
  my_theme_large +
  scale_x_continuous(labels = scales::percent) +
  xlab('Share of removed firms') +
  ylab('Count of markets') +
  annotate('text',
           x = median(excessive_entry_cbsa_total$share_deleted), 
           y = Inf, 
           vjust = 1.5,
           hjust = -0.075,
           label = paste0('Median: ',
                          percent(median(excessive_entry_cbsa_total$share_deleted), 
                                  accuracy = 0.01)), 
           family = 'Times', 
           size = 3.15) 

ggsave(filename = file.path(plots_folder_path, 
                            'share_deleted_across_markets.pdf'), 
       device = cairo_pdf, plot = pic_excessive_percent, width = 4.0, height = 2.5)
embed_fonts(file = path.expand(file.path(plots_folder_path,
                                         'share_deleted_across_markets.pdf')))

# Hide NAs from kable
options(knitr.kable.NA = '')

# Fixed cost savings associated with firm removal:
excessive_entry %>%
  group_by(price) %>% 
  summarize(n = sum(n)) %>% 
  left_join(fc_estimates) %>%
  mutate(saving = n * fc) %>%
  filter(!is.na(price)) %>% 
  select(-se) %>% 
  select(price, fc, n, saving) %>%
  bind_rows(data.frame(price = NA,
                       fc = NA,  
                       n = sum(temp$n),
                       saving = sum(temp$saving))) %>%
  mutate(price = strrep('\\$', price),
         saving = paste(format(round(saving / 1e9, 2), nsmall = 2), 
                        'bn', sep = '\\,'),
         n = comma(n),
         fc = comma(fc, accuracy = 0.1)) %>% 
  rename(`Price` = price,
         `Fixed cost` = fc, 
         `Removed` = n, 
         `Savings` = saving) %>%
  replace_na(list(Price = 'Both')) %>%
  kable(format = 'latex', align = 'llrl', booktabs = T, escape = F) %>% 
  row_spec(0, bold = F, italic = T) %>%
  cat(., file = file.path(tables_folder_path, 'fixed_cost_savings.tex'))

###############################################################################