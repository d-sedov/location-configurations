###############################################################################
#
# FILE: fc_estimation.R
#
# BY: Dmitry Sedov 
#
# DATE: Mon Jun 8 2020
#
# DESC: This code contains the code to estimate fixed costs off the zero-profit 
#       equilibrium.
#
# IN:
#     1. Dataset with the estimated markups.
#
###############################################################################


################################ Libraries ####################################

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gsubfn))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(stargazer))
library(kableExtra)
library(ggridges)
library(extrafont)
library(lfe)

###############################################################################


################################ Constants ####################################

days <- 31
input_folder <- '/home/quser/project_dir/urban/data/output/entry/markups'
tables_output_folder = '/home/quser/project_dir/urban/output/tables/model'
population_folder <- '/home/quser/project_dir/urban/data/output/descriptive'
summary_file_path <- file.path(input_folder, 'minimization_summary_optimized.csv')

# Time discounting parameter
# phi <- 0.97

# Population normalization: ACTUALLY THIS NEEDS TO BE COMPUTED PROPERLY!!!
population_norm <- 10

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

###############################################################################


################################ Functions ####################################

# Function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 2 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Create two models as a function of the discount rate:
estimate_fc <- function(phi, restaurants) {
  # Compute profits:
  restaurants <- restaurants %>% 
    mutate(profit = ((population_norm * raw_visit_counts * estimated_markup) - 
                       10.7 * mean_rate * area_m2
    )
    ) %>%
    mutate(profit = profit / (1 - phi))
  
  # Drop missing prices and turn price into vector
  restaurants <- restaurants %>% 
    mutate(price = replace(price, is.na(price), 0)) %>%
    mutate(price = replace(price, price == -1, 0)) %>%
    filter(price != 0) %>%
    mutate(price = as.character(price)) %>%
    mutate(price = factor(price)) %>% 
    mutate(price = relevel(price, ref = '1')) 
  
  # Recover the fixed costs
  
  # Strategy 1
  fit1 <- felm(data = restaurants, profit ~ 0 + price | 0 | 0 | r_cbsa)
  
  # Strategy 2
  ef <- function(gamma, addnames) {
    ref1 <- mean(gamma[1 : 381])
    gamma[1 : 381] <- gamma[1:381] - ref1
    gamma[[382]] <- gamma[[382]] + ref1
    gamma[[383]] <- gamma[[383]] + ref1
    if(addnames) {
      names(gamma) <- c(paste('r_cbsa',1:381,sep='.'),
                        paste('price',1:2, sep='.'))
    }
    gamma
  }
  fit2 <- felm(data = restaurants, profit ~ 1 | r_cbsa + price | 0 | r_cbsa)
  fit2_fe <- getfe(fit2, ef = ef, bN = 1000,se = TRUE)
  fit2_shadow <- fit1
  fit2_shadow$coefficients <- matrix(data = c(fit2_fe['price.1', 'effect'], 
                                              fit2_fe['price.2', 'effect']),
                                     nrow = 2, ncol = 1, 
                                     dimnames = list(c('price1', 'price2'), 'profit'))
  fit2_shadow$beta <- matrix(data = c(fit2_fe['price.1', 'effect'],
                                      fit2_fe['price.2', 'effect']),
                             nrow = 2, ncol = 1,
                             dimnames = list(c('price1', 'price2'), 'profit'))
  fit2_shadow$cse <- c('price1' = fit2_fe['price.1', 'clusterse'],
                       'price2' = fit2_fe['price.2', 'clusterse'])
  fit2_shadow$cpval <- c('price1' = (1 - fit2_fe['price.1', 'effect'] / fit2_fe['price.1', 'clusterse']) / 2,
                         'price2' = (1 - fit2_fe['price.2', 'effect'] / fit2_fe['price.2', 'clusterse']) / 2
  )

  return(list(fit1, fit2_shadow))  
}

###############################################################################


############################ Data prepartation ################################

# Import
estimated_markups_path <- file.path(input_folder, 'restaurants_estimated_markups.csv')
restaurants <- read_csv(estimated_markups_path)
# Remove outliers
restaurants <- restaurants %>%  
  mutate_at(vars(estimated_markup), 
            funs(remove_outliers))
# Replace NA brands with 'none'
restaurants <- restaurants %>% mutate(brands = replace(brands, is.na(brands), 'none'))
# Group small brands an categories into one
restaurants <- restaurants %>% group_by(brands) %>% 
  mutate(n_brands = n()) %>% 
  ungroup() %>%
  mutate(brands = replace(brands, n_brands <= 100, 'none')) %>%
  select(-n_brands)

non_branded_restaurants <- restaurants %>% filter(brands == 'none')

estimated_models_all <- lapply(c(0.965, 0.97, 0.975), estimate_fc, restaurants)
estimated_models_non_branded <- lapply(c(0.965, 0.97, 0.975), 
                                       estimate_fc, 
                                       non_branded_restaurants)

# Display the results
latex_table <-
  stargazer(estimated_models_all[[1]][[1]], 
          estimated_models_all[[1]][[2]],
          estimated_models_all[[2]][[1]], 
          estimated_models_all[[2]][[2]],
          estimated_models_all[[3]][[1]], 
          estimated_models_all[[3]][[2]],
          type = 'latex', 
          title = paste('Fixed cost estimates.',
                        'Columns (2), (4), (6) report estimates with across-market zero average profit condition enforced.',
                        'CBSA-clustered standard errors in parentheses.',
                        'Only the restaurants with non-missing price category are included in the estimation.'),
          label = 'tab:fc_estimates',
          table.placement = '!t',
          align = TRUE,
          omit.stat = c('rsq', 'adj.rsq', 'ser'),
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 1, 
          digits.extra = 0,
          notes.append = TRUE,
          column.labels = c('$\\varphi=0.965$', '$\\varphi=0.97$', '$\\varphi=0.975$'),
          column.separate = c(2, 2, 2),
          model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          dep.var.caption = '',
          covariate.labels = c('FC(\\$)', 
                               'FC(\\$\\$)'),
          add.lines = list(`CBSA FE` = c('\\rowfont{\\footnotesize}CBSA FE',
                                         '', '\\multicolumn{1}{c}{\\checkmark}', 
                                         '', '\\multicolumn{1}{c}{\\checkmark}',
                                         '', '\\multicolumn{1}{c}{\\checkmark}')) 
                           
)

latex_table <- c(latex_table[1:7],
                 '\\resizebox{\\textwidth}{!}{',
                 latex_table[8:10],
                 '\\cmidrule(l){2-3} \\cmidrule(l){4-5} \\cmidrule(l){6-7}',
                 latex_table[11:24], 
                 '}',
                 latex_table[25])

latex_table <- gsub('tabular', 'tabu', latex_table, fixed = T)
latex_table <- gsub('Observations', '\\rowfont{\\footnotesize} Observations', latex_table, fixed = T)
latex_table <- gsub('R$^{2}$', '\\rowfont{\\footnotesize} R$^{2}$', latex_table, fixed = T)
latex_table <- gsub('Adjusted', '\\rowfont{\\footnotesize} Adjusted', latex_table, fixed = T)

outfile <- file.path(tables_output_folder,
                     'fixed_costs_estimates_all.tex')

cat(paste(latex_table, collapse = '\n'), file = outfile)

stargazer(estimated_models_non_branded[[1]][[1]], 
          estimated_models_non_branded[[1]][[2]],
          estimated_models_non_branded[[2]][[1]], 
          estimated_models_non_branded[[2]][[2]],
          estimated_models_non_branded[[3]][[1]], 
          estimated_models_non_branded[[3]][[2]],
          type = 'latex', 
          align = TRUE,
          omit.stat = c('rsq', 'adj.rsq', 'ser'),
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 1, 
          digits.extra = 0,
          out = file.path(tables_output_folder,
                          'fixed_costs_estimates_non_branded.tex'),
          notes = 'CBSA-clustered standard errors in parentheses.',
          notes.append = TRUE,
          column.labels = c('phi=0.965', 'phi=0.97', 'phi=0.975'),
          column.separate = c(2, 2, 2),
          model.numbers = TRUE,
          dep.var.labels.include = FALSE,
          dep.var.caption = '',
          covariate.labels = c('FC($)', 
                               'FC($$)')
)


###############################################################################
