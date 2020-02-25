################################################################################
################################################################################
#
# FILE: interactions-visualization.R
#
# BY: Dmitry Sedov 
#
# CREATED: Mon Feb 24 2020
#
# DESC: This file creates the graphs on the interaction coefficients from Stata
#       regressions.
#
################################################################################
################################################################################


################################## Libraries ###################################

library(readr)

################################################################################

################################## Options #####################################

# Input folder
input_folder_path = '/Users/muser/dfolder/Research/urban/output/misc/reduced-form'

# Output folders
plots_folder_path = '/Users/muser/dfolder/Research/urban/output/plots/reduced-form'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(size = 6),
                  axis.title = element_text(size = 8)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

################################################################################

################################ Visualize #####################################

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

# Import data
panel_regression_naics <- read_csv(file.path(input_folder_path, 'panel_regression_naics.csv'), 
                                   col_types = cols(naics_2 = col_integer()))
panel_regression_price <- read_csv(file.path(input_folder_path,
                                             'panel_regression_price_heterogeneity.csv'),
                                   col_types = cols(price = col_integer()))
panel_regression_rating <- read_csv(file.path(input_folder_path,
                                              'panel_regression_rating_heterogeneity.csv'))

panel_regression_naics <- left_join(panel_regression_naics,
                                    naics_codes_names, 
                                    by = c('naics_2' = 'naics_first2'))

p1 <- ggplot(panel_regression_naics %>% 
                 filter(type == 'urban'), 
             aes(x = reorder(Industry, -estimate),
                 y = estimate,
                 fill = estimate < 0,
                 alpha = p_value < 0.15)) + 
    geom_bar(stat = 'identity', 
             width = 0.65) + 
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(axis.text.x = element_text(angle = 30,
                                     hjust = 1,
                                     vjust = 1)) +
    theme(legend.key.size = unit(0.125, 'inches'),
          legend.title=element_blank(),
          legend.position = c(.05, .05),
          legend.justification = c('left', 'bottom'),
          plot.margin = margin(0, 0, 0, 0, 'pt')) +
    xlab('') +
    ylab('Coefficient') +
    scale_y_continuous(label = comma) + 
    scale_fill_manual(values = c('#e70300','#00279a'), 
                      labels = c('Pos., sig. at 15%', 
                                 'Neg., insig. at 15%'), 
                      guide = guide_legend(override.aes = list(alpha = c(1.0, 0.3)))) +
    scale_alpha_discrete(range = c(0.3, 1.0), guide = FALSE)

ggsave(filename = file.path(plots_folder_path, 'naics_coefficients.pdf'),
       device = cairo_pdf,
       plot = p1, 
       width = 3.5,
       height = 2.5)

p_p_e <- ggplot(data = panel_regression_price %>% filter(type == 'est')) + 
    geom_pointrange(aes(x = price, y = estimate, ymin = ci_l, ymax = ci_u), 
                    colour = mycolorscheme3[1], 
                    size = 0.1) + 
    geom_hline(yintercept = 0,
               size = 0.3, 
               linetype = 'dotted') + 
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          legend.title=element_blank(),
          legend.position = c(.05, .05),
          legend.justification = c('left', 'bottom'),
          plot.margin = margin(0, 5, 0, 0, 'pt')) +
    xlab('Price') +
    ylab('Interaction coefficient') +
    ggtitle('Establishments')

p_p_d <- ggplot(data = panel_regression_price %>% filter(type == 'dev')) + 
    geom_pointrange(aes(x = price, y = estimate, ymin = ci_l, ymax = ci_u), 
                    colour = mycolorscheme3[2], 
                    size = 0.1) + 
    geom_hline(yintercept = 0,
               size = 0.3, 
               linetype = 'dotted') + 
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          legend.title=element_blank(),
          legend.position = c(.05, .05),
          legend.justification = c('left', 'bottom'),
          plot.margin = margin(0, 0, 0, 5, 'pt')) +
    xlab('Price') +
    ylab('Interaction coefficient') +
    ggtitle('Devices')

p_p <- ggarrange(p_p_e, p_p_d + theme(axis.title.y = element_blank()), ncol = 2, nrow = 1, align = 'h')

ggsave(filename = file.path(plots_folder_path, 'price_interactions.pdf'),
       device = cairo_pdf,
       plot = p_p, 
       width = 3.75,
       height = 2.0)

p_r_e <- ggplot(data = panel_regression_rating %>% filter(type == 'est')) + 
    geom_pointrange(aes(x = rating, y = estimate, ymin = ci_l, ymax = ci_u), 
                    colour = mycolorscheme3[1], 
                    size = 0.1) + 
    geom_hline(yintercept = 0,
               size = 0.3, 
               linetype = 'dotted') + 
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          legend.title=element_blank(),
          legend.position = c(.05, .05),
          legend.justification = c('left', 'bottom'),
          plot.margin = margin(0, 5, 0, 0, 'pt')) +
    xlab('Rating') +
    ylab('Interaction coefficient') +
    ggtitle('Establishments')

p_r_d <- ggplot(data = panel_regression_rating %>% filter(type == 'dev')) + 
    geom_pointrange(aes(x = rating, y = estimate, ymin = ci_l, ymax = ci_u), 
                    colour = mycolorscheme3[2], 
                    size = 0.1) + 
    geom_hline(yintercept = 0,
               size = 0.3, 
               linetype = 'dotted') + 
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          legend.title=element_blank(),
          legend.position = c(.05, .05),
          legend.justification = c('left', 'bottom'),
          plot.margin = margin(0, 0, 0, 5, 'pt')) +
    xlab('Rating') +
    ylab('Interaction coefficient') +
    ggtitle('Devices')

p_r <- ggarrange(p_r_e, p_r_d + theme(axis.title.y = element_blank()), ncol = 2, nrow = 1, align = 'h')

ggsave(filename = file.path(plots_folder_path, 'rating_interactions.pdf'),
       device = cairo_pdf,
       plot = p_r, 
       width = 3.75,
       height = 2.0)

################################################################################