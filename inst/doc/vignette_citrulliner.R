## ---- setup, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE,
                      error = FALSE,
                      message = FALSE,
                      include = TRUE,
                      fig.width = 11,
                      fig.height = 5)

## ---- root---------------------------------------------------------------
if(!require("rprojroot")) install.packages("rprojroot", dependencies = TRUE)
library(rprojroot)
root <- rprojroot::find_root_file(criterion = is_rstudio_project)
root

## ---- packages-----------------------------------------------------------
# install.packages("gdata")
# install.packages("stargazer")
# install.packages("arm")
# library(arm)
# library(stargazer)
# library(gdata)
# library(RCurl)
# install.packages("svglite")
library(tidyverse)
library(purrr)
library(readxl)
library(readr)
library(svglite)
library(ggplot2)
library(gridExtra)
library(grid)

## ---- include=TRUE-------------------------------------------------------
library(citrulliner)
data <- citrulliner::citrulline_data

## ------------------------------------------------------------------------
head(data)
levels(data$subject)
levels(data$protocol)
levels(data$time)
levels(data$analyte %>% as.factor())

## ------------------------------------------------------------------------
names(data)
diagrams <- data %>%
  dplyr::select(subject,
         protocol,
         time,
         analyte,
         concentration,
         duplicated
         )

## ------------------------------------------------------------------------
names(data)
data_summary <- data %>%
#  na.omit() %>%
   filter(duplicated == "FALSE") %>%
   group_by(protocol,
           time,
           analyte) %>%
  
  summarise(mean_conc = round(mean(concentration), digits = 4),
            sd = round(sd(concentration), digits = 3),
            n_obs = n(),
            sem = round(sd/sqrt(n_obs), digits = 3)
            ) %>%
  arrange(protocol, analyte)
  
#names(diagrams_summary)[1] <- c("protocol")
#names(diagrams_summary)[2] <- c("time")
data_summary


## ------------------------------------------------------------------------

## split for analytes
data_by_analyte <- data_summary %>%
#  as_tibble() %>%
#  gather(exam:numeracy, key = "measure", value = "value") %>%
  group_by(analyte) %>%
  nest() %>% print()

data_by_analyte$data[[1]]


## ------------------------------------------------------------------------

data_summary$analyte <- as.factor(data_summary$analyte)
levels(data_summary$analyte)

data_split <- split(data_summary, 
                        droplevels(data_summary$analyte))



## ------------------------------------------------------------------------
RColorBrewer::display.brewer.all()
palette <- RColorBrewer::brewer.pal(7, "Set1")

# GetColorHexAndDecimal <- function(color){
#   c <- col2rgb(color)
#   
#   sprintf("#%02X%02X%02X%3d%3d%3d", c[1],c[2],c[3],c[1],c[2],c[3])
# } 
# 
# black <- GetColorHexAndDecimal("black")
palette <- c("#000000", palette[c(1:3,4)])

## ------------------------------------------------------------------------
# define image directory
image_directory <- file.path(root, "inst", "images")
dir.create(file.path(root, "inst",
                           "images"))

# argument for function test
analyte = "ala"

print_lines <- function(analyte){

  ymax_data <- max(data_split[[analyte]]$mean_conc)
  sem_max <- max(data_split[[analyte]]$sem)
  ymax <- 1.2*(ymax_data + sem_max)
  
  ymin_data <- min(data_split[[analyte]]$mean_conc)
  ymin <- ymin_data - (1.2*sem_max)
  
data_plot <- data_split[[analyte]]
    
  
 plot <- citrulliner::draw_lines(DF = data_plot, palette_graph = palette) 
                         # ymax = ymax,
                        #  ymin = ymin)

  

  return(plot)
}

ala_lines <- print_lines(analyte = "ala")
ala_lines
plot_list <- lapply(levels(data_summary$analyte), print_lines)
names(plot_list) <- levels(data_summary$analyte)
names(plot_list)

## ------------------------------------------------------------------------

library(gtable)
library(cowplot)
print_lines_panel <- function(analyte){

  ymax_data <- max(data_split[[analyte]]$mean_conc)
  sem_max <- max(data_split[[analyte]]$sem)
  ymax <- 1.2*(ymax_data + sem_max)
  
  ymin_data <- min(data_split[[analyte]]$mean_conc)
  ymin <- ymin_data - (1.2*sem_max)
  
data_plot <- data_split[[analyte]]
    
  
 plot <- citrulliner::draw_lines_panel(DF = data_plot, palette_graph = palette) 
                         # ymax = ymax,
                        #  ymin = ymin)

  

  return(plot)
}


panel_plot_list <- lapply(levels(data_summary$analyte), print_lines_panel)
names(panel_plot_list) <- levels(data_summary$analyte)
names(panel_plot_list)


# figure 3; panel
p1_a <- panel_plot_list[["gln"]]
p1_b <- panel_plot_list[["ala"]]
p1_c <- panel_plot_list[["citrul"]]
p1_d <- panel_plot_list[["arg"]]
p1_e <- panel_plot_list[["UREUM"]]

# figure 4; panel
p2_a <- panel_plot_list[["ifabp"]]
p2_b <- panel_plot_list[["CORT"]]


# https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html


# arrange the three plots in a single row

prow <- plot_grid( p1_a + theme(legend.position="none"),
           p1_b + theme(legend.position="none"),
           p1_c + theme(legend.position="none"),
           align = 'vh',
           labels = c("A", "B", "C"),
           hjust = -1,
           nrow = 1
           )

prow

# extract the legend from one of the plots

# (clearly the whole thing only makes sense if all plots

# have the same legend, so we can arbitrarily pick one.)

legend_b <- get_legend(p1_a + theme(legend.position="bottom"))

# add the legend to the row we made earlier. Give it one-third of the width

# of one plot (via rel_widths).

p <- plot_grid( prow, legend_b, ncol = 1, rel_heights = c(1, .2))
p
citrulliner::save_in_image_directory(filename = "test.png", height = 6, width = 15)

# 
# png(filename = file.path(root, "inst", "images", "figure_3_panel.png"),
# width = 24, height = 40, res = 300, units = "cm")
# citrulliner::grid_arrange_shared_legend(p1_a, 
#                                         p1_b,
#                                         p1_c,
#                                         p1_d,
#                                         p1_e,
#                                         ncol = 2, 
#                                         nrow = 3)
# dev.off()
# 
# grid[1, ]
# 
# 
# left.foot = textGrob("Source: \"Russia's Billionaires\",\nD.Treisman, American Economic Review (2016).", 
# x = 0, y = 0.8, just = c("left", "top"), 
# gp = gpar(fontsize = 11, col =  "black", fontfamily = "ITCOfficinaSans LT Book"))
# labs.foot = gTree("LabsFoot", children = gList(left.foot))
# 
# g1 <- gtable_acdd_grob(grid, labs.foot, t=1, l=2, r=4)
# 
# png(filename = file.path(root, "inst", "images", "figure_3_panel.png"),
#     width = 30, height = 12, res = 300, units = "cm")
# grid
# dev.off()
# 
# 
# g <- grid.draw(g1)
# 
# 
# #ggsave(filename = file.path(root, "inst", "images", "figure_3_panel.png"), panel_3)


