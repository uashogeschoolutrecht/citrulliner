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
library(citrulliner)

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
data <- data %>%
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
   dplyr::filter(duplicated == "FALSE") %>%
   dplyr::group_by(protocol,
           time,
           analyte) %>%
  
  dplyr::summarise(mean_conc = round(mean(concentration), digits = 4),
            sd = round(sd(concentration), digits = 3),
            n_obs = n(),
            sem = round(sd/sqrt(n_obs), digits = 3)
            ) %>%
  dplyr::arrange(protocol, analyte)
  
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

print_lines <- function(analyte, ...){

  #ymax_data <- max(data_split[[analyte]]$mean_conc)
#  sem_max <- max(data_split[[analyte]]$sem)
#  ymax <- 1.6*(ymax_data)
  
#  ymin_data <- min(data_split[[analyte]]$mean_conc)
#  ymin <- ymin_data - 1.6*(ymin_data)
  
data_plot <- data_split[[analyte]]
    
  
 plot <- citrulliner::draw_lines(DF = data_plot, 
                                 palette_graph = palette,
                                # ymax = ymax,
                                # ymin = ymin 
                                f.width = 14, f.height = 10) 
 
 


  return(plot)
}

ala_lines <- print_lines(analyte = "ala")
ala_lines
plot_list <- lapply(levels(data_summary$analyte), print_lines)
names(plot_list) <- levels(data_summary$analyte)
names(plot_list)

print_lines("ala", f.width = 18, f.height = 8)


## ------------------------------------------------------------------------
library(citrulliner)
library(gtable)
library(cowplot)

print_lines_panel <- function(analyte){

  ymax_data <- max(data_split[[analyte]]$mean_conc)
  sem_max <- max(data_split[[analyte]]$sem)
  ymax <- 1.05*(ymax_data)
  
  ymin_data <- min(data_split[[analyte]]$mean_conc)
  ymin <- ymin_data - (0.05*ymin_data)
  
data_plot <- data_split[[analyte]]
    
  
 plot <- citrulliner::draw_lines_panel(DF = data_plot, 
                                       palette_graph = palette, 
                                       ymax = ymax,
                                       ymin = ymin)

  

  return(plot)
}


panel_plot_list <- lapply(levels(data_summary$analyte), print_lines_panel)
names(panel_plot_list) <- levels(data_summary$analyte)
names(panel_plot_list)

panel_plot_list[[1]]

## ------------------------------------------------------------------------

# figure 3; panel
p3_a <- panel_plot_list[["gln"]]
p3_b <- panel_plot_list[["ala"]]
p3_c <- panel_plot_list[["citrul"]]
p3_d <- panel_plot_list[["arg"]]
p3_e <- panel_plot_list[["UREUM"]]
p3_f <- panel_plot_list[["CK"]]
# figure 4; panel
p4_a <- panel_plot_list[["ifabp"]]
p4_b <- panel_plot_list[["CORT"]]


## ---- fig.width=16, fig.height=24----------------------------------------
# https://cran.r-project.org/web/packages/cowplot/vignettes/shared_legends.html

figure_3 <- plot_grid( p3_a + theme(legend.position="none"),
           p3_b + theme(legend.position="none"),
           p3_c + theme(legend.position="none"),
           p3_d + theme(legend.position="none"),
           p3_e + theme(legend.position="none"),
           p3_f + theme(legend.position="none"),
           align = 'vh',
           labels = c("A", "B", "C", "D", "E", "F"),
           hjust = -8,
           ncol = 2,
           nrow = 3,
           label_size = 18
           )

figure_3

## ------------------------------------------------------------------------

legend_b <- get_legend(p3_a + theme(legend.position="bottom"))

p_3 <- plot_grid(legend_b, figure_3, ncol = 1, rel_heights = c(0.05, 1))

## citrulliner::save_in_image_directory(filename = "test2.svg", height = 26, width = 7)


## ---- eval=FALSE---------------------------------------------------------
#  
#  svg(filename = "./inst/images/panel_fig_3.svg", height = 20, width = 20)
#  p_3
#  dev.off()
#  
#  pdf(file = "./inst/images/panel_fig_3.pdf", height = 20, width = 20)
#  p_3
#  dev.off()
#  
#  
#  
#  p_3
#  
#  

## ---- fig.width=16, fig.height=20----------------------------------------
figure_4 <- plot_grid( p4_a + theme(legend.position="none"),
           p4_b + theme(legend.position="none"),
           align = 'vh',
           labels = c("A", "B"),
           hjust = -6,
           ncol = 1,
           label_size = 18
           )

figure_4


legend_b <- get_legend(p4_a + theme(legend.position="bottom"))

p_4 <- plot_grid(legend_b, figure_4, ncol = 1, rel_heights = c(0.05, 1))

## citrulliner::save_in_image_directory(filename = "test2.svg", height = 26, width = 7)

## ---- eval = FALSE-------------------------------------------------------
#  
#  
#  svg(filename = "./inst/images/panel_fig_4.svg", height = 14, width = 9)
#  p_4
#  dev.off()
#  
#  
#  pdf(file = "./inst/images/panel_fig_4.pdf", height = 14, width = 9)
#  p_4
#  dev.off()
#  
#  
#  
#  p_4

