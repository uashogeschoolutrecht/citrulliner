## ---- root, include=FALSE------------------------------------------------
if(!require("rprojroot")) install.packages("rprojroot", dependencies = TRUE)
library(rprojroot)
root <- rprojroot::find_root_file(criterion = is_rstudio_project)
root

## ---- packages-----------------------------------------------------------
library(tidyverse)
library(readr)
library(gramlyr)

## ---- read_data----------------------------------------------------------
data_file_path <- file.path(root, "data", "diagrams_clean.rds")

#diagrams <- gramlyr::diagrams_clean
 diagrams <- read_rds(path = data_file_path)

## ------------------------------------------------------------------------
head(diagrams, 3)
# check factor levels
levels(diagrams$subject)
levels(diagrams$protocol)
levels(diagrams$time)
levels(diagrams$analyte %>% as.factor)

## ------------------------------------------------------------------------
# create vector with analyte level-names
analytes_filter <- c("gln", "ala", "citrul", "arg", "UREUM", "ifabp", "CORT", "CK")

## ---- filter-------------------------------------------------------------
citrulline_data <- diagrams %>%
  filter(analyte %in% analytes_filter) %>%
  filter(study == "grinta") %>% droplevels(.)

# check factor levels
levels(citrulline_data$subject)
levels(citrulline_data$protocol)
levels(citrulline_data$time)
levels(citrulline_data$analyte %>% as.factor)
head(citrulline_data)

## ------------------------------------------------------------------------
sum(is.na(citrulline_data))

## ------------------------------------------------------------------------
write_rds(citrulline_data, path = file.path(root, "data-raw", "citrulline_paper_kartaram_et_al_1_0.rds"))

