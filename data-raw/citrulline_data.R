###########################################################
# Read the data for the paper, and create a dataset in the namespace
###########################################################

citrulline_data <- readr::read_rds("./data-raw/citrulline_paper_kartaram_et_al_1_0.rds")
devtools::use_data(citrulline_data, overwrite = TRUE)

