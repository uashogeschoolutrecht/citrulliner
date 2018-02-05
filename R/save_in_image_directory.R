#' Function to save a ggplot2 graph to disk
#'
#' @param filename A filename as a full path character string, including extensions
#' @export


## save a file in specified folder with ggsave

save_in_image_directory<-function(filename, height = 7, width = 12){
  imageFile <- file.path(image_directory, filename)
  cowplot::ggsave(imageFile, dpi = 900, width = width, height = height)
}
