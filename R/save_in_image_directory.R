#' Function to save a ggplot2 graph to disk
#'
#' @param filename A filename as a full path character string, including extensions
#' @export


## save a file in specified folder with ggsave

save_in_image_directory<-function(filename, f.height = 9, f.width = 14, ...){
  imageFile <- file.path(image_directory, filename)
  cowplot::ggsave(imageFile, dpi = 900, width = f.width, height = f.height)
}
