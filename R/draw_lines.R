#' Create a summary line graph from a single analyte
#'
#' A function to create a summary line graph from a
#' dataset based on a single analyte.
#'
#' @param DF A data.frame containing data for a single analyte
#' @param palette_graph A palette with 5 colours
#'
#' @return a ggplot2 object and the function writes to
#' disk in the directory defined by the function
#' saveInImageDirectory
#'
#' @export

# DF = DF_grinta
# palette_graph = grinta_palette
# ymax = ymax
# ymin = ymin

draw_lines <- function(DF,
                       palette_graph)
                      # ymax,
                      # ymin
                      {

  ##############################
  # load library & functions
  ##############################
#  library(RColorBrewer)
#  library(wesanderson)
#  library(tidyverse)
#  library(xlsx)
#  library(dplyr)
#  source(paste0(root, "/code/article_graphs/theme_individual.R"))
#  source(file = file.path(root,
#                          "code",
#                          "article_graphs",
#                          "save_in_image_folder.R"))


  ################################
  # get analyte name
  ################################
  nameLine <- DF$analyte[1] %>%
    tolower()

#  center <- DF$center[1]

#  study <- paste0(toupper(as.character(DF$study[1])), "!")

  nameFile <- c(nameLine)


  ################################
  # get analyte metadata
  ################################
  analyte_annotations <- readxl::read_excel(path =
              file.path(
              root,
              "data-raw",
              "Copy of analytes_complete_ref_unit_SKa.xlsx"),
              sheet = 1)

  y_axis_label <- analyte_annotations %>%
    dplyr::filter(tolower(analyte_short) == nameLine) %>%
    dplyr::select(units) %>%
    as.character()

  ###########################
  # long name for analyte
  ###########################
  long_name <- analyte_annotations %>%
    dplyr::filter(tolower(analyte_short) == nameLine) %>%
    dplyr::select(analyte_long_name) %>%
    as.character()

   firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
   }

   plot_title <- firstup(long_name)
   #############################
   # axis limits
   #############################


 #  max_sem <- max(DF$sem)
#   max_mean <- max(DF$mean_conc)

  # y_lim_max <- 1.2*(max_mean + max_sem)
  # y_lim_min <- -0.2*(max_mean)

  # y_lim_max <- ymax
  # y_lim_min <- ymin

    #############################
   # getting p-value from statistical analysis results (Eric Schoen)
   #############################
    #  statistics <- readxl::read_excel(path =
     #                                 file.path(
      #                                  root,
      #                                  "data-raw",
      #                                  "D017_2",
      #                                  "FDR corrected P values.xlsx"),
      #                              sheet = 1)

 #  p_value <- statistics %>%
  #   filter(analyte == long_name) %>%
   #  select(`FDR corrected P`) %>%
    # as.numeric() %>%
    # round(digits = 4)

  # p_value_annotations <- paste("P-value mixed model =", p_value)

  # DF$protocol <- as.factor(DF$protocol)
  # DF$time <- as.factor(DF$time)
  # DF$matrix <- as.factor(DF$matrix)

# palette_manual <- wes_palette(n=5,
#                         name="Cavalcanti")

   plot <- ggplot2::ggplot(data = DF, aes(x = time,
                               y = mean_conc,
                               colour = protocol)) +

 # #  ggplot2::geom_errorbar(aes(ymin = mean_conc - sem,
 #                     ymax = mean_conc + sem),
 #                     width = 0.6,
 #                     position = position_dodge(width = 0.2),
 #                     show.legend = FALSE) +

   ggplot2::geom_point(aes(shape = protocol),
                  size = 3,
                  position = position_dodge(width = 0.2),
                  show.legend = FALSE) +

  ggplot2::geom_line(aes(group = protocol),
                size = 1.6,
                position = position_dodge(width = 0.2)) +

  ggplot2::scale_colour_manual(labels = c("Rest",
                                 "70%",
                                 "70% DH",
                                 "50%",
                                 "55%/85%"),
                     values = palette_graph)   +

  ## scale_color_identity(guide = "none") +

      #scale_colour_grey(start = 0, end = .85) +
     # facet_wrap(matrix ~ analyte, scales = "fixed", ncol = ncol) +

  ggplot2::scale_shape_manual(values=c(5,6, 15:17)) +
 # ggplot2::scale_y_continuous(limits = c(ymin, ymax)) +
       # ggtitle(nameLine)  +

  citrulliner::theme_individual() +

   ggplot2::xlab("Time (hours)") +

      ggplot2::ylab(paste("Concentration", paste0("(",
                                       y_axis_label,
                                       ")"))) +

   #  geom_segment(aes(x=1, xend=1, y=-2, yend=-1.5),
  #                arrow = arrow(length = unit(1, "cm"))) +


   #  annotate("segment",
    #           x = 3, y = 0.25*y_lim_min,
    #           xend = 3, yend = y_lim_min,
    #           size = 0.7,
    #           arrow=arrow(length=unit(0.3, "cm"))) +

 #    annotate("text",
#              x = 2,
 #             y = 0.9*y_lim_max,
  #            label = p_value_annotations) +


     ggplot2::ggtitle(plot_title)

  ##assigning a name to the file
 png <- paste(nameFile,"line.png", sep = "")
#  eps <- paste(nameFile,"line.eps", sep = "")
  svg <- paste(nameFile,"line.svg", sep = "")

  ## saving the file to the imageDir
 citrulliner::save_in_image_directory(svg)
 # saveInImageDirectory(eps)
  citrulliner::save_in_image_directory(png)

  return(plot)


 }

# line_graph_indiv(DF)

#line_graph_indiv(DF, im_dir = im_dir, width = 5, dpi = 300, height = 5)

# line_graph(DF = subset_1, ncol = 4)

#    labs(title = paste(nameLine, "\n", sep=""),
#         line = +4,
#         units = "pt") +
# geom_errorbar(aes(ymin = mean_conc - sem,
#                  ymax = mean_conc + sem),
#               width = .1, size = 0.5) +
#        scale_shape_manual(values=c(5,1,2,7,6)) +

# apply self-made theme
# Change shapes
#    scale_linetype_manual(values=c("solid",
#           "dotted", "longdash", "dotdash", "dashed"))
