# a function to create panels
create_panel <- function(df_list, analyte_names, labels){
  
  analytes <- df_list[c(analyte_names)]
  
  panel_plots <- lapply(analytes, line_graph_indiv)
  
  legend_plot <- line_legend(analytes[[1]])
  
  legend <- get_legend(legend_plot)
  
  panel <- cowplot::plot_grid(plotlist = panel_plots, labels = labels,
                              hjust = -1, vjust = 4, align = "hv")
  
  with_legend <- plot_grid(panel, legend, rel_widths = c(2, .2), 
                           scale = 1.2)
  
  
  return(with_legend)
  
}