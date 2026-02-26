# load pkgs
pacman::p_load( "ggplot2", "tidyverse" )

# load data
load( file = "otgp_markers.Rdata" )

plot_otgp.f <- function( the_table  ){
  
  the_marker.v <- the_table$marker %>% unique( )
  
  otgp_panel.p <- ggplot( data = the_table,
                          mapping = aes( x = del_tag,
                                         y = marker_gt,
                                         label = n,
                                         fill = n ) ) +
    geom_tile( ) +
    geom_text( ) +
    scale_fill_gradient( low = "white",
                         high = "tomato" ) +
    labs( title = paste( "Marker Genotype", the_marker.v ),
          x = "SNP density in RHD region",
          y = "marker genotype" ) +
    facet_wrap( ~`Superpopulation code`, nrow = 1 ) +
    theme_classic( base_size = 20 ) +
    theme( legend.position = "none",
           strip.background = element_rect( color = "white" ) )
  
  ggsave( filename = paste0( the_marker.v, "_otgp_panel.svg"),
          plot = otgp_panel.p, width = 21,
          height = 7 )
  
}

plot_otgp.f( the_table = mark_1$toplot )
plot_otgp.f( the_table = mark_2$toplot )

### move plots ----
# 1. Define the directory name
dir_name <- "results"

# 2. Create the directory (won't fail if it already exists)
dir.create(dir_name, showWarnings = FALSE)

# 3. Identify all .svg and .pdf files in the current working directory
files_to_move <- list.files(pattern = "\\.(svg|png)$")

# 4. Move the files into the 'results' folder
# This constructs the new path (e.g., "results/plot.pdf") for each file
if (length(files_to_move) > 0) {
  success <- file.rename(from = files_to_move, 
                         to = file.path(dir_name, files_to_move))
  
  # Optional: Print a quick status update
  message(paste("Moved", sum(success), "files to", dir_name))
} else {
  message("No .svg or .pdf files found to move.")
}

