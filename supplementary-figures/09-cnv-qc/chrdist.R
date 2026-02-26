#load pkgs
pacman::p_load( "vroom", "dplyr", "tidyr", "ggplot2", "stringr" )

# load objects
load( file = "d2gap.Rdata" )

# create a function to  plot croms
plot_chroms.f <- function( the_df ) {
  
  allchroms.p <- ggplot( ) +
    facet_wrap( ~ chrom, 
                ncol = 1,
                scales = "free_y",
                strip.position = "left" ) +
    geom_segment( data = chr_backbone.df,
                  mapping = aes( x = chromStart,
                                 xend = chromEnd ),
                  y = 0, yend = 0  ) +
    theme_void( base_size = 20 ) +
    geom_col( data = the_df,
              mapping = aes( x = start,
                             y = cnv_log,
              ) ) +
    geom_rect( data = forplot_wide.df %>% 
                 filter( type == "centrom" ),
               mapping = aes( xmin = start, xmax = end ),
               ymin = -Inf, ymax = Inf,
               fill = "orange", alpha = 0.5 ) +
    theme( strip.text = element_text( size = 20, face = "bold" ) )
  
}

plot_original.p <- plot_chroms.f( the_df = allcnv.histo.df_original )

ggsave( filename = "allchroms_original.png", plot = plot_original.p,
        width = 10, height = 20,
        bg = "transparent" )

plot_filtered.p <- plot_chroms.f( the_df = allcnv.histo.df_filtered )

ggsave( filename = "allchroms_filtered.png", plot = plot_filtered.p,
        width = 10, height = 20,
        bg = "transparent" )

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
