# load packages
pacman::p_load( "vroom", "dplyr", "tidyr", "ggplot2" )

# custom num func
myscale.f <- function( x ){
  x <- x / 1e6
  prettyNum( x, big.mark = "," )
}

# custom num func
myscale.f2 <- function( x ){
  x <- x / 1e3
}

# custom num func
myscale.f3 <- function( x ){
  x <- x / 1
}

data.df <- vroom( file = "nvariants.tsv" ) %>% 
  as_tibble( ) %>% 
  mutate( id = rownames( . ),
          .before = 1 )

## Make a function for creating publication (almost) ready plots
makebox.f <- function( the_data, the_type, the_scale,
                       y_limits, the_breaks, y_name,
                       x_name, the_panel_minor_y ) {
  
  the_data <- pivot_longer( data = the_data,
                            cols = -id,
                            names_to = "type",
                            values_to = "n" )
  
  the_data <- the_data %>% 
    filter( type == the_type )
  
  the_box.p <- ggplot( data = the_data,
                       mapping = aes( x = type, y = n ) ) +
    geom_boxplot( width = 0.7, outliers = F, color = "black",
                  size = 0.5, fill = "white" ) +
    scale_y_continuous( labels = the_scale,
                        limits = y_limits ,
                        breaks = the_breaks ) +
    labs( x = x_name,
          y = y_name ) +
    theme_linedraw( base_size = 25 ) +
    theme( panel.border = element_blank( ),
           panel.background = element_blank( ),
           plot.background = element_blank( ),
           plot.title = element_blank( ),
           axis.text.x = element_blank( ),
           axis.ticks.x = element_blank( ),
           panel.grid.major.x = element_blank( ),
           panel.grid.major.y = element_line( linewidth = 0.1 ),
           panel.grid.minor.y = the_panel_minor_y )
  
  # save the plots in pdf
  ggsave( filename = paste0( the_type, ".pdf"),
          plot = the_box.p, width = 3, height = 5 )
  
  # svg
  ggsave( filename = paste0( the_type, ".svg"),
          plot = the_box.p, width = 3, height = 5 )
  
  # do the wave plot
  # plot the individuals as a wave
  x_order <- the_data %>% 
    arrange( n ) %>% 
    pull( id )
  
  # plot points as wave
  the_point.p <- ggplot( data = the_data,
                         mapping = aes( x = id, y = n ) ) +
    geom_point( color = "tomato", size = 0.5, shape = 21, alpha = 0.5 ) +
    scale_x_discrete( limits = x_order, expand = c(0.1,0) ) +
    scale_y_continuous( labels = the_scale,
                        limits = y_limits,
                        breaks = the_breaks ) +
    labs( x = x_name,
          y = y_name ) +
    theme_linedraw( base_size = 25 ) +
    theme( panel.border = element_blank( ),
           panel.background = element_blank( ),
           plot.background = element_blank( ),
           plot.title = element_blank( ),
           axis.text.x = element_blank( ),
           axis.ticks.x = element_blank( ),
           panel.grid.major.x = element_blank( ),
           panel.grid.major.y = element_line( linewidth = 0.1 ),
           panel.grid.minor.y = the_panel_minor_y  )
  
  # save the plots in pdf
  ggsave( filename = paste0( the_type, "_point.pdf"),
          plot = the_point.p, width = 3, height = 5 )
  
  # svg
  ggsave( filename = paste0( the_type, "_point.svg"),
          plot = the_point.p, width = 3, height = 5 )
  
}

# Do SNPs
makebox.f( the_data = data.df, the_type = "SNPs",
           the_scale = myscale.f, y_limits = c( 2.4e6, 4.2e6),
           the_breaks = seq( from = 2.5e6, to = 4.5e6, by = 0.5e6 ),
           y_name = "Million SNV", x_name = "SNVs",
           the_panel_minor_y = element_line( linewidth = 0.1 ) )

# Do indels
makebox.f( the_data = data.df, the_type = "indels",
           the_scale = myscale.f2, y_limits = c( 2.8e5, 1e6),
           the_breaks = seq( from = 3e5, to = 9e5, by = 1e5 ),
           y_name = "Thousand indels", x_name = "Indels",
           the_panel_minor_y = element_blank( ) )

# Do DELS
makebox.f( the_data = data.df, the_type = "DEL",
           the_scale = myscale.f3, y_limits = c( 60, 110),
           the_breaks = seq( from = 60, to = 110, by = 10 ),
           y_name = "Deletions", x_name = "CNV",
           the_panel_minor_y = element_blank( ) )

# Do DUPs
makebox.f( the_data = data.df, the_type = "DUP",
           the_scale = myscale.f3, y_limits = c( 25, 95),
           the_breaks = seq( from = 30, to = 90, by = 10 ),
           y_name = "Duplications", x_name = "CNV",
           the_panel_minor_y = element_blank( ) )

#
# 1. Define the directory name
dir_name <- "results"

# 2. Create the directory (won't fail if it already exists)
dir.create(dir_name, showWarnings = FALSE)

# 3. Identify all .svg and .pdf files in the current working directory
files_to_move <- list.files(pattern = "\\.(svg|pdf)$")

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