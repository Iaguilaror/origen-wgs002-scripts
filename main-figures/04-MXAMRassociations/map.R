# load packages
pacman::p_load( "vroom", "tidyr", "dplyr", "sf", "ggplot2", "scales" )
pacman::p_load( "ggsci", "patchwork" )
pacman::p_load( "rnaturalearth", "rnaturalearthdata" )

# load scater data
scater.df <- vroom( file = "anc_for_scater.csv" )

# scatter plot from dump
myanc_function.f <- function( x ) {
  x * 100
}

# define params
ami_color <- "#ef9faf"     # rosa mexa
low_ami_color <- "#5987e6"

scat_ancs.p <- ggplot( data = scater.df,
                       mapping = aes( x = ami_ancs,
                                      y = eur_ancs,
                                      fill = ami_ancs ) ) +
  geom_point( 
    size = 6,
    alpha = 1,
    shape = 21
  ) +
  geom_abline( intercept = 1, slope = -1, lty = "dashed", size = 2 ) +
  scale_x_continuous( limits = c( 0,1 ), label = myanc_function.f ) +
  scale_y_continuous( limits = c( 0,1 ), label = myanc_function.f ) +
  scale_fill_gradient( low = low_ami_color, high = ami_color ) +
  labs( x = "AMI ancestry %",
        y = "EUR ancestry %" ) +
  theme_classic( base_size = 60 ) +
  theme( panel.grid = element_blank( ),
         plot.background = element_blank( ),
         panel.background = element_blank( ),
         legend.position = "none" )

# vis
scat_ancs.p

# save the scatter
ggsave( filename = "scat_ancs.png", plot = scat_ancs.p, width = 10, height = 10  )
ggsave( filename = "scat_ancs.svg", plot = scat_ancs.p, width = 10, height = 10  )

load( file = "mexico_map.RData" )

# plot ancestries
ami.p <- ggplot( data = mexico_map ) +
  geom_sf( mapping = aes( fill = mean_ami_ancs ), color = "white" ) +
  scale_fill_gradient( low = low_ami_color, high = ami_color,
                       breaks = seq( 0.5, 0.9, by = 0.1 ),
                       # limits = c( 0, 1 ),
                       labels = percent,
                       guide = guide_colorbar( title.position = "top",
                                               frame.colour = "black",
                                               ticks.colour = "black",
                                               barwidth = 20,  # Adjust width of the color bar
                                               barheight = 3 ) ) +
  labs( title = "B - Ancestry by region of birth",
        fill = "Mean Amerindian Ancestry",
        color = "Mean Amerindian Ancestry" ) +
  theme_void( ) +
  theme( 
    # plot.title = element_text( size = 15, hjust = 0.18, face = "bold" ),
    plot.title = element_blank( ),
    legend.position = "bottom",
    # legend.title = element_text( margin = margin( r = 20, b = 20 ) )
    legend.title = element_text( hjust = 0.5, size = 30 ),
    legend.text = element_text( hjust = 0.5, size = 30 )
  )

# vis
ami.p

# save the ami plot
ggsave( filename = "ami.png", ami.p, width = 10, height = 10 )
# save the ami plot
ggsave( filename = "ami.svg", ami.p, width = 10, height = 10 )


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
