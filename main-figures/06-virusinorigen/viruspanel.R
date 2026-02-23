# load pkgs
pacman::p_load( "vroom", "dplyr", "tidyr", "ggplot2", "ggrepel", "viridis", "patchwork" )

summ.df <- vroom( file = "counts_by_virus.tsv" )

# Plot
panel.p <- ggplot( data = summ.df, aes( x = segment, y = total_reads, group = virus_name ) ) +
  # geom_area( fill = "#bfd3f5", alpha = 0.8 ) +  # Area under the curve
  geom_col( fill = "#5987e6", color = "#5987e6", width = 1 ) +
  # geom_step( color = "#5987e6", linewidth = 1 ) +  # Step line on top
  scale_y_continuous( transform = "log10", expand = c( 0, 0 ) ) +
  coord_cartesian( ylim = c( 1, 5e4 ) ) +
  labs( x = "Segment", y = "Total Reads (log10)", title = "Stair Plot of Reads per Segment" ) +
  theme_minimal() +
  theme( axis.text.x = element_text( angle = 45, hjust = 1 ),
         panel.background = element_rect( color = "black" ),
         panel.grid = element_blank( ) ) +
  facet_wrap( ~ virus_name, nrow = 1 )

panel.p

ggsave( filename = "panel.svg", plot = panel.p, width = 10, height = 4  )

# Do the volcano ----
# load data
virus_sup.df <- vroom( file = "suptable_ST4.csv",
                       show_col_types = FALSE, col_names = TRUE ) %>% 
  as_tibble( ) %>% 
  mutate( fishlog = -log10( fisher_pval ) )

# First, identify significative results
p_val_cutoff <- 0.05

# filter
ebv_pass.df <- virus_sup.df %>% 
  filter( fisher_pval < p_val_cutoff ) %>% 
  filter( LogitEBVpres_pval < p_val_cutoff )

# plot the volcano

# Create a small data frame for the text labels
text_labels.df <- data.frame(
  x = rep(0, 5),
  y = 1:5,
  label = paste( 1:5, "-" )
)

volcano.p <- ggplot( data = virus_sup.df,
                     mapping = aes( x = `Dif Cases-Controls`,
                                    y = fishlog ) ) +
  geom_point( color = "black", alpha = 0.8, size = 2 ) +
  geom_point( data = ebv_pass.df, size = 2,
              color = "tomato" ) +
  geom_hline( yintercept = -0.1 ) +
  geom_vline( xintercept = 0 ) +
  geom_text( data = text_labels.df,
             mapping = aes( x = x, y = y, label = label ),
             nudge_x = -0.01 ) +
  # geom_text_repel( data = ebv_pass.df,
  #                  mapping = aes( label = feno ),
  #                  color = "blue" ) +
  scale_x_continuous( limits = c(-0.2, 0.2) ) +
  theme( panel.grid = element_blank( ),
         panel.background = element_blank( ) )

# vis
volcano.p

volcano.p +
  geom_text_repel( data = ebv_pass.df, size = 2,
                   mapping = aes( label = feno ),
                   color = "blue" )

# save individual plots
svgtrans.f <- function( the_plot, the_title, the_width, the_height ){
  
  ggsave( filename = the_title,
          plot = the_plot +
            theme( panel.background = element_blank(),
                   plot.background = element_rect(fill = "transparent", color = NA) ),
          width = the_width, height = the_height )
  
}

# volcano
svgtrans.f( the_plot = volcano.p, the_title = "virus_volcano.svg",
            the_width = 5, the_height = 5  )

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
