# load pkgs
pacman::p_load( "vroom", "tidyverse", "scales",
                "ggrepel", "patchwork" )

# a functon to label kbp
tokbp.f <- function( the_n ){
  new_n <- the_n / 1e3
  new_n <- paste0( prettyNum( new_n, big.mark = "," ), "kb")
  return( new_n )
}

ld_tocenter.df <- vroom( file = "ld_torhd.tsv" )

genes_record.df <- vroom( file = "genes_near_rhd.tsv" )

# lts plot LD from center
snvs_ld.p <- ggplot( mapping = aes( x = distance_to_center_of_rhd,
                                    y = r2 ) ) +
  geom_hline( yintercept = c( 0.1, 0.25, 0.5, 0.75 ), lty = 2, alpha = 0.2 ) +
  geom_point( data = ld_tocenter.df %>% filter( r2 <= 0.8 ),
              alpha = 0.3, color = "black", size = 2 ) +
  geom_point( data = ld_tocenter.df %>% filter( r2 > 0.8 ),
              color = "red", size = 2 ) +
  scale_x_continuous( limits = c( -550e3, 550e3 ),
                      breaks = seq( from = -550e3,
                                    to = 550e3,
                                    by = 50e3 ),
                      labels = tokbp.f ) +
  labs( title = "SNVs in LD with RHD Deletion",
        subtitle = "each dot is a SNV in oriGen-1318",
        x = "Distance to center of RHD gene",
        y = "Correlation with RHD- (LD by r2)" ) +
  coord_cartesian( xlim = c( -250e3, 250e3 ) ) +
  theme_classic( base_size = 20 ) +
  theme( axis.text.x = element_text( angle = 90, hjust = 1, vjust = 0.5 ) )

# see with labels
snvs_ld_tx.p <- snvs_ld.p +
  geom_text_repel( data = ld_tocenter.df %>% 
                     filter( r2 > 0.8  ),
                   mapping = aes( label = id ) )

snvs_ld_tx.p

# Assuming your data frame is named df
genes.p <- ggplot( data = genes_record.df,
                   mapping = aes( xmin = first_exon_start,
                                  xmax = last_exon_end ) ) +
  # Create the rectangles
  geom_rect(ymin = 0, ymax = 1,
            fill = "darkblue",
            color = "black",
            alpha = 0.3 ) +
  scale_y_continuous( limits = c( -3, 2 ) ) +
  scale_x_continuous( limits = c( -550e3, 550e3 ),
                      breaks = seq( from = -550e3,
                                    to = 550e3,
                                    by = 50e3 ),
                      labels = tokbp.f ) +
  theme_void( ) +
  geom_text( mapping = aes( x = mid,
                            y = -1, label = gene ),
             angle = 90, hjust = 1, nudge_y = -0.01 ) +
  coord_cartesian( xlim = c( -250e3, 250e3 ) )

# patch panel
ld_panel.p <- ( snvs_ld.p + theme( axis.title.x = element_blank( ) ) ) /
  genes.p +
  plot_layout( guides = "collect" ) +
  theme( plot.margin = margin( 5.5, 5.5, 5.5, 5.5 ) )

ld_panel.p

# save the plots
ggsave( plot = ld_panel.p,
        filename = "ld_panel.svg",
        width = 7, height = 7 )

ggsave( plot = snvs_ld_tx.p,
        filename = "ld_panel_tx.svg",
        width = 7, height = 7 )

ggsave( plot = ld_panel.p,
        filename = "ld_panel.png",
        width = 7, height = 7 )

ggsave( plot = snvs_ld_tx.p,
        filename = "ld_panel_tx.png",
        width = 7, height = 7 )

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
