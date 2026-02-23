# load pkg
pacman::p_load( "vroom", "dplyr", "tidyr",
                "stringr", "ggplot2",
                "patchwork" )

# read data
pca_clean2.df <- vroom( file = "pca_coord_compare.tsv" )

pca_clean2.df$tag %>% 
  unique( )

pca_clean2.df$run_clean %>% 
  unique( )

pca_clean2.df <- pca_clean2.df %>% 
  mutate( run_clean = factor( run_clean, levels = c( "0_rand_intrain",
                                                     "500_rand_intrain",
                                                     "1318_rand_intrain"  ) ) )

# prepare colors
mycolors.v <- c( "AFR" = "#5987e5",
                 "AMR" = "#f09daf",
                 "EAS" = "#94c859",
                 "EUR" = "#ff5b42",
                 "ORI" = "#876925",
                 "SAS" = "#cc4c4d" )

# Do plot PCA

pc1_pc2.p <- ggplot( mapping = aes( x = PC1, y = PC2,
                                    color = tag ) ) +
  geom_point( data = pca_clean2.df %>% filter( tag != "SAS" & tag != "EAS"  ),
              size = 0.3 ) +
  geom_point( data = pca_clean2.df %>% filter( tag == "SAS" | tag == "EAS"  ),
              size = 0.3 ) +
  geom_hline( yintercept = 0, lty = "dashed", alpha = 0.3 ) +
  geom_vline( xintercept = 0, lty = "dashed", alpha = 0.3 ) +
  scale_color_manual( values = mycolors.v ) +
  theme_linedraw( ) +
  theme( panel.grid = element_blank( ),
         legend.position = "none",
         # strip.background = element_rect( color = "black", fill = "white" ),
         # strip.text = element_text( color = "black" ) 
         # strip.background = element_blank( ),
         # strip.text = element_blank( ),
         # axis.title = element_blank(),
         # axis.text = element_blank(),
         axis.ticks.length = unit( 20, "pt" )
  ) +
  facet_wrap( ~run_clean, ncol = 1, strip.position = "right" )

pc3_pc2.p <- ggplot( mapping = aes( x = PC3, y = PC2,
                                    color = tag ) ) +
  geom_point( data = pca_clean2.df,
              size = 0.3 ) +
  geom_hline( yintercept = 0, lty = "dashed", alpha = 0.3 ) +
  geom_vline( xintercept = 0, lty = "dashed", alpha = 0.3 ) +
  scale_color_manual( values = mycolors.v ) +
  theme_linedraw( ) +
  theme( panel.grid = element_blank( ),
         legend.position = "none",
         # strip.background = element_rect( color = "black", fill = "white" ),
         # strip.text = element_text( color = "black" )
         # strip.background = element_blank( ),
         # strip.text = element_blank( ),
         # axis.title.y = element_blank( ),
         # axis.text.y = element_blank( ),
         # axis.title = element_blank(),
         # axis.text = element_blank(),
         axis.ticks.length = unit( 20, "pt" )
  ) +
  facet_wrap( ~run_clean, ncol = 1, strip.position = "right", scales = "free_x" )

pca_panel.p <- pc1_pc2.p + pc3_pc2.p

ggsave( filename = "pca_panel.png",
        plot = pca_panel.p,
        width = 14, height = 21 )

ggsave( filename = "pca_panel.svg",
        plot = pca_panel.p,
        width = 14, height = 21 )

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