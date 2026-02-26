# load packages
pacman::p_load( "vroom", "tidyr", "dplyr", "ggplot2", "pheatmap", "grid",
                "scales", "cowplot", "patchwork", "stringr", "ggExtra" )

load( file = "heat_data.Rdata" )

# function to repeat plot
dopheat.f <- function( ) {
  
  # Generate the heatmap"red1"
  pheatmap(cnv_matrix,
           # color = discrete_colors,
           legend = FALSE,
           # legend_breaks = 0:5,
           # legend_labels = c("0 CN", "< 2 CN", "Normal CN", "<= 6 CN", "<= 10 CN", "> 10 CN" ),
           annotation_col = my_col_anno,
           annotation_colors = my_colour,
           annotation_legend = FALSE,
           cluster_rows = TRUE,
           treeheight_row = 0,
           cluster_cols = FALSE,
           # main = paste( "WGS - CopyNumber dosage for genes in events with at least",
           #               percent(origen_cut) , "of the oriGen population",
           #               "\n protein coding" ),
           show_rownames = FALSE,
           show_colnames = TRUE,
           fontsize_col = 1 )
  
}

svg( filename = "heatmap_allchroms.svg",
     width = 100, height = 100 )

dopheat.f()

dev.off( )

png( filename = "heatmap_allchroms.png",
     width = 21, height = 10, units = "in", res = 600 )

dopheat.f()

dev.off( )

# plot the manhattan
# load pkgs
pacman::p_load( "vroom", "dplyr", "ggplot2", "tidyr",
                "stringr", "scales", "patchwork", "ggpubr", "qqman", "ggrepel" )

load( file = "manhattan.Rdata" )

manhattan.p <- ggplot( mapping = aes( x = BP, y = logP ) ) +
  # geom_hline( yintercept = -log10( 1e-5 ),
  #             linetype = "dashed", color = "red", alpha = 0.2 ) +
  geom_point( data = manhattan_data,
              alpha = 0.7, size = 2,
              color = "gray" ) +
  geom_point( data = manhattan_data %>% 
                filter( highlight ),
              size = 2,
              color = "black" ) +
  scale_x_continuous( expand = c(0.2,0.2) ) +
  scale_y_continuous( breaks = 0:15 ) +
  facet_wrap( ~ CHR, scales = "free_x", nrow = 1, strip.position = "bottom" ) +
  theme_classic( base_size = 14 ) +
  labs( x = "Genomic position", y = expression(-log[10]( q ) ),
        title = "Manhattan plot (q < 0.01 labeled)", ) +
  theme( legend.position = "none",
         axis.text.x = element_blank( ),
         strip.placement = "outside",
         strip.background = element_blank(),
         strip.text = element_text(size = 14),
         panel.background = element_rect( fill = "transparent", color = "transparent" ),
         plot.background = element_rect( fill = "transparent", color = "transparent" )
  )

# vis
man_text.p <- manhattan.p +
  geom_text_repel(
    data = filter( manhattan_data, highlight ),
    mapping = aes( label = SNP ),
    size = 3, max.overlaps = Inf,
  )

# vis
man_text.p

# save the plots
ggsave( filename = "manhattan.svg", plot = manhattan.p,
        width = 21, height = 7 )

ggsave( filename = "manhattan.png", plot = manhattan.p,
        width = 21, height = 7 )

ggsave( filename = "manhattan_text.svg", plot = man_text.p,
        width = 21, height = 7 )

ggsave( filename = "small_manhattan.svg", plot = manhattan.p,
        width = 14*0.7, height = 3.5 )

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
