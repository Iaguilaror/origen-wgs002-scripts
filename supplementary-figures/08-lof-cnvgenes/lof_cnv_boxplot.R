pacman::p_load( "vroom", "dplyr", "tidyverse", "ggplot2" )
pacman::p_load( "ggrepel", "ggsci", "patchwork" )

load( file = "lofandcnv.Rdata" )

bp.p <- ggplot( data = plot_df,
                mapping = aes( x = genotype, y = count, fill = genotype ) ) +
  geom_boxplot( width = 0.6, outlier.alpha = 0.3, alpha = 0.5 ) +  # transparent fill
  facet_wrap(~ TYPE, ncol = 1, scales = "free_y") +
  labs(
    x = NULL,
    title = "Het vs Hom LoF counts per TYPE"
  ) +
  scale_fill_manual( values = c( "limegreen", "tomato" ) ) +
  theme_bw(base_size = 20) +
  theme(
    panel.spacing = unit(0.8, "lines"),
    # Transparent backgrounds:
    panel.grid.major.x = element_blank( ),
    panel.background = element_rect(fill = NA, colour = NA),
    plot.background  = element_rect(fill = NA, colour = NA),
    # If you also want transparent facet strips, uncomment the next line:
    # strip.background = element_blank(),
    axis.text.x = element_text(size = 10)
  )

bp.p

ggsave(plot = bp.p,
       filename = "het_vs_homlof.png",
       bg = "transparent",
       width = 7, height = 14, dpi = 300 )

ggsave(plot = bp.p,
       filename = "het_vs_homlof.svg",
       bg = "transparent",
       width = 7, height = 14, dpi = 300 )

# commmon params
point_s <- 2

# seed
set.seed(100)

# plot lof.pLI y ~ condition
violin.p <- ggplot( data = cnv_pli.df,
                    mapping = aes( x = condition,
                                   fill = condition ,
                                   y = lof.pLI ) ) +
  geom_violin( color = NA,
               alpha = 0.2 ) +
  # geom_boxplot( outliers = FALSE,
  #               # color = NA,
  #               alpha = 0.2 ) +
  geom_jitter( size = point_s,
               shape = 21,
               color = "black",
               fill = "white",
               alpha = 1,
               width = 0.1 ) +
  scale_fill_simpsons( ) +
  # geom_text_repel( data = cnv_pli.df %>% filter( lof.pLI > 0.5 ),
  #                  mapping = aes( label = genes ) ) +
  theme_classic( base_size = 20 )

# vis
violin.p

ggsave( filename = "ploI_violin.svg", plot = violin.p, width = 10, height =  7 )

v_name.p <- violin.p +
  geom_text_repel( data = cnv_pli.df %>% filter( lof.pLI > 0.5 ),
                   mapping = aes( label = genes, color = condition ) ) +
  scale_color_simpsons( )

ggsave( filename = "haplo_box_names.svg",
        plot = v_name.p, width = 18, height =  18 )

vroom_write( x = cnv_pli.df, file = "cnv_pli.tsv" )

# plot phaplo and ptripo y ~ condition
haplo.p <- ggplot( data = allann.df,
                   mapping = aes( x = condition,
                                  y = probability,
                                  fill = condition ) ) +
  # geom_boxplot( outliers = FALSE,
  #               # color = NA,
  #               alpha = 0.2 ) +
  geom_violin( color = NA,
               alpha = 0.2 ) +
  geom_jitter( size = point_s,
               shape = 21,
               color = "black",
               fill = "white",
               alpha = 1,
               width = 0.1 ) +
  # geom_text_repel( data = allann.df %>% filter( probability > 0.5 ),
  #                  mapping = aes( label = genes ) ) +
  scale_fill_simpsons( ) +
  theme_classic( base_size = 20 ) +
  facet_wrap( ~type, ncol = 1 )

# vis
haplo.p

ggsave( filename = "haplo_box_notext.svg",
        plot = haplo.p, width = 18, height =  18 )

name.p <- haplo.p +
  geom_text_repel( data = allann.df %>% filter( probability > 0.5 ),
                   mapping = aes( label = genes, color = condition ) ) +
  scale_color_simpsons( )

ggsave( filename = "haplo_box.svg",
        plot = name.p, width = 18, height =  18 )

# make a quick panel

the_panel.p <- (violin.p / haplo.p) +
  plot_layout(heights = c(0.33, 0.66)) &
  theme(legend.position = "none", axis.title = element_blank( ) )

# vis
the_panel.p

ggsave( filename = "panel.svg", plot = the_panel.p, width = 7, height = 14  )


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


