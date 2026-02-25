# load pkgs
pacman::p_load( "vroom", "dplyr", "ggplot2", "ggsci" )

# load data
novel.df <- vroom( file = "novel_by_freq.tsv" )

tile.p <- ggplot( data = novel.df,
                  mapping = aes( x = database,
                                 y = freq,
                                 label = prettyNum( n, big.mark = ",") ,
                                 fill = freq ) ) +
  geom_tile( color = "white", alpha = 0.8 ) +
  geom_text( ) +
  scale_y_discrete( limits = rev ) +
  scale_fill_futurama( ) +
  theme_void( ) +
  theme( legend.position = "none",
         axis.text = element_text( ) )

ggsave( filename = "novel_tiles.png", plot = tile.p,
        bg = "white",
        width = 7, height = 7 )

# plot QC in novel
# save( all_snv_long.df, all_snv_gq_long.df, all_snv_alt_depth.df, file = "snv_qc.Rdata" )
load( file = "snv_qc.Rdata" )

# plot
snv_INFO_panel.p <- ggplot( data = all_snv_long.df,
                            mapping = aes( x = tag, y = QC_value,
                                           fill = tag,
                                           color = tag ) ) +
  geom_violin(  ) +
  # geom_boxplot( outliers = FALSE, width = 0.5 ) +
  scale_color_futurama( ) +
  scale_fill_futurama( ) +
  labs( x = "only chr1, ~1,300 novel snps, and 1,300 random known" ) +
  theme_linedraw( base_size = 20 ) +
  facet_wrap( ~QC_name, scales = "free_y" ) +
  theme( legend.position = "none",
         panel.grid.major.x = element_blank(  ),
         strip.text = element_text( color = "black" ),
         strip.background = element_rect( fill = "white",
                                          color = NA ) )

# vis
snv_INFO_panel.p

ggsave( filename = "snv_INFO_panel.svg",
        plot = snv_INFO_panel.p, width = 10, height = 7 )

snv_gq_panel.p <- ggplot( data = all_snv_gq_long.df,
                          mapping = aes( x = tag, y = gq,
                                         fill = tag,
                                         color = tag ) ) +
  geom_violin(  ) +
  # geom_boxplot( outliers = FALSE, width = 0.5 ) +
  scale_color_futurama( ) +
  scale_fill_futurama( ) +
  labs( y = "genotype quality by sample",
        x = "only chr1, ~1,300 novel snps, and 1,300 random known" ) +
  theme_linedraw( base_size = 20 ) +
  theme( legend.position = "none",
         panel.grid.major.x = element_blank(  ),
         strip.text = element_text( color = "black" ),
         strip.background = element_rect( fill = "white",
                                          color = NA ) )

# vis
snv_gq_panel.p

ggsave( filename = "snv_gq_panel.svg",
        plot = snv_gq_panel.p, width = 10, height = 7 )

snv_ad_panel.p <- ggplot( data = all_snv_alt_depth.df,
                          mapping = aes( x = tag, y = alt_AD,
                                         fill = tag,
                                         color = tag ) ) +
  geom_violin(  ) +
  scale_color_futurama( ) +
  scale_fill_futurama( ) +
  labs( 
    # y = "ALT/REF depth ratio by sample",
    x = "only chr1, ~1,300 novel snps, and 1,300 random known",
    caption = "only counted samples with at least 1 alt called" ) +
  theme_linedraw( base_size = 20 ) +
  theme( legend.position = "none",
         panel.grid.major.x = element_blank(  ),
         strip.text = element_text( color = "black" ),
         strip.background = element_rect( fill = "white",
                                          color = NA ) )
# vis
snv_ad_panel.p

ggsave( filename = "snv_ad_panel.svg",
        plot = snv_ad_panel.p, width = 10, height = 7 )

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

