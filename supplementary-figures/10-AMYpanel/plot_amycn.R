# load packages
pacman::p_load( "vroom", "tidyverse", "ggplot2" )
source("https://www.dropbox.com/s/kgszl45a85fqqj4/R2LS.r?dl=1")

# load data for histos
load( file = "amy2.Rdata" )
load( file = "amy.Rdata" )

exonlong.df <- exonlong.df %>% 
  mutate( gene_name = factor( gene_name, levels = c( "RNPC3", "AMY2B", "AMY2A", "AMY1A",
                                                     "AMY1B", "AMY1C", "NTNG1" ) ) )

amyrange.p <- ggplot( data = exonlong.df,
        mapping = aes( x = exoncov * 2 ) ) +
  geom_histogram( fill = "skyblue", color = "black",
                  bins = 60 ) +
  facet_wrap( ~gene_name, ncol = 1,
              # scales = "free_y", 
              strip.position = "right",
              ) +
  labs( x = "CN est." ) +
  theme_void( ) +
  theme( axis.title = element_text( ),
         axis.ticks = element_line( ),
         axis.text = element_text( ),
         text = element_text( size = 20 ) )

ggsave( filename = "amy_range.svg", plot = amyrange.p, width = 10, height = 15 )

# now heatmap
png("AMY_heatmapcnv.png", width = 14, height = 5, units = "in", res = 600)

plot.heatmap.info( 2 * ( amysok ),
                   Rowv = NULL,
                   breaks = seq( 0, 6, len = 33 ),
                   margins = c( 2, 10 ),
                   col = redgreenblue( c( "darkred", "white", "darkgreen", "darkcyan" ),
                                       len = 32 )
)

dev.off()

# summarise all amy
rm( list = ls( ) )

load( file = "sumamy.Rdata"  )

curve.p <- ggplot( data = sorted.df,
                   mapping = aes( x = amy_totalCN,
                                  y = cum_perc,
                                  group = 1 ) ) +
  geom_line( color = "darkblue" ) +
  scale_x_continuous( limits = c( 0, 26 ),
                      breaks = seq( from = 0, to = 24, by = 1 ) ) +
  scale_y_continuous( labels = percent,
                      breaks = seq( from = 0, to = 1, by = 0.2 ) ,
                      position = "right" ) +
  theme_classic( base_size = 15 ) +
  theme( plot.background = element_rect( fill = NA, color = NA ),
         panel.background = element_rect( fill = NA, color = NA ),
         panel.grid.major = element_line( color = "gray90" ),
         panel.grid.minor.y = element_line( color = "gray90" ) )

curve.p

histo.p <- ggplot( data = sorted.df,
                   mapping = aes( x = amy_totalCN ) ) +
  geom_histogram( binwidth = 0.5, fill = NA, color = "black" ) +
  scale_x_continuous( limits = c( 0, 26 ),
                      breaks = seq( from = 0, to = 24, by = 1 ) ) +
  theme_classic( base_size = 15 ) +
  theme( plot.background = element_rect( fill = NA, color = NA ),
         panel.background = element_rect( fill = NA, color = NA ) )

histo.p

ggsave( filename = "histo.png", plot = histo.p, width = 7, height = 7  )
ggsave( filename = "curve.png", plot = curve.p, width = 7, height = 7  )

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

