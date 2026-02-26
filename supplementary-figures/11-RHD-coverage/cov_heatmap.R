# Load pkgs
source("https://www.dropbox.com/s/kgszl45a85fqqj4/R2LS.r?dl=1")

pacman::p_load( "vroom", "dplyr", "ggplot2","stringr", "tidyr", "scales", "patchwork", "umap" )

load( file = "rhdheat.Rdata" )

png("RHD_heatmapcnv.png", width = 14, height = 5, units = "in", res = 600)

plot.heatmap.info( 2 * ( rhdok ),
                   Rowv = NULL,
                   ColvFactor = 1,
                   breaks = seq( 0, 6, len = 33 ),
                   margins = c( 2, 10 ),
                   col = redgreenblue( c( "darkred", "white", "darkgreen", "darkcyan" ),
                                       len = 32 ),
                   ColSideFactors = all_ann.df,
                   ColSideFactors.col = list( AMR = redgreenblue( c( "#1f78b4", "#a6cee3", "#b2df8a" ), len = 3 ),
                                              tag = c( "#d61fbf", "orange", "darkolivegreen" ) ),
                   ColSideFactors.block = 1,
                   ColSideFactors.order.function = order.by.umap
)

dev.off()

# do hist
# for RHD
color.v <- c( rep( "orange", 19 ),
              rep( "#d61fbf", 23 ),
              rep( "darkolivegreen", 50 ),
              rep( "black", 30 )
)

png("RHD_histogram.png", width = 10, height = 5, units = "in", res = 600)

hist(gene_long2.df$gene_mean_cov * 2, breaks = seq(0, 3.5, len=100),
     col = color.v ,
     border = 1 )

dev.off( )

rm( list = ls() )

# Do pdxdc1
source("https://www.dropbox.com/s/kgszl45a85fqqj4/R2LS.r?dl=1")

load( file = "pdxdc1heat.Rdata" )

png("PDX_heatmapcnv.png", width = 14, height = 5, units = "in", res = 600)

plot.heatmap.info( 2 * ( rhdok ),
                   Rowv = NULL,
                   ColvFactor = 1,
                   breaks = seq( 0, 6, len = 33 ),
                   margins = c( 2, 10 ),
                   col = redgreenblue( c( "darkred", "white", "darkgreen", "darkcyan" ),
                                       len = 32 ),
                   ColSideFactors = all_ann.df %>% select( -tag ),
                   ColSideFactors.col = list( AMR = redgreenblue( c( "#1f78b4", "#a6cee3", "#b2df8a" ), len = 3 ) ),
                   ColSideFactors.block = 1,
                   ColSideFactors.order.function = order.by.umap
)

dev.off()

# do hist
# for PDX
color.v <- c( rep( "orange", 22 ),
              rep( "#d61fbf", 20 ),
              rep( "darkolivegreen", 50 ),
              rep( "black", 30 )
)

png("PDX_histogram.png", width = 10, height = 5, units = "in", res = 600)

hist(gene_long2.df$gene_mean_cov * 2,
     breaks = seq(1.5, 5, len = 100),
     col = color.v ,
     border = 1 )

dev.off( )

# Finally LCE

rm( list = ls() )

# Do pdxdc1
source("https://www.dropbox.com/s/kgszl45a85fqqj4/R2LS.r?dl=1")

load( file = "LCE3E_heat.Rdata" )

png("LCE_heatmapcnv.png", width = 14, height = 5, units = "in", res = 600)

plot.heatmap.info( 2 * ( rhdok ),
                   Rowv = NULL,
                   ColvFactor = 1,
                   breaks = seq( 0, 6, len = 33 ),
                   margins = c( 2, 10 ),
                   col = redgreenblue( c( "darkred", "white", "darkgreen", "darkcyan" ),
                                       len = 32 ),
                   ColSideFactors = all_ann.df %>% select( -tag ),
                   ColSideFactors.col = list( AMR = redgreenblue( c( "#1f78b4", "#a6cee3", "#b2df8a" ), len = 3 ) ),
                   ColSideFactors.block = 1,
                   ColSideFactors.order.function = order.by.umap
)

dev.off()

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

