# load pkgs

load( file = "gt_af_tables.Rdata"  )

str( tmp_1.ls )

plotaf.f <- function( the_table ) {
  
  the_snp <- the_table$marker %>% unique( )
  
  ggplot( data = the_table,
          mapping = aes( x = del_tag,
                         y = AF,
                         fill = del_tag,
                         label = round( AF, digits = 3 ) ) ) +
    geom_col( ) +
    geom_text( vjust = -0.1 ) +
    scale_y_continuous( limits = c( 0, 1 ) ) +
    scale_x_discrete( limits = rev ) +
    labs( title = the_snp ) +
    theme_linedraw( )
  
  ggsave( filename = paste0( the_snp, "_af.svg" ) )
  
}

plotaf.f( the_table = tmp_1.ls$af )
plotaf.f( the_table = tmp_2.ls$af )
plotaf.f( the_table = tmp_3.ls$af )

plotaf.f( the_table = tmp_4.ls$af )
plotaf.f( the_table = tmp_5.ls$af )
plotaf.f( the_table = tmp_6.ls$af )

# now plot gts

plotgt.f <- function( the_table ) {
  
  the_snp <- the_table$marker %>% unique( )
  
  ggplot( data = the_table,
          mapping = aes( x = del_tag,
                         y = snp_gt2,
                         label = n,
                         fill = n ) ) + 
    geom_tile( color = "gray" ) +
    scale_fill_gradient( low = "white", high = "tomato" ) +
    geom_text( size = 15 ) +
    labs( title = the_snp ) +
    theme_void( base_size = 20 ) +
    theme( legend.position = "none",
           axis.text = element_text( ) )
  
  ggsave( filename = paste0( the_snp, "_gt.svg" ) )
  
}

plotgt.f( the_table = tmp_1.ls$gt )
plotgt.f( the_table = tmp_2.ls$gt )
plotgt.f( the_table = tmp_3.ls$gt )

plotgt.f( the_table = tmp_4.ls$gt )
plotgt.f( the_table = tmp_5.ls$gt )
plotgt.f( the_table = tmp_6.ls$gt )

# plot accuracy

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

