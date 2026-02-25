# load packages
pacman::p_load( "vroom", "tidyr", "dplyr", "ggplot2",
                "patchwork", "ggvenn", "scales" )

cnv.df <- vroom( file = "cnvqcmatrix.tsv" )

#plots
svlen.qc <- ggplot( data = cnv.df,
                    mapping = aes( x = svlen,
                                   fill = alt,
                                   color = alt ) ) +
  geom_histogram( binwidth = 1e3, color = NA ) +
  scale_x_continuous( label = comma,
                      breaks = seq(0, 100e3, 10e3  ) ) +
  scale_color_manual( values = c( "tomato", "skyblue" ) ) +
  scale_fill_manual( values = c( "tomato", "skyblue" ) ) +
  labs( title = "Range of sizes in CNVs",
        x = "svlen (basepairs)" ) +
  theme_classic( base_size = 20 ) +
  facet_wrap( ~ alt, scales = "free" ) +
  coord_cartesian( xlim = c( 0, 1e5 ) ) +
  theme( axis.text.x = element_text( angle = 90,
                                     hjust = 0.5, vjust = 0.5 ) )

svlen.qc

ggsave( filename = "svlen.png", plot = svlen.qc,
        width = 14, height = 7 )

# plot q0 not uniquely mapped reads
q0.qc <- ggplot( data = cnv.df,
                 mapping = aes( x = q0,
                                fill = alt,
                                color = alt ) ) +
  geom_histogram( binwidth = 1e-2, color = NA ) +
  labs( title = "Range of Fracc of not uniquely mapped reads in CNVs",
        x = "q0 ( fracction of not uniquely mapped reads )" ) +
  scale_color_manual( values = c( "tomato", "skyblue" ) ) +
  scale_fill_manual( values = c( "tomato", "skyblue" ) ) +
  theme_classic( base_size = 20 ) +
  facet_wrap( ~ alt, scales = "free" ) +
  coord_cartesian( xlim = c( 0, 0.5 ) ) 

q0.qc

ggsave( filename = "q0.png", plot = q0.qc,
        width = 14, height = 7 )

# plot dG distance to gap
dG.qc <- ggplot( data = cnv.df,
                 mapping = aes( x = log10( dG ),
                                fill = alt,
                                color = alt ) ) +
  geom_histogram( bins =  100,
                  color = NA ) +
  scale_x_continuous( label = comma ) +
  labs( title = "Range of Distance to Gaps in Genome in CNVs",
        x = "dG ( Distance to gap in ref Genome )" ) +
  scale_color_manual( values = c( "tomato", "skyblue" ) ) +
  scale_fill_manual( values = c( "tomato", "skyblue" ) ) +
  theme_classic( base_size = 20 ) +
  facet_wrap( ~ alt, scales = "free" )

dG.qc

ggsave( filename = "dG.png", plot = dG.qc,
        width = 14, height = 7 )

#plot ordered samples
bysample.df <- vroom( file = "sumbysamples.tsv" )

# sort 
ordered.p <- ggplot( data = bysample.df,
                     mapping = aes( x = ord_sample,
                                    y = n,
                                    fill = svtype,
                                    color = svtype ) ) +
  geom_col( ) +
  scale_color_manual( values = c( "tomato", "skyblue" ) ) +
  scale_fill_manual( values = c( "tomato", "skyblue" ) ) +
  facet_wrap( ~svtype, scales = "free_x" ) +
  theme_linedraw( )


ggsave( filename = "bysample.png", plot = ordered.p,
        width = 14, height = 7 )


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

