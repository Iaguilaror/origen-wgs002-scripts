pacman::p_load( "ggplot2", "cowplot", "tidyr", "dplyr", "scales" )

#####

# define my theme
mytheme <- theme( panel.grid.major.x = element_blank( ),
                  panel.grid.minor.x = element_blank( ),
                  panel.grid.major.y = element_line( linewidth = 1 ),
                  legend.position = "none",
                  panel.background = element_rect( fill = NA,
                                                   colour = NA ),
                  plot.background = element_rect( fill = NA,
                                                  colour = NA ),
                  panel.border = element_blank( ),
                  axis.title.x = element_blank( ),
                  axis.ticks.x = element_blank( ),
                  axis.text.x = element_blank( ),
                  plot.title = element_text( hjust = 0.5 ) )

plot_david.f <- function( the_file1, the_file2, the_ofile, the_axis ) {
  
  file1 <- the_file1
  
  file2 <- the_file2
  
  p1 <- readRDS( file = file1 )
  
  p2 <- readRDS( file = file2 )
  
  # get the data
  # Extract data from the ggplot object
  p1_data <- ggplot_build( p1 )$data[[1]]
  p2_data <- ggplot_build( p2 )$data[[1]]
  
  # all data
  all_data <- dplyr::bind_rows( p1_data, p2_data )
  
  # get min and mx y
  miny_val <- min( all_data$ymin )
  miny_rounded <- floor( miny_val / 10 ) * 10
  
  maxy_val <- max( all_data$ymax )
  maxy_rounded <- ceiling( maxy_val / 10 ) * 10
  
  # detect the guide lines from data 1
  min_guide <- min( p1_data$middle ) -1
  
  max_guide <- max( p1_data$middle ) +1
  
  # my scale func
  dis2percent.f <- function( x ){
    
    as.numeric(x) %>% percent()
    
  }
  
  # format plots
  p1 <- p1 +
    geom_hline( yintercept = c( min_guide, max_guide ), lty = "dashed", size = 0.5 ) +
    geom_boxplot( outliers = FALSE, fill = "#5987e6" ) +
    scale_y_continuous( limits = c( miny_rounded, maxy_rounded  ) ) +
    ggtitle( "Calibrating HCHS/SOL (n=8982)" ) +
    labs( x = "PGS" ) +
    scale_x_discrete( labels = dis2percent.f  ) +
    theme_light( base_size = 10 ) +
    mytheme
  
  p2 <- p2 +
    geom_hline( yintercept = c( min_guide, max_guide ), lty = "dashed", size = 0.5 ) +
    geom_boxplot( outliers = FALSE, fill = "#5987e6" ) +
    scale_y_continuous( limits = c( miny_rounded, maxy_rounded  ) ) +
    ggtitle( "oriGen (n=1427)" ) +
    labs( x = "PGS" ) +
    scale_x_discrete( labels = dis2percent.f  ) +
    theme_light( base_size = 10 ) +
    mytheme
  
  # make panel
  the_panel <- cowplot::plot_grid( p1,
                                   p2 +
                                     theme( axis.title.y = element_blank( ),
                                            axis.ticks.y = element_blank( ),
                                            axis.text.y = element_blank( ) ),
                                   nrow = 1 )
  
  # save the panel
  ggsave( filename = the_ofile,
          plot = the_panel,
          width = 13, height = 5 )
  
  return( the_panel )
  
}

plot_david.f( the_file1 = "results--TG--EXT_c000810_2--EXT_PRScsx.pdf_2(1).rds",
              the_file2 = "results--TG--EXT_c000810_2--EXT_PRScsx.pdf_1(1).rds",
              the_ofile = "TG_panel.svg" )

plot_david.f( the_file1 = "results--TCHOL--EXT_c000810_2--EXT_PRScsx.pdf_2(1).rds",
              the_file2 = "results--TCHOL--EXT_c000810_2--EXT_PRScsx.pdf_1(1).rds",
              the_ofile = "TCHOL_panel.svg" )

plot_david.f( the_file1 = "results--HDL--EXT_c000810_2--EXT_PRScsx.pdf_2(1).rds",
              the_file2 = "results--HDL--EXT_c000810_2--EXT_PRScsx.pdf_1(1).rds",
              the_ofile = "HDL_panel.svg" )

plot_david.f( the_file1 = "results--LDL--EXT_c000810_2--EXT_PRScsx.pdf_2(1).rds",
              the_file2 = "results--LDL--EXT_c000810_2--EXT_PRScsx.pdf_1(1).rds",
              the_ofile = "LDL_panel.svg" )





# do for T2D
barras1.p <- readRDS( file = "results--T2D--EXT_c000810_2--EXT_PRScsx.pdf_2(1).rds" ) +
  geom_col( fill = "#5987e6" ) +
  scale_y_continuous( limits = c( 0, 0.5 ) ) +
  ggtitle( "Calibrating HCHS/SOL (n=8993)" ) +
  labs( x = "PGS" ) +
  theme_light( base_size = 10 ) +
  mytheme

barras2.p <- readRDS( file = "results--T2D--EXT_c000810_2--EXT_PRScsx.pdf_1(1).rds" ) +
  geom_col( fill = "#5987e6" ) +
  scale_y_continuous( limits = c( 0, 0.5 ) ) +
  ggtitle( "oriGen (n=1300)" ) +
  labs( x = "PGS" ) +
  theme_light( base_size = 10 ) +
  mytheme

# make panel
the_panel <- cowplot::plot_grid( barras1.p,
                                 barras2.p + theme( axis.title.y = element_blank( ),
                                                    axis.ticks.y = element_blank( ),
                                                    axis.text.y = element_blank( ) ),
                                 nrow = 1 )

# save the panel
ggsave( filename = "T2D_panel.svg",
        plot = the_panel,
        width = 13, height = 5 )

# Save everything and og home
# 1. Create the directory if it doesn't exist
dir.create( path = "results", showWarnings = FALSE )

# 2. Identify the files to move
files_to_move <- list.files( path = ".", 
                             pattern = "\\.(png|svg)$", 
                             full.names = TRUE )

# 3. Move the files
# file.rename acts as a 'move' command in Ubuntu/R
file.rename( from = files_to_move, 
             to = file.path( "results", basename( files_to_move ) ) )