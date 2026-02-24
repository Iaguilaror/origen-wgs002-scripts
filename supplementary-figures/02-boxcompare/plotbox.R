pacman::p_load( "openxlsx", "stringr", "dplyr", "tidyr", "ggplot2", "scales", "patchwork" )

load( file = "forboxplots.RData" )

mycolors.v <- c( "AFR" = "#fde524", "EUR" = "#25878c", "AMR" = "#91d73c",
                 "asian" = "gray" )

# create a plot function
mybox.f <- function( the_data ) {
  
  data_name <- deparse( substitute( the_data ) )  # capture the name of the object
  
  the_data %>% 
    mutate( asian = SAS + EAS ) %>% 
    pivot_longer( data = ., cols = 3:8,
                  names_to = "pop", values_to = "anc" ) %>% 
    filter( pop != "EAS" & pop != "SAS"  ) %>%
    ggplot( mapping = aes( x = pop, y = anc, fill = pop ) ) +
    geom_boxplot( outlier.shape = 21, outlier.alpha = 0.3,
                  size = 0.5, width = 0.5 ) +
    scale_x_discrete( limits = c( "asian", "AFR", "EUR", "AMR" ) ) +
    scale_y_continuous( limits = c( 0, 1 ),
                        breaks = seq( 0, 1, 0.1 ),
                        labels = percent )  +
    scale_fill_manual( values =  mycolors.v,
                       limits = c( "asian","AFR", "EUR", "AMR" ),
                       labels = c( "asian", "African", "European", "Indigenous\nAmerican" )
    ) +
    labs( caption = data_name,
          y = "Ancestry" )  +
    theme_linedraw( base_size = 20 ) +
    theme( axis.title.x = element_blank( ),
           axis.text.x = element_blank( ),
           axis.ticks.x = element_blank( ),
           # legend.position = "bottom",
           panel.grid.minor = element_blank( ),
           panel.grid.major.x = element_blank( ),
           legend.text = element_blank( ),
           legend.title = element_blank( ) )
  
}

# test
plot1 <- mybox.f( the_data = ADMX.K5.0Training.ALL.Proyectado.df )
plot2 <- mybox.f( the_data = ADMX.K5.500Training.818.Proyectados.df )
plot3 <- mybox.f( the_data = ADMX.K5.ALLTraining.No.Proyectado.df )

the_panel <- plot1 / plot2 / plot3

ggsave( filename = "the_panel.png", plot = the_panel, width = 7, height = 21 )
ggsave( filename = "the_panel.svg", plot = the_panel, width = 7, height = 21 )

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

