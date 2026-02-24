# load pkg
pacman::p_load( "vroom", "dplyr", "tidyr",
                "stringr", "ggplot2",
                "patchwork" )

# read data
pca_clean2.df <- vroom( file = "allpca_runs.tsv" )

pca_clean2.df$tag %>% 
  unique( )

pca_clean2.df$run_clean %>% 
  unique( )

newlvls <- paste0( c(0,100,200,300,400,500,
                     600,800,900,1000,1200,1318),
                   "_rand_intrain" )

pca_clean2.df <- pca_clean2.df %>% 
  mutate( run_clean = factor( run_clean, levels = newlvls ) )

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
  # theme( panel.grid = element_blank( ),
  #        legend.position = "none",
  #        # strip.background = element_rect( color = "black", fill = "white" ),
  #        # strip.text = element_text( color = "black" ) 
  #        # strip.background = element_blank( ),
  #        # strip.text = element_blank( ),
  #        # axis.title = element_blank(),
  #        # axis.text = element_blank(),
  #        axis.ticks.length = unit( 20, "pt" )
  # ) +
  facet_wrap( ~run_clean, ncol = 1, strip.position = "right" )

pc3_pc2.p <- ggplot( mapping = aes( x = PC3, y = PC2,
                                    color = tag ) ) +
  geom_point( data = pca_clean2.df,
              size = 0.3 ) +
  geom_hline( yintercept = 0, lty = "dashed", alpha = 0.3 ) +
  geom_vline( xintercept = 0, lty = "dashed", alpha = 0.3 ) +
  scale_color_manual( values = mycolors.v ) +
  theme_linedraw( ) +
  # theme( panel.grid = element_blank( ),
  #        legend.position = "none",
  #        # strip.background = element_rect( color = "black", fill = "white" ),
  #        # strip.text = element_text( color = "black" )
  #        # strip.background = element_blank( ),
  #        # strip.text = element_blank( ),
  #        # axis.title.y = element_blank( ),
  #        # axis.text.y = element_blank( ),
  #        # axis.title = element_blank(),
  #        # axis.text = element_blank(),
  #        axis.ticks.length = unit( 20, "pt" )
  # ) +
  facet_wrap( ~run_clean, ncol = 1,
              strip.position = "right",
              scales = "free_x" )

pca_panel.p <- pc1_pc2.p + pc3_pc2.p

ggsave( filename = "pca_panel.png",
        plot = pca_panel.p,
        width = 14, height = 21 )

ggsave( filename = "pca_panel.svg",
        plot = pca_panel.p,
        width = 14, height = 21 )

#### Do ADMIXTURE ----
pacman::p_load( "vroom", "dplyr", "tidyr",
                "stringr", "ggplot2",
                "patchwork", "tidyverse" )

# Prepare a function to plot admixture ----
plot_admx.f <- function( the_pop, the_sorted ) {
  
  the_sub.df <- the_sorted %>% 
    filter( region == the_pop ) %>% 
    select( -region ) %>% 
    mutate( sample = rownames( . ) %>% as.numeric( ) )
  
  # find the abundance of each gsim
  the_abun.df <- the_sub.df %>% 
    pivot_longer( cols = -sample,
                  names_to = "pop",
                  values_to = "proportion" ) %>% 
    group_by( pop ) %>% 
    summarise( meanprop = mean( proportion ) ) %>% 
    arrange( -meanprop )
  
  pop_order.v <- the_abun.df %>% 
    pull( pop )
  
  # for the highly admx AMR,lets sort by hand
  if ( the_pop == "AMR" ) {
    
    the_sub.df <- the_sub.df %>% 
      ungroup( ) %>% 
      # arrange( ORI, EUR, AFR, EAS, SAS ) %>% 
      arrange( -EUR ) %>% 
      mutate( sample = rownames( . ) %>% as.numeric( ) )
    
  }
  
  toplot.df <- the_sub.df %>% 
    pivot_longer( cols = -sample,
                  names_to = "group",
                  values_to = "proportion" ) %>% 
    mutate( group = factor( group,
                            levels = rev( pop_order.v ) ) )
  
  admxpanel.p <- ggplot( data = toplot.df,
                         mapping = aes(  x = rev( sample ),
                                         y = proportion,
                                         fill = group,
                                         color = group ) ) +
    geom_col( ) +
    scale_fill_manual( values = mycolors.v ) +
    scale_color_manual( values = mycolors.v ) +
    # scale_y_continuous(expand = c( 0, 0 )) + 
    scale_x_discrete(expand = c( 0.01, 0.01 )) +
    labs( title = the_pop ) +
    theme_void( )
  
  # get the plot
  return( admxpanel.p )
}

# prepare colors
mycolors.v <- c( "AFR" = "#5987e5",
                 "ORI" = "#f09daf",
                 "EAS" = "#94c859",
                 "EUR" = "#ff5b42",
                 "SAS" = "#cc4c4d" )

admx_list <- readRDS( file = "admix_list.rds" )

## Here we create a function that takes a list element, and creates its admixture strip ----
plot_strip.f <- function( the_object ) {
  
  # great, now take one table
  the_df <- the_object
  
  the_df2 <- the_df 
  
  # need a function that....
  # 1) groups by region
  # 2) finds the group that has a biger mean value in each region
  # 3) renames that group colname to the region
  
  find_groups.df <- the_df2 %>% 
    group_by( region ) %>% 
    summarise( group1 = mean( group1 ),
               group2 = mean( group2 ),
               group3 = mean( group3 ),
               group4 = mean( group4 ),
               group5 = mean( group5 ) ) %>%
    ungroup( ) %>% 
    pivot_longer( cols = -region,
                  names_to = "group",
                  values_to = "mean_proportion" ) %>% 
    group_by( region ) %>% 
    filter( mean_proportion == max( mean_proportion ) ) %>% 
    # well take ORI as the AMR measure because we have seen that ORI samples are more AMR similar
    filter( region != "AMR" ) %>% 
    ungroup( )
  
  # rename cols 
  # str( the_df2 )
  
  # Create the named vector manually
  lookup <- find_groups.df$region
  names(lookup) <- find_groups.df$group
  
  # Match and rename
  names(the_df2) <- ifelse(names(the_df2) %in% names(lookup), 
                           lookup[names(the_df2)], 
                           names(the_df2))
  
  # str( the_df2 )
  
  # now lets sort samples in each group from the most abundant 
  sorted.df <- the_df2 %>%
    # Split into a list by region
    group_split(region) %>%
    # For each region's sub-df, sort by the column that matches the region name
    map_dfr(function(df) {
      target_region <- unique(df$region)
      # Check if a column named after the region exists to avoid errors
      if (target_region %in% colnames(df)) {
        return(arrange(df, desc(.data[[target_region]])))
      } else {
        return(df)
      }
    })
  
  sorted.df %>% 
    group_by(region) %>% 
    slice_head(n = 3)
  
  # testing plot
  # order for admx strip is:
  # EUR AFR EAS SAS AMR ORI
  
  # get one subset
  mytheme <- theme( 
    # panel.spacing = unit(0, "lines"),
    # plot.margin = margin(0, 0, 0, 0, "pt"),
    legend.position = "none",
    plot.title = element_blank( ) )
  
  eur.p <- plot_admx.f( the_pop = "EUR", the_sorted = sorted.df ) + mytheme
  afr.p <- plot_admx.f( the_pop = "AFR", the_sorted = sorted.df ) + mytheme
  eas.p <- plot_admx.f( the_pop = "EAS", the_sorted = sorted.df ) + mytheme
  sas.p <- plot_admx.f( the_pop = "SAS", the_sorted = sorted.df ) + mytheme
  amr.p <- plot_admx.f( the_pop = "AMR", the_sorted = sorted.df ) + mytheme
  ori.p <- plot_admx.f( the_pop = "ORI", the_sorted = sorted.df ) + mytheme
  
  # 1. Define the proportions based on your calculated counts
  panel_proportions.v <- c( 0.1349, 0.175, 0.13, 0.1313, 0.0902, 0.3386 )
  
  # 2. Combine the plots into a single row with specific relative widths
  final_panel <- ( eur.p + afr.p + eas.p + sas.p + amr.p + ori.p ) + 
    plot_layout(
      nrow = 1, 
      widths = panel_proportions.v
    )
  
  # Display the panel
  return( final_panel )
  
}

# test for one strip
plot_strip.f( the_object = admx_list$`_0` )

# plot all strips
all_stripsr.ls <- lapply( admx_list, plot_strip.f )

# see a plot
all_stripsr.ls$`_1318`

# 1. Combine all plots into a vertical stack
final_vertical_stack <- wrap_plots(all_stripsr.ls, ncol = 1)

# 2. (Optional) If you want to sync the legends and remove spacing between them
final_vertical_stack <- final_vertical_stack + 
  plot_layout(guides = "collect") +
  theme(plot.margin = margin(5, 5, 5, 5, "pt")) # Adjust as needed

# Display
ggsave( filename = "final_vertical_stack.png",
        plot = final_vertical_stack, width = 14, height = 28 )


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

