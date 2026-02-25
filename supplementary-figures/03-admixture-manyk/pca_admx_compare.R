#### Do ADMIXTURE ----
pacman::p_load( "vroom", "dplyr", "tidyr",
                "stringr", "ggplot2",
                "patchwork", "tidyverse" )

# Prepare a function to plot admixture ----
plot_admx.f <- function( the_pop, the_sorted, the_colors ) {
  
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
      # arrange( -EUR ) %>% 
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
    scale_y_continuous(expand = c( 0, 0 )) +
    scale_x_discrete(expand = c( 0.01, 0.01 )) +
    scale_fill_manual( values = the_colors ) +
    scale_color_manual( values = the_colors ) +
    labs( title = the_pop ) 
  
  admxpanel.p
  
  # get the plot
  return( admxpanel.p )
}

## Here we create a function that takes a list element, and creates its admixture strip ----
plot_strip.f <- function( the_object, the_title ) {
  
  # great, now take one table
  the_df <- the_object
  
  the_df2 <- the_df 
  
  # need a function that....
  # 1) groups by region
  # 2) finds the group that has a biger mean value in each region
  # 3) renames that group colname to the region
  
  # find_groups.df <- the_df2 %>% 
  #   group_by( region ) %>% 
  #   summarise( across( .cols = starts_with( "group" ), 
  #                      .fns = ~mean( .x, na.rm = TRUE ) ) ) %>% 
  #   ungroup( ) %>% 
  #   pivot_longer( cols = -region,
  #                 names_to = "group",
  #                 values_to = "mean_proportion" ) %>% 
  #   group_by( region ) %>% 
  #   filter( mean_proportion == max( mean_proportion ) ) %>% 
  #   # well take ORI as the AMR measure because we have seen that ORI samples are more AMR similar
  #   filter( region != "AMR" ) %>% 
  #   ungroup( )
  # 
  # # rename cols 
  # # str( the_df2 )
  # 
  # # Create the named vector manually
  # lookup <- find_groups.df$region
  # names(lookup) <- find_groups.df$group
  # 
  # # Match and rename
  # names(the_df2) <- ifelse(names(the_df2) %in% names(lookup), 
  #                          lookup[names(the_df2)], 
  #                          names(the_df2))
  
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
    axis.title.y = element_blank( ),
    axis.text = element_blank( ),
    axis.title.x = element_blank( ) )
  
  mynames.v <- colnames( sorted.df %>% select( -region ) )
  
  mycolors.v <- rainbow( n = length( mynames.v ), 
                         s = 0.4, 
                         v = 0.9 )
  
  names( mycolors.v ) <- mynames.v
  
  eur.p <- plot_admx.f( the_pop = "EUR", the_sorted = sorted.df, the_colors = mycolors.v ) + mytheme
  afr.p <- plot_admx.f( the_pop = "AFR", the_sorted = sorted.df, the_colors = mycolors.v  ) + mytheme
  eas.p <- plot_admx.f( the_pop = "EAS", the_sorted = sorted.df, the_colors = mycolors.v  ) + mytheme
  sas.p <- plot_admx.f( the_pop = "SAS", the_sorted = sorted.df, the_colors = mycolors.v  ) + mytheme
  amr.p <- plot_admx.f( the_pop = "AMR", the_sorted = sorted.df, the_colors = mycolors.v  ) + mytheme
  ori.p <- plot_admx.f( the_pop = "ORI", the_sorted = sorted.df, the_colors = mycolors.v  ) + mytheme
  
  # 1. Define the proportions based on your calculated counts
  panel_proportions.v <-  c( 0.175, 0.1313, 0.13, 0.0902, 0.3386, 0.1349 )
  
  # 2. Combine the plots into a single row with specific relative widths
  final_panel <- ( afr.p + sas.p + eas.p + amr.p + ori.p + eur.p ) +
    plot_layout(
      nrow = 1, 
      widths = panel_proportions.v
    ) +
    plot_annotation( tag_levels = NULL, 
                     title = NULL ) & 
    theme( plot.tag.position = "left" )
  
  # To actually set the text:
  final_panel <- final_panel + 
    plot_annotation( tag_levels = list( the_title ) )
  
  # Display the panel
  return( final_panel )
  
}

k4 <- plot_strip.f( the_object =  vroom( file = "data/allwgs.converted2plink.4.admixture_proportion.tsv" ),
                    the_title = "K4" )

ggsave( plot = k4, filename = "k4.png", width = 14, height = 5 )

k5 <- plot_strip.f( the_object =  vroom( file = "data/allwgs.converted2plink.5.admixture_proportion.tsv" ),
                    the_title = "K5" )

ggsave( plot = k5, filename = "k5.png", width = 14, height = 5 )

k6 <- plot_strip.f( the_object =  vroom( file = "data/allwgs.converted2plink.6.admixture_proportion.tsv" ),
                    the_title = "K6" )

ggsave( plot = k6, filename = "k6.png", width = 14, height = 5 )

k7 <- plot_strip.f( the_object =  vroom( file = "data/allwgs.converted2plink.7.admixture_proportion.tsv" ),
                    the_title = "K7" )

ggsave( plot = k7, filename = "k7.png", width = 14, height = 5 )

# load packages
pacman::p_load( "vroom", "dplyr", "tidyr", "ggplot2", "stringr", "scales" )

# load data
load( file = "EUR_admx.Rdata" )

# now, lets see the number of subtags by eur subpop
# prepare data
eur_ordered.v <- full_eur_retagged.df %>% 
  ungroup( ) %>% 
  arrange( EUR_sub1 ) %>% 
  pull( sample )

# order groups for fill
ordered_pops.v <- c( "AFR", "SAS", "MXAMR", "EAS_sub1", "EAS_sub2", "EUR_sub2", "EUR_sub1" ) 

# prepare colors
mycolors.v =  c( "AFR" = "gray",
                 "SAS" = "gray",
                 "MXAMR" = "gray",
                 "EAS_sub1" = "gray",
                 "EAS_sub2" = "gray",
                 "EUR_sub2" = "#b6de6fff",
                 "EUR_sub1" = "#91d4c8ff" ) 

eur_panel.p <- full_eur_retagged.df %>% 
  pivot_longer( cols = EUR_sub1:EUR_sub2,
                names_to = "admx_groups",
                values_to = "proportion" ) %>% 
  mutate( admx_groups = factor( admx_groups, levels = ordered_pops.v ) ) %>% 
  mutate( EURsubgroup = factor( EURsubgroup, levels = c( "eur3", "eur2", "eur1" ) ) ) %>% 
  mutate( sample = factor( sample, levels = eur_ordered.v ) ) %>% 
  ggplot( data = .,
          mapping = aes( x = sample,
                         y = proportion,
                         fill = admx_groups,
                         color = admx_groups ) ) +
  geom_col( ) +
  scale_color_manual( values = mycolors.v ) +
  scale_fill_manual( values = mycolors.v ) +
  scale_y_continuous( limits = c( 0, 1 ),
                      breaks = seq( 0, 1, by = 0.25 ),
                      labels = percent,
                      expand = c( 0, 0 )  ) +
  theme_classic( base_size = 20 ) +
  theme( axis.title = element_blank( ),
         axis.text.x = element_blank( ),
         axis.ticks.x = element_blank( ),
         strip.background = element_blank( ),
         axis.line = element_blank( ),
         legend.position = "none" )  +
  facet_wrap( ~ EURsubgroup, scales = "free_x" )

ggsave( filename = "eur_panel.png", plot = eur_panel.p, width = 10, height = 7 )

# count country/region by eur subgroup
full_eur_retagged.df %>% 
  group_by( EURsubgroup, `Population code` ) %>% 
  count( )


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