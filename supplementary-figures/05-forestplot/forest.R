# load pkgs
pacman::p_load( "vroom", "dplyr", "tidyr", "ggplot2", "ggrepel", "viridis", "patchwork" )

load( file = "forest.Rdata" )

forplot_forest.df <- forplot.df %>%
  select(feno, odds_ratio, or_ci_lower, or_ci_upper)

# Optional: reorder rows by odds ratio (or log odds ratio, or another metric)
forplot_forest.df <- forplot_forest.df %>%
  mutate( feno = forcats::fct_reorder( feno, odds_ratio ) )

# get the vector for orders
yorder.v <- forplot_forest.df$feno %>% 
  as.character( )

# Create forest plot
forest_color <- "cornflowerblue"

forest.p <- ggplot( forplot_forest.df,
                    mapping = aes( x = odds_ratio, y = feno ) ) +
  geom_point( size = 2, color = forest_color ) +  # Odds ratio point
  geom_errorbarh( mapping = aes( xmin = or_ci_lower,
                                 xmax = or_ci_upper ),
                  height = 0, color = forest_color ) +
  geom_vline( xintercept = 1, linetype = "dashed", color = "black" ) +
  scale_x_log10(  # Log scale for odds ratio axis
    # breaks = c(0.001, 0.01, 0.1, 1, 10, 100),
    # labels = c("0.01", "0.1", "1", "10", "100")
  ) +
  labs(
    x = "Odds Ratio (EBV+:EBV–)",
    y = NULL,
    title = "Association with EBV presence"
  ) +
  theme_linedraw( base_size = 12 ) +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# vis
forest.p

# save individual plots
svgtrans.f <- function( the_plot, the_title, the_width, the_height ){
  
  ggsave( filename = the_title,
          plot = the_plot +
            theme( panel.background = element_blank(),
                   plot.background = element_rect(fill = "transparent", color = NA) ),
          width = the_width, height = the_height )
  
}

# volcano
svgtrans.f( the_plot = forest.p, the_title = "virus_forest.svg",
            the_width = 10, the_height = 7 )
