# load pkgs
pacman::p_load( "vroom", "ggplot2", "scales", "dplyr", "ggsci" )

# load data
data.df <- vroom( file = "novel_by_type.tsv" ) %>% 
  mutate( tag = paste( type, status ) )

mynums <- function( x ){
  prettyNum( x, big.mark = "," )
}

bars.p <- ggplot( data = data.df,
                  mapping = aes( x = 1,
                                 y = n,
                                 fill = tag ) ) +
  geom_col( color = "black" ) +
  scale_y_continuous( labels = mynums ) +
  scale_fill_simpsons( ) +
  facet_wrap( ~database,
              scales = "free_y" ) +
  theme_light( )

ggsave( filename = "bars.svg",
        plot = bars.p, width = 7, height = 10 )
