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
