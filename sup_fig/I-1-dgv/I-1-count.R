# load packages
pacman::p_load( "vroom", "dplyr", "ggplot2", "tidyr", "tidyverse", "ggrepel", "scales", "ggsci" )

# load data
dgv.df <- vroom( file = "GRCh38_hg38_variants_2025-12-01.txt" )

# filter only CNVs
cnv.df <- dgv.df %>% 
  filter( varianttype == "CNV" )

# Number of studies reporting CNVs
byreference.df <- cnv.df %>% 
  count( reference, sort = TRUE )

str( byreference.df )

#### plot donut ----
# Define how many specific groups you want to show
N_top <- 9 

plot_data <- byreference.df %>%
  # fct_lump_n automatically keeps the top N weighted by 'n' and groups the rest
  mutate(reference_grouped = fct_lump_n(reference, n = N_top, w = n, other_level = "Others")) %>%
  group_by(reference_grouped) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  # Calculate percentages for labels
  mutate(percentage = n / sum(n)) %>%
  # Reorder factors so the largest slices come first, with "Others" at the end
  mutate(reference_grouped = fct_reorder(reference_grouped, n, .desc = TRUE)) %>%
  mutate(reference_grouped = fct_relevel(reference_grouped, "Others", after = Inf)) 

# Assuming plot_data is the same as the previous step
plot_data_labeled <- plot_data %>%
  # 1. Arrange by the factor levels to match the visual stacking order
  arrange(reference_grouped) %>% 
  
  # 2. Calculate the cumulative midpoints for the labels
  mutate(
    # Cumulative end of the slice
    y_end = cumsum(n), 
    
    # Midpoint: start of slice + half of slice
    # (lag(y_end, default=0) is the start of the current slice)
    y_pos = lag(y_end, default = 0) + 0.5 * n 
  ) %>% 
  mutate( for_text = paste0( reference_grouped, "\n", percent( percentage, accuracy = 1 ) ) )

# ---------------------------------------------------------
# 3. PLOTTING
# ---------------------------------------------------------

ggplot( data = plot_data_labeled,
        mapping = aes( x = 2, y = rev( n ), fill = reference_grouped ) ) +
  # 2. OUTER LABELS (Names using ggrepel)
  geom_text_repel(
    mapping = aes( x = 2, y = rev( y_pos ),
                   label = rev( for_text ) ),
    size = 3.5,
    hjust = 0.5,
    nudge_x = 2,       # Push labels slightly further out
    segment.size = 0.5,  # Thickness of the connector line
    show.legend = FALSE  # Ensure this doesn't create a legend key
  ) +
  # Create the stacked bar chart
  geom_col( 
    #color = "white" 
  ) + 
  
  # Convert to polar coordinatess
  coord_polar( theta = "y",
               # start = 0 
               ) +
  
  # # 1. INNER LABELS (Percentages)
  # geom_text( mapping = aes( label = percent( percentage, accuracy = 1 ) ),
  #            position = position_stack( vjust = 0.5 ), 
  #            color = "white", size = 3, fontface = "bold" ) +
  # 

  
  # 3. Adjust xlim to make the hole AND room for outer labels
  # 0.5 = inner hole edge
  # 2.5 = outer ring edge
  # 4.0 = margin for the labels
  xlim( 0.5, 4 ) +
  
  # Aesthetics
  # scale_fill_viridis_d( option = "turbo", name = "Reference" ) + 
  scale_fill_npg( ) +
  theme_void( ) + 
  labs( title = "CNVs reported by Reference (Project)",
        subtitle = "Source: https://dgv.tcag.ca/dgv/app/downloads",
        caption = paste( "total CNVs in database:", prettyNum( sum( plot_data_labeled$n ), big.mark = "," ) ) ) +
  
  # 4. REMOVE LEGEND
  theme(
    plot.title = element_text( hjust = 0.5, face = "bold" ),
    legend.position = "none" 
  )
