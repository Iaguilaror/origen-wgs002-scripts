# load packages
pacman::p_load( "vroom", "dplyr",
                "ggplot2", "ggsci", "scales",
                "patchwork", "ggtern", "ggrepel", "cowplot" )

# order of relations
rel_order <- c( "Dup/MZ", "PO", "FS", "2nd", "3rd", "4th", "UN" )

# create my color pallete
mycolors <- c( "Dup/MZ" = "red3", 
               "PO" = "#e94440",
               "FS" = "#ec7538",
               "2nd" = "#98593b",
               "3rd" = "#5987e6",
               "4th" = "yellow2",
               "UN" = "gray" )

### Tern plot ----
ibd_clean_data <- vroom( file = "ibds.tsv" )

# calculate number of samples in dataset
total_samples_in_dataset <- 1427

## plotting IBD triangle
# clean data
ibd_clean_data <- ibd_clean_data %>% 
  select( IBD1Seg, IBD2Seg, InfType ) %>% 
  mutate( InfType = factor( InfType, levels = rel_order ) ) %>% 
  mutate( IBDsum = IBD1Seg + IBD2Seg  ) %>% 
  mutate( IBD0Seg = 1 - IBDsum ) %>% 
  filter( InfType == "Dup/MZ" | InfType == "PO" | InfType == "FS" | InfType == "2nd" | InfType == "3rd" )

# plot analyze
# Create the ternary plot
tern.p <- ggtern(data = ibd_clean_data,
                 mapping = aes(x = IBD0Seg,
                               y = IBD1Seg,
                               z = IBD2Seg,
                               fill = InfType ) ) +
  geom_point( shape = 21, color = "black",
              size = 6, stroke = 0.2, alpha = 0.8 ) +
  tern_limits( 1.1, 1.1, 1.1 ) +
  scale_fill_manual( values = mycolors ) +
  labs( title = "A - Percentage of genome with 0, 1 or 2 IBD alleles",
        xarrow = "IBD0", x = "",
        yarrow = "IBD1", y = "",
        zarrow = "IBD2", z = "",
        fill = "Relationship type" )  +
  # theme_rgbw() +
  theme_custom(
    # base_size = 12,
    # base_family = "",
    # tern.plot.background = NULL,
    # tern.panel.background = NULL,
    col.T = "#98593b",   # IBD1
    col.L = "#5987e6", # IBD0
    col.R = "#e94440",  # IBD2
    # col.grid.minor = "white"
  ) +
  theme( text = element_text( size = 20 ),
         # plot.title = element_text( face = "bold" ),
         plot.title = element_blank(),
         legend.text = element_text( size = 30 ),
         legend.title = element_text( size = 30 ),
         panel.background = element_rect(fill = NA, color = NA), # transparent panel
         plot.background  = element_rect(fill = NA, color = NA),  # transparent outer plot
         panel.border = element_rect( color = "black", linewidth = 0.5 ),
         panel.grid = element_line( color = "gray", linetype = "dotted", linewidth = 1 ),
         tern.axis.text = element_text( size = 30 ),
         legend.position = "bottom" ) +
  theme_showarrows( )

# Vis
tern.p

ggsave( filename = "IBD_ternaryplot.pdf", bg = "transparent",
        plot = tern.p, width = 10, height = 10 )

ggsave( filename = "IBD_ternaryplot.svg", bg = "transparent",
        plot = tern.p, width = 10, height = 10 )

### Relationships bar plot ----
participiants_by_number_of_rels <- vroom( file = "number_of_relationships.tsv" )

participiants_by_number_of_rels_summ <- participiants_by_number_of_rels %>% 
  group_by( InfType ) %>% 
  summarise( total = sum( n_participants ) ) %>%
  mutate( tag = paste0( InfType, " (n = ", total, ")" ) )

# retag the table
participiants_by_number_of_rels <- left_join( x = participiants_by_number_of_rels,
                                              y = participiants_by_number_of_rels_summ,
                                              by = "InfType" ) %>% 
  mutate( InfType = factor( InfType,
                            levels = c("3rd", "2nd", "FS", "PO") %>% rev( ) ) ) %>% 
  arrange( InfType )

# prepare order for legend
legend_order <- participiants_by_number_of_rels_summ %>% 
  select( InfType, tag ) %>% 
  mutate( InfType = factor( InfType,
                            levels = c("3rd", "2nd", "FS", "PO") %>% rev( ) ) ) %>% 
  arrange( InfType )

related_bar.p <- ggplot( data = participiants_by_number_of_rels,
                         mapping = aes( x = total_relations,
                                        y = n_participants,
                                        fill = InfType ) ) +
  geom_col( width = 0.5 ) +
  scale_fill_manual( values = mycolors,
                     limits = legend_order$InfType,
                     labels = legend_order$tag ) +
  labs( title = "B - Distribution by number of relatives",
        x = "Number of relatives per participant",
        y = "Number of participants",
        fill = "Relationship type",
        caption = paste( "from",
                         length( total_samples_in_dataset ),
                         "samples; NOTE: the same participant in X can count in multiple Relationship types" ) ) +
  theme_classic( base_size = 45 ) +
  theme( 
    plot.title = element_blank( ),
    plot.caption = element_blank( ),
    legend.position = c( 0.97, 0.97 ),
    legend.justification = c(1, 1),
    legend.title = element_text( size = 35 ),  # Increase legend title size
    legend.text = element_text( size = 35 ),   # Increase legend labels size
    panel.background = element_rect(fill = NA, color = NA), # transparent panel
    plot.background  = element_rect(fill = NA, color = NA)  # transparent outer plot
  )

related_bar.p

ggsave( filename = "related_bar.pdf",  bg = "transparent",
        plot = related_bar.p, width = 12, height = 12 )

ggsave( filename = "related_bar.svg",  bg = "transparent",
        plot = related_bar.p, width = 12, height = 12 )

# Do the donut plot
# donut for inset
n_related <- 242

n_total <- total_samples_in_dataset

# Create the data frame
donut_data <- data.frame(
  category = c("Related", "Unrelated"),
  count = c(n_related, n_total - n_related)
)

# Compute percentages and positions
donut_data$fraction <- donut_data$count / sum(donut_data$count)
donut_data$ymax <- cumsum(donut_data$fraction)
donut_data$ymin <- c(0, head(donut_data$ymax, n = -1))
donut_data$label <- paste( percent(donut_data$fraction), donut_data$category )

# Compute label positions (middle of each segment)
donut_data$ypos <- (donut_data$ymax + donut_data$ymin) / 2

# Create the plot
donut.p <- ggplot(donut_data,
                  mapping = aes(ymax = ymax,
                                ymin = ymin,
                                xmax = 1.7,
                                xmin = 1,
                                fill = category ) ) +
  geom_rect( color = "black" ) +
  coord_polar( theta = "y" ) +  # Convert to circular
  xlim( c( -1, 1.7 ) ) +  # Adjust x limits to create donut hole
  scale_fill_manual(values = c("Related" = "#E15759", "Unrelated" = "#4E79A7")) +  # Custom colors
  theme_void( ) +  # Remove background elements
  ggtitle("Total individuals") +
  labs( caption = paste( n_related, "related samples shown in red" ) ) +
  theme( legend.position = "none",
         plot.title = element_text( hjust = 0.5,
                                    size = 30,
                                    face = "bold" ),
         panel.background = element_rect(fill = NA, color = NA), # transparent panel
         plot.background  = element_rect(fill = NA, color = NA) )  # transparent outer plot

# Print the plot
donut.p

ggsave( filename = "donut_related.pdf",  bg = "transparent",
        plot = donut.p, width = 12, height = 12 )

ggsave( filename = "donut_related.svg",  bg = "transparent",
        plot = donut.p, width = 12, height = 12 )

### move plots ----
# 1. Define the directory name
dir_name <- "results"

# 2. Create the directory (won't fail if it already exists)
dir.create(dir_name, showWarnings = FALSE)

# 3. Identify all .svg and .pdf files in the current working directory
files_to_move <- list.files(pattern = "\\.(svg|pdf)$")

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