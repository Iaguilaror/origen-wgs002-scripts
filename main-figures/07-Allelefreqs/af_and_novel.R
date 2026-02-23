# load packs
pacman::p_load( "ggplot2", "patchwork", "dplyr", "cowplot" )

# load base plots ----

afr.p <- readRDS( file = "gnomad_AF_afr_scatter.rds" )

amr.p <- readRDS( file = "gnomad_AF_amr_scatter.rds" )

eas.p <- readRDS( file = "gnomad_AF_eas_scatter.rds" )

nfe.p <- readRDS( file = "gnomad_AF_nfe_scatter.rds" )

sas.p <- readRDS( file = "gnomad_AF_sas_scatter.rds" )

mcps_raw.p <- readRDS( file = "mcps_AF_RAW_scatter.rds" )

# crate themes ----
toprow_theme <- theme( plot.title = element_blank( ),
                       axis.title = element_blank( ),
                       plot.subtitle = element_blank( ),
                       plot.caption = element_blank( ),
                       legend.position = "none",
                       axis.title.x = element_blank( ),
                       # axis.ticks.x = element_blank( ),
                       axis.text.x = element_blank( ),
                       axis.text = element_text( size = 30 ) ) 

bottomrow_theme <- theme( plot.title = element_blank( ),
                          axis.title = element_blank( ),
                          plot.subtitle = element_blank( ),
                          plot.caption = element_blank( ),
                          legend.position = "none",
                          axis.title.x = element_blank( ),
                          axis.text = element_text( size = 30, hjust = 0.5 ) 
                          # axis.ticks.x = element_blank( ),
                          # axis.text.x = element_blank( )
) 

common_theme <- theme( panel.grid = element_blank( ) )

side_theme <- theme( axis.text.y = element_blank( )  )

# params -----
pop_text_size <- 15
pop_x_pos <- 1

# Build top row first ----

# 
toprow.p <- ( eas.p + 
                # annotate( "text", label = "EAS",
                #                 x = pop_x_pos, y = 0.05,
                #                 hjust = 1, size = pop_text_size ) +
                toprow_theme + common_theme +
                afr.p + 
                # annotate( "text", label = "AFR",
                #                   x = pop_x_pos, y = 0.05,
                #                   hjust = 1, size = pop_text_size ) +
                toprow_theme + side_theme + common_theme +
                nfe.p + 
                # annotate( "text", label = "NF-EUR",
                #                   x = pop_x_pos, y = 0.05,
                #                   hjust = 1, size = pop_text_size ) +
                toprow_theme + side_theme + common_theme )

bottomrow.p <- ( sas.p  + 
                   # annotate( "text", label = "SAS",
                   #                  x = pop_x_pos, y = 0.05,
                   #                  hjust = 1, size = pop_text_size ) +
                   bottomrow_theme + common_theme +
                   amr.p + 
                   # annotate( "text", label = "AMR",
                   #                   x = pop_x_pos, y = 0.05,
                   #                   hjust = 1, size = pop_text_size ) +
                   bottomrow_theme + side_theme + common_theme +
                   mcps_raw.p + 
                   # annotate( "text", label = "MCPS",
                   #                        x = pop_x_pos, y = 0.05,
                   #                        hjust = 1, size = pop_text_size ) +
                   bottomrow_theme + side_theme + common_theme )

full_panel.p <- toprow.p / bottomrow.p

# full_panel.p

ggsave( filename = "full_panel.svg", plot = full_panel.p, height = 5, width = 10 )

#
# Extract only the legend
legend.p <- get_legend( mcps_raw.p +
                          theme( legend.background = element_rect( fill = NA,
                                                                   color = NA ) ) ) %>% 
  plot_grid( )

ggsave(  filename = "legend.svg", plot = legend.p, height = 5, width = 5  )


## ----
# load pkgs
pacman::p_load( "vroom", "dplyr", "ggplot2", "tidyr", "scales", "ggrepel", "ggsci" )

# load the data
novel_ann.df <- vroom( file = "all_novel_vep_annotated.tsv",
                       na = ".",
                       show_col_types = FALSE ) %>% 
  as_tibble( )

# Now lets plot the AF
onlyAFs.df <- novel_ann.df %>% 
  # select( AF_ori, ANN_gnomADg_AF_afr:ANN_gnomADg_AF_sas  )
  select( ANN_gnomADg_AF_grpmax, ANN_gnomADg_grpmax, AF_ori ) %>% 
  mutate(across(everything(), ~replace_na(.x, 0)) ) %>% 
  mutate( ANN_gnomADg_grpmax = ifelse( test = ANN_gnomADg_grpmax == "",
                                       yes = "unknown",
                                       no = ANN_gnomADg_grpmax ) )

onlyAFs.df <- onlyAFs.df %>%
  mutate(AF_category = case_when(
    AF_ori < 0.01            ~ "rare",
    AF_ori >= 0.01 & AF_ori <= 0.05 ~ "low-freq",
    AF_ori > 0.05            ~ "common"
  ))

# doa donut
## Prepare summary data
af_summary <- onlyAFs.df %>%
  count(AF_category) %>%
  mutate(
    percent = n / sum(n),
    label = paste0(AF_category, "\n", n, " (", percent(percent), ")"),
    ymax = cumsum(percent),
    ymin = lag(ymax, default = 0),
    ymid = (ymin + ymax) / 2
  )

# Plot
donut.p <- ggplot(af_summary, aes(x = 4, y = percent, fill = AF_category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text_repel(
    mapping = aes(x = 5, y = ymid, label = label, color = AF_category ),  # <- was x = 4.5
    size = 4,
    # direction = "y",
    # nudge_x = 0.5,
    # segment.size = 0.3,
    # segment.color = "gray30",
    max.overlaps = Inf
  ) +
  xlim(2, 5.5) +
  scale_fill_futurama( ) +
  scale_color_futurama( ) +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Allele Frequency Categories")

donut.p

ggsave( plot = donut.p,
        filename = "novel_donut.png",
        bg = "white",
        width = 9, height =  9 )

# count exonic
forconsequence.df <- novel_ann.df %>% 
  select( AF_ori, ANN_Consequence )

forconsequence.df

## Prepare summary data
consequence_summary <- forconsequence.df %>%
  count(ANN_Consequence) %>%
  mutate(
    percent = n / sum(n),
    label = paste0(ANN_Consequence, "\n", n, " (", percent(percent), ")"),
    ymax = cumsum(percent),
    ymin = lag(ymax, default = 0),
    ymid = (ymin + ymax) / 2
  )

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
