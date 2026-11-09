# load packages
pacman::p_load( "vroom", "dplyr", "ggplot2", "ggtern", "scales" )

# important params
total_samples_in_dataset_fullexome <- 69912

# load example
example <- vroom( file = "../ibds.tsv" )

# load regdata
exomedatreg.df <- vroom( file = "TEC-Monterrey_Freeze_Four.GxS.filtered.primus_input.genome",
                         show_col_types = FALSE ) |> 
  as_tibble() |> 
  select( IBD1Seg, IBD2Seg, InfType )

# save for future plotting
vroom_write( x = exomedatreg.df, file = "ibds.tsv" )

# clean env
rm( list = ls( ) )
gc( )

# ----
example_bars <- vroom( file = "../number_of_relationships.tsv" )

# put a name to args
ifile <-  "TEC-Monterrey_Freeze_Four.GxS.filtered.primus_input.genome"

# read data
king_data <- vroom( file = ifile ) |> 
  as_tibble( ) |> 
  select( -10 ) |> 
  rename( Kinship = `PropIBD...6` )  %>%
  dplyr::filter( InfType != "Dup/MZ" )

# calculate number of samples in dataset
total_samples_in_dataset <- c( pull( king_data, ID1 ), pull( king_data, ID2 ) ) %>% 
  unique( )

# total real samples in VCF 69,912

# order of relations
# rel_order <- c( "PO", "FS", "2nd", "3rd", "4th", "UN" )
rel_order <- c( "Dup/MZ", "PO", "FS", "2nd", "3rd", "4th", "UN" )

# clean data
clean_data <- king_data %>% 
  select( ID1, ID2, Kinship, InfType ) %>% 
  mutate( InfType = factor( InfType, levels = rel_order ) )

# create my color pallete
mycolors <- c( "Dup/MZ" = "red3", 
               "PO" = "#cc79a7",
               "FS" = "#0072b2",
               "2nd" = "#019e74",
               "3rd" = "#d55e00",
               "4th" = "yellow2",
               "UN" = "gray" )

# plot matrix of relatedness
unrelated_data <- clean_data %>% 
  filter( InfType == "UN" )

related_data <- clean_data %>% 
  filter( InfType != "UN" ) %>% 
  arrange( InfType )

# related samples
all_relations <- related_data %>% 
  group_by( ID1 ) %>% 
  summarise( relations = n( ),
             related_ids = paste( ID2, collapse = "," ),
             relation_type = unique(InfType) %>% sort () %>%   paste( .,
                                                                      collapse = "," ) ) %>% 
  ungroup( ) %>% 
  arrange( -relations )

# create plot for all samples
allsamples.p <- ggplot( mapping = aes( x = ID2,
                                       y = ID1,
                                       fill = InfType ) ) +
  geom_tile( data = unrelated_data,
             alpha = 0.3 ) +
  geom_tile( data = related_data,
             color = "black" ) +
  scale_x_discrete( limits = rev ) +
  scale_fill_manual( values = mycolors,
                     limits = rel_order,
                     name = "Relationship" ) +
  labs( title = "Relatedness in project",
        caption = paste( "Inferred by King v2; total samples in dataset =", length( total_samples_in_dataset ) ),
        subtitle = paste( "related pairs up to 4th degree =",  nrow( related_data ), "\n",
                          "from", nrow( all_relations ), "samples" ) ) +
  theme_classic( ) +
  theme( axis.text = element_blank( ) ,
         axis.ticks = element_blank( ) )

# Vis
allsamples.p

# sasve all samples
ggsave( filename = "1_allsamples.png",
        plot = allsamples.p,
        width = 10, height = 7 )

# create plot for all samples
relatedsamples.p <- ggplot( related_data,
                            mapping = aes( x = ID2,
                                           y = ID1,
                                           fill = InfType ) ) +
  geom_tile( data = related_data,
             color = "black" ) +
  scale_x_discrete( limits = rev ) +
  scale_fill_manual( values = mycolors,
                     limits = rel_order[ ! rel_order %in% c( "4th", "UN") ],
                     name = "Relationship" ) +
  labs( title = "Relatedness in project",
        caption = paste( "Inferred by King v2; total samples in dataset =", length( total_samples_in_dataset ) ),
        subtitle = paste( "related pairs up to 4th degree =",  nrow( related_data ), "\n",
                          "from", nrow( all_relations ), "samples" ) ) +
  theme_classic( ) +
  theme( axis.text = element_blank( ),
         axis.ticks = element_blank( ) )

# vis
relatedsamples.p

ggsave( filename = "2_related_samples.png",
        plot = relatedsamples.p,
        width = 10, height = 7 )

# show number of relations detected
allpairs.df <- related_data %>% 
  group_by( InfType ) %>% 
  summarise( counts = n( ) )

allpairs.p <- ggplot( data = allpairs.df,
                      mapping = aes( x = InfType,
                                     y = counts,
                                     fill = InfType,
                                     label = counts ) ) +
  geom_col( color = "black", alpha = 0.5 ) +
  geom_text( nudge_y = max( allpairs.df$counts ) / 20  ) +
  scale_x_discrete( limits = rel_order[ ! rel_order %in% "UN" ] ) +
  scale_fill_manual( values = mycolors ) +
  labs( title = "All pair of relations found",
        caption = paste( "Inferred by King v2; total samples in dataset =", length( total_samples_in_dataset ) ),
        subtitle = paste( "related pairs up to 4th degree =",  nrow( related_data ), "\n",
                          "found between", nrow( all_relations ), "samples" ),
        x = "Type of Relation",
        y = "Pairs" ) +
  theme_classic( base_size = 15 ) +
  theme( legend.position = "none" )

# vis
allpairs.p

ggsave( filename = "3_types_of_pair.png",
        plot = allpairs.p,
        width = 10, height = 7 )

# focus on close related idns
close_realted.df <- clean_data %>% 
  filter( InfType == "Dup/MZ" | InfType == "PO" | InfType == "FS" | InfType == "2nd" | InfType == "3rd" )

close_realted.df2 <- close_realted.df %>% 
  mutate( id_1 = ID2,
          id_2 = ID1 ) %>% 
  mutate( ID1 = id_1,
          ID2 = id_2 ) %>% 
  select( -id_1, -id_2 )

close_fullmatrix <- bind_rows( close_realted.df,
                               close_realted.df2 )

# show number of relations detected
close_pairs.p <- ggplot( close_fullmatrix,
                         mapping = aes( x = ID2,
                                        y = ID1,
                                        fill = InfType ) ) +
  geom_tile( color = "black" ) +
  scale_x_discrete( limits = rev ) +
  scale_fill_manual( values = mycolors,
                     limits = rel_order[ ! rel_order %in% c( "4th", "UN") ],
                     name = "Relationship" ) +
  labs( title = "Relatedness in project",
        caption = "Inferred by King v2",
        subtitle = paste( "related pairs =",  nrow( related_data ), "\n",
                          "from", nrow( all_relations ), "samples" ) ) +
  theme_classic( ) +
  theme( axis.text = element_blank( ),
         axis.ticks = element_blank( ) )

# vis
close_pairs.p

ggsave( filename = "4_close_pairs.png",
        plot = close_pairs.p,
        width = 10, height = 7 )

# show relations by sample
close_related_by_sample <- close_fullmatrix %>% 
  group_by( ID1, InfType ) %>% 
  summarise( counts = n( ) ) %>% 
  ungroup( ) %>%
  group_by( ID1 ) %>% 
  mutate( total_relations = sum( counts ) ) %>% 
  ungroup( ) %>% 
  arrange( -total_relations, ID1 )

# calculamos distribucion de parientes por muestra ----
relsbyparticipant.df <- close_related_by_sample %>%
  group_by( ID1 ) |> 
  summarise( 
    total_participants_related_upto3rd = sum( counts ),
    total_participants_related_upto2nd = sum( counts[ InfType != "3rd" ] )
  ) %>%
  mutate( participant_random_id = paste( "random", rownames( . ) ), .before = 1 ) %>%
  select( -2 )

# save
vroom_write( x = relsbyparticipant.df, file = "relsbyparticipant.tsv"  )

# sample order
ordered_samples_related <- close_related_by_sample %>% 
  select( ID1, total_relations ) %>% 
  unique( )

# plot bars for sample
relations_by_sample.p <- ggplot( data = close_related_by_sample,
                                 mapping = aes( x = ID1,
                                                y = counts,
                                                fill = InfType ) ) +
  geom_col( ) +
  scale_x_discrete( limits = ordered_samples_related$ID1,
                    expand = c( 0.1, 0.1 ) ) +
  scale_fill_manual( values = mycolors ) +
  labs( title = "Samples with relations in the dataset",
        x = "sample",
        y = "Number of relationships found by King2" ) +
  theme_classic( base_size = 15 ) +
  theme( axis.text.x = element_blank( ),
         axis.ticks.x = element_blank( ) )

# Vis
relations_by_sample.p

ggsave( filename = "5_samples_relation.png",
        plot = relations_by_sample.p,
        width = 10, height = 14 )

relations_by_sample.p +
  coord_cartesian( xlim = c( 1, 30 ) ) +
  theme( axis.text.x = element_text( angle = 90, size = 5 ) ) 

## plotting IBD triangle

# clean data
ibd_clean_data <- king_data %>% 
  select( ID1, ID2, IBD1Seg, IBD2Seg, InfType ) %>% 
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
              size = 4, stroke = 0.2, alpha = 0.8 ) +
  scale_fill_manual( values = mycolors ) +
  labs( title = "Percentage of genome with 0, 1 or 2 IBD alleles",
        xarrow = "IBD0", x = "",
        yarrow = "IBD1", y = "",
        zarrow = "IBD2", z = "",
        fill = "Relationship type" )  +
  theme_showarrows( ) +
  theme_rgbw() +
  theme( text = element_text( size = 20 ),
         panel.background = element_rect( fill = "white" ),
         panel.border = element_rect( color = "black", linewidth = 0.5 ),
         panel.grid = element_line( color = "gray", linetype = "dotted", linewidth = 1 ),
         tern.axis.text = element_text( size = 15 ),
         legend.position = "bottom" )

# Vis
tern.p

ggsave( filename = "6_IBD_ternaryplot.png",
        plot = tern.p, width = 10, height = 10 )

# plot ranks by Relatedness
# focus on close related idns
new_realted.df <- clean_data %>% 
  filter( InfType == "PO" | InfType == "FS" | InfType == "2nd" | InfType == "3rd" )

new_realted.df2 <- new_realted.df %>% 
  mutate( id_1 = ID2,
          id_2 = ID1 ) %>% 
  mutate( ID1 = id_1,
          ID2 = id_2 ) %>% 
  select( -id_1, -id_2 )

# Aqui se registran todas las relaciones bilaterales
new_close_fullmatrix <- bind_rows( new_realted.df,
                                   new_realted.df2 )

# group by type
rels_by_type <- new_close_fullmatrix %>% 
  group_by( ID1, InfType ) %>% 
  summarise( n_relations = n( ) ) %>% 
  ungroup( )

rels_by_sample <- new_close_fullmatrix %>% 
  group_by( ID1 ) %>% 
  summarise( total_relations = n( ) ) %>% 
  ungroup( )

# get factor ordered by number
total_rel_factor <- sort( rels_by_sample$total_relations ) %>% 
  unique( ) %>% 
  as.character( )

# refactor
rels_by_sample <- rels_by_sample %>% 
  mutate( total_relations = factor( total_relations, level = total_rel_factor ) )

# join rels by sample
joined_rels_by <- left_join( x = rels_by_type,
                             y = rels_by_sample,
                             by = "ID1" )

# sumarise more
participiants_by_number_of_rels <- joined_rels_by %>% 
  group_by( InfType, total_relations ) %>% 
  summarise( n_participants = n( ) ) %>% 
  ungroup( )

# calculate number by type
participiants_by_number_of_rels_summ <- participiants_by_number_of_rels %>% 
  group_by( InfType ) %>% 
  summarise( total = sum( n_participants ) ) %>%
  mutate( tag = paste0( InfType, " (n = ", total, ")" ) )

# retag the table
participiants_by_number_of_rels <- left_join( x = participiants_by_number_of_rels,
                                              y = participiants_by_number_of_rels_summ,
                                              by = "InfType" )

# prepare order for legend
legend_order <- participiants_by_number_of_rels_summ %>% 
  select( InfType, tag )

# create a stacked bar plot
related_bar.p <- ggplot( data = participiants_by_number_of_rels,
                         mapping = aes( x = total_relations,
                                        y = n_participants,
                                        fill = InfType ) ) +
  geom_col( width = 0.5 ) +
  scale_fill_manual( values = mycolors,
                     limits = legend_order$InfType,
                     labels = legend_order$tag ) +
  # scale_y_continuous( expand = c(0.0,0) ) +
  labs( title = "Distribution by number of relatives",
        x = "Number of relatives per participant",
        y = "Number of participants",
        fill = "Relationship type",
        caption = paste( "from",
                         length( total_samples_in_dataset ),
                         "samples; NOTE: the same participant in X can count in multiple Relationship types" ) ) +
  theme_classic( base_size = 20 ) +
  theme( axis.text.x = element_text( angle = 45, vjust = 0.5 ) )

# Vis
related_bar.p

# save plot
ggsave( filename = "7_Distribution_by-number_of_relatives.png",
        plot = related_bar.p, width = 10, height = 10 )

# create a bar of related samples
message( "number of relations summarized: ",
         
         joined_rels_by %>% 
           pull( n_relations ) %>% 
           sum( ) / 2
         
)

# Get the sample IDs in related samples
upto3rd_samples.df <- joined_rels_by %>% 
  select( ID1 ) %>% 
  unique( )

# get the sample IDs in closely related samples
upto2nd_samples.df <- joined_rels_by %>% 
  filter( InfType != "3rd" ) %>% 
  select( ID1  ) %>% 
  unique( )

# create a function for the barplot
makebars.f <- function( the_data, the_caption ){
  
  data.frame( tag = c( "unrelated", "related" ),
              number = c( total_samples_in_dataset_fullexome - nrow( the_data ),
                          nrow( the_data ) ) ) %>% 
    mutate( frac = number / sum( number ),
            perc = percent( frac, accuracy = 0.1 ),
            label = paste0( tag, " = ", perc ) ) %>% 
    ggplot( data = .,
            mapping = aes( x = 1,
                           y = frac,
                           fill = label ) ) +
    geom_col( width = 0.5, color = "black" ) +
    scale_x_continuous( limits = c( 0.5,1.5 ) ) +
    scale_y_continuous( labels = percent ) +
    labs( title = "Relatedness in project",
          fill = "samples",
          y = "dataset",
          caption = the_caption ) +
    theme_classic( ) +
    theme( axis.text.x = element_blank( ),
           axis.title.x = element_blank( ),
           axis.ticks.x = element_blank( ) )
  
}

# create for all relatedness  
upto3rd_bars.p <- makebars.f( the_data = upto3rd_samples.df,
                              the_caption = "up to 3rd degree (e.g. great-grandparent, great-grandchild)" )

# create for close relatedness  
upto2nd_bars.p <-makebars.f( the_data = upto2nd_samples.df,
                             the_caption = "up to 2nd degree (e.g. grandparent, grandchild)" )  

# save plot
ggsave( filename = "8a_relatedness_bar.png",
        plot = upto3rd_bars.p, width = 7, height = 7 )

ggsave( filename = "8b_close_relatedness_bar.png",
        plot = upto2nd_bars.p, width = 7, height = 7 )

# save the data for figure repro
vroom_write( x = select( participiants_by_number_of_rels, -total, -tag ),
             file = "number_of_relationships.tsv" )
