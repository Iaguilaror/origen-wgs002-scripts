# load packages
pacman::p_load( "vroom", "dplyr",
                "ggplot2", "ggsci", "scales", "hexbin", "colorspace", "ggnewscale",
                "patchwork", "ggtern", "ggrepel", "cowplot", "plotly", "scatterplot3d" )

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
total_samples_in_dataset <- 69912

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
              size = 2, stroke = 0.2, alpha = 0.8 ) +
  tern_limits( 1.05, 1.05, 1.05 ) +
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

# try densities

ggtern( data = ibd_clean_data,
        mapping = aes(x = IBD0Seg,
                      y = IBD1Seg,
                      z = IBD2Seg,
                      fill = InfType
        ) ) +
  geom_hex_tern( binwidth = 0.01 ) + 
  scale_fill_manual( values = mycolors ) +
  labs( fill = "Conteo" ) +
  theme_bw( base_size = 14 ) +
  theme_showarrows( ) +
  theme( 
    tern.axis.ticks.minor = element_line( color = "white", linewidth = 0.5 ),  # Elimina las marcas menores en los bordes
    tern.panel.grid.minor = element_line( color = "white", linewidth = 0.5 ),   # Elimina las líneas guía menores en el fondo
    # tern.axis.line = element_line( color = "red", linewidth = 1 ) 
    tern.axis.line = element_line( color = "white", linewidth = 0.5 )
  )

# test monochrome
# ggtern( data = ibd_clean_data,
#         mapping = aes(x = IBD0Seg,
#                       y = IBD1Seg,
#                       z = IBD2Seg,
#                       # fill = InfType
#         ) ) +
#   geom_hex_tern( binwidth = 0.01 ) + 
#   theme_bw( base_size = 14 ) +
#   theme_showarrows( )

# plot bidimensional ----
ggplot( data = ibd_clean_data,
        mapping = aes( x = IBD0Seg,
                       y = IBD1Seg,
                       fill = InfType ) ) +
  geom_hex( bins = 100  ) +
  theme_classic( )

# --- try to make each one

plot_hexs.f <- function( the_type, the_col, the_bins ) {
  
  ibd_clean_data |> 
    filter( InfType == the_type ) |> 
    ggplot( mapping = aes( x = IBD0Seg,
                           y = IBD1Seg ) ) +
    geom_hex( bins = the_bins  ) +
    scale_fill_gradient( low = lighten( the_col, amount = 0.7 ),
                         high = the_col ) +
    scale_y_continuous( limits = c( -0.05, 1.1 ), breaks = seq( 0, 1, 0.2 ) ) +
    scale_x_continuous( limits = c( -0.05, 0.9 ), breaks = seq( 0, 1, 0.2 ) ) +
    theme_classic( ) +
    theme( legend.position = "none" )
  
}

mybins <- 20

# hex_po.p <- plot_hexs.f( the_type = names( mycolors[ 2 ] ),
#                          the_col = mycolors[ 2 ],
#                          the_bins = mybins )
# 
# hex_po.p
# 
# hex_fs.p <- plot_hexs.f( the_type = names( mycolors[ 3 ] ),
#                          the_col = mycolors[ 3 ],
#                          the_bins = mybins )
# 
# hex_fs.p
# 
# hex_2nd.p <- plot_hexs.f( the_type = names( mycolors[ 4 ] ),
#                           the_col = mycolors[ 4 ],
#                           the_bins = mybins )
# 
# hex_2nd.p
# 
# hex_3rd.p <- plot_hexs.f( the_type = names( mycolors[ 5 ] ),
#                           the_col = mycolors[ 5 ],
#                           the_bins = mybins )
# 
# hex_3rd.p

# ----
mybins <- 100

mycolors <- c( "Dup/MZ" = "red3", 
               "PO" = "#e94440",
               "FS" = "#ec7538",
               # "2nd" = "#98593b",
               "2nd" = "limegreen",
               "3rd" = "#5987e6",
               "4th" = "yellow2",
               "UN" = "gray" )

allhex.p <- ggplot() +
  # First dataset layer
  geom_hex( data = ibd_clean_data |> filter( InfType == names( mycolors[ 2 ] ) ),
            aes( x = IBD0Seg, y = IBD1Seg ), alpha = 0.7, bins = mybins ) +
  scale_fill_gradient( low = lighten( mycolors[ 2 ], amount = 0.7 ),
                       high = mycolors[ 2 ],
                       name = names( mycolors[ 2 ] ) ) +
  # scale_fill_gradient( low = "#C6DBEF", high = "#08306B", name = "2nd Degree") +
  # Reset the fill scale for the next layer
  new_scale_fill() +
  
  # Second dataset layer
  geom_hex( data = ibd_clean_data |> filter( InfType == names( mycolors[ 3 ] ) ),
            aes( x = IBD0Seg, y = IBD1Seg ), alpha = 0.7, bins = mybins ) +
  
  scale_fill_gradient( low = lighten( mycolors[ 3 ], amount = 0.7 ),
                       high = mycolors[ 3 ],
                       name = names( mycolors[ 3 ] ) ) +
  
  # Reset the fill scale for the next layer
  new_scale_fill() +
  
  # 3rd dataset layer
  geom_hex( data = ibd_clean_data |> filter( InfType == names( mycolors[ 4 ] ) ),
            aes( x = IBD0Seg, y = IBD1Seg ), alpha = 0.7, bins = mybins ) +
  
  scale_fill_gradient( low = lighten( mycolors[ 4 ], amount = 0.7 ),
                       high = mycolors[ 4 ],
                       name = names( mycolors[ 4 ] ) ) +
  
  # Reset the fill scale for the next layer
  new_scale_fill() +
  
  # 4th dataset layer
  geom_hex( data = ibd_clean_data |> filter( InfType == names( mycolors[ 5 ] ) ),
            aes( x = IBD0Seg, y = IBD1Seg ), alpha = 0.7, bins = mybins ) +
  
  scale_fill_gradient( low = lighten( mycolors[ 5 ], amount = 0.7 ),
                       high = mycolors[ 5 ],
                       name = names( mycolors[ 5 ] ) ) +
  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_y_continuous( limits = c( -0.05, 1.1 ), breaks = seq( 0, 1, 0.2 ) ) +
  scale_x_continuous( limits = c( -0.05, 0.9 ), breaks = seq( 0, 1, 0.2 ) ) +
  theme_classic( ) +
  theme( legend.position = "none" )

allhex.p

ggsave( plot = allhex.p, file = "allhex.png", width = 7, height = 7 )

# ----
allbin2d.p <- ggplot() +
  # First dataset layer
  geom_bin_2d( data = ibd_clean_data |> filter( InfType == names( mycolors[ 2 ] ) ),
               aes( x = IBD0Seg, y = IBD1Seg ), alpha = 0.7, bins = mybins ) +
  scale_fill_gradient( low = lighten( mycolors[ 2 ], amount = 0.7 ),
                       high = mycolors[ 2 ],
                       name = names( mycolors[ 2 ] ) ) +
  # scale_fill_gradient( low = "#C6DBEF", high = "#08306B", name = "2nd Degree") +
  # Reset the fill scale for the next layer
  new_scale_fill() +
  
  # Second dataset layer
  geom_bin_2d( data = ibd_clean_data |> filter( InfType == names( mycolors[ 3 ] ) ),
               aes( x = IBD0Seg, y = IBD1Seg ), alpha = 0.7, bins = mybins ) +
  
  scale_fill_gradient( low = lighten( mycolors[ 3 ], amount = 0.7 ),
                       high = mycolors[ 3 ],
                       name = names( mycolors[ 3 ] ) ) +
  
  # Reset the fill scale for the next layer
  new_scale_fill() +
  
  # 3rd dataset layer
  geom_bin_2d( data = ibd_clean_data |> filter( InfType == names( mycolors[ 4 ] ) ),
               aes( x = IBD0Seg, y = IBD1Seg ), alpha = 0.7, bins = mybins ) +
  
  scale_fill_gradient( low = lighten( mycolors[ 4 ], amount = 0.7 ),
                       high = mycolors[ 4 ],
                       name = names( mycolors[ 4 ] ) ) +
  
  # Reset the fill scale for the next layer
  new_scale_fill() +
  
  # 4th dataset layer
  geom_bin_2d( data = ibd_clean_data |> filter( InfType == names( mycolors[ 5 ] ) ),
               aes( x = IBD0Seg, y = IBD1Seg ), alpha = 0.7, bins = mybins ) +
  
  scale_fill_gradient( low = lighten( mycolors[ 5 ], amount = 0.7 ),
                       high = mycolors[ 5 ],
                       name = names( mycolors[ 5 ] ) ) +
  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  scale_y_continuous( limits = c( -0.05, 1.1 ), breaks = seq( 0, 1, 0.2 ) ) +
  scale_x_continuous( limits = c( -0.05, 0.9 ), breaks = seq( 0, 1, 0.2 ) ) +
  theme_classic( ) +
  theme( legend.position = "none" )

allbin2d.p

ggsave( plot = allbin2d.p, file = "allbin2d.png", width = 7, height = 7 )

## try dots alpha -----
# mycolors <- c( "FS" = "#ec7538",
#                # "2nd" = "#98593b",
#                "2nd" = "limegreen",
#                "3rd" = "#5987e6",
#                "4th" = "yellow2",
#                "UN" = "gray" )

ibd_clean_data %>%
  filter( InfType != "Dup/MZ" ) %>%
  ggplot( data = .,
          mapping = aes( x = IBD0Seg,
                         y = IBD1Seg,
                         fill = InfType ) ) +
  geom_point( shape = 21, alpha = 0.3, size = 2.5, color = "white" ) +
  scale_fill_manual( values = mycolors ) +
  theme_classic( )

ibd_clean_data %>%
  filter( InfType != "Dup/MZ" ) %>%
  ggplot( data = .,
          mapping = aes( x = IBD0Seg,
                         y = IBD1Seg,
                         fill = InfType ) ) +
  geom_point( shape = 21, alpha = 0.3, size = 2.5 ) +
  scale_fill_manual( values = mycolors ) +
  theme_classic( base_size = 20 )

ggplot( data = ibd_clean_data,
        mapping = aes( x = IBD0Seg,
                       y = IBD1Seg,
                       color = InfType ) ) +
  geom_point( alpha = 0.1, size = 2.5 ) +
  scale_color_manual( values = mycolors ) +
  theme_classic( )

## IBD in 3D ----
# Aseguramos que la paleta manual coincida
plot_ly(
  data = filter( ibd_clean_data, InfType != "Dup/MZ" ),
  x = ~IBD2Seg,   # Ajusta tu variable del eje X si aplica
  y = ~IBD1Seg,
  z = ~IBD0Seg,
  color = ~InfType,
  colors = mycolors,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    size = 2.5,
    opacity = 0.3  # Similar al alpha de ggplot
  )
) %>%
  layout(
    scene = list(
      xaxis = list(title = "IBD2Seg"),
      yaxis = list(title = "IBD1Seg"),
      zaxis = list(title = "IBD0Seg")
    )
  )

# Mapeamos los colores vectoriales según la categoría InfType
color_vector <- mycolors[as.factor(ibd_clean_data$InfType)]
# Añadimos transparencia (alpha) a los colores en formato HEX
color_alpha <- adjustcolor(color_vector, alpha.f = 0.3)

plot.new( )

s3d <- scatterplot3d(
  x = ibd_clean_data$IBD2Seg,
  y = ibd_clean_data$IBD1Seg,
  z = ibd_clean_data$IBD0Seg,
  color = color_alpha,
  pch = 19,
  cex.symbols = 0.8,
  xlab = "IBD2Seg",
  ylab = "IBD1Seg",
  zlab = "IBD0Seg"
)

# Agregar leyenda manual
legend(
  "topright",
  legend = levels(as.factor(ibd_clean_data$InfType)),
  col = mycolors,
  pch = 19,
  bty = "n"
)

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

# try different style
str( participiants_by_number_of_rels )

participiants_by_number_of_rels |>
  # 1. Agrupar por tipo de parentesco
  group_by( InfType ) |>
  # 2. Ordenar por número de relaciones para que la suma acumulada sea correcta
  arrange( total_relations, .by_group = TRUE ) |>
  # 3. Calcular la frecuencia acumulada dividiendo la suma acumulada entre el total del grupo
  mutate( cum_pct = cumsum( n_participants ) / sum( n_participants ) ) |>
  ungroup( ) |>
  # 4. Graficar los datos calculados
  ggplot( mapping = aes( x = total_relations, y = cum_pct, color = InfType ) ) +
  geom_step( linewidth = 1 ) +
  scale_y_continuous( labels = scales::percent ) +
  labs( 
    x = "Número de parientes por participante", 
    y = "Porcentaje acumulado de participantes",
    color = "Tipo de Relación"
  ) +
  theme_bw( base_size = 14 ) +
  coord_cartesian( xlim = c( 0, 15 ) )

# simple plot
participiants_by_number_of_rels |>
  filter( InfType %in% c( "PO", "FS", "2nd" ) ) |>
  mutate( pct_full_sample = n_participants / total_samples_in_dataset ) |>
  ggplot( mapping = aes( x = total_relations,
                         y = pct_full_sample,
                         fill = InfType ) ) +
  geom_col( position = position_dodge(),
            # color = "black"
  ) +
  scale_fill_manual( values = mycolors,
                     limits = legend_order$InfType,
                     labels = legend_order$tag ) +
  scale_y_continuous( labels = scales::percent_format( accuracy = 0.1 ),
                      breaks =  seq( 0, 1, 0.01 ) ) +
  scale_x_continuous( breaks = seq( 1, 30, 1 ) ) +
  coord_cartesian( xlim = c( 1, 15 ) ) +
  labs( 
    x = "Número de parientes", 
    y = paste( "Porcentaje de la muestra total ( N =", total_samples_in_dataset, ")" ),
    fill = "Tipo de Relación"
  ) +
  theme_bw( base_size = 14 ) +
  theme( panel.grid.minor.x = element_blank( ),
         panel.grid.major.x = element_blank( ),
         panel.grid.minor.y = element_blank( ),
         panel.grid.major.y = element_blank( ) )

# ----
participiants_by_number_of_rels |>
  # Filtrar solo hasta segundo grado
  filter( InfType %in% c( "PO", "FS", "2nd" ) ) |>
  # Agrupar y sumar todos los tipos de relación en un solo total por cada 'total_relations'
  group_by( total_relations ) |>
  summarise( n_participants = sum( n_participants ), .groups = "drop" ) |>
  # Calcular el porcentaje global
  mutate( pct_full_sample = n_participants / total_samples_in_dataset ) |>
  # Graficar la línea de decaimiento
  ggplot( mapping = aes( x = total_relations, 
                         y = pct_full_sample ) ) +
  geom_line( color = "#2c3e50", linewidth = 1 ) +
  geom_point( color = "#2c3e50", size = 2 ) +
  scale_y_continuous( labels = scales::percent_format( accuracy = 0.1 ),
                      breaks = seq( 0, 1, 0.01 ) ) +
  scale_x_continuous( breaks = seq( 1, 30, 1 ) ) +
  coord_cartesian( xlim = c( 1, 15 ) ) +
  labs( 
    x = "Número de parientes", 
    y = paste( "Porcentaje de la muestra total ( N =", total_samples_in_dataset, ")" )
  ) +
  theme_bw( base_size = 14 ) +
  theme( panel.grid.minor.x = element_blank( ),
         panel.grid.major.x = element_blank( ),
         panel.grid.minor.y = element_blank( ),
         panel.grid.major.y = element_blank( ) )

# ----
participiants_by_number_of_rels |>
  # 1. Filtrar solo hasta segundo grado
  filter( InfType %in% c( "PO", "FS", "2nd" ) ) |>
  # Se omite el paso de summarise para mantener las categorías separadas
  # 2. Calcular el porcentaje global directamente
  mutate( pct_full_sample = n_participants / total_samples_in_dataset ) |>
  # 3. Mapear el color a InfType para generar una línea por categoría
  ggplot( mapping = aes( x = total_relations, 
                         y = pct_full_sample,
                         color = InfType ) ) +
  # 4. Quitar el color fijo para que hereden la paleta de aes()
  geom_line( linewidth = 1 ) +
  geom_point( size = 2 ) +
  scale_y_continuous( labels = scales::percent_format( accuracy = 0.1 ),
                      breaks = seq( 0, 1, 0.01 ) ) +
  scale_x_continuous( breaks = seq( 1, 30, 1 ) ) +
  coord_cartesian( xlim = c( 1, 15 ) ) +
  labs( 
    x = "Número de parientes", 
    y = paste( "Porcentaje de la muestra total ( N =", total_samples_in_dataset, ")" ),
    color = "Tipo de Relación" # Etiqueta para la leyenda
  ) +
  scale_color_manual( values = mycolors,
                      limits = legend_order$InfType,
                      labels = legend_order$tag ) +
  theme_bw( base_size = 14 ) +
  theme( panel.grid.minor.x = element_blank( ),
         panel.grid.major.x = element_blank( ),
         panel.grid.minor.y = element_blank( ),
         panel.grid.major.y = element_blank( ) )

# ----
# ESTE NO FUNCIONA, porque varios individuos cuentan en multiples valores de X, tendría que contar solo una vez. Corregir el data cleaning previo
# participiants_by_number_of_rels |>
#   # 1. Filtrar solo hasta segundo grado
#   filter( InfType %in% c( "PO", "FS", "2nd" ) ) |>
#   # 2. Agrupar y sumar todos los tipos de relación en un solo total por cada 'total_relations'
#   group_by( total_relations ) |>
#   summarise( n_participants = sum( n_participants ), .groups = "drop" ) |>
#   # 3. Asegurar el orden ascendente para la suma acumulada
#   arrange( total_relations ) |>
#   # 4. Calcular el porcentaje acumulativo global
#   mutate( cum_pct_full_sample = cumsum( n_participants ) / total_samples_in_dataset ) |>
#   # 5. Graficar la línea acumulativa
#   ggplot( mapping = aes( x = total_relations, 
#                          y = cum_pct_full_sample ) ) +
#   geom_vline( xintercept = seq( 5, 60, 5 ),
#               alpha = 0.1,
#               lty = 2 ) +
#   geom_step( color = "#2c3e50", linewidth = 1 ) + 
#   # geom_point( color = "#2c3e50", size = 2 ) +
#   scale_y_continuous( labels = scales::percent_format( accuracy = 0.1 ),
#                       # Considera cambiar 0.01 por 0.05 o 0.1 si la suma acumulada llega muy alto
#                       breaks = seq( 0, 1, 0.02 ) ) +
#   scale_x_continuous( breaks = seq( 1, 60, 1 ) ) +
#   coord_cartesian( xlim = c( 1, 60 ) ) +
#   labs( 
#     x = "Número de parientes", 
#     y = paste( "Porcentaje acumulado de la muestra ( N =", total_samples_in_dataset, ")" )
#   ) +
#   theme_bw( base_size = 14 ) +
#   theme( panel.grid.minor.x = element_blank( ),
#          panel.grid.major.x = element_blank( ),
#          panel.grid.minor.y = element_blank( ),
#          panel.grid.major.y = element_blank( ) )

# Do the donut plot
# donut for inset
n_related <- 24670

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

# Analisis de cuantos parientes tiene cada individuo ----
relsbyp.df <- vroom( file = "relsbyparticipant.tsv" ) %>%
  as_tibble( )

str( relsbyp.df )

# how many have N relatives
upto3rd_cum.df <- relsbyp.df %>%
  count( total_participants_related_upto3rd ) %>%
  mutate( total = sum( n ),
          cumul_n = cumsum( n ),
          cumul_frac = cumul_n / total ) %>%
  rename( n_rels = 1,
          cumul_frac_3rd = cumul_frac )

upto2nd_cum.df <- relsbyp.df %>%
  count( total_participants_related_upto2nd ) %>%
  filter( total_participants_related_upto2nd > 0 ) %>%
  mutate( total = sum( n ),
          cumul_n = cumsum( n ),
          cumul_frac = cumul_n / total )  %>%
  rename( n_rels = 1,
          cumul_frac_2nd = cumul_frac )

curves.df <- left_join( 
  x = select( upto2nd_cum.df, n_rels, cumul_frac_2nd ),
  y = select( upto3rd_cum.df, n_rels, cumul_frac_3rd ),
  by = "n_rels" ) %>%
  bind_rows( data.frame( n_rels = 1,
                         cumul_frac_2nd = 0,
                         cumul_frac_3rd = 0 ) ,
             . )

# plot
mycolors <- c( "FS" = "#ec7538",
               # "2nd" = "#98593b",
               "2nd" = "limegreen",
               "3rd" = "#5987e6",
               "4th" = "yellow2",
               "UN" = "gray" )

relsbyparticipant.p <- ggplot( data = curves.df,
                               mapping = aes( x = n_rels ) ) +
  # geom_segment( x = 1, xend = 1, y = 0, yend = 0.5,
  #               linetype = "dashed", color = "tomato" ) +
  # geom_segment( x = 0, xend = 1, y = 0.5, yend = 0.5, alpha = 0.05,
  #               linetype = "dashed", color = "tomato" ) +
  geom_hline( yintercept = 0.5, alpha = 0.3,
              linetype = "dashed", color = "tomato" ) +
  # geom_line(  mapping = aes( y = cumul_frac_2nd ),
  #             color = "limegreen" ) +
  # geom_point(  data = curves.df %>% slice( -1 ),
  #              mapping = aes( y = cumul_frac_3rd ),
  #              color = "#5987e6", size = 3 ) +
  # geom_line(  mapping = aes( y = cumul_frac_3rd ),
  #             color = "#5987e6", size = 1 ) +
  geom_step(  mapping = aes( y = cumul_frac_3rd ),
              color = "#5987e6", size = 1 ) +
  scale_x_continuous( breaks = seq( 1, 10, 1 ), limits = c( 1, 10 ) ) +
  scale_y_continuous( breaks = seq( 0, 1, 0.1 ), labels = percent ) +
  labs( x = "(N) number of relatives",
        y = "percentage of participants with N relatives" ) +
  theme_classic( base_size = 30 )

relsbyparticipant.p

ggsave( plot = relsbyparticipant.p, filename = "relsbyparticipant.png", width = 10, height = 7 )
