# National by province map (PDF format)----

globalVariables(c('VALUE',
                  'DPA_PROVIN',
                  'DPA_DESPRO',
                  'LABEL',
                  'X',
                  'Y',
                  '.',
                  ':=',
                  'font_add_google'))

#' @export

choropleth_map_prov_pdf <- function(values,
                                    bins = 4,
                                    map_title = 'Ecuador',
                                    filename = 'mapa.pdf',
                                    legend_title = 'Leyenda',
                                    bins_style= 'quantile',
                                    map_x_size_cm = 25,
                                    bin_colors = c('red', 'yellow', 'forestgreen'),
                                    background_color = NULL,
                                    legend_position = c(0.195, 0.3),
                                    legend_title_font = 'Montserrat',
                                    legend_items_font = 'Montserrat',
                                    legend_back_color = '#9bddff',
                                    title_font = 'Montserrat',
                                    title_font_scale = 1,
                                    title_font_face = 'bold',
                                    title_font_color = 'black',
                                    title_hjust = 0.5,
                                    names_font = 'Montserrat',
                                    names_font_scale = 1,
                                    names_font_face = 'bold',
                                    names_font_color = 'black',
                                    names_back_color = 'white',
                                    values_preffix = '',
                                    values_suffix = '',
                                    values_big_mark = ' ',
                                    values_dec_places = 2,
                                    values_font = 'Montserrat',
                                    values_font_scale = 1,
                                    values_font_face= 'bold',
                                    values_font_color = 'white',
                                    values_back_color = 'black',
                                    footer = 'https://github.com/albuja/mapsEcuador',
                                    footer_position = c(0.9, 0),
                                    footer_font = 'Montserrat',
                                    footer_font_scale = 1,
                                    footer_font_face = 'plain',
                                    footer_font_color = '#606060',
                                    province_line_color = 'black',
                                    logo_filename = NULL,
                                    logo_position = c(0.9, 0.15),
                                    logo_scale = 1,
                                    logo_aspect_ratio = 1) {

  font_add_google(title_font, 'title_font')
  font_add_google(names_font, 'names_font')
  font_add_google(values_font, 'values_font')
  font_add_google(footer_font, 'footer_font')
  font_add_google(legend_title_font, 'legend_title_font')
  font_add_google(legend_items_font, 'legend_items_font')
  showtext_auto()

  box <- st_bbox(shp_pro)

  if (!is.null(logo_filename)) {
    logo <- rasterGrob(readPNG(logo_filename),
                       interpolate = TRUE)
  }

  map_scale <- map_x_size_cm/25

  scores <- tibble(DPA_PROVIN = sprintf('%02d', as.integer(names(values))),
                   VALUE = values,
                   LABEL = paste0(values_preffix, format(round(values, values_dec_places), nsmall = values_dec_places, big.mark = values_big_mark, trim = TRUE), values_suffix)) %>%
    mutate(!!legend_title := cut_format(VALUE,
                                        breaks = classIntervals(var = VALUE, n = bins, style = bins_style, 5)$brks,
                                        include.lowest = TRUE,
                                        sep = ';  '))

  galapagos <- shp_pro %>%
    filter(DPA_PROVIN == '20') %>%
    st_set_geometry(., st_geometry(.) + c(8e5, 0)) %>%
    st_set_crs(32717)

  shp_pro <- shp_pro %>%
    st_set_crs(32717) %>%
    filter(DPA_PROVIN != '20') %>%
    rbind(galapagos)

  mapa <- shp_pro %>%
    filter(DPA_PROVIN != '90') %>%
    left_join(scores)

  labels <- mapa %>%
    select(DPA_PROVIN, DPA_DESPRO, LABEL) %>%
    bind_cols(as_tibble(st_coordinates(st_centroid(.$geometry)))) %>%
    st_drop_geometry() %>%
    mutate(X = ifelse(DPA_PROVIN == '09', X + 15E3, X),
           Y = ifelse(DPA_PROVIN == '12', Y + 15E3, Y),
           Y = ifelse(DPA_PROVIN == '17', Y + 10E3, Y),
           Y = ifelse(DPA_PROVIN == '18', Y + 5E3, Y),
           X = ifelse(DPA_PROVIN == '19', X + 6E3, X),
           Y = ifelse(DPA_PROVIN == '19', Y + 8E3, Y),
           X = ifelse(DPA_PROVIN == '23', X - 8E3, X),
           Y = ifelse(DPA_PROVIN == '23', Y - 10E3, Y),)

  chart <- ggplot(data = mapa) +
    #annotation_map_tile(zoom = 7) +

    geom_sf(aes(fill = !!sym(legend_title)),
            color = province_line_color) +

    scale_fill_manual(values = colorRampPalette(bin_colors)(bins)) +

    annotate('rect',
             xmin = 7E4,
             xmax = 42E4,
             ymin = 98E5,
             ymax = 1011E4,
             colour = 'black',
             size = 0.4,
             fill = NA) +

    geom_label(data = labels,
               aes(x = X, y = Y + 10E3, label = DPA_DESPRO),
               size = 2.2*names_font_scale*map_scale,
               color = names_font_color,
               fill = names_back_color,
               fontface = names_font_face,
               family = 'names_font',
               label.padding = unit(0.2, 'lines'),
               label.size = 0) +

    geom_label(data = labels,
               aes(x = X, y = Y - 10E3, label = LABEL),
               size = 3.3*values_font_scale*map_scale,
               color = values_font_color,
               fill = values_back_color,
               fontface = values_font_face,
               family = 'values_font',
               label.padding = unit(0.15, 'lines'),
               label.size = 0) +

    annotation_scale(location = 'bl',
                     height = unit(0.25*map_scale, "cm"),
                     width_hint = 0.3,
                     text_cex = 0.7*map_scale) +

    annotation_north_arrow(location = 'bl',
                           which_north = 'true',
                           height = unit(1.5*map_scale, "cm"),
                           width = unit(1.5*map_scale, "cm"),
                           pad_x = unit(map_scale, 'cm'),
                           pad_y = unit(map_scale, 'cm'),
                           style = north_arrow_fancy_orienteering(text_size = 10*map_scale)) +

    ggtitle(str_wrap(map_title, width = 50)) +

    annotate('text',
             x = box$xmin + (box$xmax - box$xmin)*footer_position[1],
             y = box$ymin + (box$ymax - box$ymin)*footer_position[2],
             label = footer,
             size = 2.7*footer_font_scale*map_scale,
             color = footer_font_color,
             fontface = footer_font_face,
             family = 'footer_font') +

    theme_void() +

    theme(plot.title = element_text(size = 15*title_font_scale*map_scale,
                                    face = title_font_face,
                                    family = 'title_font',
                                    color = title_font_color,
                                    hjust = title_hjust,
                                    vjust = 2),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
          legend.title = element_text(size = 10*map_scale,
                                      face = 'bold',
                                      family = 'legend_title_font'),
          legend.text = element_text(size = 8*map_scale,
                                     family = 'legend_items_font'),
          legend.position = legend_position,
          legend.margin = margin(6, 6, 6, 6),
          legend.background = element_rect(fill = legend_back_color),
          plot.background = element_rect(color = NA,
                                         fill = background_color),
          panel.background = element_rect(color = NA,
                                          fill = background_color))

  if (!is.null(logo_filename)) {
    chart <- chart +
      annotation_custom(logo,
                        xmin = box$xmin + (box$xmax - box$xmin)*logo_position[1] - 5E4*logo_scale*logo_aspect_ratio,
                        xmax = box$xmin + (box$xmax - box$xmin)*logo_position[1] + 5E4*logo_scale*logo_aspect_ratio,
                        ymin = box$ymin + (box$ymax - box$ymin)*logo_position[2] - 5E4*logo_scale*logo_aspect_ratio,
                        ymax = box$ymin + (box$ymax - box$ymin)*logo_position[2] + 5E4*logo_scale*logo_aspect_ratio)
  }

  ggsave(filename = filename,
         plot = chart,
         width = 25,
         height = 18.22,
         dpi = 100,
         scale = map_scale,
         units = 'cm')

}
