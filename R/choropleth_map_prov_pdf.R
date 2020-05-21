# National by province map (PDF format)----

globalVariables(c('VALUE',
                  'DPA_PROVIN',
                  'DPA_DESPRO',
                  'LABEL',
                  'X',
                  'Y',
                  '.',
                  ':='))

#' @export

choropleth_map_prov_pdf <- function(values,
                                    bins,
                                    map_title,
                                    filename,
                                    legend_title,
                                    map_x_size_cm = 25,
                                    bin_colors = c('red', 'yellow', 'forestgreen'),
                                    background_color = '#9bddff',
                                    legend_position = c(0.195, 0.3),
                                    title_font_scale = 1,
                                    title_font_face = 'bold',
                                    title_font_color = 'black',
                                    title_hjust = 0.5,
                                    names_font_scale = 1,
                                    names_font_face = 'plain',
                                    names_font_color = 'black',
                                    names_back_color = 'white',
                                    values_preffix = '',
                                    values_suffix = '',
                                    values_big_mark = ' ',
                                    values_dec_places = 2,
                                    values_font_scale = 1,
                                    values_font_face= 'bold',
                                    values_font_color = 'white',
                                    values_back_color = 'black',
                                    footer = '',
                                    footer_position = c(0.85, 0.04),
                                    footer_font_scale = 1,
                                    footer_font_face = 'plain',
                                    footer_font_color = '#606060',
                                    province_line_color = 'black',
                                    logo_filename = NULL,
                                    logo_position = c(0.85, 0.15),
                                    logo_scale = 1,
                                    logo_aspect_ratio = 1) {

  box <- st_bbox(shp_pro)

  if (!is.null(logo_filename)) {
    logo <- rasterGrob(readPNG(logo_filename),
                       interpolate = TRUE)
  }

  map_scale <- map_x_size_cm/25

  scores <- tibble(DPA_PROVIN = sprintf('%02d', as.integer(names(values))),
                   VALUE = values,
                   LABEL = paste0(values_preffix, format(round(values, values_dec_places), nsmall = values_dec_places, big.mark = values_big_mark, trim = TRUE), values_suffix)) %>%
    mutate(!!legend_title := quantileCut(VALUE, bins, dig.lab = 10))

  mapa <- shp_pro %>%
    filter(DPA_PROVIN != '90') %>%
    left_join(scores)

  labels <- mapa %>%
    select(DPA_PROVIN, DPA_DESPRO, LABEL) %>%
    bind_cols(as_tibble(st_coordinates(st_centroid(.$geometry)))) %>%
    st_drop_geometry() %>%
    mutate(X = ifelse(DPA_PROVIN == '09', X + 15E3, X),
           Y = ifelse(DPA_PROVIN == '12', Y + 20E3, Y),
           X = ifelse(DPA_PROVIN == '17', X + 20E3, X),
           X = ifelse(DPA_PROVIN == '19', X + 6E3, X),
           X = ifelse(DPA_PROVIN == '23', X - 8E3, X))

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
             size = 0.2,
             fill = NA) +

    geom_label(data = labels,
               aes(x = X, y = Y + 10E3, label = DPA_DESPRO),
               size = 2*names_font_scale*map_scale,
               color = names_font_color,
               fill = names_back_color,
               label.padding = unit(0.1, 'lines')) +

    geom_label(data = labels,
               aes(x = X, y = Y - 10E3, label = LABEL),
               size = 3.3*values_font_scale*map_scale,
               color = values_font_color,
               fill = values_back_color,
               fontface = values_font_face,
               label.padding = unit(0.1, 'lines')) +

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
             fontface = footer_font_face) +

    theme_void() +

    theme(plot.title = element_text(size = 15*title_font_scale*map_scale,
                                    face = title_font_face,
                                    color = title_font_color,
                                    hjust = title_hjust),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 'cm'),
          legend.title = element_text(size = 10*map_scale,
                                      face = 'bold'),
          legend.text = element_text(size = 8*map_scale),
          legend.position = legend_position,
          legend.margin = margin(6, 6, 6, 6),
          legend.background = element_rect(fill = 'white'),
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
         height = 19,
         dpi = 600,
         scale = map_scale,
         units = 'cm')

  chart
}
