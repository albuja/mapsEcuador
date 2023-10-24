#' @title National Ecuador choroplet map in PDF.
#' @description This function creates a PDF printer ready file with Ecuador choropleth map 
#' using provided data at province level. No GIS knowledge is required. Provide just a named
#' vector of numeric data. Geographic data will be internally added.
#' 
#' A PDF map can be created using default values simply calling the function only with named 
#' values vector as unique argument (see examples).
#' 
#' A lot of options are available for controlling data and customizing titles, labels, fonts,
#' sizes and colors. See https://fonts.google.com/ for available fonts. 
#' Base map shape files were downloaded from:
#' https://www.ecuadorencifras.gob.ec/documentos/web-inec/Cartografia/Clasificador_Geografico/2015/
#' 
#' Developed by Jorge Albuja (albuja@yahoo.com)
#' 
#' @param values named numeric vector; values to be represented in map. Names must be the province codes 
#' @param bins integer; number of bins to cut values into. 
#' @param map_title string with general title. 
#' @param filename string; output filename, must end with '.PDF' extension. 
#' @param legend_title string with legend title. 
#' @param bins_style string; chosen bins style one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails". 
#' @param map_x_size_cm numeric; horizontal size in cm for output PDF file, vertical size is automatic. 
#' @param bin_colors length-n string vector; must contain base colors names or codes to interpolate using grDevices::colorRampPalette(). 
#' @param background_color string with color name or code for map background. 
#' @param legend_position length-2 numeric vector in [0, 1] interval. Relative position of map legend. 
#' @param legend_title_font string; legend title font name choosen from https://fonts.google.com/. 
#' @param legend_items_font string; legend items font name choosen from https://fonts.google.com/. 
#' @param legend_back_color string with color name or code for legend background. 
#' @param title_font string; main title font name choosen from https://fonts.google.com/. 
#' @param title_font_scale numeric value to scale up/down the main title font size, 1 means no scaling. 
#' @param title_font_face string with the main title font type. Choose between c('plain', 'bold', 'italic', 'bold.italic'). 
#' @param title_font_color string with color name or code with main title font color. 
#' @param title_hjust numeric in [0, 1] with main title horizontal justification (0.5 is center). 
#' @param names_font string; province names font name choosen from https://fonts.google.com/. 
#' @param names_font_scale numeric value to scale up/down province names font size, 1 means no scaling. 
#' @param names_font_face string with province names font type. Choose between c('plain', 'bold', 'italic', 'bold.italic'). 
#' @param names_font_color string with color name or code for province names font. 
#' @param names_back_color string with color name or code for province names background. 
#' @param values_preffix string with preffix that will be added to value labels. 
#' @param values_suffix string with suffix that will be added to value labels. 
#' @param values_big_mark character; mark between every big.interval decimals before (hence big) the decimal point. 
#' @param values_dec_places numeric; number of decimal places for value labels. 
#' @param values_font string; value labels font name choosen from https://fonts.google.com/. 
#' @param values_font_scale numeric value to scale up/down value labels font size, 1 means no scaling. 
#' @param values_font_face string with value labels font type, choosen between c('plain', 'bold', 'italic', 'bold.italic'). 
#' @param values_font_color string with color name or code for value labels font. 
#' @param values_back_color string with color name or code for value labels background. 
#' @param footer string with a footer/caption 
#' @param footer_position length-2 numeric vector in [0, 1] interval. Footer relative position. 
#' @param footer_font string; footer font name choosen from https://fonts.google.com/. 
#' @param footer_font_scale numeric value to scale up/down the footer font size, 1 means no scaling. 
#' @param footer_font_face string with footer font type. Choose between c('plain', 'bold', 'italic', 'bold.italic'). 
#' @param footer_font_color string with color name or code for footer font. 
#' @param province_line_color string with color name or code for province limits line. 
#' @param logo_filename string with full path filename of a PNG logo image to insert in PDF output. 
#' @param logo_position length-2 numeric vector in [0, 1] interval.  Relative position of PNG logo image. 
#' @param logo_scale numeric value to scale up/down the PNG logo image size, 1 means no scaling. 
#' @param logo_aspect_ratio numeric. Relative 'y' size with respect to 'x' size of PNG logo image, 1 means a square image. 
#' 
#' @import scales
#' @import dplyr
#' @import ggplot2
#' @import stringr
#' @import sf
#' @import classInt
#' @import ggspatial
#' @import png
#' @import grid
#' @import showtext
#' @import sysfonts
#' @import grDevices
#' @import kimisc
#' @import utils
#' @export
#' 
#' @examples
#' # Choropleth map with dummy data
#' values <- round(runif(24, 10, 1000), 3)
#' names(values) <- 1:24
#' choropleth_map_prov_pdf(values, bins = 5)

choropleth_map_prov_pdf <- function(values,
                                    bins = 4,
                                    map_title = "Ecuador",
                                    filename = "mapa.pdf",
                                    legend_title = "Leyenda",
                                    bins_style= "quantile",
                                    map_x_size_cm = 25,
                                    bin_colors = c("red", "yellow", "forestgreen"),
                                    background_color = NULL,
                                    legend_position = c(0.195, 0.3),
                                    legend_title_font = "Montserrat",
                                    legend_items_font = "Montserrat",
                                    legend_back_color = "#dddddd",
                                    title_font = "Montserrat",
                                    title_font_scale = 1,
                                    title_font_face = "bold",
                                    title_font_color = "black",
                                    title_hjust = 0.5,
                                    names_font = "Montserrat",
                                    names_font_scale = 1,
                                    names_font_face = "bold",
                                    names_font_color = "black",
                                    names_back_color = "white",
                                    values_preffix = "",
                                    values_suffix = "",
                                    values_big_mark = " ",
                                    values_dec_places = 2,
                                    values_font = "Montserrat",
                                    values_font_scale = 1,
                                    values_font_face= "bold",
                                    values_font_color = "white",
                                    values_back_color = "black",
                                    footer = "https://github.com/albuja/mapsEcuador",
                                    footer_position = c(0.9, 0),
                                    footer_font = "Montserrat",
                                    footer_font_scale = 1,
                                    footer_font_face = "plain",
                                    footer_font_color = "#606060",
                                    province_line_color = "black",
                                    logo_filename = NULL,
                                    logo_position = c(0.9, 0.15),
                                    logo_scale = 1,
                                    logo_aspect_ratio = 1) {
  
  sysfonts::font_add_google(title_font, "title_font")
  sysfonts::font_add_google(names_font, "names_font")
  sysfonts::font_add_google(values_font, "values_font")
  sysfonts::font_add_google(footer_font, "footer_font")
  sysfonts::font_add_google(legend_title_font, "legend_title_font")
  sysfonts::font_add_google(legend_items_font, "legend_items_font")
  showtext::showtext_auto()
  
  #mapa_base <- st_set_crs(mapa_base, 32717)
  
  st_crs(mapa_base) <- 32717
  
  box <- sf::st_bbox(mapa_base)
  
  map_scale <- map_x_size_cm / 25
  
  scores <- dplyr::tibble(DPA_PROVIN = sprintf("%02d", as.integer(names(values))),
                          VALUE = values,
                          LABEL = dplyr::case_when(!is.na(values) ~ paste0(values_preffix, 
                                                                           format(round(values, values_dec_places),
                                                                                  nsmall = values_dec_places, 
                                                                                  big.mark = values_big_mark, trim = TRUE), 
                                                                           values_suffix),
                                                   TRUE ~ "")) |>
    dplyr::mutate(!!legend_title := kimisc::cut_format(VALUE,
                                                       breaks = classInt::classIntervals(var = VALUE, n = bins, style = bins_style, 5)$brks,
                                                       format_fun = function(x) paste0(values_preffix, x, values_suffix),
                                                       include.lowest = TRUE,
                                                       sep = ";  "))
  
  mapa <- mapa_base |>
    dplyr::left_join(scores,
                     by = "DPA_PROVIN")
  
  labels <- mapa |>
    select(DPA_PROVIN, DPA_DESPRO, LABEL) |> 
    dplyr::bind_cols(
      dplyr::as_tibble(
        sf::st_coordinates(
          sf::st_centroid(mapa$geometry)
        )
      )
    ) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(X = ifelse(DPA_PROVIN == "09", X + 15E3, X),
                  Y = ifelse(DPA_PROVIN == "12", Y + 15E3, Y),
                  Y = ifelse(DPA_PROVIN == "17", Y + 10E3, Y),
                  Y = ifelse(DPA_PROVIN == "18", Y + 5E3, Y),
                  X = ifelse(DPA_PROVIN == "19", X + 6E3, X),
                  Y = ifelse(DPA_PROVIN == "19", Y + 8E3, Y),
                  X = ifelse(DPA_PROVIN == "23", X - 8E3, X),
                  Y = ifelse(DPA_PROVIN == "23", Y - 10E3, Y),)
  
  chart <- ggplot2::ggplot(data = mapa) +
    #annotation_map_tile(zoom = 7) +
    
    ggplot2::geom_sf(ggplot2::aes(fill = !!ggplot2::sym(legend_title)),
                     color = province_line_color) +
    
    ggplot2::scale_fill_manual(values = grDevices::colorRampPalette(bin_colors)(bins)) +
    
    ggplot2::annotate("rect",
                      xmin = 7E4,
                      xmax = 42E4,
                      ymin = 98E5,
                      ymax = 1011E4,
                      colour = "black",
                      size = 0.4,
                      fill = NA) +
    
    ggplot2::geom_label(data = labels,
                        ggplot2::aes(x = X, y = Y + 10E3, label = DPA_DESPRO),
                        size = 2.2 * names_font_scale * map_scale,
                        color = names_font_color,
                        fill = names_back_color,
                        fontface = names_font_face,
                        family = "names_font",
                        label.padding = ggplot2::unit(0.2, "lines"),
                        label.size = 0) +
    
    ggplot2::geom_label(data = labels,
                        ggplot2::aes(x = X, y = Y - 10E3, label = LABEL),
                        size = 3.3 * values_font_scale * map_scale,
                        color = values_font_color,
                        fill = values_back_color,
                        fontface = values_font_face,
                        family = "values_font",
                        label.padding = ggplot2::unit(0.15, "lines"),
                        label.size = 0) +
    
    ggspatial::annotation_scale(location = "bl",
                                height = ggplot2::unit(0.25 * map_scale, "cm"),
                                width_hint = 0.3,
                                text_cex = 0.7 * map_scale) +
    
    ggspatial::annotation_north_arrow(location = "bl",
                                      which_north = "true",
                                      height = ggplot2::unit(1.5 * map_scale, "cm"),
                                      width = ggplot2::unit(1.5 * map_scale, "cm"),
                                      pad_x = ggplot2::unit(map_scale, "cm"),
                                      pad_y = ggplot2::unit(map_scale, "cm"),
                                      style = ggspatial::north_arrow_fancy_orienteering(text_size = 10*map_scale)) +
    
    ggplot2::ggtitle(stringr::str_wrap(map_title, width = 50)) +
    
    ggplot2::annotate("text",
                      x = box$xmin + (box$xmax - box$xmin) * footer_position[1],
                      y = box$ymin + (box$ymax - box$ymin) * footer_position[2],
                      label = footer,
                      size = 2.7 * footer_font_scale * map_scale,
                      color = footer_font_color,
                      fontface = footer_font_face,
                      family = "footer_font") +
    
    ggplot2::theme_void() +
    
    ggplot2::theme(plot.title = ggplot2::element_text(size = 15 * title_font_scale * map_scale,
                                                      face = title_font_face,
                                                      family = "title_font",
                                                      color = title_font_color,
                                                      hjust = title_hjust,
                                                      vjust = 2),
                   plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                   legend.title = ggplot2::element_text(size = 10 * map_scale,
                                                        face = "bold",
                                                        family = "legend_title_font"),
                   legend.text = ggplot2::element_text(size = 8 * map_scale,
                                                       family = "legend_items_font"),
                   legend.position = legend_position,
                   legend.margin = ggplot2::margin(6, 6, 6, 6),
                   legend.background = ggplot2::element_rect(fill = legend_back_color),
                   plot.background = ggplot2::element_rect(color = NA,
                                                           fill = background_color),
                   panel.background = ggplot2::element_rect(color = NA,
                                                            fill = background_color))
  
  if (!is.null(logo_filename)) {
    logo <- grid::rasterGrob(
      png::readPNG(logo_filename),
      interpolate = TRUE)
    
    chart <- chart +
      ggplot2::annotation_custom(logo,
                                 xmin = box$xmin + (box$xmax - box$xmin) * logo_position[1] - 5E4 * logo_scale * logo_aspect_ratio,
                                 xmax = box$xmin + (box$xmax - box$xmin) * logo_position[1] + 5E4 * logo_scale * logo_aspect_ratio,
                                 ymin = box$ymin + (box$ymax - box$ymin) * logo_position[2] - 5E4 * logo_scale * logo_aspect_ratio,
                                 ymax = box$ymin + (box$ymax - box$ymin) * logo_position[2] + 5E4 * logo_scale * logo_aspect_ratio)
  }
  
  ggplot2::ggsave(filename = filename,
                  plot = chart,
                  width = 25,
                  height = 18.22,
                  dpi = 100,
                  scale = map_scale,
                  units = "cm")
  
}

globalVariables(c(".", ":=", "DPA_DESPRO", "DPA_PROVIN", "LABEL", "VALUE", "X", "Y"))
