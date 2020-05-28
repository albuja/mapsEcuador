\name{choropleth_map_prov_pdf}
\alias{choropleth_map_prov_pdf}
\title{National by province coropleth map (PDF format)}
\usage{     choropleth_map_prov_pdf(values,
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
                                    logo_aspect_ratio = 1)
}
\arguments{
\item{values}{named numeric vector; values to be represented in map. Names must be the province codes}
\item{bins}{integer; number of bins to cut values into.}
\item{map_title}{string with general title.}
\item{filename}{string; output filename, must end with '.PDF' extension.}
\item{legend_title}{string with legend title.}
\item{bins_style}{string; chosen bins style one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih" or "headtails".}
\item{map_x_size_cm}{numeric; horizontal size in cm for output PDF file, vertical size is automatic.}
\item{bin_colors}{length-n string vector; must contain base colors names or codes to interpolate using grDevices::colorRampPalette().}
\item{background_color}{string with color name or code for map background.}
\item{legend_position}{length-2 numeric vector in [0,1] interval. Relative position of map legend.}
\item{legend_title_font}{string; legend title font name choosen from https://fonts.google.com/.}
\item{legend_items_font}{string; legend items font name choosen from https://fonts.google.com/.}
\item{legend_back_color}{string with color name or code for legend background.}
\item{title_font}{string; main title font name choosen from https://fonts.google.com/.}
\item{title_font_scale}{numeric value to scale up/down the main title font size, 1 means no scaling.}
\item{title_font_face}{string with the main title font type. Choose between c('plain', 'bold', 'italic', 'bold.italic').}
\item{title_font_color}{string with color name or code with main title font color.}
\item{title_hjust}{numeric in [0,1] with main title horizontal justification (0.5 is center).}
\item{names_font}{string; province names font name choosen from https://fonts.google.com/.}
\item{names_font_scale}{numeric value to scale up/down province names font size, 1 means no scaling.}
\item{names_font_face}{string with province names font type. Choose between c('plain', 'bold', 'italic', 'bold.italic').}
\item{names_font_color}{string with color name or code for province names font.}
\item{names_back_color}{string with color name or code for province names background.}
\item{values_preffix}{string with preffix that will be added to value labels.}
\item{values_suffix}{string with suffix that will be added to value labels.}
\item{values_big_mark}{character; mark between every big.interval decimals before (hence big) the decimal point.}
\item{values_dec_places}{numeric; number of decimal places for value labels.}
\item{values_font}{string; value labels font name choosen from https://fonts.google.com/.}
\item{values_font_scale}{numeric value to scale up/down value labels font size, 1 means no scaling.}
\item{values_font_face}{string with value labels font type, choosen between c('plain', 'bold', 'italic', 'bold.italic').}
\item{values_font_color}{string with color name or code for value labels font.}
\item{values_back_color}{string with color name or code for value labels background.}
\item{footer}{string with a footer/caption}
\item{footer_position}{length-2 numeric vector in [0,1] interval. Footer relative position.}
\item{footer_font}{string; footer font name choosen from https://fonts.google.com/.}
\item{footer_font_scale}{numeric value to scale up/down the footer font size, 1 means no scaling.}
\item{footer_font_face}{string with footer font type. Choose between c('plain', 'bold', 'italic', 'bold.italic').}
\item{footer_font_color}{string with color name or code for footer font.}
\item{province_line_color}{string with color name or code for province limits line.}
\item{logo_filename}{string with full path filename of a PNG logo image to insert in PDF output.}
\item{logo_position}{length-2 numeric vector in [0,1] interval.  Relative position of PNG logo image.}
\item{logo_scale}{numeric value to scale up/down the PNG logo image size, 1 means no scaling.}
\item{logo_aspect_ratio}{numeric. Relative 'y' size with respect to 'x' size of PNG logo image, 1 means a square image.}
}
\value{
NULL (PDF file created)
}
\description{
This function creates a PDF printer ready file with Ecuador choropleth map using provided data at province level. No GIS knowledge is required. Provide just a named vector of numeric data. Geographic data will be internally added.

A PDF map can be created using default values simply calling the function only with named values vector as unique argument (see examples).

A lot of options are available for controlling data and customizing titles, labels, fonts, sizes and colors. See https://fonts.google.com/ for available fonts.

Base map shape files were downloaded from:
https://www.ecuadorencifras.gob.ec/documentos/web-inec/Cartografia/Clasificador_Geografico/2015/

Developed by Jorge Albuja (albuja@hyahoo.com)
}
\examples{
# Choropleth map with dummy data

values <- round(runif(24, 10, 1000), 3)
names(values) <- 1:24
choropleth_map_prov_pdf(values)

# Changing bin number
choropleth_map_prov_pdf(values, bins = 5)
}
