\name{choropleth_map_prov_pdf}
\alias{choropleth_map_prov_pdf}
\title{National by province coropleth map (PDF format)}
\usage{
 choropleth_map_prov_pdf(values,
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
                         logo_aspect_ratio = 1)
}
\arguments{
\item{values}{named numeric vector; province codes as names.}
\item{bins}{integer; number of bins to cut values into.}
\item{map_title}{string with general title.}
\item{filename}{string; output filename, must end with '.PDF' extension.}
\item{legend_title}{string with legend title.}
\item{map_x_size_cm}{numeric; size (cm) in x axis (y size is automatic).}
\item{bin_colors}{length-n string vector; must contain base colors names or codes to interpolate using grDevices::colorRampPalette().}
\item{background_color}{string with color name or code for map background.}
\item{legend_position}{length-2 numeric vector in [0,1] interval. Relative position of map legend.}
\item{title_font_scale}{numeric value to scale up/down the main title font size, 1 means no scaling.}
\item{title_font_face}{string with the main title font type. Choose between c('plain', 'bold', 'italic', 'bold.italic').}
\item{title_font_color}{string with color name or code with main title font color.}
\item{title_hjust}{numeric in [0,1] with main title horizontal justification (0.5 is center).}
\item{names_font_scale}{numeric value to scale up/down province names font size, 1 means no scaling.}
\item{names_font_face}{string with province names font type. Choose between c('plain', 'bold', 'italic', 'bold.italic').}
\item{names_font_color}{string with color name or code for province names font.}
\item{names_back_color}{string with color name or code for province names background.}
\item{values_preffix}{string with preffix that will be added to value labels.}
\item{values_suffix}{string with suffix that will be added to value labels.}
\item{values_big_mark}{character; used as mark between every big.interval decimals before (hence big) the decimal point.}
\item{values_dec_places}{numeric; number of decimal places for value labels.}
\item{values_font_scale}{numeric value to scale up/down province names font size, 1 means no scaling.}
\item{values_font_face}{string with value labels font type. Choose between c('plain', 'bold', 'italic', 'bold.italic').}
\item{values_font_color}{string with color name or code for value labels font.}
\item{values_back_color}{string with color name or code for value labels background.}
\item{footer}{string with a footer}
\item{footer_position}{length-2 numeric vector in [0,1] interval. Relative position of footer.}
\item{footer_font_scale}{numeric value to scale up/down the footer font size, 1 means no scaling.}
\item{footer_font_face}{string with footer font type. Choose between c('plain', 'bold', 'italic', 'bold.italic').}
\item{footer_font_color}{string with color name or code for footer font.}
\item{province_line_color}{string with color name or code for province limits line.}
\item{logo_filename}{string with full path filename of a PNG logo image to insert in PDF map}
\item{logo_position}{length-2 numeric vector in [0,1] interval.  Relative position of PNG logo image.}
\item{logo_scale}{numeric value to scale up/down the PNG logo image size, 1 means no scaling.}
\item{logo_aspect_ratio}{numeric. Relative 'y' size with respect to 'x' size of PNG logo image, 1 means a square image.}
}
\value{
A ggplot object
}
\description{
Creates PDF printer ready Ecuador choropleth map with user provided data. No GIS knowledge is required, just a named vector of numeric data. Geographic data is added internally.
}
\examples{
# Choropleth map with dummy data

indicador <- runif(24, 10, 1000)
names(indicador) <- 1:24

choropleth_map_prov_pdf(values = indicador,
                        bins = 3,
                        map_title = 'Indicador ABC en Ecuador',
                        filename = 'mapa.pdf',
                        legend_title = 'Indicador')
}
