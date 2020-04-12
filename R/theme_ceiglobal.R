## DECLARE PLOTTING FUNCTION

#' Title
#'
#' @param base_size
#' @param base_family
#' @param ticks
#'
#' @return
#' @export
#'
#' @examples
#'mtcars %>%
#'ggplot() +
#'  aes(x = cyl, y = mpg, fill = gear) +
#'  geom_bar(stat = "identity") +
#'  facet_grid(gear ~ vs) +
#'  theme_ceiglobal()

theme_ceiglobal <- function(base_size = 10, base_family = "Raleway", ticks = TRUE) {
  ret <- theme_bw(base_family=base_family, base_size=base_size) +
    theme(
      plot.subtitle = element_text(size=10, vjust =+0.75, family="Raleway"),
      axis.title.x = element_text(size=12, vjust =-0.25, family="Raleway"),
      axis.title.y = element_text(size=12, angle=90, vjust=0.75, family="Raleway"),
      plot.caption = element_text(face="italic", vjust=1, hjust=1, size = 10, family = "Raleway"),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      panel.background  = element_blank(),
      panel.border      = element_blank(),
      strip.text = element_text(face = "bold", colour = "white", size = 7.5),
      strip.background = element_rect(fill = cei_dark_grey),
      plot.background = element_rect(fill = cei_grey, colour = cei_dark_grey, size = 2),
      axis.line         = element_blank(),
      panel.grid = element_blank())
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret}

## SET COLOURS

library(unikn)

## CREATE CEI COLOUR PALETTE

cei_colours <- c(
  rgb(0, 169, 143, maxColorValue = 255),  # teal
  rgb(0, 154, 218, maxColorValue = 255),  # aqua
  rgb(89, 86, 165, maxColorValue = 255),  # purple
  rgb(41, 55, 117, maxColorValue = 255), # dark blue
  rgb(22, 33, 31, maxColorValue = 255))  # dark grey

cei_grey <- rgb(230, 230, 230, maxColorValue = 255)
cei_dark_grey <- rgb(22, 33, 31, maxColorValue = 255)

cei_colour_names <- c("teal", "aqua", "purple", "dark blue", "dark grey")

cei_colour_palette <- unikn::newpal(col = cei_colours, names = cei_colour_names)

cei_colour_2 <- unikn::seecol(cei_colour_palette, n = 2)
cei_colour_3 <- unikn::seecol(cei_colour_palette, n = 3)
cei_colour_4 <- unikn::seecol(cei_colour_palette, n = 4)
cei_colour_5 <- unikn::seecol(cei_colour_palette, n = 5)
cei_colour_6 <- unikn::seecol(cei_colour_palette, n = 6)
cei_colour_7 <- unikn::seecol(cei_colour_palette, n = 7)
cei_colour_8 <- unikn::seecol(cei_colour_palette, n = 8)
cei_colour_9 <- unikn::usecol(cei_colour_palette, n = 9)
cei_colour_10 <- unikn::usecol(cei_colour_palette, n = 10)
