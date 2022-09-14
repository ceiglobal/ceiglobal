#' Additional Themes and Theme Components for 'ggplot2'
#'
#' A theme for 'ggplot2' based on the cei style guide
#'
#' The core theme: `theme_ceiglobal` is awesome.
#'
#' @md
#' @name ceiglobal
#' @docType package
#' @author Dave Taylor (david.taylor@@ceiglobal.org)
#' @keywords internal
#' @import tidyverse
#' @import ggplot2
#' @import grid
#' @import scales
#' @import extrafont
#' @import grDevices
#' @import unikn
#' @import rmarkdown
#' @import magrittr
#' @import htmltools
#' @import knitr
#' @importFrom magrittr %>%
#' @importFrom tools file_path_sans_ext
NULL

#' ceiglobal exported operators
#'
#' The following functions are imported and then re-exported
#' from the ceiglobal package to enable use of the magrittr
#' pipe operator with no additional library calls
#'
#' @name ceiglobal-exports
NULL

#' @name %>%
#' @export
#' @rdname ceiglobal-exports
NULL
#'
#' A [ggplot2] theme consistent with the new CEI style guide developed by Effusion
#'
#' @md
#' @section Building upon `theme_ceiglobal`:
#' The function is setup in such a way that you can customize your own one by just
#' wrapping the call and changing the parameters. See source for examples.
#'
#' @section Gotchas:
#' There are distinctions between font names and various devices. Names that work
#' for display graphics devices and bitmap ones such as `png` may not work well
#' for PostScript or PDF ones. You may need two versions of a font-based
#' theme function for them to work in a particular situation. This situation
#' usually only arises when using a newer font with many weights but somewhat
#' irregular internal font name patterns.
#'
#' There is an option `ceiglobal.loadfonts` which -- if set to `TRUE` -- will
#' call `extrafont::loadfonts()` to register non-core fonts with R PDF & PostScript
#' devices. If you are running under Windows, the package calls the same function
#' to register non-core fonts with the Windows graphics device.
#'
#' @md
#' @param base_family,base_size base font family and size
#' @param plot_title_family,plot_title_face,plot_title_size,plot_title_margin plot title family, face, size and margi
#' @param subtitle_family,subtitle_face,subtitle_size plot subtitle family, face and size
#' @param subtitle_margin plot subtitle margin bottom (single numeric value)
#' @param strip_text_family,strip_text_face,strip_text_size,strip_text_colour facet label font family, face, size and colour
#' @param strip_background facet title background
#' @param panel_background panel background colour
#' @param panel_border panel border colour
#' @param strip_colour facet title background line
#' @param caption_family,caption_face,caption_size,caption_margin plot caption family, face, size and margin
#' @param axis_title_family,axis_title_face,axis_title_size axis title font family, face and size
#' @param axis_title_just axis title font justification, one of `[blmcrt]`
#' @param plot_margin plot margin (specify with `ggplot2::margin()`)
#' @param grid_col,axis_col grid & axis colors; both default to `#cccccc`
#' @param grid panel grid (`TRUE`, `FALSE`, or a combination of `X`, `x`, `Y`, `y`)
#' @param axis_text_size font size of axis text
#' @param axis add x or y axes? `TRUE`, `FALSE`, "`xy`"
#' @param ticks ticks if `TRUE` add ticks
#' @export
#' @examples \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Scatterplot
#'
#' mtcars %>%
#'   ggplot() +
#'   aes(x = mpg, y = wt)) +
#'   geom_point() +
#'   labs(x="Fuel effiiency (mpg)",
#'        y="Weight (tons)",
#'        title="Seminal ggplot2 scatterplot example",
#'        subtitle="A plot that is only useful for demonstration purposes",
#'        caption="Brought to you by the letter 'g'") +
#'   theme_ceiglobal()
#'
#' # Bar chart
#'
#' update_geom_font_defaults()
#'
#' mtcars %>%
#'    mutate(cyl = factor(cyl)) %>%
#'    ggplot() +
#'    aes(x = cyl, y = wt, fill = cyl) +
#'    geom_bar(stat = "identity") +
#'    labs(x="Number of cylinders)", y="Weight (tons)",
#'       title="Seminal ggplot2 bar chart example",
#'       subtitle="A plot that is only useful for demonstration purposes",
#'       caption="Brought to you by the letter 'g'") +
#'    theme_ceiglobal() +
#'    theme(axis.text.y=element_blank())
#' }

theme_ceiglobal <- function(base_family="Apercu Pro", base_size = 12,
                        plot_title_family=base_family,
                        plot_title_size = 13,
                        plot_title_face="bold",
                        plot_title_margin = 12,
                        plot_background_fill = "white",
                        plot_background_line_colour = "white",
                        plot_background_line_weight = 0,
                        panel_background = cei_grey,
                        panel_border = cei_grey,
                        subtitle_family=base_family,
                        subtitle_size = 12,
                        subtitle_face = "plain",
                        subtitle_margin = 12,
                        strip_text_family = base_family,
                        strip_text_size = 12,
                        strip_text_face = "bold",
                        strip_text_colour = "black",
                        strip_background = "white",
                        strip_colour = "white",
                        caption_family = base_family,
                        caption_size = 9,
                        caption_face = "italic",
                        caption_margin = 10,
                        axis_text_size = base_size-1,
                        axis_text_colour = "black",
                        axis_text_face = "bold",
                        axis_title_family = subtitle_family,
                        axis_title_size = 12,
                        axis_title_face = "bold",
                        axis_title_just = "rt",
                        plot_margin = margin(base_size/2, base_size/2, base_size/2, base_size/2),
                        grid_col = cei_grey, grid = TRUE,
                        axis_col = cei_grey, axis = FALSE, ticks = FALSE
                        ) {

  ret <- ggplot2::theme_minimal(base_family=base_family, base_size=base_size)

  ret <- ret + theme(legend.background=element_blank())
  ret <- ret + theme(legend.key=element_blank())
  ret <- ret + theme(panel.background = element_rect(
    fill = panel_background,
    colour = "white"))
  ret <- ret + theme(panel.border = element_blank())
  ret <- ret + theme(axis.line = element_blank())

  if (inherits(grid, "character") | grid == TRUE) {

    ret <- ret + theme(panel.grid=element_blank())
    ret <- ret + theme(panel.grid.major=element_blank())
    ret <- ret + theme(panel.grid.minor=element_blank())

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + theme(panel.grid=element_blank()) #theme(panel.grid.major.x=element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + theme(panel.grid=element_blank()) #theme(panel.grid.major.y=element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + theme(panel.grid=element_blank()) #theme(panel.grid.minor.x=element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + theme(panel.grid=element_blank()) #theme(panel.grid.minor.y=element_blank())
    }

  } else {
    ret <- ret + theme(panel.grid=element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {

    ret <- ret + theme(axis.line=element_blank())
        if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + theme(axis.line.x=element_blank())
      } else {
        ret <- ret + theme(axis.line.x=element_blank())
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + theme(axis.line.y=element_blank())
      } else {
        ret <- ret + theme(axis.line.y=element_blank())
      }
    } else {
      ret <- ret + theme(axis.line.x=element_blank())
      ret <- ret + theme(axis.line.y=element_blank())
    }
  } else {
    ret <- ret + theme(axis.line=element_blank())
  }

  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
    ret <- ret + theme(axis.ticks.x = element_blank())
    ret <- ret + theme(axis.ticks.y = element_blank())
  } else {
    ret <- ret + theme(axis.ticks = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.x = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.y = element_line(size=0.15))
    ret <- ret + theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  #xj <- switch(tolower(substr(axis_title_just, 1, 1)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)
  #yj <- switch(tolower(substr(axis_title_just, 2, 2)), b=0, l=0, m=0.5, c=0.5, r=1, t=1)

  ret <- ret + theme(axis.text.x=element_text(
    size=axis_text_size,
    colour=axis_text_colour,
    vjust=0.9,
    margin=margin(t=0.0)))

  ret <- ret + theme(axis.text.y=element_text(
    size=axis_text_size,
    colour=axis_text_colour,
    vjust=0.75,
    margin=margin(r=0.0)))

  ret <- ret + theme(axis.title=element_text(
    size=axis_title_size,
    family=axis_title_family))

  ret <- ret + theme(axis.title.x=element_text(
    size=axis_title_size,
    family=axis_title_family,
    colour=axis_text_colour,
    face=axis_title_face))

  ret <- ret + theme(axis.title.y=element_text(
    size=axis_title_size,
    family=axis_title_family,
    colour=axis_text_colour,
    face=axis_title_face))

  ret <- ret + theme(axis.title.y.right=element_text(
    size=axis_title_size,
    angle=90,
    family=axis_title_family,
    colour=axis_text_colour,
    face=axis_title_face))

  ret <- ret + theme(strip.text=element_text(
    size=strip_text_size,
    face=strip_text_face,
    family=strip_text_family,
    colour = strip_text_colour))

  ret <- ret + theme(strip.background = element_rect(
    fill = strip_background,
    colour = strip_colour))

  ret <- ret + theme(panel.spacing=grid::unit(2.5, "pt"))

  ret <- ret + theme(plot.background = element_rect(
    fill = plot_background_fill,
    colour = plot_background_line_colour,
    size = plot_background_line_weight))

  ret <- ret + theme(plot.title=element_text(
    size=plot_title_size,
    margin=margin(b=plot_title_margin),
    family=plot_title_family,
    face=plot_title_face))

  ret <- ret + theme(plot.subtitle=element_text(
    size=subtitle_size,
    margin=margin(b=subtitle_margin),
    family=subtitle_family,
    face=subtitle_face))

  ret <- ret + theme(plot.caption=element_text(
    hjust=1, size=caption_size,
    margin=margin(t=caption_margin),
    family=caption_family,
    face=caption_face))

  ret <- ret + theme(plot.margin=plot_margin)

  ret

}

#' Update matching font defaults for text geoms
#'
#' Updates [ggplot2::geom_label] and [ggplot2::geom_text] font defaults
#'
#' @param family,face,size,color font family name, face, size and color
#' @export
update_geom_font_defaults <- function(family="Apercu Pro",
                                      face="plain", size=3.5,
                                      color = "#2b2b2b") {
  update_geom_defaults("text", list(family=family, face=face, size=size, color=color))
  update_geom_defaults("label", list(family=family, face=face, size=size, color=color))
}

#' Import Aperco Pro for use in charts
#'
#' Aperco Pro a trademark of Colophon Foundry
#'
#' There is an option `ceiglobal.loadfonts` which -- if set to `TRUE` -- will
#' call `extrafont::loadfonts()` to register non-core fonts with R PDF & PostScript
#' devices. If you are running under Windows, the package calls the same function
#' to register non-core fonts with the Windows graphics device.
#'
#' @md
#' @note This will take care of ensuring PDF/PostScript usage. The location of the
#'   font directory is displayed after the base import is complete. It is highly
#'   recommended that you install them on your system the same way you would any
#'   other font you wish to use in other programs.
#' @export
import_aperco_pro <- function() {

  ap_font_dir <- system.file("fonts", "apercu-pro", package="ceiglobal")

  # suppressWarnings(suppressMessages(extrafont::font_import(tw_font_dir, prompt=FALSE)))
  #
  # message(
  #   sprintf(
  #     "You will likely need to install these fonts on your system as well.\n\nYou can find them in [%s]",
  #     ap_font_dir)
  # )

}

#' @rdname `Apercu Pro`
#' @md
#' @title Aperçu font name R variable aliases
#' @description `font_apercu_pro` == "`Apercu Pro`"
#' @format length 1 character vector
#' @export
font_apercu_pro <- "Apercu Pro"

#' @md
#' @section
#' This section details the colour scheme used in `theme_ceiglobal`
#' @param cei_colours List of hex codes for CEI colour palette from style guide
#' @param cei_grey CEI grey colour from style guide
#' @param cei_colour_palette List of named hex codes for CEI colour palette from style guide
#' @param cei_colour_2 two toned palette for figures using CEI style guide
#' @param cei_colour_3 three toned palette for figures using CEI style guide
#' @param cei_colour_4 four toned palette for figures using CEI style guide
#' @param cei_colour_5 five toned palette for figures using CEI style guide
#' @param cei_colour_6 six toned palette for figures using CEI style guide
#' @param cei_colour_7 seven toned palette for figures using CEI style guide
#' @param cei_colour_8 eight toned palette for figures using CEI style guide
#' @param cei_colour_9 nine toned palette for figures using CEI style guide
#' @param cei_colour_10 ten toned palette for figures using CEI style guide
#' @param cei_colour_11 eleven toned palette for figures using CEI style guide
#' @param cei_colour_12 twelve toned palette for figures using CEI style guide
#'
#' @rdname cei_colours
#' @md
#' @title List of hex codes for CEI colour palette from style guide
#' @format list
#' @export
cei_colours <- c(
  rgb(0, 169, 143, maxColorValue = 255),  # green
  rgb(17, 206, 170, maxColorValue = 255), # light green
  rgb(0, 154, 218, maxColorValue = 255),  # blue
  rgb(89, 86, 165, maxColorValue = 255),  # purple
  rgb(65, 65, 150, maxColorValue = 255), # dark purple
  rgb(0, 120, 201, maxColorValue = 255)#, # dark blue
  #rgb(22, 33, 31, maxColorValue = 255), # dark grey
  #rgb(242, 242, 242, maxColorValue = 255) # light grey
  #rgb(255, 246, 164, maxColorValue = 255) # yellow
  )

#' @rdname cei_purple
#' @md
#' @title CEI purple colour from style guide
#' @format length 1 character vector
#' @export
cei_purple <- rgb(89, 86, 165, maxColorValue = 255)

#' @rdname cei_green
#' @md
#' @title CEI purple colour from style guide
#' @format length 1 character vector
#' @export
cei_green <- rgb(0, 169, 143, maxColorValue = 255)

#' @rdname cei_grey
#' @md
#' @title CEI grey colour from style guide
#' @format length 1 character vector
#' @export
cei_grey <- rgb(242, 242, 242, maxColorValue = 255)

#' @rdname cei_dark_purple
#' @md
#' @title CEI dark purple colour from style guide
#' @format length 1 character vector
#' @export
cei_dark_purple <- rgb(65, 65, 150, maxColorValue = 255)

#' @rdname cei_dark_blue
#' @md
#' @title CEI dark blue colour from style guide
#' @format length 1 character vector
#' @export
cei_dark_blue <- rgb(0, 120, 201, maxColorValue = 255)

#' @rdname cei_blue
#' @md
#' @title CEI blue colour from style guide
#' @format length 1 character vector
#' @export
cei_blue <- rgb(0, 154, 218, maxColorValue = 255)

#' @rdname cei_light_green
#' @md
#' @title CEI light green from style guide
#' @format length 1 character vector
#' @export
cei_light_green <- rgb(17, 206, 170, maxColorValue = 255)

#' @rdname cei_yellow
#' @md
#' @title CEI yellow from style guide
#' @format length 1 character vector
#' @export
cei_yellow <- rgb(255, 246, 164, maxColorValue = 255)

#' @rdname cei_line_grey
#' @md
#' @title CEI line grey from style guide
#' @format length 1 character vector
#' @export
cei_line_grey <- rgb(216, 216, 216, maxColorValue = 255)

#' @rdname cei_colour_palette
#' @md
#' @title List of named hex codes for CEI colour palette from style guide
#' @format list
#' @export
cei_colour_palette <- unikn::newpal(col = cei_colours, names = c("green", "light green", "blue", "purple", "dark purple", "dark blue"))

#' @rdname cei_colour_2
#' @md
#' @title CEI colour palette with two colours
#' @format list
#' @export
cei_colour_2 <- c("#00A98F", "#5956A5")

#' @rdname cei_colour_3
#' @md
#' @title CEI colour palette with three colours
#' @format list
#' @export
cei_colour_3 <- c("#00A98F", "#009ADA", "#5956A5")

#' @rdname cei_colour_4
#' @md
#' @title CEI colour palette with four colours
#' @format list
#' @export
cei_colour_4 <- c("#00A98F", "#009ADA", "#5956A5", "#0078C9")

#' @rdname cei_colour_5
#' @md
#' @title CEI colour palette with two colours
#' @format list
#' @export
cei_colour_5 <- unikn::seecol(cei_colour_palette, n = 5)

#' @rdname cei_colour_6
#' @md
#' @title CEI colour palette with six colours
#' @format list
#' @export
cei_colour_6 <- unikn::seecol(cei_colour_palette, n = 6)

#' @rdname cei_colour_7
#' @md
#' @title CEI colour palette with seven colours
#' @format list
#' @export
cei_colour_7 <- unikn::seecol(cei_colour_palette, n = 7)

#' @rdname cei_colour_8
#' @md
#' @title CEI colour palette with eight colours
#' @format list
#' @export
cei_colour_8 <- unikn::seecol(cei_colour_palette, n = 8)

#' @rdname cei_colour_9
#' @md
#' @title CEI colour palette with nine colours
#' @format list
#' @export
cei_colour_9 <- unikn::usecol(cei_colour_palette, n = 9)

#' @rdname cei_colour_10
#' @md
#' @title CEI colour palette with ten colours
#' @format list
#' @export
cei_colour_10 <- unikn::usecol(cei_colour_palette, n = 10)

#' @rdname cei_colour_11
#' @md
#' @title CEI colour palette with ten colours
#' @format list
#' @export
cei_colour_11 <- unikn::usecol(cei_colour_palette, n = 11)

## TESTING

 library(tidyverse)

  #test_plot <- mtcars %>%
  #  ggplot() +
  #  aes(x = gear, y = mpg, fill = factor(gear)) +
  #  scale_fill_manual(values = cei_colour_3) +
  #  geom_bar(stat = "identity") +
  #  facet_wrap(~cyl, ncol = 1) +
  #  theme_ceiglobal()
  #test_plot

  # mtcars %>%
  #   ggplot() +
  #    aes(
  #      x = wt,
  #    y = mpg,
  #      label = rownames(mtcars)) +
  #    geom_text(family = "Apercu Pro",
  #              colour = cei_blue) +
  #   geom_point(colour = cei_purple) +
  #   theme_ceiglobal()


 #library(devtools)

 #options(buildtools.check = function(action) TRUE ) # suppresses RStudio warning
 #document() # update documents
 #check() # perform checks
 #load_all() # load new version
