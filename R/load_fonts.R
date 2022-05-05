.onAttach <- function(libname, pkgname) {

  ap_font_dir <- system.file("fonts", "apercu-pro", package="ceiglobal")

  if (.Platform$OS.type == "windows")  { # nocov start
    if (interactive()) packageStartupMessage("Registering Windows fonts with R")
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (getOption("ceiglobal.loadfonts", default = TRUE)) {
    if (interactive()) packageStartupMessage("Registering PDF & PostScript fonts with R")
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }

  fnt <- extrafont::fonttable()
  if (!any(grepl("Apercu Pro", fnt$FamilyName))) {
    packageStartupMessage("NOTE: Apercu Pro font is required to use this theme.")
    packageStartupMessage("      The font will have been downloaded with this package and should be installed automatically.")
    packageStartupMessage("      If this does not occur, please source online from https://www.cufonfonts.com/font/apercu-pro")
    packageStartupMessage("      (its free) and install the font manually")
  } # nocov end

}
