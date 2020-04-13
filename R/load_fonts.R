.onAttach <- function(libname, pkgname) {

  if (.Platform$OS.type == "windows")  { # nocov start
    if (interactive()) packageStartupMessage("Registering Windows fonts with R")
    extrafont::loadfonts("win", quiet = TRUE)
  }

  if (getOption("ceiglobal.loadfonts", default = FALSE)) {
    if (interactive()) packageStartupMessage("Registering PDF & PostScript fonts with R")
    extrafont::loadfonts("pdf", quiet = TRUE)
    extrafont::loadfonts("postscript", quiet = TRUE)
  }

  fnt <- extrafont::fonttable()
  if (!any(grepl("Raleway", fnt$FamilyName))) {
    packageStartupMessage("NOTE: Raleway font is required to use this theme.")
    packageStartupMessage("      The font will have been downloaded with this package and should be installed automatically.")
    packageStartupMessage("      If this does not occur, please source from CEI's dropbox and install the font manually")
  } # nocov end

}
