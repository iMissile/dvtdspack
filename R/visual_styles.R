#' Custom ggplot theme based on ipsum-theme
#'
#' @importFrom hrbrthemes theme_ipsum_rc
#'
#' @param target Target for plot. May be on of "screen" or "word_A4". Target will
#'   affect on size, font, colour, resolution, etc.
#' @export
theme_dvt <- function(target="screen"){
  checkmate::assertChoice(target, c("screen", "word_A4"))
  # flog.info(paste0("Theme target is '", target, "'"))

  ret <- purrr::when(
    target,
    .=="screen"  ~ theme_ipsum_rc(base_size=20, axis_title_size=18,
                                  subtitle_size=15, strip_text_size=16),
    .=="word_A4" ~ theme_ipsum_rc(base_size=14, axis_title_size=12,
                                  subtitle_size=11, strip_text_size=10),
    ~ theme_ipsum_rc()
  )

  # залогируем для контроля прохождения изменения
  # flog.info(paste0("base_size=", ret$text$size))
  # flog.info(paste0("DVT theme ret size: ", capture.output(pryr::object_size(ret))))

  ret # + theme(axis.text.x = element_text(angle=90))
}

##' Cairo initialization in linux with Roboto Condensed font
##'
#' @export
initRCfontInCairo <- function(){
  if (Sys.info()["sysname"] == "Linux") {
    # Server environment
    Cairo::CairoFonts(
      regular="Roboto Condensed:style=Regular",
      bold="Roboto Condensed:style=Bold",
      italic="Roboto Condensed:style=Italic",
      bolditalic="Roboto Condensed:style=Bold Italic,BoldItalic",
      symbol="Symbol"
    )
  } else {
    # local development, windows
    # ?? extrafont::font_import() can save us??
    # windowsFonts(Verdana="TT Verdana")
    # par(family="Roboto Condensed")
    # par(family="Times")
  }
}
