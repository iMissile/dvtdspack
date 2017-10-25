#' Custom ggplot theme based on ipsum-theme
#'
#' @importFrom futile.logger flog.info
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
