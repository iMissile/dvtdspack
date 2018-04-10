#' Construct separate condition of SQL request filter
#'
#' Construct separate condition of SQL request filter.
#' Conditions can be vector.
#'
#' @param field DB field to include in filter restriction
#' @param conds Character vector of permitted field values. In case conditions
#'   are absent (NULL/NA or 'all' value) any restrictions will be omitted
#' @param add Type of unfolding. TRUE value shows that filter will be a part of
#'   complex restriction and should be added
#' @return Filter condition unfolded into string
#' @export
buildReqFilter <- function(field, conds, add=TRUE){
  # Входные ограничения могут отсутствовать, поэтому делается проверка, а не ассерт
  # Допустимо либо значение "all", либо string vector, но никак не числа
  # теоретически могут приходить от Shiny элементов NA, NULL, character(0)

  ret <- " "
  if (checkmate::qtest(conds, "S+") & all(conds!="all")){
    ret <- stringi::stri_join(
      ifelse(add, " AND ", " "),
      dbplyr::escape(unname(field), collapse=NULL, parens=FALSE),
      " IN ",
      dbplyr::escape(unname(conds), collapse=", ", parens=TRUE),
      # stringi::stri_join(purrr::map_chr(conds, ~stringi::stri_join("'", .x, "'", sep="")),
      #                   sep=" ", collapse=","),
      #") ",
      sep="", collapse=""
    )
  }
  ret
}

#' Build SQL request restriction based on user-defined set of fields
#'
#' All
#' @param begin Start date (strictly Date class)
#' @param end End date, (strictly Date class)
#' @param min_duration Minimal watch duration (seconds) to include watch event
#'   in statistics. All below will be declined
#' @param max_duration Maximal watch duration (seconds) to include watch event
#'   in statistics. All above will be declined
#' @param region Region filter (string vector)
#' @param prefix Prefix filter (string vector)
#' @param channel Channel filter (string vector)
#' @param event Event filter (string vector)
#' @param segment Segment filter (string vector)
#' @param serial_mask String to find in serial number whithin \%LIKE\% condition
#' @return Limits unfolded into string
#' @export
buildReqLimits <- function(begin, end, min_duration=0*60, max_duration=12*60*60,
                           region=NULL, prefix=NULL, channel=NULL, event=NULL,
                           segment=NULL, serial_mask="") {
  # region, prefix, channel -- могут быть векторами
  # Убедимся, что на вход поступают допустимые значения
  # checkmate::assertNames(names(params), subset.of=c("region", "prefix", "segment", "channel", "event"))
  # checkmate::checkDate(c(lubridate::ymd("17-12-03"), lubridate::ymd("17-12-07")), any.missing=FALSE, len=2, null.ok=FALSE)
  checkmate::assertDate(begin, any.missing=FALSE, len=1, null.ok=FALSE)
  checkmate::assertDate(end, any.missing=FALSE, len=1, null.ok=FALSE)
  checkmate::qassert(serial_mask, "S=1")

  res <- stringi::stri_join(
    stringi::stri_join(" date>='", begin, "' AND date<='", end, "'", sep=""),
    stringi::stri_join("AND duration>=", min_duration, " AND duration<=", max_duration),
    buildReqFilter("region", region, add=TRUE),
    buildReqFilter("prefix", prefix, add=TRUE),
    buildReqFilter("segment", segment, add=TRUE),
    buildReqFilter("channelId", channel, add=TRUE),
    buildReqFilter("switchevent", event, add=TRUE),
    ifelse(serial_mask=="", "", stringi::stri_join("AND like(serial, '%", serial_mask, "%') ")),
    sep=" ")
  # нормализуем пробелы
  stringi::stri_replace_all_regex(res, "(\\s+)", " ")
}

#' Build SQL request restriction based on user-defined set of fields
#'
#' All
#' @param begin Start date (strictly Date class)
#' @param end End date, (strictly Date class)
#' @param min_duration Minimal watch duration (seconds) to include watch event
#'   in statistics. All below will be declined
#' @param max_duration Maximal watch duration (seconds) to include watch event
#'   in statistics. All above will be declined
#' @param serial_mask String to find in serial number whithin \%LIKE\% condition
#' @param ... List with additional named parameters "db_field=string vector"
#' @return Limits unfolded into string
#' @export
buildReqLimitsExt <- function(begin, end, min_duration=0*60, max_duration=12*60*60, serial_mask="", ...) {
  # ... -- могут быть векторами
  lvals <- rlang::dots_list(...)
  # Убедимся, что на вход поступают допустимые значения
  checkmate::assertDate(begin, any.missing=FALSE, len=1, null.ok=FALSE)
  checkmate::assertDate(end, any.missing=FALSE, len=1, null.ok=FALSE)
  checkmate::qassert(serial_mask, "S=1")

  res <- stringi::stri_join(
    stringi::stri_join(" date>='", begin, "' AND date<='", end, "'", sep=""),
    stringi::stri_join("AND duration>=", min_duration, " AND duration<=", max_duration),
    stringi::stri_join(purrr::map2_chr(names(lvals), lvals,
                                       ~buildReqFilter(field=.x, conds=.y, add=TRUE)), collapse=" "),
    ifelse(serial_mask=="", "", stringi::stri_join("AND like(serial, '%", serial_mask, "%') ")),
    sep=" ", collapse=" ")
  # нормализуем пробелы
  stringi::stri_replace_all_regex(res, "(\\s+)", " ")
}

#' Build SQL request restriction based on user-defined set of fields
#'
#' @importFrom magrittr %>%
#'
#' @param dates - a named list of date ranges [min, max],
#' names must be the names of the corresponding DB fields
#' @param ranges - a named list of numeric ranges [min, max],
#' names must be the names of the corresponding DB fields
#' @param masks - named vector of match string
#' @param ... - set of named dictionary string values (atomic or vector)
#'
#' @return escaped SQL request string
#' @export
buildReqLimitsExt2 <- function(dates=NULL, ranges=NULL, masks="", ...) {
  # ... -- могут быть векторами
  # Убедимся, что на вход поступают допустимые значения
  checkmate::assertList(dates, types="Date", any.missing=FALSE, unique=FALSE, null.ok=FALSE)
  checkmate::assertList(ranges, any.missing=FALSE, unique=FALSE, null.ok=FALSE)
  checkmate::qassert(masks, "S>=1")
  lvals <- rlang::dots_list(...)

  dates_part <- dates %>%
    purrr::map2(names(.), ~glue::glue("{.y} BETWEEN '{.x[1]}' AND '{.x[2]}'"))
  ranges_part <- ranges %>%
    purrr::map2(names(.), ~glue::glue("{.y} BETWEEN {.x[1]} AND {.x[2]}"))
  # если маска не одна, а несколько, но они все пустые, то будет коллизия по длинам
  masks_part <- tibble::enframe(masks) %>%
    glue::glue_data("AND like({name}, '%{value}%')") %>%
    stringi::stri_join(collapse=" ")
  # print(glue("dates_part={dates_part}, masks_part={masks_part}"))
  # browser()
  # dates\ranges limits must be aligned to level1

  res <- stringi::stri_join(
    stringi::stri_join(unlist(c(dates_part, ranges_part)), collapse=" AND "),
    stringi::stri_join(purrr::map2_chr(names(lvals), lvals,
                                       ~buildReqFilter(field=.x, conds=.y, add=TRUE)), collapse=" "),
    dplyr::if_else(masks=="", "", masks_part),
    sep=" ", collapse=" ")
  # trim whitespaces
  stringi::stri_replace_all_regex(res, "(\\s+)", " ")  %>%
    trimws()
}

#' Build SQL request restriction based on user-defined set of fields
#'
#' @importFrom magrittr %>%
#' @importFrom magrittr %T>%
#'
#' @param dates - a tibble with date ranges: [name, min, max],
#' names must be the names of the corresponding DB fields
#' @param ranges - a tibble with numeric ranges: [name, min, max],
#' names must be the names of the corresponding DB fields
#' @param masks - a tibble with masks values: [name, value]
#' @param ... - set of named dictionary string values (atomic or vector)
#'
#' @return escaped SQL request string
#' @export
buildReqLimitsExt3 <- function(dates=NULL, ranges=NULL, masks=NULL, ...) {
  # ... -- могут быть векторами
  lvals <- rlang::dots_list(...)

  # Убедимся, что на вход поступают допустимые значения
  dates_part <- if(!is.null(dates)) {
    checkmate::assertDataFrame(dates, ncols=3) %T>%
      {checkmate::assertNames(names(.), type="unique", must.include=c("name", "min", "max"))} %>%
      # assertr::assert(assertr::has_all_names("name", "min", "max")) %>%
      # экранируем символы, на всякий случай принимаем меры предосторожности
      dplyr::mutate_at(dplyr::vars(name, min, max), dbplyr::escape, collapse=NULL, parens=FALSE) %>%
      glue::glue_data("{name} BETWEEN {min} AND {max}")
  } else { character(0) }

  ranges_part <- if(!is.null(ranges)) {
    checkmate::assertDataFrame(ranges, ncols=3) %T>%
    {checkmate::assertNames(names(.), type="unique", must.include=c("name", "min", "max"))} %>%
      # экранируем символы, на всякий случай принимаем меры предосторожности
      dplyr::mutate_at(dplyr::vars(name, min, max), dbplyr::escape, collapse=NULL, parens=FALSE) %>%
      glue::glue_data("{name} BETWEEN {min} AND {max}")
  } else { character(0) }

  # объединим все диапазоны, после glue мы имеем character vector
  granges <- stringi::stri_join(c(dates_part, ranges_part), collapse=" AND ")

  masks_part <- if(!is.null(masks)) {
    checkmate::assertDataFrame(masks, ncols=2) %T>%
    {checkmate::assertNames(names(.), type="unique", must.include=c("name", "value"))} %>%
      dplyr::filter(value!="") %>%
      dplyr::mutate(like_value=paste0("%", value, "%")) %>%
      # экранируем символы, на всякий случай принимаем меры предосторожности
      dplyr::mutate_at(dplyr::vars(name, like_value), dbplyr::escape, collapse=NULL, parens=FALSE) %>%
      glue::glue_data("AND like({name}, {like_value})") %>%
      stringi::stri_join(collapse=" ")
  } else { character(0) }

  # browser()
  # escape в применении к спискам дает конструкцию "val AS name(val)"
  params_part <- purrr::map2_chr(names(lvals), unname(lvals),
                                 ~buildReqFilter(field=.x, conds=.y, add=TRUE)) %>%
    stringi::stri_join(collapse=" ")

  # собираем все воедино
  res <- stringi::stri_join(
    ifelse(identical(granges, character(0)), "", granges),
    ifelse(identical(params_part, character(0)), "", params_part),
    ifelse(identical(masks_part, character(0)), "", masks_part),
    sep=" ", collapse=" ") %>%
    # trim whitespaces
    stringi::stri_replace_all_regex("(\\s+)", " ")  %>%
    trimws()
}

# lintr::lint("./R/build_req_filter.R")
