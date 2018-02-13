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
      field,
      " IN (",
      stringi::stri_join(purrr::map_chr(conds, ~stringi::stri_join("'", .x, "'", sep="")),
                         sep=" ", collapse=","),
      ") ",
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

#' Execute SQL request at remote ClickHouse, analyze and process errors in Shiny context
#'
#' All
#' @param conn ClickHouse connection object
#' @param req Formatted SQL request (string)
#' @return Data.frame recieved from ClickHouse
#' @export
bad_getChQuery <- function(conn, req){
  checkmate::qassert(req, "S=1")

  futile.logger::flog.info(flue::glue("DB request: {req}"))
  tictoc::tic()
  resp <- purrr::safely(dbGetQuery)(conn, req)
  if(is.null(resp$result)) {
    futile.logger::flog.error(glue("CH request error: '{resp$error}'"))
    shiny::showNotification("Некорректный ответ от бэкенда", type="error")
    return(NULL)
  }

  # colums must be defined but values can be absent
  df <-  checkmate::assertDataFrame(resp$result, all.missing=TRUE, min.cols=1)

  futile.logger::flog.info(glue::glue("---------- RAW data query: {capture.output(tictoc::toc())} ----------"))
  futile.logger::flog.info(glue::glue("Loaded {nrow(df)} rows"))
  futile.logger::flog.info(glue::glue("Table: {capture.output(head(df, 2))}"))

  df
}

# lintr::lint("./R/build_req_filter.R")
