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
  } else {
    ret <- " "
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
buildReqLimits <- function(begin, end, min_duration=0*60, max_duration=2*60*60,
                           region=NULL, prefix=NULL, channel=NULL, event=NULL,
                           segment=NULL, serial_mask="") {
  # region, prefix, channel -- могут быть векторами
  # params <- list(...)
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
    buildReqFilter("switchEvent", event, add=TRUE),
    ifelse(serial_mask=="", "", c("AND like(serial, '%", serial_mask, "%') ")),
    sep=" ")

  # нормализуем пробелы
  stringi::stri_replace_all_regex(res, "(\\s+)", " ")
}


# lintr::lint("./R/build_req_filter.R")
