#' Set of helpers to build SQL request
#' @name build_request
NULL

#' Build separate subcondition of SQL request filter
#'
#' Build separate subcondition of SQL request filter.
#' Conditions can be vector.
#'

#' @param field DB field to include in filter restriction
#' @param conds Character vector of permitted field values. In case conditions
#'   are absent (NULL/NA or 'all' value) any restrictions will be omitted
#' @param add Type of unfolding. TRUE value shows that filter will be a part of
#'   complex restriction and should be added
#' @return Unfolded into string filter condition
#' @rdname build_request
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
#' @param begin Start date-range
#' @param end End of date-range
#' @param ... Custom named params
#' @return Unfolded into string filter condition
#' @rdname build_request
#' @export
buildReqLimits <- function(begin, end, region=NULL, prefix=NULL, channel=NULL, event=NULL,
                           segment=NULL, serial_mask="") {
  # region, prefix, channel -- могут быть векторами
  # params <- list(...)
  # Убедимся, что на вход поступают допустимые значения
  # checkmate::assertNames(names(params), subset.of=c("region", "prefix", "segment", "channel", "event"))
  # checkmate::checkDate(c(lubridate::ymd("17-12-03"), lubridate::ymd("17-12-07")), any.missing=FALSE, len=2, null.ok=FALSE)
  checkmate::checkDate(begin, any.missing=FALSE, len=1, null.ok=FALSE)
  checkmate::checkDate(end, any.missing=FALSE, len=1, null.ok=FALSE)
  checkmate::qassert(serial_mask, "S=1")

  fields <- purrr::map2_chr(
    c("region", "prefix", "segment", "channelId", "switchEvent"),
    c(region, prefix, segment, channel, event),
    buildReqFilter, add=TRUE
  )

  res <- stringi::stri_join(" date>='", begin, "' AND date<='", end, "'",
                            # указали жестко длительность, в секундах
                            "AND duration>0*60 AND duration <2*60*60",
                            buildReqFilter("region", region, add=TRUE),
                            buildReqFilter("prefix", prefix, add=TRUE),
                            buildReqFilter("segment", segment, add=TRUE),
                            buildReqFilter("channelId", channel, add=TRUE),
                            buildReqFilter("switchEvent", event, add=TRUE),
                            #ifelse(serial_mask=="", "", с("AND like(serial, '%", serial_mask, "%') ")),
  sep=" ")

  res
}


# lintr::lint("./R/build_req_filter.R")
