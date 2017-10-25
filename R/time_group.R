#' Arrange time vector according equidistant time intervals
#'
#' There are two possible ways of arrangement: by hour's intervals OR by
#' minute's interval. Hour's arrangement has high priority. Hour arrangement
#' must be integer (1, 2, ...) or fractional part of hour (0.25 or 0.5 only)
#'
#' @param date POSIXct vector to arrange
#' @param hours_bin Duration (in hours) between arrangement points
#' @param mins_bin Duration (in minutes) between arrangement points
#' @export
hgroup.enum <- function(date, hours_bin=NULL, mins_bin=5){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется hour.bin
  # отсчет измерений идет с 0:00
  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут
  # если hour.bin=NULL, то идет привязка к интервалам min.bin, заданном в минутах
  # необходим пакет lubridate

  tick_time <- date

  if (is.null(hours_bin)){
    # привязываем к минутным интервалам
    n <- floor(lubridate::minute(tick_time)/mins_bin)
    dt <- lubridate::floor_date(tick_time, unit="hour") + lubridate::minutes(n * mins_bin)

  }else{
    # привязываем к часовым интервалам
    if (hours_bin < 1 & !(hours_bin %in% c(0.25, 0.5))) hours_bin=1
    n <- floor((lubridate::hour(tick_time)*60 + lubridate::minute(tick_time))/ (hours_bin*60))
    dt <- lubridate::floor_date(tick_time, unit="day") + lubridate::minutes(n * hours_bin*60)
  }

  dt
}

