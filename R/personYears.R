#' personYears
#'
#' Create a simple person-years table.
#'
#' @param time1 (numeric vector) Times to follow-up by default. If time2 is stated, time1 is the start time.
#' @param time2 (numeric vector) NULL by default. If stated, time2 is the end time.
#' @param event (numeric vector) NULL by default. If stated, event indicates an event occured or did not occured.
#' @param group (factor vector) NULL by default. If stated, group stratifies the dataset by length(unique(group)) stratas.
#' @param unit (string vector 1L) "year" by default. State the unit of time1, time2 with either c("year", "month", "week", "day"). This scales time1,time2 to years.
#' @param timeCut (numeric vector) NULL by default. If desired, cuts elapsed time into intervals. Can be unique breakpoints or number of intervals. Creates a second table.
#' @param rate (logical vector 1L) F by default. If = T, adds a new variable column displaying the rate n events per 1000 person years.
#' @importFrom stats aggregate
#' @return A list of one table by default. If !is.null(group), a list of two tables.


personYears =
  function(time1, time2 = NULL,
           event = NULL, group = NULL,
           unit = "year", timeCut = NULL,
           rate = F) {

    if (is.null(time2)) {
      personYears = time1
    } else {
      if(any(time1 > time2)) {stop("Computed negative elapsed time")}
      personYears = time2 - time1
    }

    if(!(length(unit) == 1)) {stop("length(unit) != 1L")}

    if (!(unit %in% c("day", "week", "year", "month"))) {
      stop("Incorrect unit argument")
    } else
      if (unit == "day") {
        personYears = personYears / 365.25
        warning("(unit == \"day\") scales personYears by 365.25")
      } else
        if (unit == "week") {
          personYears = personYears / 52.1786
          warning("(unit == \"week\") scales personYears by 52.1786")
        } else
          if (unit == "month") {
            personYears = personYears / 12
          }

    if(!is.null(event)) {
      if(length(unique(event)) != 2) {stop("length(event) != 2L")}
      if(!is.numeric(event)) {stop("event must be numeric")}
      event = event
    } else {event = rep(0, length(time1))}

    if(!is.null(group)) {
      if(is.vector(group)) {
        if(length(group) == length(time1)) {
          group = group
          if (!is.null(timeCut)) {
            timeInterval = cut(personYears, breaks = timeCut)
            mat = as.matrix(table(group, timeInterval))
            result1 = as.data.frame.matrix(mat)
            colnames(result1) <- paste0("t:", colnames(mat))

            result2 = aggregate(
              data.frame(
                personYears = personYears,
                events = event,
                n = 1
              ),
              by = list(group),
              FUN = sum
            )

            result2 = data.frame(result2, row.names = "Group.1")

            result = list(`person-years and events table` = result2,
                          `person-year intervals and events table` = result1)

          } else {
            result2 = aggregate(
              data.frame(
                personYears = personYears,
                events = event,
                n = 1
              ),
              by = list(group),
              FUN = sum
            )

            result = list(`person-years and events table` = result2)
          }
        } else {stop("length(group) != length(time1)")}
      } else {stop("group argument must be a vector")}
    } else {
      result2 =
        data.frame(
          personYears = sum(personYears),
          events = sum(event),
          n = length(event))
      result = list(`person-years and events table` = result2)
    }

    if(rate == T) {result$`person-years and events table`$`rate per 1000 person-years` = paste0(
      round(result2$events/result2$personYears * 1000, 2))

    }

    return(result)
  }
