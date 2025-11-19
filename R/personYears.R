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


