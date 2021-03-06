###################################################
# A timeframe object has a start_date, end_date and frequency component.
# The frequency must be of the form 'D' for daily, 'W' for weekly or 'M' for monthly.
# The start_date must be before the end date.
# 'get_start_date' and 'get_end_date' functions are supplied to ensure the correct business dates are always used.
# The function 'get_test_date' will be used to get the next day/week/month date after the end date (always a business date).
###################################################

require(lubridate)
source('./lib/Functions.R')

# The base constructor
timeframe <- setClass(
    # Set the name
    "timeframe",

    # Define the slots
    slots = c(
        start_date = "Date",
        end_date = "Date",
        frequency = "character"
    ),

    # Default values are yesterday, a year ago yesterday, and daily frequency
    prototype = list(
        start_date = previous_business_date_if_weekend(lubridate::as_date(today() - 367)),
        end_date = previous_business_date_if_weekend(lubridate::as_date(today() - 1)),
        frequency = "D"
    ),

    validity = function(object) {
        if (object@start_date >= object@end_date) {
            return("start_date needs to be before end_date")
        }
        else {
            if (object@frequency %in% c("D", "W", "M")) {
                return(TRUE)
            } else {
                return("frequency must be 'D', 'W' or 'M'")
            }
        }
    }
)

setGeneric(name = "get_start_date", def = function(obj) { standardGeneric("get_start_date") })
setMethod(f = "get_start_date", signature = "timeframe", definition = function(obj) { return(previous_business_date_if_weekend(obj@start_date)) })
setGeneric(name = "get_end_date", def = function(obj) { standardGeneric("get_end_date") })
setMethod(f = "get_end_date", signature = "timeframe", definition = function(obj) { return(previous_business_date_if_weekend(obj@end_date)) })
setGeneric(name = "get_test_date", def = function(obj) { standardGeneric("get_test_date") })
setMethod(f = "get_test_date", signature = "timeframe", definition = function(obj) {
    next_date = obj@end_date
    if (obj@frequency == "D") {
        next_date = next_date + 1
        if (weekdays(next_date) == "Saturday") { next_date = next_date + 1 }
        if (weekdays(next_date) == "Sunday") { next_date = next_date + 1 }
    }
    if (obj@frequency == "W") {
        next_date = next_date + 7
    }
    if (obj@frequency == "M") {
        next_date = EOMonth(next_date, 1)
    }
    return(previous_business_date_if_weekend(next_date))
})

setGeneric(name = "get_frequency", def = function(obj, long.form = FALSE) { standardGeneric("get_frequency") })
setMethod("get_frequency",  definition = function(obj, long.form = FALSE) {
    if (long.form == FALSE) {
        return(obj@frequency)
    } else {
        if (obj@frequency == "D") {
            return("daily")
        }
        if (obj@frequency == "W") {
            return("weekly")
        }
        if (obj@frequency == "M") {
            return("monthly")
        }
    }
})


## Example:
#(t1 <- new("timeframe", start_date = as_date("2016-01-02"), end_date = as_date("2016-12-31"), frequency = "W"))  # <=== Always works
#(t2 <- timeframe(yrs = 3))  # <=== I can write an extra constructor so this works, but it hide the base constructor and then the following fails
#(t3 <- timeframe(start_date = as_date("2016-01-02"), end_date = as_date("2016-12-31"), frequency = "W"))



