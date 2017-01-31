require(lubridate)

timeframe <- setClass(
    # Set the name
    "timeframe",

    # Define the slots
    slots = c(
        start_date = "Date", 
        end_date = "Date", 
        frequency = "character"
    )
)


t <- timeframe(start_date = as_date("2015-12-31"), end_date = as_date("2016-12-31"), frequency = "daily")
