round_date_at_hour <- function(dt, hr) {
  timestamp <- ifelse (
    hour(dt) >= hr,
    ceiling_date(dt, unit = "days"),
    floor_date(dt, unit = "days")
  )
  return(as_datetime(timestamp, tz = "UTC"))
}