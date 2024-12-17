styleBatteryBar <- function(data) {
  if (!is.logical(data)) {
    warning("a logical vector is required")
    return("")
  }

  n_data <- length(data)

  margin <- 1
  size <- 100 / n_data

  output <- "linear-gradient("

  boxes <- c("to right")

  for (i in seq_along(data)) {
    col <- ifelse(data[i], "#d2f7b7", "#f7c6b7")

    start <- (size * (i - 1) + margin)
    end <- (size * i - margin)
    boxes <- c(boxes, glue("{col} {start}% {end}%"))

    if (i < length(data)) {
      boxes <- c(boxes, glue("white {end}% {end+2*margin}%"))
    }
  }

  output <- paste0(output, paste(boxes, collapse = ", "), ")")

  return(output)
}

styleBatteryBar(sample(c(TRUE, FALSE), 10, replace = TRUE))
