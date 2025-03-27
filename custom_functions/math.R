## custom round function
custom_round <- function(x) {
  # Round values less than .5 down and .5 and above up
  return(floor(x + 0.5))
}
