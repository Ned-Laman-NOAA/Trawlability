nearest <- function (value, find.vector) 
{
  find.vector <- find.vector[!is.na(find.vector)]
  diffs <- abs(as.numeric(find.vector) - as.numeric(value))
  nearest <- match(min(diffs), diffs)
  nearest[1]
}
