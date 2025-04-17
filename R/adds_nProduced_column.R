adds_nProduced_column <- function(d,start_at, says_code, says_code2 = NULL) {
  # First three columns are assumed to be "subjectkey", "interview_age", and
  # "gender", and all others correspond to words
  x <- rowSums(d[,start_at:ncol(d)] == says_code |d[,start_at:ncol(d)] == says_code2, na.rm = TRUE)
  return(x)
}
