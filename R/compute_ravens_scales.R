#' Compute series (A, B, C, D, E) and total score for Raven's Standard Progressive Matrices
#'
#' @param data 60-column matrix/data.frame
#' @param key numeric vector of correct responses for RSPM, must be specified separately
#' @return matrix of the size \code{nrow(data)} x 6
#' @examples \dontrun{
#' D <- cbind(D, compute_ravens_scales(D, key = key))
#' }

compute_ravens_scales <- function(data, key = NULL) {
  if (ncol(data) != 60)
    stop('Data must have 60 columns')

  if (length(key) != 60)
    stop('key must have length 60')

  #             1  2  3  4  5  6  7  8  9 10 11 12
  nchoices <- c(6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, # A
                6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, # B
                8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, # C
                8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, # D
                8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8) # E

  for (i in 1:60)
    data[which(!(data[, i] %in% 1:nchoices[i])), i] <- NA

  data2 <- data == matrix(key,
                          nrow = nrow(data),
                          ncol = 60,
                          byrow = TRUE)

  outp <- matrix(NA,
                 nrow = nrow(data),
                 ncol = 6,
                 dimnames = list(
                   row.names(data),
                   c(LETTERS[1:5], 'Total')
                 ))

  for (i in 1:5) {
    series <- LETTERS[i]
    is <- (i - 1) * 12 + (1:12)
    outp[, series] <- rowSums(data2[, is], na.rm = TRUE)
  }
  outp[, 'Total'] <- rowSums(data2, na.rm = TRUE)

  return(outp)
}
