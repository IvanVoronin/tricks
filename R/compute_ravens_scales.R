#' Compute series (A, B, C, D, E) and total score for Raven's Standard Progressive Matrices
#'
#' @param data 60-column matrix/data.frame
#' @return matrix of the size \code{nrow(data)} x 6
#' @examples \dontrun{
#' D <- cbind(D, compute_ravens_scales(D))
#' }

compute_ravens_scales <- function(data) {
  if (ncol(data) != 60)
    stop('Data must have 60 columns')

  #         1  2  3  4  5  6  7  8  9 10 11 12
  key <- c( 4, 5, 1, 2, 6, 3, 6, 2, 1, 3, 4, 5, # A
            2, 6, 1, 2, 1, 3, 5, 6, 4, 3, 4, 5, # B
            8, 2, 3, 8, 7, 4, 5, 1, 7, 6, 1, 2, # C
            3, 4, 3, 7, 8, 6, 5, 4, 1, 2, 5, 6, # D
            7, 6, 8, 2, 1, 5, 1, 6, 3, 2, 4, 5) # E

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
