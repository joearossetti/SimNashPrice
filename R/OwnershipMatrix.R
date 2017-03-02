#' Make Ownership Matrices
#'
#' @param Firm logical vector indicating if firm 1 owns product j
#' @param Jt the number of products
#'
#' @return a matrix where O[j,k] is one if Firm[j]==Firm[k]
#' @export
#'
#' @examples
#' firm <- c(1,1,1,0,0)
#' jt <- length(firm)
#' O <- O_fun(Firm=firm, Jt=jt)
O_fun <- function(Firm, Jt){
  O_mat <- matrix(ncol = Jt, nrow = Jt, data = 0)
  for(j in 1:Jt){
    for(k in 1:Jt){
      O_mat[j,k] <- Firm[j] == Firm[k]
    }
  }
  return(O_mat)
}
