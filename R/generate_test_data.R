#' Testdata Creation with missing values
#' @importFrom tibble tibble
#' @importFrom lubridate as_date
#' @return tibble return tibble data.frame
#' @export

generate_testdata <- function(n, with_date = FALSE, with_na = TRUE){

  overwrite_na <- function(v,n){
    replace_num <- round(runif(1,1,n/10))
    replace_at <- sample(1:n,size=replace_num,replace=FALSE)
    v[c(replace_at)] <- NA
    return(v)
  }

  var1 <- rnorm(n, 100, 10)
  var2 <- sample(LETTERS[1:4], n, replace=TRUE)
  var3 <- rnorm(n, 100, 10)

  if(with_date){
    var4 <- as_date(sample(20000:22000, n, replace=TRUE))
  }else{
    var4 <- sample(20000:22000, n, replace=TRUE)
  }

  if(with_na){
    var1 <- overwrite_na(var1,n)
    var2 <- overwrite_na(var2,n)
    var3 <- overwrite_na(var3,n)
    var4 <- overwrite_na(var4,n)
  }


  res <- tibble(
    var1 = var1,
    var2 = var2,
    var3 = var3,
    var4 = var4
  )

  return(res)
}

#' Wide test data creation
#' @importFrom tibble tibble
#' @return tibble return tibble data.frame
#' @export
generate_widedata <- function(n,w){
  as_tibble(matrix(rnorm(n*w), nrow = n, ncol = w),.name_repair = "unique")
}
