library(purrr)
sales <- list(
  profit = c(10, 5, -3), 
  units = c(5, 15, 70)
)


lapply(sales, function(x) {
  x[x < 0] <- 0
  x
})


install.packages('devtools')
library(devtools)
?one_of
slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz"
install_url(slam_url)

require(utils)

## Note: the first two examples will give different results
## if run by example().
ff <- function(x) gg(x)
gg <- function(y) sys.status()
str(ff(1))

gg <- function(y) {
  ggg <- function() {
    cat("current frame is", sys.nframe(), "\n")
    cat("parents are", sys.parents(), "\n")
    for (i in seq(0,3)){
      print(i)
      print(sys.function(i)) #
    }
  }
  if(y > 0) gg(y-1) else ggg()
}
gg(3)
