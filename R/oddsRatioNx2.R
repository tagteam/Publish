oddsRatioNx2 <- function(tab){
  ref <- tab[1,]
  lapply(2:NROW(tab),function(r){
    oddsRatio2x2(tab[c(1,r),])
  })
}
