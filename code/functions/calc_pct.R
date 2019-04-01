# calculate percentages with function
calc_pct <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100))
  colnames(res) <- c('Count','Percentage')
  res
}
