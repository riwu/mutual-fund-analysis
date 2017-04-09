funds <- read.csv("mutual funds.csv", header=T)
variable_columns <- c(3, 5:8, 11, 12)
print(cat("Variables to test: ", names(funds[variable_columns])))
var_count <- length(variable_columns)
correlated_vars <- vector()
not_correlated_vars <- vector()
for (i in 1:(var_count-1)) {
  for (j in (i+1):var_count) {
    var1 <- funds[,variable_columns[i]]
    var2 <- funds[,variable_columns[j]]
    
    res <- cor.test(var1, var2)
    
    is_correlated <- (res$p.value < 0.05)
    correlated_str <- if (is_correlated) "" else "not "
    res_str <- sprintf("The variables %s and %s are %scorrelated with a p-value of %s", 
                       names(funds[variable_columns[i]]), names(funds[variable_columns[j]]), 
                       correlated_str, res$p.value)
    if (is_correlated) {
      correlated_vars <- rbind(correlated_vars, res_str)
    } else {
      not_correlated_vars <- rbind(not_correlated_vars, res_str)
    }
  }
}
print(correlated_vars)
print(not_correlated_vars)



