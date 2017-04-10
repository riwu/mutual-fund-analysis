install.packages("car")
install.packages("plyr")

library(car)
library(plyr)

funds <- read.csv("~/Dropbox/Year 4 Sem 2/ST2137/Project/mutual funds.csv", header=T)
attach(funds)
# gets all variables conforming to normal distribution
variable_columns <- vector()
for (i in 1:length(funds)) {
  if (is.numeric(funds[,i])) { 
    p <- shapiro.test(funds[,i])$p.value
    p_compare_str <- if (p < 0.01) "smaller" else "greater"
    print(sprintf("shapiro test for %s has a p-value of %s, which is %s than 0.01", 
                  names(funds[i]), p, p_compare_str))
    # we include Worst_Quarter since its p-value of 0.0109 is very close to 0.01
    variable_columns <- c(variable_columns, i) 
  }
}

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

count(funds, "Fees")

# T-test with Welch Correction
t.test(Return_2001 ~ Fees, data=funds, var.equal = FALSE)
t.test(Three_Year_Return ~ Fees, data=funds, var.equal = FALSE)
t.test(Five_Year_Return ~ Fees, data=funds, var.equal = FALSE)


# Levene's Test for Equal Variances
leveneTest(Return_2001, Type)
leveneTest(Three_Year_Return, Type)
leveneTest(Five_Year_Return, Type)

# One-way ANOVA without Welch Correction
model_2001_type <- aov(Return_2001~Type)
model_3_type <- aov(Three_Year_Return~Type)
model_5_type <- aov(Five_Year_Return~Type)

# Summary for models
summary(model_2001_type)
summary(model_3_type)
summary(model_5_type)

boxplot(Return_2001 ~ Fees, main="2001 Returns against Presence of Sales Charges", xlab="Presence of Sales Charges", ylab="2001 Returns")
boxplot(Three_Year_Return ~ Fees, main="Three Year Returns against Presence of Sales Charges", xlab="Presence of Sales Charges", ylab="Three Year Returns")
boxplot(Five_Year_Return ~ Fees, main="Five Year Returns against Presence of Sales Charges", xlab="Presence of Sales Charges", ylab="Five Year Returns")
