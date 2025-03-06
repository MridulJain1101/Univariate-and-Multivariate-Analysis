### Assignment 1

#Packages
install.packages("expss")
install.packages("tidyverse")
install.packages("xtable")
install.packages("stargazer")
install.packages("maditr")
library(expss)
library(tidyverse)
library(xtable)
library(stargazer)
summary(Dax30data)

# Variable Construction
Dax30data$debtratio <- Dax30data$`Book Debt` / (Dax30data$`Book Debt` + Dax30data$`Market cap`)
print(Dax30data$debtratio)
# Highest Debt Ratios: Financial Firms (example: Deutsche Bank) and Insurance Companies (Allianz,...)
# Lowest Debt Ratios: Especially Producers of Consumer Goods and Technology firms (SAP)
Dax30data$bookassets <- Dax30data$`Book equity` + Dax30data$`Book Debt`
Dax30data$ln_at <- log(Dax30data$bookassets)
Dax30data$tobinq <- (Dax30data$`Market cap` + Dax30data$`Book Debt`) / Dax30data$bookassets

# Apply Labels to the new variables

Dax30data = apply_labels(Dax30data,
                         debtratio = "Debt Ratio",
                         bookassets = "Book Value of Assets",
                         ln_at = "Logarithm of the Book Value of Total Assets",
                         tobinq = "Tobin's Q" )


##### 4) Descriptive Statistics

### Create Summary Statistics Table

# Summary Statistics Dataset
# to use the select function, I downloaded the tidyverse package

datasummary <- select(Dax30data, c(`Market cap`, `1yr return`, `Volatility`, `Beta`, `Book Debt`, `Book equity`, `Sales`, `EBITDA`, `EBIT`, `Net income`, debtratio, bookassets, ln_at, tobinq ))

  # Max for each column
max <- apply(datasummary, 2, max)

# Min for each column
min <- apply(datasummary, 2, min)

# Means for each column
means <- apply(datasummary, 2, mean)

# Medians for each column
medians <- apply(datasummary, 2, median)

# 10th percentile for each column
percentiles_10 <- apply(datasummary, 2, function(x) quantile(x, 0.1))

# 90th percentile for each column
percentiles_90 <- apply(datasummary, 2, function(x) quantile(x, 0.9))

# Standard deviations for each column
standard_deviations <- apply(datasummary, 2, sd)

# Number of observations
obs <- apply(datasummary,2,length)

firm_no <- rep(length(unique(Dax30data$Ticker)), 10)


#Create the Summary Statistics table
descriptive_table <- cbind("Min" = min, "10th Quantile" = percentiles_10, "Median" = medians, "Mean" = means, "90th Quantile" = percentiles_90, "Max" = max, "S.D." = standard_deviations)
print(xtable(descriptive_table, type='text')) 

# Observations and Firm No.
obsfirms <- cbind("No. Observations" = obs, "No. Firms" = firm_no)

#To print out the Summary Statistics table (with the stargazer package)
stargazer(descriptive_table, type = "latex", out = "DESCRIPTIVDATA.html")     #Units were added manually in latex
stargazer(obsfirms, type = "latex", out = "OBSFIRMS.html")


###Outliers

#Create custom Boxplots with 10th and 90th quantiles to find out which variables have Outliers

# Custom function for boxplot with custom quantiles
boxplot9010 <- function(Dax30data, var, lower_quantile = 0.10, upper_quantile = 0.90) {
  
  # Extract the variable
  values <- Dax30data[[var]]
  
  # Calculate custom quantiles
  lower <- quantile(values, lower_quantile, na.rm = TRUE)
  upper <- quantile(values, upper_quantile, na.rm = TRUE)
  median <- quantile(values, 0.50, na.rm = TRUE) # Median
  
  # Create the boxplot
  boxplot(values, outline = FALSE, main = paste("Custom Boxplot for", var),
          ylim = range(lower, upper, na.rm = TRUE), col = "lightblue")
  
  # Add custom whiskers
  segments(1, lower, 1, upper, lwd = 2, col = "blue") # Custom whiskers
  points(1, median, col = "red", pch = 19) # Highlight median
}


boxplot9010(Dax30data, var = "Market cap")
boxplot9010(Dax30data, var = "1yr return")
boxplot9010(Dax30data, var = "Volatility")
boxplot9010(Dax30data, var = "Beta")
boxplot9010(Dax30data, var = "Book Debt")
boxplot9010(Dax30data, var = "Book equity")
boxplot9010(Dax30data, var = "Sales")
boxplot9010(Dax30data, var = "EBITDA")
boxplot9010(Dax30data, var = "EBIT")
boxplot9010(Dax30data, var = "Net income")
boxplot9010(Dax30data, var = "debtratio")
boxplot9010(Dax30data, var = "bookassets")
boxplot9010(Dax30data, var = "ln_at")
boxplot9010(Dax30data, var = "tobinq")


# Function to find outliers based on whiskers (90th/10th quantiles and IQR)
find_outliers_whiskers <- function(Dax30data, var, lower_quantile = 0.10, upper_quantile = 0.90) {
  
  # Extract the variable
  values <- Dax30data[[var]]
  
  # Calculate the quantiles
  q10 <- quantile(values, lower_quantile, na.rm = TRUE)
  q90 <- quantile(values, upper_quantile, na.rm = TRUE)
  
  # Calculate the IQR (Interquantile Range)
  iqr <- q90 - q10
  
  # Define the lower and upper whiskers
  lower_whisker <- q10 - 1.5 * iqr
  upper_whisker <- q90 + 1.5 * iqr
  
  # Identify outliers (outside the whiskers)
  outlier_indices <- which(values < lower_whisker | values > upper_whisker)
  
  # Retrieve corresponding company names and outlier values
  outliers <- Dax30data[outlier_indices, c("Name", var)]
  
  # Return the results
  list(
    variable = var,
    lower_whisker = lower_whisker,
    upper_whisker = upper_whisker,
    outliers = outliers
  )
}

# These are the variables that are being checked for outliers
variables <- c("Market cap", "1yr return", "Volatility", "Beta", "Book Debt", 
               "Book equity", "Sales", "EBITDA", "EBIT", "Net income", 
               "debtratio", "bookassets", "ln_at", "tobinq")

# Iterate over each variable and find the outliers
outlier_results <- lapply(variables, function(var) {
  find_outliers_whiskers(Dax30data, var)
})

# Display the results
for (result in outlier_results) {
  cat("\nVariable:", result$variable, "\n")
  cat("Lower Whisker:", result$lower_whisker, "Upper Whisker:", result$upper_whisker, "\n")
  print(result$outliers)
}



##### 5) Univariate Analysis

#a)

#Regressions
reg1 <- lm(debtratio ~ bookassets, data = Dax30data)
reg2 <- lm(debtratio ~ ln_at, data = Dax30data)

summary(reg1)
summary(reg2)
stargazer(reg1, reg2, type = "latex", out = "REG12")

#Scatterplot


plot(Dax30data$debtratio ~ Dax30data$ln_at, xlab="log(book assets)", ylab="Debt Ratio", main="Scatterplot of log(book assets) and the Debt Ratio", pch=16, type ="p")
abline(reg2, col = "red", lwd = 2)


#b)

#Regressions
reg3 <- lm(debtratio ~ tobinq, data = Dax30data)
summary(reg3)
stargazer(reg3, type = "latex", out = "REG3")

#Scatterplot
plot(Dax30data$debtratio ~ Dax30data$tobinq, xlab="Tobin's Q", ylab="Debt Ratio", main="Scatterplot of Tobin's Q and the Debt Ratio", pch=16, type ="p")
abline(reg3, col = "red", lwd = 2)

#c)
reg4 <- lm(debtratio ~ Volatility, data = Dax30data)
reg5 <- lm(debtratio ~ Beta, data = Dax30data)

summary(reg4)
summary(reg5)
stargazer(reg4, reg5, type = "latex", out = "REG45")

#New variables 

Dax30data$vvol <- (1 - Dax30data$debtratio)*Dax30data$Volatility 
Dax30data$vbeta <- (1 - Dax30data$debtratio)*Dax30data$Beta 


reg6 <- lm(debtratio ~ vvol, data = Dax30data)
reg7 <- lm(debtratio ~ vbeta, data = Dax30data)

summary(reg6)
summary(reg7)
stargazer(reg6, reg7, type = "latex", out = "REG67")

#Scatterplot
plot(Dax30data$debtratio ~ Dax30data$vbeta, xlab="Firm Level Beta", ylab="Debt Ratio", main="Scatterplot of Firm Beta and the Debt Ratio", pch=16, type ="p")
abline(reg7, col = "red", lwd = 2)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, reg7, type = "latex", out = "REG1-7")

#d)

reg8 <- lm(debtratio ~ `1yr return`, data = Dax30data)
reg9 <- lm(debtratio ~ `Book equity`, data = Dax30data)
reg10 <- lm(debtratio ~ Sales, data = Dax30data)
reg11 <- lm(debtratio ~ EBITDA, data = Dax30data)
reg12 <- lm(debtratio ~ EBIT, data = Dax30data)
reg13 <- lm(debtratio ~ `Net income`, data = Dax30data)

summary(reg8)
summary(reg9)
summary(reg10)
summary(reg11)
summary(reg12)
summary(reg13)

stargazer(reg8, reg9, reg10, type = "latex", out = "REG8910")
stargazer(reg11, reg12, reg13, type = "latex", out = "REG111213")

# -> of the remaining variables, the Sales has the highest explanatory power with an R squared of 0.210. 
# -> of the remaining variables, the 1yr return has the lowest explanatory power with an R-squared of 0.029.

stargazer(reg8, reg9, reg10, reg11, reg12, reg13, type = "latex", out = "REG7-13")

#6-Multivarate Analysis

multi1 <- lm(debtratio ~ ln_at + tobinq + vvol + vbeta + `Book equity` + Sales + EBITDA + EBIT + `Net income`, data = Dax30data)
summary(multi1)

multi2 <- lm(debtratio ~ ln_at + tobinq + vvol + vbeta + `1yr return` + `Book equity` + Sales + EBITDA + EBIT + `Net income`, data = Dax30data) 
summary(multi2)

stargazer(multi1, multi2, type = "latex", out = "MULTI12")

# -> Multi2 has a higher Adjusted R-squared!