# ANOVA_2

# Creating a dataframe with sample data
data <- data.frame(
  'c1' = c(0.045, 0.047, 0.051, 0.054),
  'c2' = c(0.084, 0.087, NA, NA),
  'c3' = c(0.115, 0.116, NA, NA),
  'c4' = c(0.183, 0.191, NA, NA),
  'c5' = c(0.395, 0.399, NA, NA)
)

# Calculate the sum of each column and create a row with the results
sum_row <- colSums(data, na.rm = TRUE)

# Count the number of values per column and create a row with the results
count_row <- colSums(!is.na(data))

# Calculate the division of the sum by the count and create a row with the result
division_row <- sum_row / count_row

# Calculate the variance for each column
variance_row <- apply(data, 2, function(x) var(x, na.rm = TRUE))

# Add the rows to the original dataframe
data2 <- rbind(Sumatorio = sum_row, Conteo = count_row, media = division_row, Varianza = variance_row)

# Display the resulting dataframe
print(data2)

# Step 1 and 2: Calculate the weighted sum of individual variances
sum_weighted_variances <- sum(sapply(data, function(x) {
  (length(na.omit(x)) - 1) * var(x, na.rm = TRUE)
}))

# Step 3 and 4: Calculate the sum of degrees of freedom
sum_degrees_of_freedom <- sum(sapply(data, function(x) {
  length(na.omit(x)) - 1
}))

# Step 5: Calculate the pooled variance
pooled_variance <- sum_weighted_variances / sum_degrees_of_freedom

# Print the pooled variance
print(pooled_variance)

# Calculate the logarithms of individual variances weighted by their degrees of freedom
log_var_individuals <- sapply(data, function(x) (length(na.omit(x)) - 1) * log(var(x, na.rm = TRUE)))

# Weighted sum of the logarithms of individual variances
sum_log_var_individuals <- sum(log_var_individuals)

# Weighted sum of the logarithms of pooled variance
log_pooled_variance <- sum(sapply(data, function(x) (length(na.omit(x)) - 1))) * log(pooled_variance)

# Calculate q
q <- log_pooled_variance - sum_log_var_individuals

# Print q
print(q)

# Number of groups
k <- ncol(data)

# Sum of the inverses of the degrees of freedom of each group
sum_inverse_degrees_of_freedom <- sum(sapply(data, function(x) 1 / (length(na.omit(x)) - 1)))

# Inverse of the sum of the degrees of freedom of all groups
inverse_sum_degrees_of_freedom <- 1 / sum(sapply(data, function(x) length(na.omit(x)) - 1))

# Calculate c
c <- 1 + (1 / (3 * (k - 1))) * (sum_inverse_degrees_of_freedom - inverse_sum_degrees_of_freedom)

# Print c
print(c)

# Calculate x0
x_0 = q / c
print(x_0)

# Significance level (alpha)
alpha <- 0.05

# Degrees of freedom for the test (depends on the number of groups)
df <- 4  # Replace with your degrees of freedom

# Calculate the critical value of chi-square
chi_square_critical <- qchisq(1 - alpha, df)

# Print the critical value
print(chi_square_critical)

# Perform Bartlett's test
bartlett_result <- bartlett.test(data)

# Print the result
print(bartlett_result)

# Calculate the sum of squares of all values
sum_squares <- sum(sapply(data, function(x) sum(x^2, na.rm = TRUE)))

# Calculate the total sum of values then square it and divide by the total number of observations
total_sum <- sum(sapply(data, sum, na.rm = TRUE))^2
N <- sum(sapply(data, function(x) length(na.omit(x))))

# Calculate SSTotal
SSTotal <- sum_squares - (total_sum / N)

# Print SSTotal
print(SSTotal)

# Calculate the sum of values of each group then square it and divide by the group size
sum_squares_treatments <- sum(sapply(data, function(x) sum(na.omit(x))^2 / length(na.omit(x))))

# Calculate the total sum of values then square it and divide by the total number of observations
total_sum_squared <- total_sum^2 / N

# Calculate SSTreatments
SSTreatments <- sum_squares_treatments - total_sum_squared

# Print SSTreatments
print(SSTreatments)

# Assuming 'SSTotal' and 'SSTreatments' have been previously calculated

# Calculate SSError
SSError <- SSTotal - SSTreatments

# Print SSError
print(SSError)

# Create a dataframe for the ANOVA table
anova_table <- data.frame(
  'Fuente de variación' = c('Tratamientos (Entre)', 'Errores (Dentro)', 'Total'),
  'S.S.' = c(SSTreatments, SSError, SSTotal),
  'g.l.' = c('k-1=4', 7, 11),
  'C.M.' = c(SSTreatments / 4, SSError / 7, NA), # NA for total as CM is not calculated for total
  'F' = c((SSTreatments / 4) / (SSError / 7), NA, NA) # NA for errors and total where F value is not applicable
)

# Print the ANOVA table
print(anova_table)

# Significance level
alpha <- 0.05

# Degrees of freedom for treatments (numerator) and error (denominator)
df_treatments <- 4
df_error <- 20

# Calculate the critical value of the F distribution
f_critical_value <- qf(1 - alpha, df_treatments, df_error)

# Print the critical value
print(f_critical_value)
