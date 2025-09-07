sample_statistic_distribution <- function(
    statistic=mean, dataset=iris$Sepal.Length,
    number_of_samples=10000, size_of_samples=10
) {
  sample_statistic <- numeric(number_of_samples);
  
  for (sample_index in 1:number_of_samples) {
    sample <- sample(dataset, size_of_samples, replace=TRUE)
    sample_statistic[sample_index] <- statistic(sample)
  };
  
  return(sample_statistic)
}

compare_statistic_vs_parameter <- function(
    statistic=mean, dataset=iris$Sepal.Length,
    number_of_samples=10000, size_of_samples=10
) {
  samples <- sample_statistic_distribution(
    statistic, dataset, number_of_samples, size_of_samples
  )
  
  mean_p <- mean(dataset)
  variance <- var(dataset)
  deviation <- sd(dataset)
  
  sample_statistic <- mean(samples)
  standard_error_sq <- var(samples)
  standard_error <- sd(samples)
  
  # Plot dataset distribution
  par(mfrow=c(1,2))
  hist(dataset);
  abline(v=mean_p, col="red", lwd=2)
  abline(v=mean_p + deviation, col="blue", lwd=2, lty=2)
  abline(v=mean_p - deviation, col="blue", lwd=2, lty=2)
  # mtext(paste("Parameter =", parameter), side=4, line=3, col="black")
  
  
  # Plot sample distribution
  hist(samples);
  abline(v=sample_statistic, col="red", lwd=2)
  abline(
    v=sample_statistic + standard_error, col="blue", lwd=2, lty=2
  )
  abline(
    v=sample_statistic-standard_error, col="blue", lwd=2, lty=2
  )
  # mtext(paste("Statistic =", sample_statistic), side=4, line=3, col="black")
  
  print(
    sprintf(
      "Mean = %s | Variance = %s | S.D = %s",
      mean_p, variance, deviation
    )
  )
  
  print(
    sprintf(
      "Sample Statistic = %s | S.E.S. = %s | Standard Error = %s",
      sample_statistic, standard_error_sq, standard_error
    )
  )
}

compare_statistic_vs_parameter(mean)
compare_statistic_vs_parameter(var)

