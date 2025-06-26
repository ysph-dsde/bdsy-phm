## ----------------------------------------------------------------
## Variations of seasonal time series data and offsetting the baseline
##
##      Authors: Shelby Golden, MS from Yale's YSPH DSDE group
## Date Created: June 24th, 2025
## 
## Description: Generate figures that illustrate time series principles
##              and offsetting the baseline. Code was written with the
##              assistance of Yale's AI Clarity.
## 

## ----------------------------------------------------------------
## SET UP THE ENVIRONMENT

# Initiate the package environment.
# renv::init()
renv::restore()

suppressPackageStartupMessages({
  library("ggplot2")
  library("gridExtra")
})

"%!in%" <- function(x,y)!("%in%"(x,y))

# For reproducibility
set.seed(123)




## ----------------------------------------------------------------
## CREATE SYNTHETIC DATA - SEASONS

# Create a sequence of time points
time_points <- seq(from = 1, to = 120, by = 1)  # 10 years of monthly data

# Constant mean, variance, and covariance
data_constant <- data.frame(
  time = time_points,
  value = 10 + sin(2 * pi * time_points / 12)
)

# Time-dependent mean
data_mean_td <- data.frame(
  time = time_points,
  value = 5 + 0.05 * time_points + sin(2 * pi * time_points / 12)
)

# Time-dependent variance
data_var_td <- data.frame(
  time = time_points,
  value = 10 + (1 + 0.5 * sin(2 * pi * time_points / 120)) * sin(2 * pi * time_points / 12)
)

# Time-dependent covariance
data_cov_td <- data.frame(
  time = time_points,
  value = 10 + sin(2 * pi * time_points / (12 + 0.1 * time_points))
)




## ----------------------------------------------------------------
## PLOT - SEASONS

base_plot <- function(data, title) {
  ggplot(data, aes(x = time, y = value)) +
    geom_line(color = "blue") +
    labs(title = title, x = "Time", y = "Value") +
    theme_linedraw()
}

plot1 <- base_plot(data_constant, "Stationary Seasonality")
plot2 <- base_plot(data_mean_td, "Time-dependent Mean")
plot3 <- base_plot(data_var_td, "Time-dependent Variance")
plot4 <- base_plot(data_cov_td, "Time-dependent Covariance")


grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)




## ----------------------------------------------------------------
## CREATE SYNTHETIC DATA - BASELINE

# Create a sequence of time points
time_points <- seq(from = 1, to = 120, by = 1)  # 10 years of monthly data

# Create an AR(1) process to induce autocorrelation
ar_process <- arima.sim(n = 120, list(ar = 0.4))

# Trend seasonal data with raised line (changing base)
linear_trend <- 0.05 * time_points
data_base <- data.frame(
  time = time_points,
  value = 10 + linear_trend + ar_process + sin(2 * pi * time_points / 12),
  trend = linear_trend + 15
)

# Pure seasonal data with autocorrelation (offset applied)
data_offset <- data.frame(
  time = time_points,
  value = 10 + ar_process + sin(2 * pi * time_points / 12)
)

# Calculate the angle for the annotation text
slope <- diff(range(data_base$trend)) / diff(range(data_base$time))
angle <- atan(slope) * 180 / pi  # Convert from radians to degrees


## ----------------------------------------------------------------
## PLOT - BASELINE

base_plot <- function(data, title) {
  ggplot(data, aes(x = time, y = value)) +
    geom_line(color = "blue") +
    labs(title = title, x = "Time", y = "Value") +
    theme_minimal()
}


plot_base <- base_plot(data_base, "Shifting Base") +
  geom_line(aes(y = trend), color = "red", linetype = "dashed") +
  annotate("text", x = 60, y = max(data_base$trend) - 1, label = "Population", color = "red", angle = angle, hjust = 0) +
  annotate("text", x = 60, y = min(data_base$trend) - 6, label = "Number of Hospitalizations", color = "blue", angle = angle, hjust = 0)

plot_offset <- base_plot(data_offset, "Offset Applied") +
  annotate("text", x = 60, y = min(data_base$trend) - 2, label = "Number of Hospitalizations", color = "blue", hjust = 0)


grid.arrange(plot_base, plot_offset, ncol = 1)





