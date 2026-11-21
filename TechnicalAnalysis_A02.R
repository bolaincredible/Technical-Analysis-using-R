# ==============================================================
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary)
# File   : TechnicalAnalysis_A02.R  (main script)
# Author : Bola_Hassan
# Run    : set the working directory to this folder, then
#          source("TechnicalAnalysis_A02.R")
# ==============================================================

source("technical_analysis_functions.R")

# 1. Install / load packages
install_if_missing()

# 2. Configuration
portfolio_file <- "portfolio.txt"
start_date     <- Sys.Date() - 365   # one year of history
end_date       <- Sys.Date()

# 3. Import data (one data frame per symbol)
stocks <- load_stock_data(portfolio_file, from = start_date, to = end_date)
cat("\nSymbols loaded:", paste(names(stocks), collapse = ", "), "\n")

# 4. Compute statistics (mean, median, mode, sd, moving averages)
results <- calculate_all_statistics(stocks, short_window = 20, long_window = 50)

# 5. Display data frames, quotes and statistics
for (s in names(stocks)) display_stock_data(stocks[[s]], s)
cat("\n--- Latest Quotes ---")
for (s in names(stocks)) display_quote_summary(stocks[[s]], s)
display_statistics_table(results$summary_table)

# 6. Visualizations (saved to PNG and shown in the Plots pane)
dir.create("plots", showWarnings = FALSE)
for (s in names(stocks)) {
  png(file.path("plots", paste0(s, "_dashboard.png")), 1200, 900, res = 120)
  plot_stock_dashboard(results$stats[[s]]$data, s)
  dev.off()
  plot_stock_dashboard(results$stats[[s]]$data, s)
}
png("plots/portfolio_normalized.png", 1200, 700, res = 120)
plot_normalized_performance(stocks); dev.off()
png("plots/portfolio_returns_boxplot.png", 1200, 700, res = 120)
plot_returns_boxplot(stocks); dev.off()
plot_normalized_performance(stocks)
plot_returns_boxplot(stocks)

# 7. Save the statistics table
write.csv(results$summary_table, "portfolio_statistics.csv", row.names = FALSE)
