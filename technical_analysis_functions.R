# ==============================================================
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary)
# File   : technical_analysis_functions.R
# Author : Bola_Hassan
# Purpose: Utility functions to (1) load stock data listed in
#          portfolio.txt, (2) compute basic statistics, and
#          (3) display data, statistics and visualizations.
# ==============================================================

# ---- 1. Packages -----------------------------------------------
required_packages <- c("quantmod", "TTR", "xts", "zoo")


install_if_missing <- function(pkgs = required_packages) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, repos = "https://cran.rstudio.com/")
    }
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
  invisible(TRUE)
}

# ---- 2. Configuration / initialization -------------------------
# Reads portfolio.txt (one ticker per line) and returns a clean vector.
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) stop("Portfolio file not found: ", file)
  symbols <- toupper(trimws(readLines(file, warn = FALSE)))
  symbols <- unique(symbols[symbols != "" & !grepl("^#", symbols)])
  if (length(symbols) == 0) stop("No stock symbols found in ", file)
  symbols
}

# ---- 3. Import function ----------------------------------------
# Reads portfolio.txt, downloads each symbol with quantmod and stores
# it as a separate data frame (Date, Open, High, Low, Close, Volume,
# Adjusted). Returns a named list of data frames and, by default, also
# creates one data frame per symbol in the global environment
# (e.g. AAPL_df, MSFT_df).
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date(),
                            assign_globally = TRUE) {
  symbols <- read_portfolio(file)
  stock_list <- list()

  for (sym in symbols) {
    message("Downloading ", sym, " ...")
    xts_data <- tryCatch(
      getSymbols(sym, src = "yahoo", from = from, to = to,
                 auto.assign = FALSE),
      error = function(e) {
        warning("Could not download ", sym, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(xts_data)) next

    df <- data.frame(Date = index(xts_data), coredata(xts_data),
                     row.names = NULL)
    names(df) <- c("Date", "Open", "High", "Low", "Close",
                   "Volume", "Adjusted")
    stock_list[[sym]] <- df
    if (assign_globally) assign(paste0(sym, "_df"), df, envir = .GlobalEnv)
  }

  if (length(stock_list) == 0) stop("No stock data could be loaded.")
  message("Loaded ", length(stock_list), " of ", length(symbols), " symbols.")
  stock_list
}

# ---- 4. Statistics functions -----------------------------------
# Mode of a numeric vector (most frequent value; first one on ties).
# Prices are rounded to 2 decimals so repeated values can be found.
get_mode <- function(x, digits = 2) {
  x <- round(x[!is.na(x)], digits)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Takes ONE stock data frame and returns a list with:
#   $summary : one-row data frame (mean, median, mode, sd, min, max...)
#   $data    : the input data frame plus SMA_short and SMA_long columns
calculate_statistics <- function(df, short_window = 20, long_window = 50,
                                 price_col = "Close") {
  prices <- df[[price_col]]

  df$SMA_short <- as.numeric(SMA(prices, n = short_window))
  df$SMA_long  <- as.numeric(SMA(prices, n = long_window))

  summary_df <- data.frame(
    Observations = length(prices),
    Mean     = mean(prices, na.rm = TRUE),
    Median   = median(prices, na.rm = TRUE),
    Mode     = get_mode(prices),
    Std_Dev  = sd(prices, na.rm = TRUE),
    Min      = min(prices, na.rm = TRUE),
    Max      = max(prices, na.rm = TRUE),
    Latest_Close   = tail(prices, 1),
    Latest_SMA_20  = tail(df$SMA_short, 1),
    Latest_SMA_50  = tail(df$SMA_long, 1)
  )
  names(summary_df)[9:10] <- c(paste0("Latest_SMA_", short_window),
                               paste0("Latest_SMA_", long_window))
  list(summary = summary_df, data = df)
}

# Runs calculate_statistics() for every stock in the list.
# Returns list(stats = named list of results,
#              summary_table = one combined data frame).
calculate_all_statistics <- function(stock_list, ...) {
  stats <- lapply(stock_list, calculate_statistics, ...)
  summary_table <- do.call(rbind, lapply(names(stats), function(s) {
    cbind(Symbol = s, stats[[s]]$summary)
  }))
  rownames(summary_table) <- NULL
  list(stats = stats, summary_table = summary_table)
}

# ---- 5. Display utilities --------------------------------------
# (a) First/last rows of one stock's data frame
display_stock_data <- function(df, symbol, n = 6) {
  cat("\n==============================\n")
  cat(" ", symbol, " - Historical Prices\n")
  cat("==============================\n")
  cat("Rows:", nrow(df), "| From", format(min(df$Date)),
      "to", format(max(df$Date)), "\n\n")
  cat("First", n, "rows:\n"); print(head(df, n), row.names = FALSE)
  cat("\nLast", n, "rows:\n");  print(tail(df, n), row.names = FALSE)
  cat("\nStructure:\n"); str(df)
  invisible(df)
}

# (b) Yahoo-Finance-style quote summary (latest day vs previous day)
display_quote_summary <- function(df, symbol) {
  last <- tail(df, 2)
  chg  <- last$Close[2] - last$Close[1]
  pct  <- 100 * chg / last$Close[1]
  cat(sprintf("\n%-6s Close: %9.2f  Change: %+7.2f (%+.2f%%)  Open: %.2f  High: %.2f  Low: %.2f  Vol: %s\n",
              symbol, last$Close[2], chg, pct, last$Open[2],
              last$High[2], last$Low[2],
              format(last$Volume[2], big.mark = ",")))
}

# (c) Combined statistics table for the whole portfolio
display_statistics_table <- function(summary_table, digits = 2) {
  cat("\n==============================\n")
  cat(" Portfolio Statistics (Close)\n")
  cat("==============================\n")
  tbl <- summary_table
  num <- sapply(tbl, is.numeric)
  tbl[num] <- round(tbl[num], digits)
  print(tbl, row.names = FALSE)
  invisible(tbl)
}

# (d) Visualizations ------------------------------------------------
# Price line with 20- and 50-day moving averages
plot_price_with_ma <- function(df_stats, symbol) {
  d <- df_stats
  plot(d$Date, d$Close, type = "l", lwd = 2, col = "steelblue",
       xlab = "Date", ylab = "Close price (USD)",
       main = paste(symbol, "- Close Price with Moving Averages"))
  lines(d$Date, d$SMA_short, col = "darkorange", lwd = 2)
  lines(d$Date, d$SMA_long,  col = "firebrick", lwd = 2)
  legend("topleft", bty = "n", lwd = 2,
         col = c("steelblue", "darkorange", "firebrick"),
         legend = c("Close", "SMA 20", "SMA 50"))
}

# Daily trading volume
plot_volume <- function(df, symbol) {
  barplot(df$Volume / 1e6, names.arg = format(df$Date, "%b %d"),
          col = "grey60", border = NA, las = 2, cex.names = 0.5,
          ylab = "Volume (millions)",
          main = paste(symbol, "- Daily Trading Volume"))
}

# Histogram of closing prices with mean / median markers
plot_close_histogram <- function(df, symbol) {
  hist(df$Close, breaks = 20, col = "lightsteelblue", border = "white",
       xlab = "Close price (USD)", main = paste(symbol, "- Distribution of Close"))
  abline(v = mean(df$Close),   col = "firebrick", lwd = 2)
  abline(v = median(df$Close), col = "darkgreen", lwd = 2, lty = 2)
  legend("topright", bty = "n", lwd = 2, lty = c(1, 2),
         col = c("firebrick", "darkgreen"), legend = c("Mean", "Median"))
}

# Portfolio-wide comparison: normalized performance (start = 100)
plot_normalized_performance <- function(stock_list) {
  cols <- rainbow(length(stock_list))
  norm <- lapply(stock_list, function(d) 100 * d$Close / d$Close[1])
  plot(stock_list[[1]]$Date, norm[[1]], type = "n",
       ylim = range(unlist(norm), na.rm = TRUE),
       xlab = "Date", ylab = "Indexed price (start = 100)",
       main = "Portfolio - Normalized Performance")
  for (i in seq_along(stock_list))
    lines(stock_list[[i]]$Date, norm[[i]], col = cols[i], lwd = 2)
  legend("topleft", bty = "n", lwd = 2, col = cols, legend = names(stock_list))
}

# Portfolio-wide comparison: boxplot of daily % returns
plot_returns_boxplot <- function(stock_list) {
  rets <- lapply(stock_list, function(d) 100 * diff(d$Close) / head(d$Close, -1))
  boxplot(rets, col = "lightgoldenrod", ylab = "Daily return (%)",
          main = "Portfolio - Daily Return Distribution")
  abline(h = 0, lty = 3)
}

# Draws all per-stock charts for one symbol in a 2 x 2 panel
plot_stock_dashboard <- function(df_stats, symbol) {
  op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  on.exit(par(op))
  plot_price_with_ma(df_stats, symbol)
  plot_volume(df_stats, symbol)
  plot_close_histogram(df_stats, symbol)
  boxplot(df_stats$Close, col = "lightblue", ylab = "Close price (USD)",
          main = paste(symbol, "- Close Boxplot"))
}
