library(PortfolioAnalytics)
library(portfolio.optimization)
data("edhec")
ret <- edhec[,1:8]
ret

p <- portfolio.spec(assets = colnames(ret))
p <- add.constraint(portfolio = p, type = "full_investment")
p <- add.constraint(portfolio = p, type = "long_only")
p <- add.objective(portfolio = p, type = "risk", name = "StdDev")

opt_single <- optimize.portfolio(R = ret, portfolio = p, optimize_method = "ROI", trace = TRUE)
opt_reb <- optimize.portfolio.rebalancing(ret, portfolio = p, optimize_method = "ROI", rebalance_on = "years", training_period = 60, rolling_window = 60)

chart.RiskReward(opt_single, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)

head(extractWeights(opt_reb), n = 3)
rr <- Return.portfolio(ret, weights = extractWeights(opt_reb))
charts.PerformanceSummary(rr)

sample_moments <- set.portfolio.moments(ret, portfolio = p, method = "boudt", k = 1)
round(sample_moments$sigma, 6)