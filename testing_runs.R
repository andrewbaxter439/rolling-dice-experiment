Rcpp::sourceCpp("rolling_n_options.cpp")

library(mizu)
library(tidyverse)

p <- mizu_pool(n_workers = 2L, launcher = \(token, slot) {Rcpp::sourceCpp("rolling_n_options.cpp")})

t <- mizu_map(p, 1:1e6, \(x) rollDice(4), .template = integer(1))
results <- mizu_collect(t)

mizu_pool_stop(p)
million_runs <- tibble(
  iterations = map_int(1:1e6, ~rollDice(4))
)

million_runs |> 
  ggplot(aes(iterations)) +
  geom_bar(aes(y = after_stat(prop)))


million_runs_4_sides <- tibble(
  iterations = map_int(1:1e6, ~rollDice(16))
)

million_runs_4_sides |> 
  ggplot(aes(iterations)) +
  geom_bar()

million_runs_6_sides <- tibble(
  iterations = map_int(1:1e6, ~rollDice(36))
)

million_runs_6_sides |> 
  ggplot(aes(iterations)) +
  geom_bar()


# working out pmf and cdf -------------------------------------------------


#P(T = t) = n! * S(t - 1, n - 1) / n^t

## two coins = 4 outcomes. p 4 draws:

factorial(4) * kStatistics::nStirling2(4 - 1, 4 - 1) / 4^4

# my calc:
factorial(4) / 4^4

## p 5 draws:

factorial(4) * kStatistics::nStirling2(5 - 1, 4 - 1) / 4^5

# my calc:
(factorial(4) / 4^4) * (1 + 2 + 3)/4

## Working together:
### P(T = t) = (factorial(4) / 4^4) * S(t - 1, n - 1) / 4^(t - 4)

## p 6 draws:

(factorial(4) / 4^4) * kStatistics::nStirling2(6 - 1, 4 - 1) / 4^(6-4)


work_out_probability_of_draw <- function(n_results = 4, n_draws = 4) {
  
  map_dbl(n_draws, \(n_draws) factorial(n_results) * kStatistics::nStirling2(n_draws - 1, n_results - 1) / n_results ^ n_draws)
  
}

work_out_probability_of_draw(4, 4)
work_out_probability_of_draw(4, 5)
work_out_probability_of_draw(4, 6)


## Exact CDF (inclusion-exclusion): 
## P(T <= t) = sum_j (-1)^j * choose(n, j) * (1 - j/n)^t, summing over j = 0..n
## J = 0:n
# map_dbl(t, \(tt) sum((-1)^j * choose(n, j) * (1 - j / n)^tt))
work_out_probability_of_winning <- function(n_results, n_draws) {
  j <- 0:n_results
  map_dbl(n_draws, \(tt) sum((-1)^j * choose(n_results, j) * (1 - j / n_results)^tt))
}

pcoupon_approx <- \(t, n) exp(-exp(-(t - n * log(n)) / n))

pcoupon <- function(t, n) {
  j <- 0:n
  map_dbl(t,       \(tt) sum((-1)^j * choose(n, j) * (1 - j / n)^tt))
}

work_out_probability_of_winning(6,6)
pcoupon(6, 6)

work_out_probability_of_winning(4, 4)
work_out_probability_of_winning(4, 6)

cdf_4_draws <- tibble(
  iterations = 4:40,
  pmf = work_out_probability_of_draw(4, iterations),
  cdf = work_out_probability_of_winning(4, iterations)
) 


million_runs |>
  ggplot(aes(iterations)) +
  geom_bar(aes(y = after_stat(prop))) +
  geom_line(data = cdf_4_draws, aes(x = iterations, y = cdf)) +
  geom_line(data = cdf_4_draws, aes(x = iterations, y = pmf), colour = "red")
  


cdf_36_draws <- tibble(
  iterations = 36:300,
  pmf = work_out_probability_of_draw(36, iterations),
  cdf = work_out_probability_of_winning(36, iterations)
) |> 
  mutate(cdf_approx = pcoupon_approx(iterations, 36))

cdf_36_draws |> 
  ggplot() +
  geom_line(aes(x = iterations, y = cdf))  



million_runs_6_sides |>
  ggplot(aes(iterations)) +
  geom_bar(aes(y = after_stat(prop))) +
  # geom_line(data = cdf_36_draws, aes(x = iterations, y = cdf)) +
  geom_line(data = cdf_36_draws, aes(x = iterations, y = pmf), colour = "red") +
  xlim(0, 300)

cdf_36_draws |> 
  filter(cdf > 0.5)

## After 142 rolls 50% of games are complete

pcoupon_chain <- function(t_max, n) {
  P <- matrix(0, n + 1, n + 1)
  diag(P) <- (0:n) / n
  P[cbind(1:n, 2:(n + 1))] <- (n - 0:(n - 1)) / n
  
  start <- c(1, rep(0, n))            # all mass on "0 values seen"
  
  accumulate(seq_len(t_max), \(p, i) p %*% P |> as.vector(), .init = start) |>
    map_dbl(\(p) p[n + 1])            # P(T <= t) for t = 0..t_max
}


mk_pdf_cdf <- tibble(
  cdf = pcoupon_chain(300, 36)
) |> 
  mutate(
    iterations = row_number() - 1,
    pmf = cdf - lag(cdf)
    )

million_runs_6_sides |>
  ggplot(aes(iterations)) +
  geom_bar(aes(y = after_stat(prop))) +
  # geom_line(data = mk_pdf_cdf, aes(x = iterations, y = cdf)) +
  geom_line(data = mk_pdf_cdf, aes(x = iterations, y = pmf), colour = "red") +
  xlim(0, 300)


# sneaky way? -------------------------------------------------------------


n <- 36
len <- 600   # must be long enough to contain all values; NA below means it wasn't

map_int(1:1e6, \(i) max(match(seq_len(n), sample.int(n, len, replace = TRUE))))


rollDiceMany(36, 1e6)

map_int(1:1e6, ~rollDice(36))


microbenchmark::microbenchmark(
  rollDiceMany(36, 1e4),
  map_int(1:1e4, ~rollDice(36))
  )
