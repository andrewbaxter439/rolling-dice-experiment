library(mizu)
library(tidyverse)
library(tictoc)

n_workers <- 4L
n_runs   <- 1e8
n_chunks <- n_workers * 4L
runs_per <- ceiling(n_runs / n_chunks)

set.seed(1)
seeds <- sample.int(.Machine$integer.max, n_chunks)

p <- mizu_pool(n_workers = n_workers)

tic()
tasks <- seeds |>
  map(\(s) mizu_submit(
    p,
    {
      set.seed(seed)
      diceRoller::rollDiceMany(x, runs)
    },
    src = src, seed = s, x = 36L, runs = runs_per
  ))


res <- tasks |> map(mizu_collect) |> list_c()
toc()

# ggplot(data = NULL, aes(x = res)) + geom_bar()

mizu_pool_stop(p)

n_per_iteration_count <- tibble(iterations = res) |> 
  count(iterations)

rm(res)

# Markov chain calcs ------------------------------------------------------
pcoupon_chain <- function(t_max, n) {
  P <- matrix(0, n + 1, n + 1)
  diag(P) <- (0:n) / n
  P[cbind(1:n, 2:(n + 1))] <- (n - 0:(n - 1)) / n
  
  start <- c(1, rep(0, n))            # all mass on "0 values seen"
  
  accumulate(seq_len(t_max), \(p, i) p %*% P |> as.vector(), .init = start) |>
    map_dbl(\(p) p[n + 1])            # P(T <= t) for t = 0..t_max
}


mk_pdf_cdf <- tibble(
  cdf = pcoupon_chain(450, 36)
) |> 
  mutate(
    iterations = row_number() - 1,
    pmf = cdf - lag(cdf)
  )



n_per_iteration_count |> 
  mutate(n = n / sum(n)) |> 
  ggplot(aes(iterations, n)) +
  geom_col() +
  geom_line(data = mk_pdf_cdf, aes(x = iterations, y = pmf), colour = "red") 
