src <- normalizePath("rolling_n_options.cpp")
cache <- file.path(tempdir(), "rcpp-cache")
dir.create(cache, showWarnings = FALSE)

Rcpp::sourceCpp(src, cacheDir = cache)    # compile once, on the host

library(mizu)
library(tidyverse)

n_workers <- 4L
n_runs   <- 1e6
n_chunks <- n_workers * 4L
runs_per <- ceiling(n_runs / n_chunks)

set.seed(1)
seeds <- sample.int(.Machine$integer.max, n_chunks)

p <- mizu_pool(n_workers = n_workers)

tasks <- seeds |>
  map(\(s) mizu_submit(
    p,
    {
      Rcpp::sourceCpp(src, cacheDir = cache, env = environment())
      set.seed(seed)
      rollDiceMany(x, runs)
    },
    src = src, cache = cache, seed = s, x = 36L, runs = runs_per
  ))


res <- tasks |> map(mizu_collect) |> list_c()

ggplot(data = NULL, aes(x = res)) + geom_bar()

mizu_pool_stop(p)
