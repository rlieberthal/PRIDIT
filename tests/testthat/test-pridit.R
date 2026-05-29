## ── Helpers ──────────────────────────────────────────────────────────────────
make_dat <- function(n = 30, p = 4, seed = 1) {
  set.seed(seed)
  mat <- matrix(runif(n * p), n, p)
  colnames(mat) <- paste0("x", seq_len(p))
  cbind(data.frame(id = paste0("h", seq_len(n))), as.data.frame(mat))
}

## ── pridit() ─────────────────────────────────────────────────────────────────
test_that("pridit() returns a 'pridit' object", {
  fit <- pridit(make_dat())
  expect_s3_class(fit, "pridit")
})

test_that("pridit() scores have mean near zero", {
  fit <- pridit(make_dat())
  expect_lt(abs(mean(fit$scores$PRIDITscore)), 1e-10)
})

test_that("pridit() scores are in (-1, 1)", {
  fit <- pridit(make_dat())
  expect_true(all(fit$scores$PRIDITscore > -1 & fit$scores$PRIDITscore < 1))
})

test_that("pridit() weight vector has correct length and names", {
  dat <- make_dat(p = 5)
  fit <- pridit(dat)
  expect_length(fit$weights, 5L)
  expect_named(fit$weights, paste0("x", 1:5))
})

test_that("pridit() eigenvalue_ratio is >= 1", {
  fit <- pridit(make_dat())
  expect_gte(fit$eigenvalue_ratio, 1)
})

test_that("pridit() scores data frame has n rows and correct id column", {
  dat <- make_dat(n = 20)
  fit <- pridit(dat)
  expect_equal(nrow(fit$scores), 20L)
  expect_setequal(fit$scores$id, dat$id)
})

test_that("pridit() stores n and p correctly", {
  dat <- make_dat(n = 25, p = 6)
  fit <- pridit(dat)
  expect_equal(fit$n, 25L)
  expect_equal(fit$p, 6L)
})

test_that("print.pridit() runs without error", {
  fit <- pridit(make_dat())
  expect_output(print(fit), "PRIDIT model")
})

test_that("summary.pridit() runs without error", {
  fit <- pridit(make_dat())
  expect_output(summary(fit), "PRIDIT model summary")
})

test_that("coef.pridit() returns the weight vector", {
  fit <- pridit(make_dat())
  expect_identical(coef(fit), fit$weights)
})

test_that("pridit() errors with fewer than 2 indicators", {
  bad <- data.frame(id = 1:5, x = runif(5))
  expect_error(pridit(bad), "at least two")
})

## ── PRIDITweight() ────────────────────────────────────────────────────────────
test_that("PRIDITweight() returns a named numeric vector", {
  dat <- make_dat()
  rs  <- ridit(dat)
  wts <- PRIDITweight(rs)
  expect_type(wts, "double")
  expect_named(wts)
})

## ── PRIDITscore() ─────────────────────────────────────────────────────────────
test_that("PRIDITscore() returns a data frame with id and PRIDITscore columns", {
  dat <- make_dat()
  rs  <- ridit(dat)
  wts <- PRIDITweight(rs)
  sc  <- PRIDITscore(rs, dat$id, wts)
  expect_s3_class(sc, "data.frame")
  expect_named(sc, c("id", "PRIDITscore"))
  expect_equal(nrow(sc), nrow(dat))
})

test_that("PRIDITscore() id_vector length mismatch errors", {
  dat <- make_dat()
  rs  <- ridit(dat)
  wts <- PRIDITweight(rs)
  expect_error(PRIDITscore(rs, 1:5, wts), "same length")
})

## ── pridit_boot() ─────────────────────────────────────────────────────────────
test_that("pridit_boot() returns correct class", {
  dat  <- make_dat(n = 40)
  fit  <- pridit(dat)
  boot <- pridit_boot(fit, dat, B = 20, seed = 7)
  expect_s3_class(boot, "pridit_boot")
})

test_that("pridit_boot() weight CIs contain the point estimate", {
  dat  <- make_dat(n = 40)
  fit  <- pridit(dat)
  boot <- pridit_boot(fit, dat, B = 50, seed = 7)
  wi   <- boot$weights_ci
  # The 95% CI should cover the point estimate for most indicators
  covered <- wi$estimate >= wi$lower & wi$estimate <= wi$upper
  expect_gte(mean(covered), 0.8)
})

test_that("pridit_boot() score CIs have correct number of rows", {
  dat  <- make_dat(n = 30)
  fit  <- pridit(dat)
  boot <- pridit_boot(fit, dat, B = 20, seed = 3, scores = TRUE)
  expect_equal(nrow(boot$scores_ci), 30L)
})

test_that("pridit_boot() with scores = FALSE has NULL scores_ci", {
  dat  <- make_dat(n = 30)
  fit  <- pridit(dat)
  boot <- pridit_boot(fit, dat, B = 20, seed = 3, scores = FALSE)
  expect_null(boot$scores_ci)
})

## ── pridit_longitudinal() ────────────────────────────────────────────────────
test_that("pridit_longitudinal() returns correct class", {
  set.seed(1)
  dat <- data.frame(
    id   = rep(paste0("h", 1:20), 3),
    year = rep(2020:2022, each = 20),
    x1   = runif(60), x2 = runif(60), x3 = runif(60)
  )
  fl <- pridit_longitudinal(dat, id_col = "id", time_col = "year")
  expect_s3_class(fl, "pridit_longitudinal")
})

test_that("pridit_longitudinal() weight_cors is symmetric", {
  set.seed(2)
  dat <- data.frame(
    id   = rep(paste0("h", 1:25), 2),
    year = rep(c(2020, 2021), each = 25),
    x1   = runif(50), x2 = runif(50), x3 = runif(50)
  )
  fl <- pridit_longitudinal(dat, id_col = "id", time_col = "year")
  expect_equal(fl$weight_cors, t(fl$weight_cors))
})

test_that("pridit_longitudinal() diagonal of score_cors is 1", {
  set.seed(3)
  dat <- data.frame(
    id   = rep(paste0("h", 1:20), 3),
    year = rep(2020:2022, each = 20),
    x1   = runif(60), x2 = runif(60), x3 = runif(60)
  )
  fl <- pridit_longitudinal(dat, id_col = "id", time_col = "year")
  expect_true(all(abs(diag(fl$score_cors) - 1) < 1e-10))
})

test_that("pridit_longitudinal() errors with < 2 periods", {
  dat <- data.frame(id = 1:10, year = 2020, x1 = runif(10), x2 = runif(10))
  expect_error(
    pridit_longitudinal(dat, id_col = "id", time_col = "year"),
    "two time periods"
  )
})
