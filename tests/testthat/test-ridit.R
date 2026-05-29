test_that("ridit() returns a data frame with the same dimensions", {
  dat <- data.frame(id = letters[1:5],
                    x1 = c(1, 2, 3, 4, 5),
                    x2 = c(5, 4, 3, 2, 1))
  out <- ridit(dat)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 5L)
  expect_equal(ncol(out), 3L)
  expect_equal(out[[1]], letters[1:5])
})

test_that("ridit() scores are in (-1, 1)", {
  set.seed(42)
  dat <- data.frame(id = 1:50, matrix(runif(200), 50, 4))
  out <- ridit(dat)
  scores <- unlist(out[, -1])
  expect_true(all(scores > -1 & scores < 1))
})

test_that("ridit() median observation scores near zero", {
  dat <- data.frame(id = 1:99, x = 1:99)
  out <- ridit(dat)
  # The middle observation (id = 50) should be close to zero
  mid <- out[out[[1]] == 50, 2]
  expect_lt(abs(mid), 0.02)
})

test_that("ridit() top observation scores positive, bottom scores negative", {
  dat <- data.frame(id = 1:10, x = 1:10)
  out <- ridit(dat)
  expect_gt(out[out[[1]] == 10, 2], 0)
  expect_lt(out[out[[1]] == 1,  2], 0)
})

test_that("ridit() preserves column names", {
  dat <- data.frame(id = 1:5, foo = runif(5), bar = runif(5))
  out <- ridit(dat)
  expect_named(out, c("id", "foo", "bar"))
})

test_that("ridit() errors on non-data-frame input", {
  expect_error(ridit(matrix(1:9, 3, 3)), "data frame")
})

test_that("ridit() errors with only one column", {
  expect_error(ridit(data.frame(id = 1:5)), "at least one")
})
