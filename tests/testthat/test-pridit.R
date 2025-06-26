test_that("ridit function works correctly", {
  # Create test data
  test_data <- data.frame(
    ID = c("A", "B", "C", "D", "E"),
    var1 = c(0.9, 0.85, 0.89, 1.0, 0.89),
    var2 = c(0.99, 0.92, 0.90, 1.0, 0.93),
    var3 = c(1.0, 0.99, 0.98, 1.0, 0.99)
  )
  
  # Test ridit function
  ridit_result <- ridit(test_data)
  
  expect_true(is.data.frame(ridit_result))
  expect_equal(nrow(ridit_result), 5)
  expect_equal(ncol(ridit_result), 4)  # ID + 3 variables
  expect_true("Claim.ID" %in% colnames(ridit_result))
})

test_that("PRIDITweight function works correctly", {
  # Create test ridit data
  test_data <- data.frame(
    ID = c("A", "B", "C", "D", "E"),
    var1 = c(0.9, 0.85, 0.89, 1.0, 0.89),
    var2 = c(0.99, 0.92, 0.90, 1.0, 0.93),
    var3 = c(1.0, 0.99, 0.98, 1.0, 0.99)
  )
  
  ridit_result <- ridit(test_data)
  weights <- PRIDITweight(ridit_result)
  
  expect_true(is.numeric(weights))
  expect_equal(length(weights), 3)  # 3 variables
})

test_that("PRIDITscore function works correctly", {
  # Create test data
  test_data <- data.frame(
    ID = c("A", "B", "C", "D", "E"),
    var1 = c(0.9, 0.85, 0.89, 1.0, 0.89),
    var2 = c(0.99, 0.92, 0.90, 1.0, 0.93),
    var3 = c(1.0, 0.99, 0.98, 1.0, 0.99)
  )
  
  ridit_result <- ridit(test_data)
  weights <- PRIDITweight(ridit_result)
  scores <- PRIDITscore(ridit_result, test_data$ID, weights)
  
  expect_true(is.data.frame(scores))
  expect_equal(nrow(scores), 5)
  expect_equal(ncol(scores), 2)  # ID + score
  expect_true("Claim.ID" %in% colnames(scores))
  expect_true("PRIDITscore" %in% colnames(scores))
})

test_that("complete workflow integration test", {
  # Test the complete workflow with the provided test data
  test_data <- data.frame(
    ID = c("A", "B", "C", "D", "E"),
    Smoking_cessation = c(0.9, 0.85, 0.89, 1, 0.89),
    ACE_Inhibitor = c(0.99, 0.92, 0.9, 1, 0.93),
    Proper_Antibiotic = c(1, 0.99, 0.98, 1, 0.99)
  )
  
  # Complete workflow
  ridit_scores <- ridit(test_data)
  weights <- PRIDITweight(ridit_scores)
  final_scores <- PRIDITscore(ridit_scores, test_data$ID, weights)
  
  # Check final output
  expect_true(is.data.frame(final_scores))
  expect_equal(nrow(final_scores), 5)
  expect_true(all(c("Claim.ID", "PRIDITscore") %in% colnames(final_scores)))
  expect_true(all(is.finite(final_scores$PRIDITscore)))
})
