test_that("`rollDice` works", {
  set.seed(1)
  expect_equal(rollDice(4), 5)
})

test_that("`rollDiceMany` works", {

  rolls <- rollDiceMany(4, 1e4)

  expect_equal(length(rolls), 1e4)
  expect(is.integer(rolls), failure_message = "`rollDiceMany` does not return a vector of integers")

})
