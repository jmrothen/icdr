test_that("icd_extract works",{
  expect_equal(icd_extract('E11.12 91231 Ableton 12Easmsne@'),'E11.12')
})

test_that("icd_trim works",{
  expect_equal(icd_trim('E11.12 Testing'),'Testing')
})

test_that("icd_validate works",{
  expect_equal(icd_check('E11.2'),TRUE)
})
