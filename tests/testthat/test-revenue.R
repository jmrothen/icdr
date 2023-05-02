test_that("revcode_check works",{
  expect_equal(revcode_check('0450'),T)
})

test_that("get_rev_group works",{
  expect_equal(get_rev_group('0991'),"099X - Patient Convenience Items")
})

test_that("get_rev_desc works",{
  expect_equal(get_rev_desc('0451'),"Emergency Room - EMTALA emergency medical screening services")
})

test_that("get_rev_parent works",{
  expect_equal(get_rev_parent('0991'),"099X")
})
