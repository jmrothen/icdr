test_that("pos_check works",{
  expect_equal(pos_check('11'),T)
})

test_that("get_pos_name works",{
  expect_equal(get_pos_name('11'),"Office")
})

test_that("get_pos_desc works",{
  expect_equal(get_pos_desc('11'),"Location, other than a hospital, skilled nursing facility (SNF), military treatment facility, community health center, State or local public health clinic, or intermediate care facility (ICF), where the health professional routinely provides health examinations, diagnosis, and treatment of illness or injury on an ambulatory basis.")
})

