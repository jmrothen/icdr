test_that("check_billtype works",{
  expect_equal(billtype_check('0111'),T)
})

test_that("get_facility works",{
  expect_equal(get_facility('0111'),'Hospital')
})

test_that("get_caretype works",{
  expect_equal(get_caretype('0111'),'Inpatient')
})

test_that("get_frequency works",{
  expect_equal(get_frequency('0111'),'Admit Through Discharge')
})
