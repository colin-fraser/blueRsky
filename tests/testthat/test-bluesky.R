test_that("generate_function_args moves required arguments to the front and handles override_defaults correctly", {
  property_names <- c("arg1", "arg2", "arg3", "arg4")
  required <- c("arg1", "arg3")
  override_defaults <- list(arg2 = quote("default_arg2"), arg4 = quote("default_arg4"))

  result <- generate_function_args(property_names, required, override_defaults)

  # Check that the required arguments are at the front
  expect_equal(names(result)[1:2], required)

  # Check that the optional arguments are at the back
  expect_equal(names(result)[3:4], setdiff(property_names, required))

  # Check that the required arguments have the correct expression
  expect_equal(as.character(result[[1]]), "")
  expect_equal(as.character(result[[2]]), "")

  # Check that the optional arguments have the correct default values
  expect_equal(as.character(result[[3]]), "default_arg2")
  expect_equal(as.character(result[[4]]), "default_arg4")
})
