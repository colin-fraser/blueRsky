test_that("get_schema returns the correct schema for a given ID", {
  schema <- load_schema("com.atproto.repo.getRecord")
  expect_equal(schema_type(schema), "query")

  schema <- load_schema("app.bsky.feed.post")
  expect_equal(schema_type(schema), "record")
})

test_that("get_schema throws an error for non-existent schema IDs", {
  expect_error(load_schema("nonexistent.schema.id"))
})

test_that("proc_parameters returns the correct parameters for a given ID", {
  schema <- load_schema("app.bsky.feed.getTimeline")
  params <- proc_parameters(schema)
  expect_equal(params$type, "params")
  expect_equal(names(params$properties), c("algorithm", "limit", "cursor"))
  expect_equal(params$properties$algorithm$type, "string")
  expect_equal(params$properties$limit$type, "integer")
  expect_equal(params$properties$limit$minimum, 1)
  expect_equal(params$properties$limit$maximum, 100)
  expect_equal(params$properties$limit$default, 50)
  expect_equal(params$properties$cursor$type, "string")
})

test_that("query_parameter_names returns the correct parameter names for a given schema ID", {
  schema <- load_schema("app.bsky.feed.getAuthorFeed")
  param_names <- query_parameter_names(schema)
  expect_equal(param_names, c("actor", "limit", "cursor"))
})

test_that("query_to_function_args returns the correct pairlist of function arguments for a given schema ID", {
  schema <- load_schema("app.bsky.feed.getAuthorFeed")
  args <- query_to_function_args(schema)

  expect_identical(names(args), c("actor", "limit", "cursor"))
  expect_identical(args$actor, quote(expr = ))
  expect_true(is.null(args$limit))
  expect_true(is.null(args$cursor))

  args <- query_to_function_args(schema, override_defaults = list(actor = quote(f())))
  expect_identical(args$actor, quote(f()))
})
