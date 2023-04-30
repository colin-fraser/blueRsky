# helpers ---------------------------------------------------------------------------------------------------------


view_schema <- function(s) {
  s <- load_schema(s)
  cat(jsonlite::toJSON(s, pretty = TRUE, auto_unbox = TRUE))
}

generate_function_args <- function(property_names, required, override_defaults) {
  required_args <- lapply(required, \(x) {
    if (x %in% names(override_defaults)) {
      override_defaults[[x]]
    } else {
      quote(expr = )
    }
  }) |>
    setNames(required) |>
    as.pairlist()

  optional_args <- lapply(setdiff(property_names, required), \(x) {
    if (x %in% names(override_defaults)) {
      override_defaults[[x]]
    } else {
      NULL
    }
  }) |>
    setNames(setdiff(property_names, required)) |>
    as.pairlist()

  c(required_args, optional_args)
}

# this is just a handy little development helper
# for internal use
bsky_to_query_roxygen_comment <- function(func) {
  schema_id <- attr(func, "schema_id")
  s <- load_schema(schema_id)
  params <- query_parameters(s)$properties |>
    purrr::map(\(x) {
      if (x$type == "string") {
        x$format
      } else if (x$type == "integer") {
        paste("number between", x$minimum, "and", x$maximum)
      } else {
        x$type
      }
    })
  params[["..."]] <- "Additional arguments to requestor"
  params[["credentials"]] <- "Credential."
  params[["action"]] <- "Either 'perform' or 'dry-run'"
  params[["decode_if_success"]] <- "If FALSE, returns the whole response object. If true, returns the decoded response."
  description <- schema_description(s)
  generate_roxygen_comment(func, description, param_descriptions = params)
}

# Schemas ---------------------------------------------------------------------------------------------------------

load_schema <- function(id) {
  # nesting this in a function call causes it to load lazily,
  # preventing it from calling load_schema until after the user
  # calls the bsky_ function. This prevents package build errors.
  if (is.list(id)) {
    id
  } else if (is.character(id)) {
    loc <- c("lexicons", strsplit(id, "\\.")[[1]]) |>
      paste0(collapse = "/") |>
      paste0(".json")

    file_path <- system.file(loc, package = "blueRsky", mustWork = FALSE)
    if (!file.exists(file_path)) {
      stop(paste("Schema", id, "not found"))
    }

    jsonlite::read_json(system.file(loc, package = "blueRsky", mustWork = TRUE))
  } else {
    stop("id must either be a schema ID or a loaded schema")
  }
}

schema_type <- function(s) {
  s$defs$main$type
}

schema_description <- function(s) {
  s$defs$main$description
}

schema_id_to_requestor <- function(s, wrapper, override_defaults = NULL) {
  schema <- load_schema(s)
  switch(schema_type(schema),
         query = schema_id_to_query_requestor(s, wrapper, override_defaults),
         stop(paste("schema_id_to_requestor not implemented for schema type", schema_type(schema)))
  )
}

schema_id_to_constructor <- function(schema_id, override_defaults = NULL) {
  schema_to_constructor(load_schema(schema_id), override_defaults)
}



# Record Types ------------------------------------------------------------------------------------------------------


schema_to_constructor <- function(schema, override_defaults = NULL) {
  schema <- load_schema(schema)
  properties <- schema$defs$main$record$properties
  required <- unlist(schema$defs$main$record$required)
  property_names <- names(properties)
  args <- generate_function_args(property_names, required, override_defaults)
  wrapify::super_simple_constructor(!!!args, .class = "list", .prune = TRUE)
}

post <- schema_to_constructor(
  "app.bsky.feed.post",
  override_defaults = list(createdAt = quote(format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")))
)


# Procedures ------------------------------------------------------------------------------------------------------

proc_input_schema <- function(s) {
  schema <- load_schema(s)
  schema$defs$main$input$schema
}

procedure_to_requestor <- function(s, override_defaults = NULL) {
  schema <- load_schema(s)
  input_schema <- proc_input_schema(schema)
  args <- generate_function_args(names(input_schema$properties), names(input_schema$required), override_defaults)
  wrapify::requestor(bluesky, schema$id, body_args = args, method = 'post')
}


# Queries ---------------------------------------------------------------------------------------------------------

query_parameters <- function(s) {
  s$defs$main$parameters
}

query_parameter_names <- function(s) {
  names(query_parameters(s)$properties)
}

query_to_function_args <- function(s, override_defaults = NULL) {
  qps <- query_parameters(s)
  required <- qps$required
  names <- query_parameter_names(s)
  generate_function_args(names, required, override_defaults)
}

schema_id_to_query_requestor <- function(schema_id, wrapper, override_defaults = NULL) {
  s <- load_schema(schema_id)
  stopifnot(schema_type(s) == "query")
  r <- wrapify::requestor(wrapper, s$id, query_args = query_to_function_args(s, override_defaults))
  attr(r, "schema_id") <- schema_id
  r
}


# Bluesky ---------------------------------------------------------------------------------------------------------


#' @import wrapify
bluesky <- wrapify::wrapper(
  "bsky.social",
  "/xrpc/",
  auth_type = wrapify::bearer_auth_type(),
  key_management = "env",
  env_var_name = "BLUESKY_ACCESS_TOKEN",
  credential_setter = "bsky_login",
  session = rlang::env(),
  user_agent = "blueRsky R Package"
)

bsky_requestor <- function(schema_id, override_defaults = NULL) {
  schema_id_to_requestor(schema_id, bluesky, override_defaults)
}

get_session_info <- function(nm) {
  if (!rlang::env_has(bluesky$session, nm)) {
    stop("No current session. Try running bsky_login()")
  }
  rlang::env_get(bluesky$session, nm)
}

#' @export
get_session_did <- function() {
  get_session_info("did")
}

#' @export
get_session_handle <- function() {
  get_session_info("handle")
}

# set the access token to the environment variable
set_access_token <- wrapify::credential_setter(bluesky)

#' @importFrom rlang %||%
#' @export
bsky_login <- function(user = NULL, pwd = NULL) {
  user <- user %||% getPass::getPass("Username")
  pwd <- pwd %||% getPass::getPass("Password")
  session <- bsky_create_session(user, pwd)
  rlang::env_bind(bluesky$session, !!!session)
  set_access_token(session$accessJwt)
}

#' @export
bsky_create_session <- wrapify::requestor(
  purrr::assign_in(bluesky, "key_management", "none"), # this is a hack because this call
  # doesn't need to be authenticated
  # and there's no other way to specify
  # that
  "com.atproto.server.createSession",
  body_args = wrapify::function_args(identifier = , password = ),
  method = "post"
)

#' Get information about the current session.
#'
#'
#' @param ... Additional arguments to requestor
#' @param credentials Credential.
#' @param action Either 'perform' or 'dry-run'
#' @param decode_if_success If FALSE, returns the whole response object. If true, returns the decoded response.
#'
#' @return session information
#' @export
bsky_get_session <- bsky_requestor(
  "com.atproto.server.getSession"
)

#' A view of the user's home timeline.
#'
#'
#' @param algorithm Algorithm to user
#' @param limit number between 1 and 100
#' @param cursor cursor
#' @param ... Additional arguments passed to requestor
#' @param credentials Credential.
#' @param action Either 'perform' or 'dry-run'
#' @param decode_if_success If FALSE, returns the whole response object. If true, returns the decoded response.
#'
#' @return A list of home timeline objects
#' @export
bsky_get_timeline <- bsky_requestor(
  "app.bsky.feed.getTimeline"
)

#' Get Profile
#'
#'
#'
#' @param actor Description of actor. Defaults to the logged-in user.
#' @param ... Additional arguments passed to requestor
#' @param credentials Credential.
#' @param action Either 'perform' or 'dry-run'
#' @param decode_if_success If FALSE, returns the whole response object. If true, returns the decoded response.
#'
#' @return Profile
#' @export
bsky_get_profile <- bsky_requestor(
  "app.bsky.actor.getProfile",
  override_defaults = list(
    actor = quote(blueRsky::get_session_handle())
  )
)

#' Find actors matching search criteria.
#'
#'
#' @param term Search term
#' @param limit number between 1 and 100
#' @param cursor cursor
#' @param ... Additional arguments to requestor
#' @param credentials Credential.
#' @param action Either 'perform' or 'dry-run'
#' @param decode_if_success If FALSE, returns the whole response object. If true, returns the decoded response.
#'
#' @return List of actors
#' @export
bsky_search_actors <- bsky_requestor(
  "app.bsky.actor.searchActors"
)

#' Who is following an actor?
#'
#'
#' @param actor at-identifier
#' @param limit number between 1 and 100
#' @param cursor cursor
#' @param ... Additional arguments to requestor
#' @param credentials Credential.
#' @param action Either 'perform' or 'dry-run'
#' @param decode_if_success If FALSE, returns the whole response object. If true, returns the decoded response.
#'
#' @return list of followers
#' @export
bsky_get_followers <- bsky_requestor(
  "app.bsky.graph.getFollowers",
  override_defaults = list(actor = quote(blueRsky::get_session_handle()))
)

#' Who does the viewer mute?
#'
#'
#' @param limit number between 1 and 100
#' @param cursor cursor
#' @param ... Additional arguments to requestor
#' @param credentials Credential.
#' @param action Either 'perform' or 'dry-run'
#' @param decode_if_success If FALSE, returns the whole response object. If true, returns the decoded response.
#'
#' @return list of mutes
#' @export
bsky_get_mutes <- bsky_requestor(
  "app.bsky.graph.getMutes",
  override_defaults = list(actor = quote(blueRsky::get_session_handle()))
)

#' @export
bsky_get_likes <- bsky_requestor(
  "app.bsky.feed.getLikes",
  override_defaults = list(uri = quote(blueRsky::get_session_handle()))
)

#' Get information about the repo, including the list of collections.
#'
#'
#' @param repo at-identifier
#' @param ... Additional arguments to requestor
#' @param credentials Credential.
#' @param action Either 'perform' or 'dry-run'
#' @param decode_if_success If FALSE, returns the whole response object. If true, returns the decoded response.
#'
#' @export
bsky_describe_repo <- bsky_requestor(
  "com.atproto.repo.describeRepo",
  override_defaults = list(repo = quote(blueRsky::get_session_handle()))
)


bsky_create_record <- procedure_to_requestor("com.atproto.repo.createRecord")
bsky_delete_record <- procedure_to_requestor("com.atproto.repo.deleteRecord")

#' Create a new post
#'
#' This function creates a new post in the user's feed.
#'
#' @param text A character string containing the text of the post.
#' @param user_did An at-identifier representing the user's DID. Defaults to the current session DID.
#'
#' @return A list containing the created post's information.
#' @export
#' @examples
#' \dontrun{
#'   bsky_login("username", "password")
#'   result <- bsky_create_post("Hello, world!")
#'   print(result)
#' }
bsky_create_post <- function(text, user_did = blueRsky::get_session_did()) {
  bsky_create_record(
    repo = user_did,
    collection = "app.bsky.feed.post",
    `$type` = "app.bsky.feed.post",
    record = post(text)
  )
}


#' Delete a post
#'
#' This function deletes a post from the user's feed.
#'
#' @param post_key A character string representing the post's key.
#' @param user_did An at-identifier representing the user's DID. Defaults to the current session DID.
#'
#' @return An HTTP response object. If successful, the status code will be 200.
#' @export
#' @examples
#' \dontrun{
#'   bsky_login("username", "password")
#'   post_key <- "example_post_key"
#'   response <- bsky_delete_post(post_key)
#'   print(response$status_code)
#' }
bsky_delete_post <- function(post_key, user_did = blueRsky::get_session_did()) {
  bsky_delete_record(repo = user_did, collection = "app.bsky.feed.post", rkey = post_key, decode_if_success = FALSE)
}

