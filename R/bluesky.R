load_schema <- function(id) {
  loc <- c("lexicons", strsplit(id, "\\.")[[1]]) |>
    paste0(collapse = "/") |>
    paste0(".json")

  file_path <- system.file(loc, package = "blueRsky", mustWork = FALSE)
  if (!file.exists(file_path)) {
    stop(paste("Schema", id, "not found"))
  }

  jsonlite::read_json(system.file(loc, package = "blueRsky", mustWork = TRUE))
}

schema_type <- function(s) {
  s$defs$main$type
}

schema_description <- function(s) {
  s$defs$main$description
}

schema_id_to_requestor <- function(s, wrapper, override_defaults = NULL) {
  schema <- load_schema(s)
  if (schema_type(schema) == 'query') {
    schema_id_to_query_requestor(s, wrapper, override_defaults)
  } else {
    stop(paste("schema_id_to_requestor not implemented for schema type", schema_type(s)))
  }
}

# Procedures ------------------------------------------------------------------------------------------------------

proc_parameters <- function(s) {
  s$defs$main$parameters
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
  lapply(names, \(x) {
    if (x %in% names(override_defaults)) {
      override_defaults[[x]]
    } else if (x %in% required) {
      quote(expr = )
    }
    else {
      NULL
    }
  }) |>
    setNames(names) |>
    as.pairlist()
}

schema_id_to_query_requestor <- function(schema_id, wrapper, override_defaults = NULL) {
  s <- load_schema(schema_id)
  stopifnot(schema_type(s) == 'query')
  r <- requestor(wrapper, s$id, query_args = query_to_function_args(s, override_defaults))
  attr(r, "schema_id") <- schema_id
  r
}


# Procedures ------------------------------------------------------------------------------------------------------

# this is just a handy little development helper
# for internal use
bsky_to_query_roxygen_comment <- function(func) {
  schema_id <- attr(func, "schema_id")
  s <- load_schema(schema_id)
  params <- query_parameters(s)$properties |>
    purrr::map(\(x) {
      if (x$type == 'string') {
        x$format
      } else if (x$type == 'integer') {
        paste("number between", x$minimum, "and", x$maximum)
      } else {
        x$type
      }
    })
  params[["..."]] <- "Additional arguments to requestor"
  params[["credentials"]] <- "Credential."
  params[["action"]] <- "Either 'perform' or 'dry-run'"
  params[['decode_if_success']] <- "If FALSE, returns the whole response object. If true, returns the decoded response."
  description <- schema_description(s)
  generate_roxygen_comment(func, description, param_descriptions = params)
}

# Bluesky ---------------------------------------------------------------------------------------------------------


#' @import wrapify
bluesky <- wrapper(
  "bsky.social",
  "/xrpc/",
  auth_type = bearer_auth_type(),
  key_management = 'env',
  env_var_name = 'BLUESKY_ACCESS_TOKEN',
  credential_setter = 'bsky_login',
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
  get_session_info('did')
}

#' @export
get_session_handle <- function() {
  get_session_info('handle')
}

# set the access token to the environment variable
set_access_token <- credential_setter(bluesky)

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
bsky_create_session <- requestor(
  purrr::assign_in(bluesky, "key_management", "none"), # this is a hack because this call
                                                       # doesn't need to be authenticated
                                                       # and there's no other way to specify
                                                       # that
  "com.atproto.server.createSession",
  body_args = function_args(identifier = , password = ),
  method = 'post'
)

#' Get information about the current session.
#'
#'
#' @param ... Additional arguments to requestor
#' @param credentials Credential.
#' @param action Either 'perform' or 'dry-run'
#' @param decode_if_success If FALSE, returns the whole response object. If true, returns the decoded response.
#'
#' @return [Describe the return value here]
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



bsky_post_ <- requestor(
  bluesky,
  "com.atproto.repo.createRecord",
  body_args = function_args(record = ,
                            collection = "app.bsky.feed.post",
                            "$type" = "app.bsky.feed.post",
                            repo = quote(blueRsky::get_session_did())
  ),
  method = 'post'
)

#' @export
bsky_post <- function(text) {
  bsky_post_(post(text))
}

#' @export
bsky_get_followers <- requestor(
  bluesky,
  "app.bsky.graph.getFollowers",
  query_args = function_args(actor = quote(blueRsky::get_session_handle()))
)

#' @export
bsky_get_mutes <- requestor(
  bluesky,
  "app.bsky.graph.getMutes",
  query_args = function_args(actor = quote(blueRsky::get_session_handle()))
)

#' @export
bsky_get_likes <- requestor(
  bluesky,
  "app.bsky.feed.getLikes",
  query_args = function_args(uri = quote(blueRsky::get_session_handle()))
)

# Helpers ---------------------------------------------------------------------------------------------------------

#' @export
datetime_format <- function(x) {
  format(x, tz = 'UTC', format = '%Y-%m-%dT%H:%M:%SZ')
}

