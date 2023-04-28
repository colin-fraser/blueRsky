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

#' @export
bsky_get_session <- requestor(
  bluesky,
  "com.atproto.server.getSession"
)


#' @export
bsky_get_timeline <- requestor(
  bluesky,
  "app.bsky.feed.getTimeline",
  query_args = function_args(limit = 20, algorithm = NULL, cursor = NULL)
)

#' @export
bsky_get_profile <- requestor(
  bluesky,
  "com.example.getProfile",
  query_args = function_args(
    user = quote(blueRsky::get_session_handle())
  )
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

