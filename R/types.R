post <- super_simple_constructor(
  text = ,
  createdAt = quote(blueRsky::datetime_format(Sys.time())),
  reply = NULL,
  embed = NULL,
  `$type` = "app.bsky.feed.post", .class = "list"
)
