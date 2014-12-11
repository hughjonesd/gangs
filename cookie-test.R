library(Rook)


app <- function (env) {
  r <- Rook::Request(env)
  cookies <- r$cookies()
  res <- Rook::Response$new()
  res$set_cookie(paste0("foo", sample(100000,1)), paste0("bar",sample(100000,1)))
  res$write("<html><body>");
  res$write(paste("I can see", length(cookies), "cookies in your last request.. 
    adding another one with random name  and content</body></html>"))
  res$finish()
}


    svr <- Rhttpd$new()
    rhapp <- RhttpdApp$new(name="gangs", app=app)
    svr$add(rhapp)
    require(tools)
    svr$start(port=35538)
