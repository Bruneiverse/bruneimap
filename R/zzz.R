.onLoad <- function(libname, pkgname) {
  makeActiveBinding(
    "bn_census2021",
    function() {
      lifecycle::deprecate_warn(
        "0.3",
        I("bn_census2021"),
        details = "Please update your code to use `census2021`."
      )
      get("census2021", envir = asNamespace(pkgname))
    },
    asNamespace(pkgname)
  )
}
