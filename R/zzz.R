# nocov start

#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  local_version <- packageVersion(pkgname, libname)
  if (!is.na(local_version[1, 4])) {
    # the local version is a development version
    packageStartupMessage("Checking for a new development version of '",
                          pkgname, "' on GitHub ... ", appendLF = FALSE)
    con <- url("https://raw.githubusercontent.com/akersting/" %\% pkgname %\%
                 "/develop/DESCRIPTION")
    options_bak <- options(timeout = 5, warn = -1)
    warn <- NULL
    remote_version <- tryCatch(
      withCallingHandlers(
        as.package_version(read.dcf(con, fields = "Version")),
        warning = function(w) {
          warn <<- conditionMessage(w)
        }
      ),
      error = function(e) {
        packageStartupMessage("Failed due to connection problems: ")
        packageStartupMessage(paste0(c(conditionMessage(e), warn),
                                     collapse = " | "))
        NULL
      },
      finally = {
        close(con)
        options(options_bak)
      }
    )

    if (!is.null(remote_version)) {
      if (remote_version > local_version) {
        packageStartupMessage("There is a more recent version of this ",
                              "package: v", remote_version, ". ",
                              "You can update to it via '",
                              "devtools::install_github(\"akersting/",
                              pkgname, "@develop\")'.")
      } else {
        packageStartupMessage("You already have the most recent version of ",
                              "this package.")
      }
    }
  }
}
# nocov end
