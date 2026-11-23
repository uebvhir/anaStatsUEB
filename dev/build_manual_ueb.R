build_manual_ueb <- function(pkg = ".", path = "inst/doc", quiet = FALSE) {
  pkg  <- normalizePath(pkg)
  desc <- read.dcf(file.path(pkg, "DESCRIPTION"), fields = c("Package", "Version"))
  name <- paste0(desc[1, "Package"], "_", desc[1, "Version"], ".pdf")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  out  <- file.path(normalizePath(path), name)
  tmp  <- tempfile("manual_"); dir.create(tmp)
  tarball <- devtools::build(pkg, path = tmp, vignettes = FALSE, manual = FALSE, quiet = quiet)
  untar(tarball, exdir = tmp)
  src <- file.path(tmp, desc[1, "Package"])
  callr::rcmd("Rd2pdf", c("--force", paste0("--output=", out), src),
              fail_on_status = TRUE, stderr = "2>&1", spinner = FALSE)
  invisible(out)
}