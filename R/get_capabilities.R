#' Report Current Hardware Capabilities
#'
#' See also <https://docs.rs/simsimd/latest/simsimd/capabilities/index.html>
#'
#' @return a named vector of capabilities
#' @export
#'
#' @examples
#' get_capabilities()
get_capabilities <- function() {
  unlist(get_capabilities_rs())
}