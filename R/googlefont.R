#'
#' @importFrom htmlwidgets createWidget
#' @export
GoogleFont <- function(width = NULL, height = NULL, elementId = NULL) {
  createWidget(name = 'GoogleFont', list(), width = width, height = height,
    package = 'googlefonts', elementId = elementId)
}

#' Shiny bindings for GoogleFont
#'
#' Output and render functions for using GoogleFont within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a GoogleFont
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name GoogleFont-shiny
#'
#' @export
GoogleFontOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'GoogleFont', width, height, package = 'googlefonts')
}

#' @rdname GoogleFont-shiny
#' @export
renderGoogleFont <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, GoogleFontOutput, env, quoted = TRUE)
}
