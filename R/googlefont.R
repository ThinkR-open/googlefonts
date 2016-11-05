
#' font effects
#'
#' @source \url{https://developers.google.com/fonts/docs/getting_started}
#' @format data frame with columns
#' \describe{
#' \item{name}{readable name of the effect}
#' \item{effect}{name of the effect}
#' \item{class}{css class}
#' }
"effects"

subsets <- c(
  "latin", "latin-ext", "menu", "greek", "greek-ext", "cyrillic", "cyrillic-ext", "vietnamese",
  "arabic", "khmer", "lao", "tamil", "bengali", "hindi", "korean"
)


add_family <- function(s, family){
  assert_that( is.character(family) && length(family) == 1L )
  family <- gsub( "[[:space:]]", "+", family)
  paste0(s, "family=", family)
}

add_style <- function(s, style){
  if( !is.null(style) ){
    assert_that( length(style) == 1L && is.character(style))
    s <- paste0( s, ":", style)
  }
  s
}

add_subset <- function(s, subset){
  if( !is.null(subset) ){
    assert_that( all(subset %in% subsets) )
    s <- paste0( s, "&subset=", paste( subset, collapse = "," ) )
  }
  s
}

add_text <- function(s, text){
  if( !is.null(text)){
    assert_that( length(text) == 1L )
    s <- paste0( s, "&text=", text)
  }
  s
}

add_effect <- function(s, effect){
  if( !is.null(effect) ){
    data(effects)
    assert_that( effect %in% effects$effect )
    assert_that( length(effect) == 1L )
    s <- paste0( s, "&effect=", effect )
  }
  s
}

#' get google font
#'
#'
#' @param family font family
#' @param style style, e.g. \samp{bold}, \samp{italic}, ...
#' @param subset subset, e.g. \samp{cyrillic}, ...
#' @param text text
#' @param effect font effect
#'
#' @references Google Fonts. \url{https://fonts.google.com}
#'
#' @importFrom shiny tags
#' @importFrom assertthat assert_that
#' @importFrom magrittr %>%
#' @export
googlefont <- function(
  family = "Inconsolata",
  style = NULL,
  subset = NULL,
  text = NULL,
  effect = NULL
){

  url <- "https://fonts.googleapis.com/css?" %>%
    add_family(family) %>%
    add_style(style) %>%
    add_subset(subset) %>%
    add_text(text) %>%
    add_effect(effect)

  tags$link( rel = "stylesheet", type = "text/css", href = url)

}
