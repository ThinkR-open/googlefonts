
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

#' @importFrom utils data
add_effect <- function(s, effect){
  if( !is.null(effect) ){
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
#' @examples
#' googlefont( "Rancho", effect = "shadow-multiple")
#' googlefont( "Inconsolata" )
#' @export
use_google_font <- function(
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

#' Retrieves the list of fonts
#'
#' @details the function is \code{\link[memoise]{memoise}}d to limit the number of
#' requests to google per session.
#'
#' @param token a google font api token.
#' @return a data frame with columns
#' \describe{
#'   \item{family}{the font family}
#'   \item{files}{}
#' }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr mutate select summarise group_by left_join
#' @importFrom magrittr %>%
#' @importFrom tidyr gather
#' @importFrom memoise memoise
#' @export
fonts <- memoise(function(token = getOption("googlefonts_token") ){
  if( is.null(token) ){
    stop( "no google fonts api key. get one in https://console.developers.google.com/projectselector/apis/credentials and set the 'googlefonts_token' option" )
  }
  url <- paste0("https://www.googleapis.com/webfonts/v1/webfonts?sort=popularity&key=", token )
  data <- fromJSON(url)$items
  families <- data$family
  out <- mutate( data$files, family = families) %>%
    gather( variant, file, -family, na.rm= TRUE) %>%
    group_by( family ) %>%
    summarise( files = list( setNames(file, paste0("", variant) ))) %>%   # paste0 workaround for https://github.com/hadley/dplyr/issues/2231
    left_join( select(data, family, category, variants, subsets, version, lastModified), by = "family" )
  out
})

#' check if the font is available
#'
#' @param family font family
#' @param variant font variant, .e.g \code{"italic"}
#' @param subset font subset, e.g. \code{"latin"}
#' @examples
#' \dontrun{
#'   has_font( "Inconsolata")
#' }
#' @importFrom dplyr filter
#' @export
has_font <- function( family, variant, subset) {
  assert_that( length(family) == 1L )
  f <- fonts()
  if( ! family %in% f$family  ) return(FALSE)

  data <- filter( f, family == family )
  if( !missing(variant) ){
    if( ! variant %in% data$variants) return(FALSE)
  }

  if( !missing(subset) ){
    if( ! subset %in% data$subsets) return(FALSE)
  }

  TRUE
}

