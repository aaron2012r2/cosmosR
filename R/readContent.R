#' Decode the raw contents of an HTTP response and convert it to a list
#' 
#' @param binary.stream The content field of an HTTP response to be decoded as a character
#' @return List variable of the 'contents' field from HTTP response
#' @keywords binary raw character json response
#' @export
#' @examples
#' readContent(http.response)

readContent <- function(binary.stream) {

    # Parse binary response into character
    char.response <- readBin(binary.stream$content, "character")

    # Convert the character response to json
    json.response <- fromJSON(char.response)

    # Return the json
    json.response

}