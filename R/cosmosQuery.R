#' POST a full query to the REST API for Cosmos DB.
#'
#' @param sql.what String for specifying what fields to retrieve. Typically called select condition. Defaults to *
#' @param sql.where String for specifying what filter to use on data. Typically called search condition. Defaults to empty.
#' @param debug.auth Logical value for getting verbose output of auth header being constructed. Defaults to false.
#' @param debug.header Logical value for getting verbose output of response headers. Defaults to false.
#' @param debug.query Logical value for getting verbose output of HTTP response, printing all headers. Defaults to false.
#' @param content.response Logical value to determine whether to retrieve full response or just the documents
#' @param max.retry Numeric number of retries on a 429 error
#' @return Prints status code of HTTP POST, and returns full HTTP response or just the content
#' @keywords query cosmosdb post
#' @export
#' @examples
#' cosmosQuery(sql.what = "c.contact.eloquaId", sql.where = "c.contact.eloquaId != null")

cosmosQuery <- function(sql.what = "*",
                        sql.where = "",
                        sql.params = list(),
                        max.items = 100,
                        debug.auth = FALSE,
                        debug.header = FALSE,
                        debug.query = FALSE,
                        content.response = FALSE,
                        max.retry = 5,
                        flatten = FALSE) {

    require(digest)
    require(base64enc)
    require(httr)
    require(jsonlite)
    require(stringr)

    # Use the current time to create a proper auth header
    current.time <- Sys.time()

    # Coerce current time to proper format
    ms.date.string <- tolower(format(current.time, "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT"))

    # Create POST URI for posting query to collection
    post.uri <- paste(envCosmosDB$uri, "/dbs/", envCosmosDB$dbName, "/colls/", envCosmosDB$collName, "/docs", sep = "")

    # Create the resource link and type from the environment variables
    res.link <- paste("dbs/", envCosmosDB$dbName, "/colls/", envCosmosDB$collName, sep = "")
    res.type <- "docs"

    # Create full query with function
    full.query <- constructQuery(sql.what, sql.where)

    # Convert full query to JSON for HTTP POST
    json.query <- toJSON(list(query = full.query, parameters = sql.params), auto_unbox = T)

    # Generate auth header using specifications
    auth.header <- genHeader(verb = "POST", resource.type = res.type, resource.link = res.link, stored.time = ms.date.string, debug = debug.auth)
    all.headers <- c("Authorization" = auth.header, "x-ms-version" = "2017-02-22", "x-ms-date" = ms.date.string, "Content-Type" = "application/query+json", "x-ms-documentdb-isquery" = "true", "x-ms-documentdb-query-enablecrosspartition" = "true", "x-ms-max-item-count" = max.items)
    raw.response <- NULL

    # Store all returned data frames here
    all_data_frames <- list()

    repeat {
        if (!is.null(raw.response)) {
            all.headers[["x-ms-continuation"]] <- raw.response$headers[["x-ms-continuation"]]
        }

      raw.response <- POST(post.uri, add_headers(.headers = all.headers), body = json.query)

      ## 429 Error Handling
      if(raw.response$status_code == 429){
        retry = 0
        repeat{
          retry = retry + 1
          Wait_Time = as.numeric(raw.response$headers[["x-ms-retry-after-ms"]])/1000
          print(paste0("429 Error, Retry ",retry,"/",max.retry," Waiting ",Wait_Time," Seconds"))
          Sys.sleep(Wait_Time)
          raw.response <- POST(post.uri, add_headers(.headers = all.headers), body = json.query)
          if(raw.response$status_code == 200){break}
          if(retry == max.retry){
            print((paste0("Query unsuccessful after ",max.retry," tries")))
            break
          }
        }
      }

        # Debug flag for viewing headers upon troubleshooting
      if (debug.query) {
        print("*** Query Content ***")
        print(json.query)
      }
      if(debug.header){
        print("*** Headers of Response ***")
        print(raw.response$headers)
      }

        # Check content response flag; act accordingly
        if (content.response == FALSE) {
            next_data_frame <- raw.response
        } else if (content.response == TRUE) {
            char.response <- readContent(raw.response, flatten = flatten)
            next_data_frame <- char.response$Documents
        }

        # Add the next data frame to the list
        all_data_frames = c(all_data_frames, list(next_data_frame))

        # If the x-ms-continuation header is present, there are more pages to fetch.
        # See https://docs.microsoft.com/en-us/rest/api/cosmos-db/querying-cosmosdb-resources-using-the-rest-api#pagination-of-query-results
        if (is.null(raw.response$headers[["x-ms-continuation"]])) { break }
    }

    return(rbind_pages(all_data_frames[sapply(all_data_frames, length)>0]))
}
