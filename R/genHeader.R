#' Generate authentication string for REST API interaction
#' 
#' @param verb Type of operation to be done in the request
#' @param resource.type Target scope of the operation. Defaults to docs
#' @param resource.link Path to the database and collection being used
#' @param stored.time Time to pass in for hashing
#' @param master.key Access key to the database. Defaults to the envCosmosDB environment
#' @param debug Whether to print verbose messages or not. Defaults to FALSE
#' @return String for use in the header as authentication to the CosmosDB
#' @keywords authentication cosmosdb cosmos header
#' @export
#' @examples
#' genHeader("POST", "docs", "dbs/dbName/colls/collName", Sys.time())

genHeader <- function(verb, resource.type = "docs", resource.link, stored.time, master.key = envCosmosDB, debug = FALSE) {

    require(digest)
    require(base64enc)
    require(httr)
    require(jsonlite)

    # Check if the key exists; if not, tell the user to authorize!
    if (length(envCosmosDB) == 0) {

        print("Please authorize using the cosmosAuth function")
        stop

    } else {

        # Set the break space character
        break.space <- "\n"

        # consistent casing of parameters for auth header
        verb <- tolower(verb)

        # Create the fully-formed string
        string.to.hash <- paste(verb, break.space, resource.type, break.space, resource.link, break.space, stored.time, break.space, "", break.space, sep = "")

        # The master key comes in base64 format; decode this first
        master.key <- base64decode(master.key$keyCosmos)

        # Necessary to UTF-8 encode the string being hashed; it may otherwise contain chars that break the operation
        body <- enc2utf8(string.to.hash)

        # Base64 encode the entire string, using the Master Key as the base, algorithm SHA256, using raw flag
        sig.string <- base64encode(hmac(master.key, body, algo = "sha256", raw = T))

        # Adding the required prefix to the auth header
        auth.header <- URLencode(paste("type=master&ver=1.0&sig=", sig.string, sep = ""), reserved = TRUE)

        # Debug flag for when things just don't quite work out
        if (debug == TRUE) {
            print(paste("Verb is", verb))
            print(paste("Resource type is", resource.type))
            print(paste("Resource link is", resource.link))
            print(paste("Stored time is", stored.time))
            print(paste("The full string being hashed is", string.to.hash))
            print(paste("Auth header is", auth.header))
        }

        auth.header
    }

}
