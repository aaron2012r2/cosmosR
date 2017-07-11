#' Set up initial parameters for Cosmos DB querying
#' 
#' @param sql.what String for specifying what fields to retrieve. Typically called select condition
#' @param sql.where String for specifying what filter to use on data. Typically called search condition
#' @return String containing a full SELECT statement for the database
#' @keywords query select where
#' @export
#' @examples
#' constructQuery(sql.what = "c.contact.eloquaId", sql.where = "c.contact.eloquaId != null")

constructQuery <- function(sql.what, sql.where) {

    # Create the query using predicate if it exists
    if (sql.where == "") {
        full.query <- paste("SELECT", sql.what, "FROM c", sep = " ")
    } else {
        full.query <- paste("SELECT ", sql.what, " FROM c WHERE (", sql.where, ")", sep = "")
    }

    # Return the query
    full.query
}