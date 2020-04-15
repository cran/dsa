#' Output a dataframe to HTML
#' 
#' Output a dataframe to a HTML file.
#' @param dataframe data to be parsed to HTML
#' @param file path to save to
#' @details Function used to create HTML for the results of the seasonal adjustment. But can basically be used to create HTML output for any data.frame.
#' @author Daniel Ollech
#' @examples a=data.frame(lapply(1:4, function(x) round(rnorm(10),2)))
#' colnames(a) = paste0("x", 1:4)
#' \dontrun{df2HTML(a, "out.html")}
#' @export


df2HTML <- function(dataframe, file) {
 if(file.exists(file)) {file.remove(file)}

the_names <- paste("<td style='text-align:left;'>", rownames(dataframe), "</td>")

change_right <- function(x) {x <- paste("<td style='text-align:right;'>", x, "</td>"); return(x)}
change_right_header <- function(x) {x <- paste("<th style='text-align:right;'>", x, "</th>"); return(x)}

cnames <- change_right_header(colnames(dataframe))
dataframe <- apply(dataframe, 2, change_right)

df_out <- cbind(rep("<tr>", nrow(dataframe)), the_names, dataframe, rep("</tr>", nrow(dataframe)))

header <- c("<table width=600 style='border-collapse:collapse;' class=table_3131 border=0>", "<thead> <tr style='border-bottom:2px solid black;border-top:3px solid black;'> <th style='text-align:center;'> </th>",  cnames   , "</thead>", "<tbody>")

fend <- c("</tbody>", "</table>")

cat(paste(c(header, t(df_out), fend), collapse =""), file=file)


}










