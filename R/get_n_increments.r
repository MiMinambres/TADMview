#' Creates a vector of specified length with increments of 10 units.
#'
#' This function loads an integer value (n) and retrieves a vector of length n 
#' with values starting at zero and increments of 10 units.
#' This function is used to create the Time values of a TADM table.
#'
#' @param n number of values to create
#' @return a vector of length n with increments of 10 units
#' @export
get_n_increments <- function(n){
	
	values <- c(0)
	
	for(i in c(1:(n-1))){
	
	new_val <- values[length(values)] + 10
	
	values <- c(values, new_val)
	
	}
	
	return (values)
}