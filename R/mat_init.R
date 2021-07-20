#' Initiate the start matrix
#'
#' To create the starting alive points (contained in a square).
#'
#' @param .seed, random seed to use
#' @param n, number of max points to generate, duplicates are removed.
#' @param square_sequence, integer sequence of the form -1:1 to generate the dimensions.
#' @param ndim, integer > 0 for the number of dimensions
#'
#' @return a square matrix of integer points
#' @export
#' @examples
#' mat_init(.seed = 456, n = 40, square_sequence = -5:5, ndim = 2)

mat_init <- function(.seed = NULL, n = 40, square_sequence = -5:5, ndim = 2){

	if(!is.null(.seed)){ set.seed(.seed) }

	mat_init <- unique( matrix(data = sample(x = square_sequence,
						 size = n*4, # 2 coordinates
						 replace = TRUE),
				   ncol=ndim
	) )[1:n , ]
	# print(paste("There are", nrow(mat_init), "unique points instead of", n, "points") )

	return(mat_init)
}
