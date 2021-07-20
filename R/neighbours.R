# [Conway] Game of Life functions

#' Get initial neighbours
#'
#' @param .mat, a matrix of points
#'
#' @return a matrix
#' @export

find_neig <- function(.mat){

	if(is.data.frame(.mat)){
		warning("Taking x and y dimension and converting to matrix")
		.mat <- apply(as.matrix(.mat[,c("x", "y")]), 2, as.numeric)
	}

	.mat_init <- unique(.mat)

	if(nrow(.mat_init) < nrow(.mat)){
		warning(paste("There are duplicates in the data :",
			      nrow(.mat_init), "<", nrow(.mat)
		))
	}

	mat_extend <- unique( rbind(.mat_init,
				    .mat_init + c(1, 1), .mat_init - c(1, 1),
				    .mat_init + c(1, -1), .mat_init + c(-1, 1),
				    .mat_init + c(0, 1), .mat_init - c(0, 1),
				    .mat_init + c(1, 0), .mat_init - c(1, 0)
	) )

	# to separate actual alive points
	mat_info <- cbind(mat_extend,
			  c(rep(1, nrow(.mat_init)),
			    rep(0, nrow(mat_extend) - nrow(.mat_init)))
	)

	mat_ordered <- mat_info[ order(mat_info[,1], mat_info[,2]) , ]

	return(mat_ordered)
}

#' Compute l_inf distances from a point to the alive point matrix
#'
#' @param p, a point with 2 dimensions
#' @param m, a matrix with dimension n * 2
#'
#' @return, a matrix of dimension n * 1
#' @export

mat_dist <- function(p, m){
	# to benchmark with dist(mat_ordered, method = "max") and selecting some rowss
	# or no reorder of previous matrix and select nrow(.mat_init)
	apply(m, 1, function(x){max(abs((x[1]-p[1])), abs((x[2]-p[2])))})
}


#' Find the number of alive neighbours
#'
#' @param .mat_ordered a matrix with all the points and neighbours
#' @param .mat_init a matrix of the alive points
#'
#' @return an integer : the number of neighbours.
#' @export

nb_neig <- function(.mat_init, .mat_ordered){

	# get a matrix of dimension : n_init * n_extend
	l_inf <- apply(.mat_ordered[,1:2], 1, function(x){
		mat_dist(x, .mat_init)
	})

	## keep neighbours in 1 radius l_inf ball
	nb_neighbours <- apply(as.matrix(l_inf), 2, function(d){sum(d == 1)} )

	# ind_keep <- which(nb_neighbours >= nb_ngb)
	# mat_ordered[ind_keep , ]

	return(nb_neighbours)
}
