#' Next step in the game
#'
#' @param .mat_init, a data frame or a matrix with initial points
#' @param nb_ngb, the rule for the number of neighbours to have for a cell to be alive
#'
#' @return a data frame
#' @export
#' @import dplyr
#' @import tibble
#' @import ggplot2

next_step <- function(.mat_init, nb_ngb = 3){

	# message(".mat_init")
	# print(str(.mat_init))

	if(is.data.frame(.mat_init)){
		warning("Convert data frame to numerical matrix and drop extra dims")
		.mat <- apply(as.matrix(.mat_init[,1:2]), 2, as.numeric)
	}else{
		warning("Going with matrix already")
		.mat <- .mat_init
	}

	mextend <- find_neig(.mat = .mat)

	# message("mextend")
	# print(str(mextend))

	nb_neighbours <- nb_neig(.mat, .mat_ordered = mextend[,1:2])

	# message("nb_neighbours")
	# print(str(nb_neighbours))

	# package import
	# `%>%` <- magrittr::`%>%`

	## convert result as dataframe
	dfr <- mextend %>%
		# .mat_ordered %>%
		tibble::as_tibble(.name_repair = "minimal") %>%
		stats::setNames(nm = c("x", "y", "alive")) %>%
		dplyr::mutate(nb_neighbours = nb_neighbours,
			      alive = (nb_neighbours >= nb_ngb) # stay_alive
		) %>%
		dplyr::mutate_all(as.factor)

	g_overall <- dfr %>%
		ggplot2::ggplot(ggplot2::aes(x, y, colour = nb_neighbours,
					     shape = alive,
					     size = nb_neighbours
		)) +
		ggplot2::geom_point()

	dfr_alive <- dfr %>%
		dplyr::filter(alive == TRUE) # stay_alive

	g <- dfr_alive %>%
		ggplot2::ggplot(ggplot2::aes(x, y,
					     colour = nb_neighbours,
					     shape = alive,
					     size = nb_neighbours
		)) +
		ggplot2::geom_point()

	g_overall

	return(list(dfr_alive = dfr_alive, dfr_all = dfr, graph = g, graph_overall = g_overall))
}
