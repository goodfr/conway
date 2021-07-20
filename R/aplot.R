#' Alive points plot
#'
#' @param .dfr the data frame of the points with alive status and number of neighbours.
#'
#' @return a plot
#' @export
#' @importFrom magrittr %>%
#' @import ggplot2

aplot <- function(.dfr){

	.dfr %>%
		ggplot2::ggplot(ggplot2::aes(x, y, colour = nb_neighbours,
					     shape = alive,
					     size = as.numeric(nb_neighbours)
		)) +
		ggplot2::geom_point(shape = 15, size = 10) +
		labs(title = "Status of points and distribution of neighbours") +
		theme_bw()
}

#' Base alive plot
#'
#' Takes either a 2D matrix or a data frame and plot alive points
#'
#' @param .dfr a 2D matrix or a dataframe with x and y dimension.
#'
#' @return a plot
#' @export
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @import ggplot2

base_plot <- function(.dfr){

	if( is.matrix(.dfr) ){
		.dfr <- .dfr %>%
			tibble::as_tibble() %>%
			stats::setNames(nm = c("x", "y"))
	}

	.dfr %>%
		ggplot(aes(x, y)) +
		geom_point(shape = 15, size = 10) +
		labs(title = "Alive points") +
		theme_bw()
}
