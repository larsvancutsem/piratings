#' @title Optimize Pi Ratings
#' @description This function performs grid optimization on a prespecified set
#' of parameters to find the optimal learning rates for calculating
#' the pi ratings for sport teams in competitive matches
#' for a set of teams in their respective set of sport matches.
#' The pi rating system was developed by Constantinou and
#' Fenton in their research paper 'Determining the level of
#' ability of football teams by dynamic ratings based on the relative
#' discrepancies in scores between adversaries'
#'
#' source: http://www.constantinou.info/downloads/papers/pi-ratings.pdf
#' @usage optimize_pi_ratings(teams, outcomes, lambda_in, gamma_in, b_in, c_in)
#' @importFrom methods hasArg
#' @param teams an (n x 2) character matrix,
#' contains unique names for the respective home and away teams in n
#' subsequent matches
#' @param outcomes an (n x 2) numeric matrix,
#' contains the points that the respective home and away teams scored in n
#' subsequent matches
#' @param lambda_in a numerical vector, learning rate values to consider in
#' the grid optimization, default value: seq(0, 0.1, 0.005)
#' @param gamma_in a numerical vector, learning rate values to consider in
#' the grid optimization, default value: seq(0, 1, 0.05)
#' @param b_in a constant, logarithmic base, default value: 10
#' @param c_in a constant, default value: 3
#' @return a dataframe with the results of the grid optimization,
#' the mean squared error for every combination of learning rates
#' lambda and gamma specified in the parameter vectors
#' @examples
#' # toy example
#' teams <- matrix(c("team A", "team B", "team B", "team A"), nrow = 2)
#' outcomes <- matrix(c(1, 3, 2, 1), nrow = 2)
#' optimize_pi_ratings(teams, outcomes, seq(0.05, 0.07, 0.005), seq(0.4, 0.6, 0.05))
#' @export


optimize_pi_ratings <- function(teams = NULL, outcomes = NULL, lambda_in = seq(0, 0.1, by = 0.005),
                                gamma_in = seq(0, 1, 0.05), b_in = 10, c_in = 3) {


    # ================================================================================
    # check requirements for calculation
    # ================================================================================


    # check requirements


    # requirements 'teams'
    if (!hasArg(teams)) {
        stop("'teams' is required to perform the optimization")
    } else if (!is.matrix(teams) | !is.character(teams)) {
        stop("'teams' is required to be a character matrix")
    } else if (dim(teams)[2] != 2) {
        stop("the dimensions of 'teams' are required to be n x 2")
    }

    # requirements 'outcomes'
    if (!hasArg(outcomes)) {
        stop("'outcomes' is required to perform the optimization")
    } else if (!is.matrix(outcomes) | !is.numeric(outcomes)) {
        stop("'outcomes' is required to be a numeric matrix")
    } else if (dim(outcomes)[2] != 2) {
        stop("the dimensions of 'outcomes' are required to be n x 2")
    }

    # combined requirements 'teams' and 'outcomes'
    if ((hasArg(teams) & hasArg(outcomes)) & is.matrix(teams) & is.matrix(outcomes)) {
        if (dim(teams)[1] != dim(outcomes)[1]) {
            stop("the dimensions of 'teams' and 'outcomes' need to be identical")
        }
    }

    # requirements 'lambda_in'
    if (hasArg(lambda_in) & (!is.numeric(lambda_in) |
                             (is.numeric(lambda_in) & is.matrix(lambda_in)))) {
        stop("'lambda_in' is required to be a vector or a real number")
    }

    # requirements 'gamma_in'
    if (hasArg(gamma_in) & (!is.numeric(gamma_in) |
                            (is.numeric(gamma_in) & is.matrix(gamma_in)))) {
        stop("'gamma_in' is required to be a vector or a real number")
    }

    # requirements 'b_in'
    if (hasArg(b_in) & (!is.numeric(b_in) |
                        (is.numeric(b_in) & length(b_in) != 1))) {
        stop("'b_in' is required to be a real number")
    }

    # requirements 'c_in'
    if (hasArg(c_in) & (!is.numeric(c_in) |
                        (is.numeric(c_in) & length(c_in) != 1))) {
        stop("'c_in' is required to be a real number")
    }


    # ================================================================================
    # create local variables and perform grid optimization
    # ================================================================================


    # create local variables


    # lambda_seq_l:      length of the sequence of lambdas taken into consideration
    #                    for the grid optimization
    #
    # gamma_seq_l:       length of the sequence of gammas taken into consideration
    #                    for the grid optimization
    #
    # avg_sq_e:          numerical matrix that stores the mean squared error
    #                    the pi ratings with the prespecified choice set
    #                    of parameters
    #
    # current_lambda:    the selected lambda to perform the specific test run with
    #
    # current_gamma:     the selected gamma to perform the specific test run with
    #
    # result:            squared error result of the pi ratings for the test run
    #                    with specified parameter settings

    lambda_seq_l <- length(lambda_in)

    gamma_seq_l <- length(gamma_in)

    avg_sq_e <- matrix(0, nrow = lambda_seq_l, ncol = gamma_seq_l)

    current_lambda <- 0

    current_gamma <- 0

    result <- 0


    # perform grid optimization


    for (i1 in 1:lambda_seq_l) {
        for (i2 in 1:gamma_seq_l) {
            current_lambda <- lambda_in[i1]
            current_gamma <- gamma_in[i2]
            result <- calculate_pi_ratings(teams = teams,
                                           outcomes = outcomes,
                                           lambda = current_lambda,
                                           gamma = current_gamma,
                                           b = b_in, c = c_in, return_e = TRUE)
            avg_sq_e[i1, i2] <- result
            message(paste0("calculated result for ",
                           "lambda = ", current_lambda,
                           ", ",
                           "gamma = ", current_gamma))
        }
    }


    # prepare result
    result <- data.frame("mean squared error" = as.vector(avg_sq_e),
                         "lambda" = lambda_in[rep(1:lambda_seq_l, gamma_seq_l)],
                         "gamma" = gamma_in[rep(1:gamma_seq_l,
                                                lambda_seq_l)[order(rep(1:gamma_seq_l,
                                                                        lambda_seq_l))]])

    return(result)
}
