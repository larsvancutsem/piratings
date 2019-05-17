#' @title Calculate Pi Ratings
#' @description This function calculates dynamic performance ratings
#' called "pi ratings" for sport teams in competitive matches.
#' The pi rating system was developed by Constantinou and
#' Fenton in their research paper 'Determining the level of
#' ability of football teams by dynamic ratings based on the relative
#' discrepancies in scores between adversaries'.
#'
#' source: http://www.constantinou.info/downloads/papers/pi-ratings.pdf
#' @usage calculate_pi_ratings(teams, outcomes, lambda, gamma, b, c, return_e)
#' @importFrom methods hasArg
#' @param teams an (n x 2) character matrix,
#' contains unique names for the respective home and away teams in n
#' subsequent matches
#' @param outcomes an (n x 2) numeric matrix,
#' contains the points that the respective home and away teams scored in n
#' subsequent matches
#' @param lambda a constant, the learning rate for performance from
#' recent matches, default value: 0.035
#' @param gamma a constant, the learning rate for performance from
#' home to away and vice versa, default value: 0.7
#' @param b a constant, logarithmic base, default value: 10
#' @param c a constant, default value: 3
#' @param return_e a boolean variable, conditions the function
#' to return either the mean squared error when return_e = TRUE,
#' or the pi ratings when return_e = FALSE, default value: FALSE
#' @return either an (n x 2) matrix containing the pi ratings for the teams in
#' the n input matches or the mean squared error for the specific parameter
#' setting, conditional on boolean parameter return_e being FALSE or TRUE
#' @examples
#' # toy example
#' teams <- matrix(c("team A", "team B", "team B", "team A"), nrow = 2)
#' outcomes <- matrix(c(1, 3, 2, 1), nrow = 2)
#' calculate_pi_ratings(teams, outcomes)
#' @export


calculate_pi_ratings <- function(teams = NULL, outcomes= NULL, lambda = 0.035, gamma = 0.7, b = 10, c = 3, return_e = FALSE) {


    # ================================================================================
    # check requirements for calculation
    # ================================================================================


    # check requirements


    # requirements 'teams'
    if (!hasArg(teams)) {
        stop("'teams' is required to perform the calculation")
    } else if (!is.matrix(teams) | !is.character(teams)) {
        stop("'teams' is required to be a character matrix")
    } else if (dim(teams)[2] != 2) {
        stop("the dimensions of 'teams' are required to be n x 2")
    }

    # requirements 'outcomes'
    if (!hasArg(outcomes)) {
        stop("'outcomes' is required to perform the calculation")
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

    # requirements 'lambda'
    if (hasArg(lambda) & (!is.numeric(lambda) | (is.numeric(lambda) & length(lambda) != 1))) {
        stop("'lambda' is required to be a real number")
    }

    # requirements 'gamma'
    if (hasArg(gamma) & (!is.numeric(gamma) | (is.numeric(gamma) & length(gamma) != 1))) {
        stop("'gamma' is required to be a real number")
    }

    # requirements 'b'
    if (hasArg(b) & (!is.numeric(b) | (is.numeric(b) & length(b) != 1))) {
        stop("'b' is required to be a real number")
    }

    # requirements 'c'
    if (hasArg(c) & (!is.numeric(c) | (is.numeric(c) & length(c) != 1))) {
        stop("'c' is required to be a real number")
    }


    # ================================================================================
    # create local variables and calculate pi ratings
    # ================================================================================


    # create local variables


    # amount_matches:     amount of matches in dataset
    #
    # pi_ratings:         numerical matrix with pi ratings for each match
    #                     for respective columns 1 and 2 in the
    #                     teams matrix
    #
    # teams:              list of unique teams that occur in dataset
    #                     with respective home and away rankings
    #
    # score_diff:         actual score difference Home vs Away
    #
    # pred_score_diff:    predicted score difference Home vs Away
    #
    # abs_error:          list of absolute error in predicted score vs actual score differences
    #
    # home_team:          home team name
    #
    # away_team:          away team name
    #
    # HH_list:            list of home team's home ratings
    #
    # HA_list:            list of home team's away ratings
    #
    # AA_list:            list of away team's away ratings
    #
    # AH_list:            list of away team's home ratings
    #
    # team_rate_HH:       home team home rating
    #
    # team_rate_HA:       home team away rating
    #
    # team_rate_AA:       away team away rating
    #
    # team_rate_AH:       away team home rating
    #
    # e_score_diff_H:     expected score difference
    #                     against the average opponent
    #                     for the home team
    #
    # e_score_diff_A:     expected score difference
    #                     against the average opponent
    #                     for the away team
    #
    # e_score_diff:       expected score difference
    #                     for match
    #
    # prediction_error:   prediction error
    #
    # weighted_error:     weighted error
    #
    # weighted_error_H:   weighted error home team
    #
    # weighted_error_A:   weighted error away team
    #
    # mean_sq_e:          mean squared error
    #


    amount_matches <- length(outcomes[, 1])

    pi_ratings <- matrix(0, ncol = 2, nrow = amount_matches)

    team_list <- as.list(unique(c(teams[, 1], teams[, 2])))
    for (team in team_list) {
        team_list[[team]] <- list(Home = list(0), Away = list(0))
    }

    score_diff <- 0

    pred_score_diff <- 0

    abs_error <- list()

    home_team <- ""

    away_team <- ""

    HH_list <- list()

    HA_list <- list()

    AA_list <- list()

    AH_list <- list()

    team_rate_HH <- 0

    team_rate_HA <- 0

    team_rate_AA <- 0

    team_rate_AH <- 0

    e_score_diff_H <- 0

    e_score_diff_A <- 0

    e_score_diff <- 0

    prediction_error <- 0

    weighted_error <- 0

    weighted_error_H <- 0

    weighted_error_A <- 0

    mean_sq_e <- 0


    # calculate pi ratings


    for (i in 1:amount_matches) {

        # retrieve match data
        home_team <- teams[i, 1]
        away_team <- teams[i, 2]
        score_diff <- outcomes[i, 1] - outcomes[i, 2]

        # retrieve ratings of respective teams
        HH_list <- team_list[[home_team]]$Home
        HA_list <- team_list[[home_team]]$Away
        AA_list <- team_list[[away_team]]$Away
        AH_list <- team_list[[away_team]]$Home

        # retrieve most recent ratings of respective teams
        team_rate_HH <- HH_list[[length(HH_list)]]
        team_rate_HA <- HA_list[[length(HA_list)]]
        team_rate_AA <- AA_list[[length(AA_list)]]
        team_rate_AH <- AH_list[[length(AH_list)]]

        # input pi ratings into matrix
        pi_ratings[i, 1] <- team_rate_HH
        pi_ratings[i, 2] <- team_rate_AA

        # calculate expected score differences against average team
        e_score_diff_H <- (b^(abs(team_rate_HH)/c) - 1)
        e_score_diff_A <- (b^(abs(team_rate_AA)/c) - 1)

        # account for sign of respective pi rates
        if (team_rate_HH < 0) {
            e_score_diff_H <- -e_score_diff_H
        }
        if (team_rate_AA < 0) {
            e_score_diff_A <- -e_score_diff_A
        }

        # calculate expected score difference for match
        e_score_diff <- e_score_diff_H - e_score_diff_A

        # calculate prediction error
        prediction_error <- abs(score_diff - e_score_diff)

        # store prediction error in list of absolute errors
        abs_error[[i]] <- prediction_error

        # calculate weighted error
        weighted_error <- c * log10(1 + prediction_error)
        if (e_score_diff < score_diff) {
            weighted_error_H <- weighted_error
            weighted_error_A <- -weighted_error
        } else {
            weighted_error_H <- -weighted_error
            weighted_error_A <- weighted_error
        }

        # update ratings
        team_list[[home_team]]$Home <- append(HH_list,
                                              team_rate_HH + weighted_error_H * lambda)
        team_list[[home_team]]$Away <- append(HA_list,
                                              team_rate_HA + (weighted_error_H * lambda) * gamma)
        team_list[[away_team]]$Away <- append(AA_list,
                                              team_rate_AA + weighted_error_A * lambda)
        team_list[[away_team]]$Home <- append(AH_list,
                                              team_rate_AH + (weighted_error_A * lambda) * gamma)

    }


    # return either matrix with pi ratings or
    if (!return_e) {
        return(pi_ratings)
    } else {
        mean_sq_e <- mean(unlist(abs_error)^2)
        return(mean_sq_e)
    }
}
