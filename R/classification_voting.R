#' vote on classifications
#'
#' Given a data.frame of EMF categories / classes, attempts to find most likely
#' classification using a voting method. Options for output include returning
#' *all* the categories, a *single* above a particular threshold (default = 0.66),
#' or return the class *multiple* for those things that had multiple classes.
#'
#' @param category_list a named list of categories for each EMF
#' @param return_opts what to return (`max`, `all`, `single`, `multiple`)
#' @param vote_threshold what is the vote threshold for `single`?
#' @param exclude_votes which categories should be excluded?
#'
#' @export
#' @importFrom purrr imap_dfr
vote_on_classifications = function(category_list, return_opts = "max",
                                   vote_threshold = 2/3 * 100,
                                   exclude_votes = c("not_lipid", "unclassifiable",
                                                     as.character(NA),
                                                     "not_lipid or not_classified")){

  vote_function = switch(return_opts,
                         max = max_vote,
                         single = single_vote,
                         multiple = multiple_vote,
                         all = all_vote)

  purrr::imap_dfr(category_list, ~ vote_function(.x, .y, vote_threshold, exclude_votes))

}

exclude_filter = function(in_list, exclude_votes = NULL){

  in_list2 = in_list[!(in_list %in% exclude_votes)]
  if (length(in_list2) > 0) {
    return(in_list2)
  } else {
    return(in_list)
  }
}

count_votes = function(in_list){
  vote_counts = rle(sort(in_list))
  data.frame(Categories = vote_counts$values, counts = vote_counts$lengths,
             percent = vote_counts$lengths / sum(vote_counts$lengths) * 100,
             stringsAsFactors = FALSE)
}

all_vote = function(in_list, list_id, vote_threshold, exclude_votes){
  in_list2 = exclude_filter(in_list, exclude_votes)

  votes = count_votes(in_list)
  votes$emf = list_id
  votes

}

max_vote = function(in_list, list_id, vote_threshold, exclude_votes){
  in_list2 = exclude_filter(in_list, exclude_votes)

  votes = count_votes(in_list2)
  max_perc = max(votes$percent)
  keep_votes = votes[votes$percent == max_perc, ]
  keep_votes$emf = list_id
  keep_votes
}

single_vote = function(in_list, list_id, vote_threshold, exclude_votes){
  in_list2 = exclude_filter(in_list, exclude_votes)

  votes = count_votes(in_list2)

  votes2 = votes[votes$percent >= vote_threshold, ]
  if (nrow(votes2) == 0) {
    votes2 = votes
  }
  votes2$emf = list_id
  votes2
}

multiple_vote = function(in_list, list_id, vote_threshold, exclude_votes){
  in_list2 = exclude_filter(in_list, exclude_votes)

  votes = count_votes(in_list)

  if (nrow(votes) > 1) {
    votes[1, "Categories"] = "multiple"
    votes[1, "counts"] = sum(votes$counts)
    votes[1, "percent"] = 100
    votes = votes[1, ]
  }

  votes$emf = list_id
  votes
}