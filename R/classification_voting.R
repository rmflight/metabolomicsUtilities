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
                                                     "not_lipid or not_classified"),
                                   debug = FALSE){

  vote_function = switch(return_opts,
                         max = max_vote,
                         single = single_vote,
                         multiple = multiple_vote,
                         all = all_vote)

  purrr::imap_dfr(category_list, ~ vote_function(.x, .y, vote_threshold, exclude_votes, debug))

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
  data.frame(Voted = vote_counts$values, counts = vote_counts$lengths,
             percent = vote_counts$lengths / sum(vote_counts$lengths) * 100,
             stringsAsFactors = FALSE)
}

all_vote = function(in_list, list_id, vote_threshold, exclude_votes){
  in_list2 = exclude_filter(in_list, exclude_votes)
  # if (length(in_list2) == 0) {
  #   return(NULL)
  # }

  votes = count_votes(in_list)
  votes$emf = list_id
  votes

}

max_vote = function(in_list, list_id, vote_threshold, exclude_votes, debug){
  if (debug) {
    message(list_id)
  }
  in_list2 = exclude_filter(in_list, exclude_votes)
  # if (length(in_list2) == 0) {
  #   return(NULL)
  # }

  votes = count_votes(in_list2)
  max_perc = max(votes$percent)
  keep_votes = votes[votes$percent == max_perc, ]
  keep_votes$emf = list_id
  keep_votes
}

single_vote = function(in_list, list_id, vote_threshold, exclude_votes){
  in_list2 = exclude_filter(in_list, exclude_votes)
  # if (length(in_list2) == 0) {
  #   return(NULL)
  # }

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
  # if (length(in_list2) == 0) {
  #   return(NULL)
  # }

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

#' transform multiple to a single entry
#'
#' Takes classifications where there are multiple tied max entries, and transforms them into a "multiple".
#'
#' @param voted_categories data.frame of voted results
#' @param split_by the variable to use the split on "emf"s, default is "emf"
#'
#' @return data.frame with "multiple" replacing tied entries
#' @export
voted_to_multiple = function(voted_categories, split_by = "emf"){
  split_by_emf = split(voted_categories, voted_categories[[split_by]])

  purrr::map_df(split_by_emf, function(.x){
      if (nrow(.x) == 1) {
        return(.x)
      } else {
        tmp_df = .x[1, ]
        tmp_df$Voted = "multiple"
        tmp_df$counts = nrow(.x)
        tmp_df$percent = 100
        return(tmp_df)
      }
  })
}

#' vote on categories and classes
#'
#' Given a data.frame of categories, classes, and EMFs, does voting by the EMF first
#' on the Categories, and then trims to those Classes that are part of the original Category,
#' and does voting on the remaining Classes.
#'
#' @param categories_classes data.frame of information
#' @param emfs which column is EMFs
#' @param categories which column is categories
#' @param classes which column has classes
#' @param return_opts what to return (`max`, `all`, `single`, `multiple`)
#' @param vote_threshold what is the vote threshold for `single`?
#' @param exclude_votes which categories should be excluded?
#'
#' @return data.frame
#' @export
vote_categories_classes = function(categories_classes, emfs = "emf",
                                   categories = "Categories",
                                   classes = "Classes",
                                   return_opts = "max",
                                   vote_threshold = 2/3 * 100,
                                   exclude_votes = c("not_lipid", "unclassifiable",
                                                     as.character(NA),
                                                     "not_lipid or not_classified"),
                                   debug = FALSE){
  categories_classes[is.na(categories_classes[[categories]]), categories] =
    "not_classified"
  categories_classes[is.na(categories_classes[[classes]]), classes] = "not_classified"
  exclude_votes = c(exclude_votes, "not_classified")

  split_categories = split(categories_classes[[categories]], categories_classes[[emfs]])

  voted_categories = vote_on_classifications(split_categories, return_opts = return_opts,
                                             vote_threshold = vote_threshold,
                                             exclude_votes = exclude_votes,
                                             debug = debug)
  voted_categories[[emfs]] = voted_categories$emf
  if (!(emfs %in% "emf")) {
    voted_categories$emf = NULL
  }


  split_voted = split(voted_categories$Voted, voted_categories[[emfs]])
  voted_categories_multiple = voted_to_multiple(voted_categories, split_by = emfs)

  multiple_notlipid_group = dplyr::filter(voted_categories_multiple, Voted %in% c("multiple", "not_classified"))
  other_group = dplyr::filter(voted_categories_multiple, !(Voted %in% c("multiple", "not_classified")))
  join_str = c(emfs, categories)
  names(join_str) = c(emfs, "Voted")

  secondary_vote = dplyr::left_join(other_group, categories_classes, by = join_str)
  secondary_vote[secondary_vote[[classes]] %in% "none", classes] = "not_classified"

  split_classes = split(secondary_vote[[classes]], secondary_vote[[emfs]])
  voted_classes = vote_on_classifications(split_classes, return_opts = return_opts,
                                          vote_threshold = vote_threshold,
                                          exclude_votes = exclude_votes,
                                          debug = debug)
  voted_classes[[emfs]] = voted_classes$emf
  if (!(emfs %in% "emf")) {
    voted_categories$emf = NULL
  }

  voted_classes_multiple = voted_to_multiple(voted_classes, split_by = emfs)

  suffixes = paste0(".", c(categories, classes))
  voted_all = dplyr::left_join(voted_categories_multiple, voted_classes_multiple, by = emfs, suffix = suffixes)
  voted_all
}