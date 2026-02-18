# This file is part of mvs: Methods for High-Dimensional Multi-View Learning
# Copyright (C) 2018-2024  Wouter van Loon
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 2.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# Learn is a generic function to train a learner on multi-view input data,
# and produce learned functions and (optionally) the matrix of cross-validated
# predictions. It uses switch() to apply the correct learner, depending on
# argument 'type'.

learn <- function(X, y, views, type, generate.CVs = TRUE, ...) {

  # Collect ... arguments and prevent StaPLR specific arguments from being forwarded to RF
    
    dots <- list(...)
    
    rf_drop <- c(
      "alpha1",
      "ll1",
      "seed",
      "progress",
      "parallel",
      "relax.base",
      "penalty.weights.base",
      "na.action",
      "na.arguments",
      "family"
    )
    
    dots_rf     <- dots[setdiff(names(dots), rf_drop)]
    dots_staplr <- dots
  
  # Single base learner applied to all views
  
  if (length(type) == 1L) {
    return(
      switch(
        type,
        StaPLR = StaPLR(
          X, y,
          view = views,
          skip.meta = TRUE,
          skip.cv = !generate.CVs,
          dots_staplr
        ),
        RF = RF(
          X, y,
          view = views,
          skip.meta = generate.CVs,
          skip.cv = !generate.CVs,
          dots_rf
        )
      )
    )
  } else {
    
    # Mixed base learners
    
    if (any(!type %in% c("RF", "StaPLR"))) {
      stop('Type can only be specified as "RF" or "StaPLR"')
    }
    
    if (length(type) != length(unique(views))) {
      stop("Number of base learners does not correspond to number of views")
    }
    
    # Fit mixed base learners
    
    base_learners <- list()
    
    for (i in seq_along(unique(views))) {
      X_i <- X[, views == i, drop = FALSE]
      views_i <- rep(1, ncol(X_i))
      
      if (type[i] == "RF") {
        base_learners[[i]] <- do.call(
          RF,
          c(
            list(
              X_i, y,
              view = views_i,
              skip.meta = generate.CVs,
              skip.cv = !generate.CVs
            ),
            dots_rf
          )
        )
      }
      
      if (type[i] == "StaPLR") {
        base_learners[[i]] <- do.call(
          StaPLR,
          c(
            list(
              X_i, y,
              view = views_i,
              skip.meta = TRUE,
              skip.cv = !generate.CVs
            ),
            dots_staplr
          )
        )
      }
    }
    
    # Extract fitted models and cross-validated predictions
    
    pred_functions_mixed <- list(
      base = lapply(base_learners, function(x) x[[1]][[1]]),
      meta = NULL,
      CVs  = do.call(cbind, lapply(base_learners, `[[`, "CVs")),
      view = as.vector(views)
    )
    
    return(pred_functions_mixed)
  }

}


