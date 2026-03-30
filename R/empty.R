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

utils::globalVariables(c("k", "v"))

## function to fit empty baselearner, of use when a subset of view and baselearner
## combinations should be fitted on residuals of other view and baselearner
## combinations.

empty <- function(x = NULL, y, view, view.names = NULL, 
               skip.meta = FALSE, skip.cv = FALSE, 
               progress = TRUE, ...) {

  # create output list
  out <- list(
    "base" = NULL,
    "meta" = NULL,
    "CVs" = matrix(rep(NA, times = length(y)), nrow = 1L),
    "view" = view#,
    #"metadat" = metadat ## not needed for empty baselearners  
  )

  class(out) <- "empty"

  # return output
  if (progress) message("DONE")
  return(out)
}
