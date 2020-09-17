#**************************************************************************
#* 
#* Original work Copyright (C) 2016  Antoine Pierucci
#* Modified work Copyright (C) 2016  Zdenek Kabat
#* Modified work Copyright (C) 2017  Jordan Amdahl
#*
#* This program is free software: you can redistribute it and/or modify
#* it under the terms of the GNU General Public License as published by
#* the Free Software Foundation, either version 3 of the License, or
#* (at your option) any later version.
#*
#* This program is distributed in the hope that it will be useful,
#* but WITHOUT ANY WARRANTY; without even the implied warranty of
#* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#* GNU General Public License for more details.
#*
#* You should have received a copy of the GNU General Public License
#* along with this program.  If not, see <http://www.gnu.org/licenses/>.
#**************************************************************************

#' heRomod: Health Economic Evaluation MODelling
#' 
#' `heRomod` is an R toolset for health economic 
#' evaluation modelling. It aims to provide a simple and 
#' consistent interface for Markov models specification and
#' comparison. Non-homogeneous Markov models (with time
#' varying properties) are supported.
#' 
#' @docType package
#' @name heRomod
#'   
#' @importFrom dplyr filter_
#' @importFrom dplyr mutate_
#' @importFrom dplyr do_
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarise_
#' @importFrom tibble as_tibble
#' @importFrom dplyr data_frame
#' @importFrom dplyr as_data_frame
#' @importFrom dplyr bind_rows
#' @importFrom dplyr left_join
#' @importFrom dplyr "%>%"
#' @importFrom dplyr desc
#' @importFrom dplyr ungroup
#' @importFrom dplyr mutate_if
#' @importFrom dplyr funs
#' @importFrom dplyr n row_number
#' @importFrom dplyr group_by_at
#' 
#' @importFrom plyr ldply
#' @importFrom plyr ddply
#' 
#' @importFrom reshape2 dcast
#' @importFrom reshape2 acast
#' @importFrom reshape2 melt
#'   
#' @importFrom lazyeval lazy
#' @importFrom lazyeval lazy_dots
#' @importFrom lazyeval as.lazy_dots
#' @importFrom lazyeval lazy_eval
#' @importFrom lazyeval interp
#'   
#' @importFrom pryr standardise_call
#'   
#' @importFrom utils head
#' @importFrom utils modifyList
#' @importFrom utils globalVariables
#' @importFrom utils as.roman
#'   
#' @importFrom stats pnorm
#' @importFrom stats qbeta
#' @importFrom stats qbinom
#' @importFrom stats qgamma
#' @importFrom stats qlnorm
#' @importFrom stats qnorm
#' @importFrom stats terms
#' @importFrom stats setNames
#' @importFrom stats reorder
#' @importFrom stats na.omit
#' @importFrom stats update
#' @importFrom stats as.formula
#' @importFrom stats var
#' @importFrom stats coef
#' @importFrom stats model.matrix
#' @importFrom stats formula
#' @importFrom stats stepfun
#'   
#' @importFrom mvnfast rmvn
#'   
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 xlim
#' @importFrom ggplot2 "%+replace%"
#'   
#' @importFrom memoise memoise
#' @importFrom memoise timeout
#'   
#' @importFrom utils read.csv
#' @importFrom utils write.csv
#' @importFrom utils packageVersion
#'   
#' @importFrom tools file_ext
#' 
#' @importFrom grDevices dev.off
#' @importFrom grDevices cairo_pdf
#' @importFrom grDevices png
#' 
#' @importFrom graphics plot
#' @importFrom graphics par
#'   
#' @importFrom tibble tibble
#' @importFrom tibble tibble_
#' 
#' @importFrom tidyr crossing
#' @importFrom tidyr spread
#' 
#' @importFrom purrr walk
#' @importFrom purrr walk2
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_dbl
#' 
#' @importFrom stringr str_interp
NULL

#' @export
dplyr::`%>%`

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
