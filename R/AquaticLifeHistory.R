#' Introduction to AquaticLifeHistory
#'
#' @name AquaticLifeHistory
#' @author Jonathan Smart
#' @description Estimate aquatic species life history using robust techniques.
#' This package supports users undertaking two types of analysis: 1) Growth from
#' length-at-age data, and 2) maturity analyses for length and/or age data.
#'
#' Maturity analyses are performed using generalised linear model approaches incorporating
#' either a binomial or quasibinomial distribution.
#'
#' Growth modelling is performed using the multimodel approach presented by
#' Smart et al. (2016) "Multimodel approaches in shark and ray growth studies:
#' strengths, weaknesses and the future" <doi:10.1111/faf.12154>.
#' @docType package
#' @references To cite the AquaticLifeHistory package in publications, type citation('AquaticLifeHistory').
NULL

utils::globalVariables(c("Age", "Pred", "Model", "AVG", "w.AVG", ".", "Estimate", 'high', "low",
                         'Maturity', 'N', 'Maturity.prop','Maturity.group', 'prop', 'na.omit',
                         'term', 'Length', 'upp', 'Len.bin', 'Len.start', 'Len.fin', 'Maturity.group'))

#' Length-at-age data for blacktip sharks
#'
#' A data set containing the length-at-age data for common blacktip sharks (\emph{Carcharhinus limbatus})
#'    from Indonesia. This data was published in Smart et al. (2015).
#'
#' \itemize{
#'   \item Age. Number of growth bands determined from vertebral analysis
#'   \item Length. Total Length in mm determined via back-calculation
#'   \item Sex. Females (F) or males (M)
#' }
#'
#' @docType data
#' @keywords datasets
#' @name growth_data
#' @usage data(growth_data)
#' @format A data frame with 294 rows and 3 variables
#' @references Smart et al (2015)  Age and growth of the common
#'    blacktip shark Carcharhinus limbatus from Indonesia, incorporating an improved approach to
#'    comparing regional population growth rates. African Journal of Marine Science 37:177-188.
#'    \url{https://www.tandfonline.com/doi/abs/10.2989/1814232X.2015.1025428}
NULL

#' Age and length-at-maturity data for silky sharks
#'
#' A data set containing the length-at-maturity and age-at-maturity data for female silky sharks
#'    (\emph{Carcharhinus falciformis}) from Papua New Guinea. This data was published in Grant et al (2018)
#'
#' \itemize{
#'   \item Tag. Unique identifier for each Shark
#'   \item Age. Number of growth bands determined from vertebral analysis
#'   \item Length. Total Length in cm
#'   \item Maturity. Binary maturity status: immature = 0 and mature = 1
#' }
#'
#' @docType data
#' @keywords datasets
#' @name maturity_data
#' @usage data(maturity_data)
#' @format A data frame with 284 rows and 4 variables
#' @references Grant et al (2018)
#'    Life history characteristics of the silky shark (\emph{Carcharhinus falciformis})
#'    from the central west Pacific. Marine and Freshwater Research 69:562-573
#'    \url{http://www.publish.csiro.au/mf/MF17163}
NULL
