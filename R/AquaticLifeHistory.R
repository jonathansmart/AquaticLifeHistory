#' AquaticLifeHistory
#'
#' @name AquaticLifeHistory
#' @author Jonathan Smart
#' @description A package to estimate life histories of aquatic species using robust techniques.
#' @docType package
NULL


#' Length-at-age data for blacktip sharks
#'
#' A dataset containing the length-at-age data for common blacktip sharks (\emph{Carcharhinus limbatus})
#'    from Indonesia. This data was published in Smart et al (2015).
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
#' A dataset containing the length-at-maturity and age-at-maturity data for female silky sharks
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
