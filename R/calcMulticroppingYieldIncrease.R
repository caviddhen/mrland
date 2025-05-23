#' @title calcMulticroppingYieldIncrease
#'
#' @description Calculates yield increase achieved through multiple cropping
#'              (as factor of off season to main season crop yield) under
#'              irrigated and rainfed conditions respectively.
#'              Optionally: return which grid cells are potentially suitable for
#'              multiple cropping activities under rainfed and irrigated conditions.
#'              Calculation is based on grassland gross primary production (GPP)
#'              in the growing period of the respective crop and annual grass GPP.
#'
#' @param selectyears   Years to be returned
#' @param lpjml         LPJmL version required for respective inputs as single string: "crop" version
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param fallowFactor  Factor determining yield reduction in off season due to
#'                      fallow period between harvest of first (main) season and
#'                      sowing of second (off) season
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MulticroppingYieldIncrease", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass getSets mbind getItems new.magpie
#'

calcMulticroppingYieldIncrease <- function(selectyears, lpjml, climatetype,
                                           fallowFactor = 0.75) {

  # Function requires lpjml argument in standard format
  if (length(lpjml) == 1) {
    lpjml <- c(natveg = "NULL", crop = lpjml)
  }

  ####################
  ### Read in data ###
  ####################
  # grass GPP in the entire year (main + off season) where cropping is possible
  # taking the growing period months into account (in tDM/ha)
  grassGPPannual <- setYears(calcOutput("GrassGPP", season = "wholeYear",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, aggregate = FALSE),
                             selectyears)
  # grass GPP in the growing period of LPJmL (main season) (in tDM/ha)
  grassGPPgrper  <- setYears(calcOutput("GrassGPP", season = "mainSeason",
                                        lpjml = lpjml, climatetype = climatetype,
                                        selectyears = selectyears, aggregate = FALSE),
                             selectyears)
  # crop yields in main season provided by LPJmL for LPJmL crop types (in tDM/ha)
  cropYields  <- calcOutput("YieldsLPJmL", source = lpjml,
                            climatetype = climatetype,
                            years = selectyears,
                            cells = "lpjcell", aggregate = FALSE)

  # ensure that ordering of third dimension is the same for all objects
  cropIrrigList <- getItems(grassGPPannual, dim = 3)
  # make sure crops and ordering of objects is the same
  cropYields <- cropYields[, , cropIrrigList]
  getSets(cropYields)["d3.1"] <- "crop"
  getSets(cropYields)["d3.2"] <- "irrigation"


  ####################
  ### Calculations ###
  ####################

  ### Yield Increase Factor  ###
  # Calculate multiple cropping factor based on annual grass GPP and
  # grass GPP in growing period of crop
  grassGPPoffseason <- (grassGPPannual[, , cropIrrigList] - grassGPPgrper[, , cropIrrigList])
  grassGPPoffseason[grassGPPoffseason < 0] <- 0

  increaseFACTOR <- ifelse(grassGPPgrper > 0,
                           grassGPPoffseason / grassGPPgrper,
                           0) * fallowFactor

  # Add missing crops (betr, begr, mgrass)
  # [Note: grown throughout the whole year] ---> set to 0
  missingCrops <- new.magpie(cells_and_regions = getItems(increaseFACTOR, dim = 1),
                             years = getItems(increaseFACTOR, dim = 2),
                             names = c("betr.irrigated", "betr.rainfed",
                                       "begr.irrigated", "begr.rainfed",
                                       "mgrass.irrigated", "mgrass.rainfed"),
                             fill = 0)
  getSets(missingCrops) <- getSets(increaseFACTOR)
  increaseFACTOR        <- mbind(increaseFACTOR, missingCrops)

  ##############
  ### Checks ###
  ##############

  if (any(is.na(increaseFACTOR))) {
    stop("calcMulticroppingYieldIncrease produced NA values")
  }
  if (any(increaseFACTOR < 0)) {
    stop("calcMulticroppingYieldIncrease produced negative values")
  }

  ##############
  ### Return ###
  ##############
  unit        <- "factor"
  description <- paste0("Factor of yield increase through multiple cropping ",
                        "to be applied on LPJmL crop yield")

  return(list(x            = increaseFACTOR,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
