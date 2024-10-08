#' @title calcMulticroppingIntensity
#'
#' @description Returns cropping intensity according to LandInG data
#'              given the chosen scenario
#'
#' @param selectyears   Years to be returned
#' @param scenario      "total": currently multicropped areas calculated from total harvested areas
#'                               and total physical areas per cell from readLandInG
#'                      "crop" (crop-specific), "irrigation" (irrigation-specific),
#'                      "irrig_crop" (crop- and irrigation-specific)
#' @param sectoral      "kcr" MAgPIE crop types, and "lpj" LPJmL crop types
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MulticroppingIntensity", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput readSource toolGetMapping
#' @importFrom magclass dimSums
#'

calcMulticroppingIntensity <- function(scenario, selectyears,
                                       sectoral = "lpj") {

  if (grepl("kcr", sectoral)) {

    # Crops that are not suitable for multiple cropping (either because they are a perennial
    # crop that's grown over several years or because the growing period is too long to
    # allow for another season
    cropsNoMC <- c("sugr_cane", "oilpalm", "betr", "begr")

  } else if (grepl("lpj", sectoral)) {

    # Crops that are not suitable for multiple cropping (either because they are a perennial
    # crop that's grown over several years or because the growing period is too long to
    # allow for another season
    cropsNoMC <- c("sugarcane", "betr", "begr")

  }

  # areas where multicropping takes place currently (crop- and irrigation-specific)
  phys <- calcOutput("CropareaLandInG", physical = TRUE, sectoral = sectoral,
                     cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                     selectyears = selectyears, aggregate = FALSE)
  harv <- calcOutput("CropareaLandInG", physical = FALSE, sectoral = sectoral,
                     cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                     selectyears = selectyears, aggregate = FALSE)
  # keep for dimensionality
  phys[, , ] <- NA
  harv[, , ] <- NA

  if (scenario == "total") {
    # total actual multicropping area
    tempPhys <- dimSums(calcOutput("CropareaLandInG", physical = TRUE, sectoral = sectoral,
                                   cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
                                   selectyears = selectyears, aggregate = FALSE),
                        dim = "crop")
    tempHarv <- dimSums(calcOutput("CropareaLandInG", physical = FALSE, sectoral = sectoral,
                                   cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
                                   selectyears = selectyears, aggregate = FALSE),
                        dim = "crop")

    # expand dimension
    phys[, , ] <- tempPhys
    harv[, , ] <- tempHarv

  } else if (scenario == "irrig") {
    # total actual multicropping area (irrigation-specific)
    tempPhys <- dimSums(calcOutput("CropareaLandInG", physical = TRUE, sectoral = sectoral,
                                   cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                                   selectyears = selectyears, aggregate = FALSE),
                        dim = "crop")
    tempHarv <- dimSums(calcOutput("CropareaLandInG", physical = FALSE, sectoral = sectoral,
                                   cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                                   selectyears = selectyears, aggregate = FALSE),
                        dim = "crop")

    # expand dimension
    phys[, , ] <- tempPhys
    harv[, , ] <- tempHarv

  } else if (scenario == "crop") {
    # areas where multicropping takes place currently (crop-specific)
    tempPhys <- calcOutput("CropareaLandInG", physical = TRUE, sectoral = sectoral,
                           cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
                           selectyears = selectyears, aggregate = FALSE)
    tempHarv <- calcOutput("CropareaLandInG", physical = FALSE, sectoral = sectoral,
                           cellular = TRUE, cells = "lpjcell", irrigation = FALSE,
                           selectyears = selectyears, aggregate = FALSE)

    # expand dimension
    phys[, , ] <- tempPhys
    harv[, , ] <- tempHarv

  } else if (scenario == "irrig_crop") {
    # areas where multicropping takes place currently (crop- and irrigation-specific)
    phys <- calcOutput("CropareaLandInG", physical = TRUE, sectoral = sectoral,
                       cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                       selectyears = selectyears, aggregate = FALSE)
    harv <- calcOutput("CropareaLandInG", physical = FALSE, sectoral = sectoral,
                       cellular = TRUE, cells = "lpjcell", irrigation = TRUE,
                       selectyears = selectyears, aggregate = FALSE)

  } else {
    stop("Please select whether total, irrigation-specific (irrig), crop-specific (crop),
    or irrigation- and crop-specific (irrig_crop) cropping intensity shall be
    returned in the scenario argument of calcMulticroppingIntensity")
  }

  # cropping intensity factor
  currMC <- ifelse(phys > 0, harv / phys, NA)

  # corrections
  currMC[currMC > 2]    <- 2
  currMC[is.na(currMC)] <- 1

  # change dimension order of third dimension
  currMC <- dimOrder(currMC, c(2, 1), dim = 3)

  ##############
  ### Checks ###
  ##############
  if (any(is.na(currMC))) {
    stop("mrland::calcMulticroppingIntensity produced NA values")
  }
  if (any(currMC > 2 | currMC < 1)) {
    stop("Problem in mrland::calcMulticroppingIntensity:
         Value should be between 1 and 2!")
  }
  if (any(currMC[, , cropsNoMC] > 1)) {
    stop(paste0("Problem in mrland::calcMulticroppingIntensity: ",
                "Crops that are not suitable for multiple cropping ",
                "should not have CI > 1"))
  }

  ##############
  ### Return ###
  ##############
  out         <- currMC
  unit        <- "factor"
  description <- paste0("Cropping Intensitiy of different crops ",
                        "under irrigated and rainfed conditions respectively")

  return(list(x            = out,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
