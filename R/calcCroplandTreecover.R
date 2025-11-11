#' @title calcCroplandTreecover
#'
#' @description Returns area on cropland covered by trees (Mha).

#' @param maginput Whether data should be corrected to align with cropland
#' initialised in MAgPIE.
#' @param cells magpiecell (59199 cells) or lpjcell (67420 cells)
#' @param countryLevel    Whether output shall be at country level.
#'                         Requires aggregate=FALSE in calcOutput.
#'
#' @return List with a magpie object
#' @author Patrick v. Jeetze
#' @seealso
#' \code{\link{readCopernicus}}
#'
#' @examples
#' \dontrun{
#' calcOutput("CroplandTreecover", aggregate = FALSE)
#' }
#'
#' @importFrom mstools toolCoord2Isocell
#'
calcCroplandTreecover <- function(maginput = TRUE, cells = "magpiecell", countryLevel = FALSE) {
  treecover <- readSource("Copernicus", subtype = "CroplandTreecover", convert = "onlycorrect")

  if (maginput) {
    luh <- calcOutput("LUH3",
      landuseTypes = "magpie", aggregate = FALSE,
      cellular = TRUE, irrigation = FALSE,
      yrs = 2015
    )
    getYears(luh) <- NULL
    getCells(luh) <- getCells(treecover)

    # cropland treecover area is corrected to make sure that it is not
    # larger than cropland area reported by LUH
    out <- pmin(treecover, luh[, , "crop"])
  } else {
    out <- treecover
  }

  out <- collapseDim(out, dim = 3)

  if (countryLevel) {

    out <- toolCountryFill(dimSums(out, dim = c("x", "y")), fill = 0)

  } else {

    if (cells == "magpiecell") {

      out <- toolCoord2Isocell(out)

    } else if (cells == "lpjcell") {

      out <- out

    } else {
      stop("Please specify cells argument")
    }
  }

  return(list(
    x = out,
    weight = NULL,
    unit = "Mha",
    description = paste(
      "Cropland area covered by trees in 2015"
    ),
    isocountries = FALSE
  ))
}
