#' Compute the euphotic depth from surface satellite Chlorophyll-a concentration
#'
#' @param CHLsat the value of satellite Chl (mg.m-3)
#'
#' @author Raphaëlle Sauzède \email{raphaelle.sauzede@imev-mer.fr}
#'
#' @return The function returns the value of the euphotic depth (m)
#' @export
#'
#' @examples compute_Ze(0.1)
#' 
compute_Ze <- function(CHLsat){
  
  ## Compute Ze
  ###############
  
  ## First compute Kd
  ##--------------
  # K: global attenuation coefficient
  # Kw: pure sea water attenuation coefficient (m^-1)(490 nm)
  # Kbio: biological attenuation coefficient (490 nm)
  # K490: attenuation coefficient at 490 nm
  
  # For wavelength lambda = 490 nm
  Kw <- 0.01660
  e <- 0.68955
  X <- 0.07242
  
  # From A. Morel 2001
  K490 <- Kw + X*CHLsat^e
  
  # From Rochford et al. 2001
  Kd <- 0.0085 + 1.6243*K490
  
  Ze <- -log(0.01)/Kd
  return(Ze)
}
