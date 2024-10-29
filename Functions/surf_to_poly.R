#' Function to convert a surface to a polygon
#' based on code from Gregor Siegmund, which was based on 
#' https://github.com/r-spatial/sf/issues/2289
#' Essentially keeps x,y positions but drops surface-like features
#' 
#' 
#' @param obj sf object that is a surface
#'
#' @return sf object converted to a polygon
#' @export
#'
#' @examples
#' #Default use
#' 

surf_to_poly <- function(obj) {
  
  # the function grabs the text representation of the geometry, 
  # removes the pieces that declare a particular component of that geometry to 
  # be a "MULTISURFACE", "COMPOUNDCURVE", and "CURVEPOLYGON" but leaves behind
  # all of the x/y positions of the nodes and any other geometry designations 
  # (e.g., "MULTIPOLYGON", "LINESTRING", "POLYGON"). 
  # We remove those pieces by looking for that text within the text representation
  # of the geometry, then using the gsub() function to replace that text with 
  # nothing (i.e., "").
  out_geom_txt <- obj |> 
    sf::st_geometry() |> 
    sf::st_as_text() |> 
    gsub(pattern = "MULTISURFACE (", replacement = '', fixed = TRUE) |> 
    gsub(pattern = "COMPOUNDCURVE (", replacement = '', fixed = TRUE) |> 
    gsub(pattern = "CURVEPOLYGON (", replacement = '', fixed = TRUE)
  
  #  it sets the geometry of the input 
  # object to be this newly modified text representation of the geometry.
  out <- obj |> 
    sf::st_set_geometry(
      value = sf::st_as_sfc(
        out_geom_txt, 
        crs = sf::st_crs(obj)
      )
    )
  
  # it gets the row number for all of the geometries of type "LINESTRING".
  idx <- which(sf::st_geometry_type(out) == "LINESTRING")
  
  # function calls every row that isn't of geometry type "LINESTRING" good 
  # to go (creating the out_greenlight object with all of those rows).
  out_greenlight <- out[-idx, ]
  
  # For the rows that are of geometry type "LINESTRING", 
  # we cast them to a "POLYGON" then to a "MULTIPOLYGON", 
  # calling it out_redlight. 
  out_redlight <- out |> 
    dplyr::slice(idx) |> 
    sf::st_make_valid() |> 
    sf::st_cast(to = "POLYGON") |> 
    sf::st_cast(to = "MULTIPOLYGON") 
  
  # we cast everything to MULTIPOLYGON and 
  # make those geometries valid using sf::st_make_valid()
  out <- rbind(out_greenlight, out_redlight) |> 
    sf::st_cast("MULTIPOLYGON") |> 
    sf::st_make_valid()
  
  return(out)
  
}