#' GTFS Create shapes
#' 
#' Opens GTFS and generates shapes based on stop considering straight lines connecting them.
#' @param gtfs_location String. The location of the GTFS zip file. Either local or URL.
#' @param zipfile String. Path to the zip file the feed should be written to. The file is overwritten is it already exists.
#' 
#' @details
#' Shapes is created using `GTFSwizard::get_shapes` method.
#' 
#' @seealso [GTFSwizard::get_shapes()]
#' 
#' @examples
#' \dontrun{
#' gtfs_create_shapes("gtfs_withoutShapes.zip", "gtfs_withShapes.zip")
#' }
#' 
#' @import zip
#' @import GTFSwizard
#' 
#' @export
gtfs_create_shapes <- function(gtfs_location, zipfile) {
  # GTFSwizard::get_shapes is automatically applied when it detected shapes are missing
  gtfs_fixed <- GTFSwizard::read_gtfs(gtfs_location) 
  
  # Create temporary files for shapes and the unzipped GTFS contents
  temp_shapes <- tempfile(fileext = "_shapes.txt")
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # Write shapes.txt to a temporary file
  write.csv(gtfs_fixed$shapes, temp_shapes, row.names = FALSE)
  
  # Unzip GTFS to temporary file
  unzip(gtfs_location, exdir = temp_dir)
  
  # Move the new CSV to the temp folder
  file.copy(temp_shapes, file.path(temp_dir, "shapes.txt"))
  
  # Create a new ZIP with all files (old + new)
  zip::zip(zipfile, files = list.files(temp_dir, full.names = TRUE), mode = "cherry-pick")
  
  # Clean up temporary files
  unlink(temp_shapes)
  unlink(temp_dir, recursive = TRUE)
  
  return(zipfile)
}

