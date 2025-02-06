library(GTFSwizard, include.only = c("read_gtfs", "write_gtfs"))
library(zip)

#' Opens GTFS and generates shapes based on stops (straight lines connecting them)
#' Stores it at gtfs_location, appending "_shaped" to the file name
#' @param gtfs_location The location of the GTFS ZIP file
gtfs_create_shapes <- function(gtfs_location) {
  # GTFSwizard::get_shapes is automatically applied when it detected shapes are missing
  gtfs_fixed <- GTFSwizard::read_gtfs(gtfs_location) 
  
  # Save shapes.txt in the same location, but with _shapes.txt appended to gtfs name
  shapes_location = sprintf("%s/%s_shapes.txt", dirname(gtfs_location), tools::file_path_sans_ext(basename(gtfs_location)))
  write.csv(gtfs_fixed$shapes, shapes_location)
  
  # Unzip GTFS to temporary file
  temp_dir <- tempfile()
  dir.create(temp_dir)
  unzip(gtfs_location, exdir = temp_dir)
  
  # Move the new CSV to the temp folder
  file.copy(shapes_location, file.path(temp_dir, "shapes.txt"))
  
  # Create a new ZIP with all files (old + new)
  gtfs_new_location = sprintf("%s/%s_withShapes.zip", dirname(gtfs_location), tools::file_path_sans_ext(basename(gtfs_location)))
  zip::zip(gtfs_new_location, files = list.files(temp_dir, full.names = TRUE), mode = "cherry-pick")
  
  # Remove csv created in the root folder, as it was already copied to the zip folder
  unlink(shapes_location)
  
  return(gtfs_new_location)
}

