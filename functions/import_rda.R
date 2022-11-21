# function to import Rda file but with a custom name
import_rda <- function(file, rm_rda = TRUE) {
  # load the Rda file in the function's environment (not the global one)
  load(file)
  
  # extract the object's name from the file name
  obj <- stringr::str_replace(file, 'data/telemetries/', replacement = '')
  obj <- stringr::str_replace(obj, '.Rda', replacement = '')
  
  # return the imported object
  return(get(obj))
}

# test the function
if(FALSE) {
  import_rda('data/telemetries/Oreamnos_americanus.Rda')
}
