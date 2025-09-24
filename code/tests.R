library(testthat)

# Define a test case for mkdeToTerra function
test_that("mkdeToTerra converts mkde object to SpatRaster", {
  # Create a sample mkde object
  mkde_obj <- list(
    x = c(0, 1, 2),  # Sample x coordinates
    y = c(0, 1, 2),  # Sample y coordinates
    nx = 3,          # Number of columns
    ny = 3,          # Number of rows
    dimension = 2,   # Dimension of the object
    d = array(1:9, dim = c(3, 3, 1))  # Sample data
  )
  
  expect_true(TRUE)
  
  # Test: Check if mkdeToTerra returns a SpatRaster
  test_that("Returns a SpatRaster", {
    raster_result <- mkdeToTerra(mkde_obj)
    expect_is(raster_result, "SpatRaster")
  })
  
  # Test: Check if the number of cells matches the input mkde object
  test_that("Correct number of cells", {
    raster_result <- mkdeToTerra(mkde_obj)
    expect_equal(ncell(raster_result), mkde_obj$nx * mkde_obj$ny)
  })
  
  # Test: Check the extent using individual coordinates (assuming extent retrieval method is fixed)
  test_that("Correct extent", {
    raster_result <- mkdeToTerra(mkde_obj)
    expect_equal(xmin(raster_result), min(mkde_obj$x) - 0.5 * (mkde_obj$x[2] - mkde_obj$x[1]))
    expect_equal(xmax(raster_result), max(mkde_obj$x) + 0.5 * (mkde_obj$x[2] - mkde_obj$x[1]))
    expect_equal(ymin(raster_result), min(mkde_obj$y) - 0.5 * (mkde_obj$y[2] - mkde_obj$y[1]))
    expect_equal(ymax(raster_result), max(mkde_obj$y) + 0.5 * (mkde_obj$y[2] - mkde_obj$y[1]))
  })
  
  # Test: Check if values are retrieved
  test_that("Values retrieval", {
    raster_result <- mkdeToTerra(mkde_obj)
    expected_values <- data.frame(lyr.1 = c(7, 8, 9, 4, 5, 6, 1, 2, 3))
    expect_equal(as.data.frame(values(raster_result)), expected_values)
  })
  
  # Test: Check the coordinate reference system (CRS) of the SpatRaster
  test_that("Correct CRS", {
    raster_result <- mkdeToTerra(mkde_obj)
    # Assuming the expected CRS is empty
    expect_is(crs(raster_result), "character")
    expect_equal(crs(raster_result), "")
  })
  
  # Test: Check the resolution of the SpatRaster
  test_that("Correct resolution", {
    raster_result <- mkdeToTerra(mkde_obj)
    expect_equal(res(raster_result), c(1, 1))
  })
  
  # Test: Check the range of values in the SpatRaster
  test_that("Correct range of values", {
    raster_result <- mkdeToTerra(mkde_obj)
    expect_equal(min(values(raster_result)), 1)
    expect_equal(max(values(raster_result)), 9)
  })
  
  # Test: Check for invalid input
  test_that("Handles invalid input", {
    
    # Test with invalid mkde_obj (e.g., non-numeric data)
    expect_error(mkdeToTerra(list(x = "a", y = "b", nx = 3, ny = 3, dimension = 2, d = array(1:9, dim = c(3, 3, 1)))))
    
  })
  
  message("All mkdeToTerra tests passed!") 
  
})


test_that("terraToContour function tests", {
  
  
  # Mock terra object for testing
  mock_terra_obj <- terra::rast(matrix(1:25, nrow = 5, ncol = 5))
  
  expect_true(TRUE)  # Placeholder to ensure the outer unit test runs
  
  # Test: Check if the function returns an object of class sf
  test_that("Function returns an object of class sf", {
    contour <- terraToContour(mock_terra_obj, levels = c(1, 2, 3), crsstr = "+proj=utm +zone=33 +datum=WGS84")
    expect_is(contour, "sf")
  })
  
  # Test: Check if the function correctly sets the CRS of the contour object
  test_that("Function sets the CRS of the contour object correctly", {
    contour <- terraToContour(mock_terra_obj, levels = c(1, 2, 3), crsstr = "+proj=utm +zone=33 +datum=WGS84")
    
    # Extract proj4string from CRS object
    crs <- sf::st_crs(contour)
    proj4_string <- crs$proj4string
    
    # Check proj4string
    expect_equal(proj4_string, "+proj=longlat +datum=WGS84 +no_defs", info = "proj4string for WGS84")
  })
  
  # Test: Check if the function returns NULL when given invalid input
  test_that("Function returns NULL for invalid input", {
    error <- expect_error(terraToContour(NULL, levels = c(1, 2, 3), crsstr = "+proj=utm +zone=33 +datum=WGS84"), NULL)
    
    expect_identical(error$message, "SpatRaster object is empty or NULL.")
  })
  
  # Test: Check if the function works with different levels
  test_that("Function works with different levels", {
    contour <- terraToContour(mock_terra_obj, levels = c(1, 2, 3, 4, 5), crsstr = "+proj=utm +zone=33 +datum=WGS84")
    expect_is(contour, "sf")
  })
  
  # Test: Check if the function returns NULL when levels are not provided
  test_that("Function throws an error when levels are not provided", {
    # Call the function without providing levels
    error <- expect_error(terraToContour(mock_terra_obj, levels = NULL, crsstr = "+proj=utm +zone=33 +datum=WGS84"))
    
    # Expect specific error message
    expect_identical(error$message, "Proper input must be provided.")
  })
  
  # Test: Check if the function returns NULL when crsstr is not provided
  test_that("Function throws an error when crsstr is not provided", {
    error <- expect_error(terraToContour(mock_terra_obj, levels = c(1, 2, 3), crsstr = NULL))
    
    # Expect specific error message
    expect_identical(error$message, "Proper input must be provided.")
  })
  
  # Test: Check if the function handles empty input correctly
  test_that("Function handles empty input correctly", {
    error <- expect_error(terraToContour(terra::rast(), levels = c(1, 2, 3), crsstr = "+proj=utm +zone=33 +datum=WGS84"), NULL)
    expect_identical(error$message, "SpatRaster object is empty or NULL.")
  })
  
  message("all terraToContour tests passed")
})