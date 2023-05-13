coordenadas <- read.csv("coordenadasCentrales.csv", header = TRUE, sep = ",")

# Define the 20 reference coordinates
ref_coords <- matrix(c(1, 2, 3, 4, 5, 6, ..., 37, 38, 39, 40), ncol=2, byrow=TRUE)

# Define the coordinates to classify
coords_to_classify <- matrix(c(10, 10, 20, 20, 30, 30, ..., 70, 70, 80, 80), ncol=2, byrow=TRUE)

# Compute the distances between each coordinate and each reference coordinate
distances <- apply(coords_to_classify, 1, function(x) apply(ref_coords, 1, function(y) dist(rbind(x, y))))

# Find the index of the reference coordinate with the minimum distance for each coordinate
nearest_ref_coords <- apply(distances, 1, which.min)

# Print the results
for (i in 1:nrow(coords_to_classify)) {
  cat(sprintf("Coordinate %d: (%d, %d) is nearest to reference coordinate %d: (%d, %d)\n", 
              i, coords_to_classify[i, 1], coords_to_classify[i, 2], 
              nearest_ref_coords[i], ref_coords[nearest_ref_coords[i], 1], ref_coords[nearest_ref_coords[i], 2]))
}
