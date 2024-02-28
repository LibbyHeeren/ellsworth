##########################################################################
# Load packages
library(tidyverse)

# Define a function to generate a random vector of colors
generate_color_vector <- function(size, colors) {

  # Create a size^2 vector filled with a random sample of colors from a color list
  color_vector <- sample(x = colors,
                         size = size * size,   # "size" is the # of squares on each side
                         replace = TRUE)

  return(color_vector)
}

# Set the size of the desired grid and calculate number of circuits
size <- 40
circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

# Define the colors
# I did my best to sample the unique colors from his work and this is what I got.
# I've read that the paper he used for color inspo came in 18 unique colors,
# but I've only identified about 16 unique colors for now. I do think there are some
# more to be distinguished in there between the darker blues and red-oranges.
# Removing white for this iteration.

# My original sampling:
# colors <- c(#"#ECECE4", # Paper
#             "#007b7b", # Teal
#             "#243B81", # Dark blue
#             "#3BADC7", # Light blue
#             "#0465C3", # Blue
#             "#eda29a", # Pink
#             "#d2a1c0", # Light purple
#             "#9f1c6c", # Purple
#             "#952017", # Dark red
#             "       ", # Red
#             "#c3280b", # Red orange
#             "#e4581c", # Orange
#             "#eb871b", # Light orange
#             "#fccd02", # Yellow
#             "#955507", # Brown
#             "#0b0c18", # Black
#             "#017834", # Dark green
#             "#74aa3a", # Green
#             "#b1c575", # Light green)

# My second sampling, too washed out
# colors <- c(#"#ECECE4", # Paper
#             "#0F6864", # Teal
#             "#165195", # Dark blue
#             "#26ADE5", # Light blue
#             "#0F6CBA", # Blue
#             "#DD97B3", # Pink
#             "#B1B0D0", # Light purple
#             "#7D3678", # Purple
#             "#6B170D", # Dark red
#             "#A8272B", # Red
#             "#CB4C39", # Red orange
#             "#D5653F", # Orange
#             "#E3873E", # Light orange
#             "#F3D040", # Yellow
#             "#874E33", # Brown
#             "#000000", # Black
#             "#23695F", # Dark green
#             "#86B06E", # Green
#             "#AFCBA3", # Light green)

# My final sampling, maybe too vibrant?
colors <- c(#"#EDEFEE", # Paper
            "#1A8BB3", # Teal - no longer teal, just bright blue
            "#0950AE", # Dark blue
            "#4DACE5", # Light blue
            "#126DDB", # Blue
            "#E48DC4", # Pink
            "#ABA9E8", # Light purple
            "#872791", # Purple
            "#6D1617", # Dark red
            "#B81634", # Red
            "#DF3B43", # Red orange
            "#E35C47", # Orange
            "#EB8749", # Light orange
            "#F6E254", # Yellow
            "#7B442D", # Brown
            "#000000", # Black
            "#1A6E7E", # Dark green - no longer dark green, now looks teal
            "#7CBF7B", # Green
            "#ADD2B8") # Light green

# Generate the color grid
color_vector <- generate_color_vector(size, colors)

# Create a data frame for the grid coordinates
df <- expand.grid(x = 1:size, y = 1:size)

# Add the corresponding color to each grid cell coordinate
df$color <- color_vector

# I now have a data frame full of random colors and I want to make some of
# them paper-white, based on a set of probabilities.

# Include my function that calculates probabilities based on circuits
# Maybe I should make it based on size? I will already have circuits, though.
get_prob_vector <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1) # 3

  last90perc_length <- circuits - length(first10perc)

  last10perc_length <- round(last90perc_length * (1/9)) # 2

  middle80perc_length <- last90perc_length - last10perc_length # 15

  middle80perc <- seq(0.02857143, 1, length.out = middle80perc_length+2)[-c(1, middle80perc_length+2)]

  last10perc <- rep(1, last10perc_length)

  prob_vector <- c(first10perc, middle80perc, last10perc)

  return(prob_vector)
}

prob_vector <- get_prob_vector(circuits)

# Create function that builds the prob matrix
get_prob_matrix <- function(size, prob_vector){

  # Calculate quad size same way as circuits
  quad_size <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

  # Create empty matrix for the quad
  M <- matrix(0, nrow = quad_size, ncol = quad_size)

  # For loop to assign prob_vector to correct cells in quadrant
  for (i in 1:quad_size){

    M[i, i:quad_size] <- prob_vector[i]
    M[i:quad_size, i] <- prob_vector[i]
  }

  # if size is even,
  if(size %% 2 == 0){
    # mirror horizontally and column bind
    M_right <- apply(M, 1, rev)
    M <- cbind(M, M_right)

    # then mirror vertically and row bind
    M_down <- apply(M, 2, rev)
    M <- rbind(M, M_down)

  }else{ # if size is odd
    # mirror all but last col horizontally and col bind
    M_right <- apply(M[ , 1:(quad_size-1)], 1, rev)
    M <- cbind(M, M_right)

    # then mirror all but last row vertically and row bind
    M_down <- apply(M[1:(quad_size-1), ], 2, rev)
    M <- rbind(M, M_down)

  }

  return(M)
}

M <- get_prob_matrix(size, prob_vector)


# Apply M to df as a vector
df$probs <- as.vector(M)

# Can I verify I did this correctly by plotting a rounded version of each
# prob inside a tile? I asked ChatGPT to do this quickly and it came through

ggplot(df, aes(x = x, y = y, label = round(probs, 2))) +
  geom_tile(aes(fill = probs), colour = "white") +
  geom_text() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  coord_fixed()

# Oh nooooo, something has gone wrong with my flipping. When I first create M,
# I know the initial quadrant is probably correct, but let's use this plotting
# method to verify that, and then view the plot at each stage of flipping.
# I may need to use a different method of flipping. Pasted image in Notion.

# I just cleared my environment and reran all code EXCEPT creating the
# get_prob_matrix function

quad_size <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

# Create empty matrix for the quad
M <- matrix(0, nrow = quad_size, ncol = quad_size)

# Plot the empty matrix
grid_data <- expand.grid(row = 1:20, col = 1:20)
grid_data$probs <- as.vector(M)

ggplot(grid_data, aes(x = col, y = row, label = round(probs, 2))) +
  geom_tile(aes(fill = probs), colour = "white") +
  geom_text() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  coord_fixed()

# For loop to assign prob_vector to correct cells in quadrant
for (i in 1:quad_size){

  M[i, i:quad_size] <- prob_vector[i]
  M[i:quad_size, i] <- prob_vector[i]
}

# Plot again
grid_data$probs <- as.vector(M)

ggplot(grid_data, aes(x = col, y = row, label = round(probs, 2))) +
  geom_tile(aes(fill = probs), colour = "white") +
  geom_text() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  coord_fixed()

# Nooooo. Could it be as.vector? Try reshape2::melt() instead

grid_data$probs <- reshape2::melt(M)[, 3]

ggplot(grid_data, aes(x = col, y = row, label = round(probs, 2))) +
  geom_tile(aes(fill = probs), colour = "white") +
  geom_text() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  coord_fixed()

# No, same issue, it's not building the quad correctly. Ok, let's draw it out.

ggplot(grid_data |> filter(row == 1), aes(x = col, y = row, label = round(probs, 2))) +
  geom_tile(aes(fill = probs), colour = "white") +
  geom_text() +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  coord_fixed()

# if size is even,
if(size %% 2 == 0){
  # mirror horizontally and column bind
  M_right <- apply(M, 1, rev)
  M <- cbind(M, M_right)

  # then mirror vertically and row bind
  M_down <- apply(M, 2, rev)
  M <- rbind(M, M_down)

}else{ # if size is odd
  # mirror all but last col horizontally and col bind
  M_right <- apply(M[ , 1:(quad_size-1)], 1, rev)
  M <- cbind(M, M_right)

  # then mirror all but last row vertically and row bind
  M_down <- apply(M[1:(quad_size-1), ], 2, rev)
  M <- rbind(M, M_down)

}

#########################################################################
# Notes in Notion - fixed plot code to add scale_y_reverse and fixed
# get_prob_matrix to use flipud and fliplr from {pracma}

# Load packages
library(tidyverse)
library(pracma)

# Define a function to generate a random vector of colors
generate_color_vector <- function(size, colors) {

  # Create a size^2 vector filled with a random sample of colors from a color list
  color_vector <- sample(x = colors,
                         size = size * size,   # "size" is the # of squares on each side
                         replace = TRUE)

  return(color_vector)
}

# Set the size of the desired grid and calculate number of circuits
size <- 40
circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

# Define the colors
colors <- c(#"#EDEFEE", # Paper
  "#1A8BB3", # Teal - no longer teal, just bright blue
  "#0950AE", # Dark blue
  "#4DACE5", # Light blue
  "#126DDB", # Blue
  "#E48DC4", # Pink
  "#ABA9E8", # Light purple
  "#872791", # Purple
  "#6D1617", # Dark red
  "#B81634", # Red
  "#DF3B43", # Red orange
  "#E35C47", # Orange
  "#EB8749", # Light orange
  "#F6E254", # Yellow
  "#7B442D", # Brown
  "#000000", # Black
  "#1A6E7E", # Dark green - no longer dark green, now looks teal
  "#7CBF7B", # Green
  "#ADD2B8") # Light green

# Generate the color grid
color_vector <- generate_color_vector(size, colors)

# Create a data frame for the grid coordinates
df <- expand.grid(x = 1:size, y = 1:size)

# Add the corresponding color to each grid cell coordinate
df$color <- color_vector

# Include my function that calculates probabilities based on circuits
# Maybe I should make it based on size? I will already have circuits, though.
get_prob_vector <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1) # 3

  last90perc_length <- circuits - length(first10perc)

  last10perc_length <- round(last90perc_length * (1/9)) # 2

  middle80perc_length <- last90perc_length - last10perc_length # 15

  middle80perc <- seq(0.02857143, 1, length.out = middle80perc_length+2)[-c(1, middle80perc_length+2)]

  last10perc <- rep(1, last10perc_length)

  prob_vector <- c(first10perc, middle80perc, last10perc)

  return(prob_vector)
}

prob_vector <- get_prob_vector(circuits)

# Create function that builds the prob matrix
get_prob_matrix <- function(size, prob_vector){

  # Calculate quad size same way as circuits
  quad_size <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

  # Create empty matrix for the quad
  M <- matrix(0, nrow = quad_size, ncol = quad_size)

  # For loop to assign prob_vector to correct cells in quadrant
  for (i in 1:quad_size){

    M[i, i:quad_size] <- prob_vector[i]
    M[i:quad_size, i] <- prob_vector[i]
  }

  # if size is even,
  if(size %% 2 == 0){
    # mirror horizontally and column bind
    M_right <- pracma::fliplr(M)
    M <- cbind(M, M_right)

    # then mirror vertically and row bind
    M_down <- pracma::flipud(M)
    M <- rbind(M, M_down)

  }else{ # if size is odd
    # mirror all but last col horizontally and col bind
    M_right <- pracma::fliplr(M[ , 1:(quad_size-1)])
    M <- cbind(M, M_right)

    # then mirror all but last row vertically and row bind
    M_down <- pracma::flipud(M[1:(quad_size-1), ])
    M <- rbind(M, M_down)

  }

  return(M)
}

M <- get_prob_matrix(size, prob_vector)


# Apply M to df as a vector
df$probs <- as.vector(M)

# Plot, but make sure the x axis is reversed

ggplot(df, aes(x = x, y = y, label = round(probs, 2))) +
  geom_tile(aes(fill = probs), colour = "white") +
  geom_text() +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_y_reverse() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")) +
  coord_fixed()

###################################################
# 2/26/2024

# Load packages
library(tidyverse)
library(pracma)

# Create functions needed (will source these)

# Define a function to generate a random vector of colors
generate_color_vector <- function(size, colors) {

  # Create a size^2 vector filled with a random sample of colors from a color list
  color_vector <- sample(x = colors,
                         size = size * size,   # "size" is the # of squares on each side
                         replace = TRUE)

  return(color_vector)
}

# Create function that calculates probabilities based on circuits
get_prob_vector <- function(circuits){

  first10perc <- seq(0, 0.02857143, length.out = round(circuits*.10)+1) # 3

  last90perc_length <- circuits - length(first10perc)

  last10perc_length <- round(last90perc_length * (1/9)) # 2

  middle80perc_length <- last90perc_length - last10perc_length # 15

  middle80perc <- seq(0.02857143, 1, length.out = middle80perc_length+2)[-c(1, middle80perc_length+2)]

  last10perc <- rep(1, last10perc_length)

  prob_vector <- c(first10perc, middle80perc, last10perc)

  return(prob_vector)
}

# Create function that builds the prob matrix
get_prob_matrix <- function(size, prob_vector){

  # Calculate quad size same way as circuits
  quad_size <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

  # Create empty matrix for the quad
  M <- matrix(0, nrow = quad_size, ncol = quad_size)

  # For loop to assign prob_vector to correct cells in quadrant
  for (i in 1:quad_size){

    M[i, i:quad_size] <- prob_vector[i]
    M[i:quad_size, i] <- prob_vector[i]
  }

  # if size is even,
  if(size %% 2 == 0){
    # mirror horizontally and column bind
    M_right <- pracma::fliplr(M)
    M <- cbind(M, M_right)

    # then mirror vertically and row bind
    M_down <- pracma::flipud(M)
    M <- rbind(M, M_down)

  }else{ # if size is odd
    # mirror all but last col horizontally and col bind
    M_right <- pracma::fliplr(M[ , 1:(quad_size-1)])
    M <- cbind(M, M_right)

    # then mirror all but last row vertically and row bind
    M_down <- pracma::flipud(M[1:(quad_size-1), ])
    M <- rbind(M, M_down)

  }

  return(M)
}

# Set parameters (size, color, and background color will be user inputs eventually)

# Set the size of the desired grid and calculate number of circuits
size <- 40
circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2)

# Choose background color, #EDEFEE is paper, #000000 is black
background <- "#EDEFEE"

# Define the colors
colors <- c(#"#EDEFEE", # Paper
  "#1A8BB3", # Teal - no longer teal, just bright blue
  "#0950AE", # Dark blue
  "#4DACE5", # Light blue
  "#126DDB", # Blue
  "#E48DC4", # Pink
  "#ABA9E8", # Light purple
  "#872791", # Purple
  "#6D1617", # Dark red
  "#B81634", # Red
  "#DF3B43", # Red orange
  "#E35C47", # Orange
  "#EB8749", # Light orange
  "#F6E254", # Yellow
  "#7B442D", # Brown
  "#000000", # Black
  "#1A6E7E", # Dark green - no longer dark green, now looks teal
  "#7CBF7B", # Green
  "#ADD2B8") # Light green

# End user parameters

# Generate the color vector
color_vector <- generate_color_vector(size, colors)

# Create a data frame for the grid coordinates
df <- expand.grid(x = 1:size, y = 1:size)

# Add the corresponding color to each grid cell coordinate
df$color <- color_vector

# Get the probability vector
prob_vector <- get_prob_vector(circuits)

# Assign probabilityes to matrix correctly
M <- get_prob_matrix(size, prob_vector)

# Apply prob matrix M to df as a vector
df$probs <- as.vector(M)

#######################
# New stuff starts here:

# Create a function that creates a new color column to replace the old one
get_kelly_III_vector <- function(df, background){

  # Write a loop that iterates over each row in df
  for (i in 1:nrow(df)){

    if (df$probs[i] == 0){
      df$color[i] <- background
    } else if (df$probs[i] == 1){
      df$color[i] <- df$color[i]
    } else {

      # If the random is greater than probs, assign background, if not, do nothing
      # grab a random number between 0 and 1
      random <- runif(n = 1)

      if (random > df$probs[i]){
        df$color[i] <- background
      }
    }
  }

  return(df)

}

df <- get_kelly_III_vector(df, background)

#######################
# End new stuff

# Plot
kelly_colors_III <-
  ggplot(df, aes(x = x, y = y, fill = color)) +
  geom_tile() +  # Add tiles
  scale_fill_identity() +  # Use the colors stored as strings in the color column
  theme_void() +  # Remove axis labels and background
  coord_equal()  # Use equal aspect ratio

# Print the plot
kelly_colors_III


##### Create a replacement get_color_vector function
get_color_vector <- function(size, colors){

  # Define an empty size x size matrix
  color_matrix <- matrix("", nrow = size, ncol = size)

  # For loop to go row by row
  for (i in 1:nrow(color_matrix)){

    # For loop to go column by column
    for (j in 1:ncol(color_matrix)){

      # If you're in the first cell, assign a random color
      if (i == 1 && j == 1){
        color_matrix[i,j] <- sample(x = colors,
                                    size = 1,
                                    replace = TRUE)
      } else if (i == 1 && j != 1){ # if rest of first row

        # Check the color of the cell to the left
        left_color <- color_matrix[i, j-1]
        # Assign anything but that color to this cell
        color_matrix[i,j] <- sample(x = colors[-which(colors == left_color)],
                                    size = 1,
                                    replace = TRUE)

      } else if (i != 1 && j == 1){ # if in 1st col of rows 2:end

        # Check the color of the cell above
        up_color <- color_matrix[i-1, j]
        # Assign anything but that color to this cell
        color_matrix[i,j] <- sample(x = colors[-which(colors == up_color)],
                                    size = 1,
                                    replace = TRUE)
      } else if (i != 1 && j != 1){

        # Check both left and up colors
        left_color <- color_matrix[i, j-1]
        up_color <- color_matrix[i-1, j]

        # Check if they're the same color, if so, assign other color
        if (left_color == up_color){
          color_matrix[i,j] <- sample(x = colors[-which(colors == up_color)],
                                      size = 1,
                                      replace = TRUE)
        } else {

          # If colors aren't the same, assign a random color
          color_matrix[i,j] <- sample(x = colors,
                                      size = 1,
                                      replace = TRUE)
        }
      }
    }
  }

  # return the color matrix as a vector
  return(as.vector(color_matrix))
}


# Second try to create a replacement get_color_vector function
get_color_vector <- function(size, colors){

  # Define an empty size x size matrix
  color_matrix <- matrix("", nrow = size, ncol = size)

  # For loop to go row by row
  for (i in 1:nrow(color_matrix)){

    # For loop to go column by column
    for (j in 1:ncol(color_matrix)){

      # If you're in the first cell, assign a random color
      if (i == 1 && j == 1){
        color_matrix[i,j] <- sample(x = colors,
                                    size = 1,
                                    replace = TRUE)
        (paste0("cell ", i, ", ", j, " filled"))
      } else if (i == 1 && j > 1){ # if rest of first row

        # Check the color of the cell to the left
        left_color <- color_matrix[i, j-1]
        # Assign anything but that color to this cell
        color_matrix[i,j] <- sample(x = colors[-which(colors == left_color)],
                                    size = 1,
                                    replace = TRUE)
        paste0("cell ", i, ", ", j, " filled")

      } else if (i == 2 && j == 1 || j == 2){ # if in 1st or 2nd col of row 2

        # Check the color of the cell above
        up_color <- color_matrix[i-1, j]
        # Assign anything but that color to this cell
        color_matrix[i,j] <- sample(x = colors[-which(colors == up_color)],
                                    size = 1,
                                    replace = TRUE)
        paste0("cell ", i, ", ", j, " filled")
      } else if (i == 2 && j > 2){ # if in second row, cols 3:end

        # Check both 2 left and 1 up colors, plus up_left
        left_color1 <- color_matrix[i, j-1]
        left_color2 <- color_matrix[i, j-2]
        up_color <- color_matrix[i-1, j]
        up_left_color <- color_matrix[i-1, j-1]

        # Check if they're the same color, if so, assign other color
        if (left_color1 == up_color || left_color1 == left_color2 || left_color1 == up_left_color){
          color_matrix[i,j] <- sample(x = colors[-which(colors %in% c(up_color, left_color1))],
                                      size = 1,
                                      replace = TRUE)
          paste0("cell ", i, ", ", j, " filled")
        } else {

          # If colors aren't the same, assign a random color
          color_matrix[i,j] <- sample(x = colors,
                                      size = 1,
                                      replace = TRUE)
          paste0("cell ", i, ", ", j, " filled")
        }
      } else if (i > 2 && j == 1){ # if in first col of rows 3 and down

        # Check colors of up1 and up2
        up_color1 <- color_matrix[i-1, j]
        up_color2 <- color_matrix[i-2, j]

        # Check if they're the same color, if so, assign other color
        if (up_color1 == up_color2){
          color_matrix[i,j] <- sample(x = colors[-which(colors == up_color1)],
                                      size = 1,
                                      replace = TRUE)
          paste0("cell ", i, ", ", j, " filled")
        } else {

          # If colors aren't the same, assign a random color
          color_matrix[i,j] <- sample(x = colors,
                                      size = 1,
                                      replace = TRUE)
          paste0("cell ", i, ", ", j, " filled")
        }
      } else if (i > 2 && j > 2){ # if in row 3 and down, col 3 and over

        # Check for all three conditions plus up_left
        left_color1 <- color_matrix[i, j-1]
        left_color2 <- color_matrix[i, j-2]
        up_color1 <- color_matrix[i-1, j]
        up_color2 <- color_matrix[i-2, j]
        up_left_color <- color_matrix[i-1, j-1]

        # If any matches, assign other color
        if (left_color1 == left_color2 || left_color1 == up_color1 || up_color1 == up_color2 || up_left_color == left_color1){

          color_matrix[i,j] <- sample(x = (colors[-which(colors %in% c(up_color1, left_color1))]),
                                      size = 1,
                                      replace = TRUE)
          paste0("cell ", i, ", ", j, " filled")
        } else {

          # If colors aren't the same, assign a random color
          color_matrix[i,j] <- sample(x = colors,
                                      size = 1,
                                      replace = TRUE)

        }
      }
    }
  }

  # return the color matrix as a vector
  return(as.vector(color_matrix))
}

################ THIRD TRY
# Third try to create a replacement get_color_vector function
get_color_vector <- function(size, colors){

  # Define an empty size x size matrix
  color_matrix <- matrix("", nrow = size, ncol = size)

  # For loop to go row by row
  for (i in 1:nrow(color_matrix)){

    # For loop to go column by column
    for (j in 1:ncol(color_matrix)){

      # If you're in the first (top left) cell, assign a random color
      if (i == 1 && j == 1){

        color_matrix[i,j] <- sample(x = colors,
                                    size = 1,
                                    replace = TRUE)

      # If you're in any other cell than the top left
      } else {

        # Get the colors of the five surrounding cells
        left_color1 <- ifelse((j-1) > 0, color_matrix[i, j-1], "")
        left_color2 <- ifelse((j-2) > 0, color_matrix[i, j-2], "")
        up_color1 <- ifelse((i-1) > 0, color_matrix[i-1, j], "")
        up_color2 <- ifelse((i-2) > 0, color_matrix[i-2, j], "")
        up_left_color <- ifelse((j-1) > 0 && (i-1) > 1, color_matrix[i-1, j-1], "")
        up_right_color <- ifelse((i-1) > 0 && (j+1) < (ncol(color_matrix)+1),
                                 color_matrix[i-1, j+1], "")

        # Put them in a vector called surrounding
        surrounding <- c(left_color1,
                         left_color2,
                         up_color1,
                         up_color2,
                         up_left_color,
                         up_right_color)

        # Check to see if any of the relavent cell colors match
        matching <- vector(mode = "character", length = 8)

        matching[1] <- ifelse(left_color1 == left_color2, left_color1, "")
        matching[2] <- ifelse(left_color1 == up_color1, left_color1, "")
        matching[3] <- ifelse(up_color1 == up_color2, up_color1, "")
        matching[4] <- ifelse(up_left_color == left_color1, up_left_color, "")
        matching[5] <- ifelse(up_left_color == up_color1, up_left_color, "")
        matching[6] <- ifelse(up_right_color == up_color1, up_right_color, "")
        matching[7] <- ifelse(up_left_color == left_color1, up_color1, "")
        matching[8] <- ifelse(up_left_color == up_color1, left_color1, "")


        matching <- unique(matching[which(matching != "")])

        # If there were no matches
        if (length(matching) == 0){

          # Assign any random color
          color_matrix[i,j] <- sample(x = colors,
                                      size = 1,
                                      replace = TRUE)

        } else { # If there WERE matches

          # Assign any other color than those in matches vector
          color_matrix[i,j] <- sample(x = colors[-which(colors %in% matching)],
                                      size = 1,
                                      replace = TRUE)
        }
      }
    }
  }
  # return the color matrix as a vector
  return(as.vector(color_matrix))
}

