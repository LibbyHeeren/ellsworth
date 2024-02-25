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
