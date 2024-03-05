#####
# Test the smallest size that still works with probs

library(ggplot2)

# Set the size of the desired grid and calculate number of circuits
size <- 13
circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2)


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




# Create a data frame for the grid coordinates
df <- expand.grid(x = 1:size, y = 1:size)

# Get the prob vector
prob_vector <- get_prob_vector(circuits)

# Get the prob matrix
M <- get_prob_matrix(size, prob_vector)

# Apply M to df as a vector
df$probs <- as.vector(M)

# Plot

ggplot(df, aes(x = x, y = y, label = round(probs, 2))) +
  geom_tile(aes(fill = probs), colour = "white") +
  geom_text() +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_y_reverse() +
  theme_void() +
  coord_fixed()


# 13 is the smallest size that's viable

#####
# Test the smallest set of unique colors

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

# Define the colors
colors18 <- c(#"#EDEFEE", # Paper
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

# Define the colors
colors5 <- c(#"#EDEFEE", # Paper
  "#1A8BB3", # Teal - no longer teal, just bright blue
  "#0950AE", # Dark blue
  "#4DACE5", # Light blue
  "#126DDB", # Blue
  "#E48DC4") # Pink
  # "#ABA9E8", # Light purple
  # "#872791", # Purple
  # "#6D1617", # Dark red
  # "#B81634", # Red
  # "#DF3B43", # Red orange
  # "#E35C47", # Orange
  # "#EB8749", # Light orange
  # "#F6E254", # Yellow
  # "#7B442D", # Brown
  # "#000000", # Black
  # "#1A6E7E", # Dark green - no longer dark green, now looks teal
  # "#7CBF7B", # Green
  # "#ADD2B8") # Light green

# Define the colors
colors6 <- c(#"#EDEFEE", # Paper
  "#1A8BB3", # Teal - no longer teal, just bright blue
  "#0950AE", # Dark blue
  "#4DACE5", # Light blue
  "#126DDB", # Blue
  "#E48DC4", # Pink
  "#ABA9E8") # Light purple
  # "#872791", # Purple
  # "#6D1617", # Dark red
  # "#B81634", # Red
  # "#DF3B43", # Red orange
  # "#E35C47", # Orange
  # "#EB8749", # Light orange
  # "#F6E254", # Yellow
  # "#7B442D", # Brown
  # "#000000", # Black
  # "#1A6E7E", # Dark green - no longer dark green, now looks teal
  # "#7CBF7B", # Green
  # "#ADD2B8") # Light green

# Define the colors
colors7 <- c(#"#EDEFEE", # Paper
  "#1A8BB3", # Teal - no longer teal, just bright blue
  "#0950AE", # Dark blue
  "#4DACE5", # Light blue
  "#126DDB", # Blue
  "#E48DC4", # Pink
  "#ABA9E8", # Light purple
  "#872791") # Purple
  # "#6D1617", # Dark red
  # "#B81634", # Red
  # "#DF3B43", # Red orange
  # "#E35C47", # Orange
  # "#EB8749", # Light orange
  # "#F6E254", # Yellow
  # "#7B442D", # Brown
  # "#000000", # Black
  # "#1A6E7E", # Dark green - no longer dark green, now looks teal
  # "#7CBF7B", # Green
  # "#ADD2B8") # Light green

# Define the colors
colors4 <- c(#"#EDEFEE", # Paper
  "#1A8BB3", # Teal - no longer teal, just bright blue
  "#0950AE", # Dark blue
  "#4DACE5", # Light blue
  "#126DDB") # Blue
  # "#E48DC4", # Pink
  # "#ABA9E8", # Light purple
  # "#872791", # Purple
  # "#6D1617", # Dark red
  # "#B81634", # Red
  # "#DF3B43", # Red orange
  # "#E35C47", # Orange
  # "#EB8749", # Light orange
  # "#F6E254", # Yellow
  # "#7B442D", # Brown
  # "#000000", # Black
  # "#1A6E7E", # Dark green - no longer dark green, now looks teal
  # "#7CBF7B", # Green
  # "#ADD2B8") # Light green

# Define the colors
colors3 <- c(#"#EDEFEE", # Paper
  "#1A8BB3", # Teal - no longer teal, just bright blue
  "#0950AE", # Dark blue
  "#4DACE5") # Light blue
# "#126DDB", # Blue
# "#E48DC4", # Pink
# "#ABA9E8", # Light purple
# "#872791", # Purple
# "#6D1617", # Dark red
# "#B81634", # Red
# "#DF3B43", # Red orange
# "#E35C47", # Orange
# "#EB8749", # Light orange
# "#F6E254", # Yellow
# "#7B442D", # Brown
# "#000000", # Black
# "#1A6E7E", # Dark green - no longer dark green, now looks teal
# "#7CBF7B", # Green
# "#ADD2B8") # Light green

# size 13, colors4 - this works!
color_vector <- get_color_vector(13, colors4)

df <- expand.grid(x = 1:size, y = 1:size)

df$color <- color_vector

# Print to see if it worked

ggplot(df, aes(x = x, y = y, fill = color)) +
  geom_tile() +  # Add tiles
  scale_y_reverse() +
  scale_fill_identity() +  # Use the colors stored as strings in the color column
  theme_void() +  # Remove axis labels and background
  coord_equal()

# size 13, colors3 - still works!
color_vector <- get_color_vector(13, colors3)

df <- expand.grid(x = 1:size, y = 1:size)

df$color <- color_vector

# Print to see if it worked

ggplot(df, aes(x = x, y = y, fill = color)) +
  geom_tile() +  # Add tiles
  scale_y_reverse() +
  scale_fill_identity() +  # Use the colors stored as strings in the color column
  theme_void() +  # Remove axis labels and background
  coord_equal()

colors2 <- c("#ABA9E8", # Light purple
             "#872791") # Purple

# size 13, colors2 - still technically works!
color_vector <- get_color_vector(13, colors2)

df <- expand.grid(x = 1:size, y = 1:size)

df$color <- color_vector

# Print to see if it worked

ggplot(df, aes(x = x, y = y, fill = color)) +
  geom_tile() +  # Add tiles
  scale_y_reverse() +
  scale_fill_identity() +  # Use the colors stored as strings in the color column
  theme_void() +  # Remove axis labels and background
  coord_equal()

# size 13, colors is just one color - fails
color_vector <- get_color_vector(13, c("pink"))
