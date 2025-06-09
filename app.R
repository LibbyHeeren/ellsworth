# Load packages
library(shiny)
library(bslib)
library(ggplot2)
library(pracma)
library(markdown)
library(grDevices) # For PDF creation

# Source the file containing all the functions, assumes files are in same R folder
source("R/functions.R")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),

  tags$head(
    tags$style(HTML("
    label.control-label {
      font-size: 16px;
      font-weight: bold;
    }

    .button-wrapper {
  margin-top: 10px;
  margin-bottom: 10px;
    }
  "))
  ),

  # Center align all content
  tags$div(
    style = "text-align: center; max-width: 600px; margin: auto;",

    titlePanel("An Ode to Ellsworth Kelly"),

    # Spacer
    tags$br(),

    HTML(markdown::markdownToHTML(text = "This app was created by [Libby Heeren](www.libbyheeren.com). It's a festival of probability and color inspired by the Spectrum Colors Arranged by Chance series of artwork by [Ellsworth Kelly](https://ellsworthkelly.org/) and the fabulous Stacey, aka [The Crooked Hem](https://thecrookedhem.net/), who created a Kelly-inspired quilt and got my wheels turning. <br> <br>If you'd like to make your own quilt, you can generate one below and download a quilt-planning PDF. But, beware - it's random! If you see a pattern you like, save it! Want to see the code I wrote for this app? Visit [my GitHub repo](https://github.com/LibbyHeeren/ellsworth), and you can go to [my blog](https://libbyheeren.com/blog.html#category=Shiny) to see my development notes. <br> <br> ❤ Libby")),

    # Spacer
    tags$br(),

    # Inputs
    sliderInput("size", "What size grid would you like?", min = 13, max = 60, value = 40, width = "100%"),
    selectInput("bg_color", "Which background color would you like?",
                choices = c("Paper White" = "#EDEFEE", "Black" = "black"),
                selected = "#EDEFEE", width = "100%"),

    # Spacer
    tags$br(),

    # Buttons row
    fluidRow(
      column(6, div(class = "button-wrapper", actionButton("generate", "Create Art Piece", class = "btn-primary"))),
      column(6, div(class = "button-wrapper", downloadButton("download_pdf", "Download PDF", class = "btn-primary")))
    ),

    # Spacer
    tags$br(),

    # Note to scroll down!
    p("Scroll down 👇 to see artwork"),

    # Plot of the art piece
    plotOutput("art_plot", height = "800px"),

    # Plot of the paint-by-numbers version
    plotOutput("pbn_plot", height = "800px"),

    # Swatch
    plotOutput("swatch_plot", height = "600px")
  )
)

server <- function(input, output, session) {

  artwork_data <- eventReactive(input$generate, {
    size <- input$size
    background <- input$bg_color

    # Calculate number of circuits based on size
    circuits <- ifelse(size %% 2 == 0, size/2, (size+1)/2) # Num of concentric circuits

    # Define the colors
    all_colors <- c(#"#EDEFEE", # Paper
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

    if (background == "#EDEFEE"){ # If background is paper white
      colors <- all_colors
    } else { # else (if black), remove black from colors
      colors <- all_colors[all_colors != "#000000"]
    }

    # Add logic to create all the plots I want in the final app,
    # Even if I don't use them all right now

    # Create a data frame for the grid coordinates
    df <- expand.grid(x = 1:size, y = 1:size)

    # Get the vector of probabilities based on the size of the piece
    # One probability per circuit
    prob_vector <- get_prob_vector(circuits)

    # Generate the color vector - no more than two same colors touching
    color_vector <- get_color_vector(size, colors)

    # Add the corresponding color to each grid cell coordinate
    df$color <- color_vector

    # Assign probabilities to matrix correctly
    M <- get_prob_matrix(size, prob_vector)

    # Apply prob matrix M to df as a vector
    df$probs <- as.vector(M)

    # Turn the piece VII vector (all colors) into the piece III vector (add background)
    final_df <- get_kelly_III_vector(df, background)

    # Create a vector of numbers to represent colors
    final_df$color_numbers <- convert_colors_to_numbers(final_df$color, background)

    # Create a vector of the unique colors and their representative numbers to swatch
    colors_and_numbers <- unique(final_df[,c('color','color_numbers')])

    return(final_df)
  })

  # Function to create the art plot
  get_art_plot <- function(final_df) {

    ggplot(final_df, aes(x = x, y = y, fill = color)) +
      geom_tile() +  # Add tiles
      scale_fill_identity() +  # Use the colors stored as strings in the color column
      theme_void() +  # Remove axis labels and background
      coord_equal()  # Use equal aspect ratio
  }

  # Function to create the paint-by-numbers plot
  get_pbn_plot <- function(final_df) {
    grid_size <- sqrt(nrow(final_df))
    text_size <- 120 / grid_size


    ggplot(final_df, aes(x = x, y = y, fill = color, label = color_numbers)) +
      geom_tile(alpha = 0.5, color = "black") +  # Add tiles
      geom_text(size = text_size) +
      scale_x_continuous(position = "top",
                         expand = c(0,0),
                         breaks =  1:grid_size) +
      scale_y_continuous(expand = c(0,0),
                         breaks =  1:grid_size,
                         labels = rev(1:grid_size)) +
      scale_fill_identity() +  # Use the colors stored as strings in the color column
      coord_equal() +  # Use equal aspect ratio
      labs(x = NULL, y = NULL) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 90))
  }

  # Function to create the swatch plot
  get_swatch_plot <- function(final_df) {
    colors_and_numbers <- unique(final_df[,c('color','color_numbers')])

    plot_number_swatch(colors_and_numbers)
  }

  output$art_plot <- renderPlot({
    final_df <- artwork_data()
    get_art_plot(final_df)
  })

  output$pbn_plot <- renderPlot({
    final_df <- artwork_data()

    get_pbn_plot(final_df)
  }, res = 108)

  output$swatch_plot <- renderPlot({
    final_df <- artwork_data()
    get_swatch_plot(final_df)
  })

  # PDF download handler
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("EllsworthKelly-Artwork-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".pdf", sep = "")
    },
    content = function(file) {
      final_df <- artwork_data()

      # Recreate the swatch data inside the download function
      colors_and_numbers <- unique(final_df[, c("color", "color_numbers")])

      # Open the PDF device
      pdf(file, width = 8.5, height = 11, onefile = TRUE)

      # Page 1: Art Plot
      print(get_art_plot(final_df))

      # Page 2: Paint-by-numbers Plot
      print(get_pbn_plot(final_df))

      # Page 3: Swatch Plot (base graphics)
      plot_number_swatch(colors_and_numbers)

      # Close the PDF
      dev.off()
    }
  )

}

shinyApp(ui, server)
