# Load Required Packages
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)


# Prepare Data:
# Create data frames for each set of data.
predictions <- data.frame(
  lime_tha = 0:7,
  yhat = c(3.576989, 4.21631, 4.723637, 5.098969, 5.342306, 5.453647, 5.432994, 5.280346)
)

#marginal effects
marginal_effects <- data.frame(
  lime_tha = 0:7,
  yhat = c(0.7053192, 0.5733242, 0.4413292, 0.3093342, 0.1773392, 0.0453442, -0.0866508, -0.2186458)
)

#Net Revenue
net_rev_ci <- data.frame(
  lime_tha = 0:7,
  net_rev = c(357.6989, 491.631, 612.3637, 719.8969, 814.2306, 895.3647, 963.2994, 1018.0346)
)

#  AVCR
# i have had to replace high and low values with yhat prediction values to make it easy for inclusion in the formulae
# otherwise i keep getting erros when i try to use the values when they are in the prediction data frame
avcr_ci <- data.frame(
  lime_tha = 1:7,
  low = c(4.21631, 4.723637, 5.098969, 5.342306, 5.453647, 5.432994, 5.280346),
  avcr = c(6.02, 3.37, 2.43, 1.91, 1.56, 1.29, 1.08),
  high = c(4.21631, 4.723637, 5.098969, 5.342306, 5.453647, 5.432994, 5.280346)
)

# MVCR
# i had to replace high and low values with margialeffects yhat values 
# makes it easier to plot.
mvcr_ci <- data.frame(
  lime_tha = 0:7,
  low = c(0.7053192, 0.5733242, 0.4413292, 0.3093342, 0.1773392, 0.0453442, 0.0, 0.0),
  mvcr = c(1.0, 0.81, 0.63, 0.44, 0.25, 0.06, 0.0, 0.0),
  high = c(0.7053192, 0.5733242, 0.4413292, 0.3093342, 0.1773392, 0.0453442, 0.0, 0.0)
)

# Create a Shiny app that includes tabs for each graph.
ui <- fluidPage(
  titlePanel("Lime responses and Profitability Graphs"),
  sidebarLayout(
    sidebarPanel(
      numericInput("p_x", "p_x", value = 70),
      numericInput("p_y", "p_y", value = 100),
      numericInput("x_uncertainty", "X Uncertainty", value = 0),
      numericInput("y_uncertainty", "Y Uncertainty", value = 50),
      sliderInput("lime_prediction", "Lime value", min = 0, max = 10, value = 5),  # Add lime prediction slider
      sliderInput("yield_prediction", "Yield Prediction", min = 0, max = 10, value = 5)  # Add yield prediction slider
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Predictions", plotOutput("plot_predictions")),
        tabPanel("Marginal Effects", plotOutput("plot_marginal_effects")),
        tabPanel("Net Revenue", plotOutput("plot_net_rev")),
        tabPanel("AVCR", plotOutput("plot_avcr")),
        tabPanel("MVCR", plotOutput("plot_mvcr")),
        verbatimTextOutput("hover_text")
      )
    )
  )
)

# Server part
server <- function(input, output) {
  output$plot_predictions <- renderPlot({
    # Use input$lime_prediction and input$yield_prediction in the calculations
    lime_value <- input$lime_prediction
    yield_value <- input$yield_prediction
    
    ggplot(predictions, aes(x = lime_tha, y = yhat)) +
      geom_point() +
      geom_errorbar(aes(ymin = yhat - 0.156282, ymax = yhat + 0.156281), width = 0.2) +
      geom_line(aes(x = lime_tha, y = yhat, color = "Predictions"), size = 1) +
      geom_line(aes(x = lime_tha, y = lime_value), color = "red", linetype = "dashed", size = 1) +  # Incorporate lime_value
      geom_line(aes(x = lime_tha, y = yield_value), color = "green", linetype = "dashed", size = 1) +  # Incorporate yield_value
      labs(title = "yhat", x = "lime_tha", y = "yhat") +
      theme(axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)) +
      scale_color_manual(values = c("blue", "red", "green")) +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
  
  output$plot_marginal_effects <- renderPlot({
    ggplot(marginal_effects, aes(x = lime_tha, y = yhat)) +
      geom_point() +
      geom_line(aes(x = lime_tha, y = yhat, color = "Marginal Effects"), size = 1) +
      labs(title = "marginal_effects", x = "lime_tha", y = "yhat") +
      theme(axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)) +
      scale_color_manual(values = "red") +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
  
  output$plot_net_rev <- renderPlot({
    lower_bound_net_rev <- (predictions$yhat * (input$p_y - input$y_uncertainty)) + (predictions$lime_tha * (input$p_x - input$x_uncertainty))
    upper_bound_net_rev <- (predictions$yhat * (input$p_y + input$y_uncertainty)) + (predictions$lime_tha * (input$p_x + input$x_uncertainty))
    
    ggplot(net_rev_ci, aes(x = lime_tha)) +
      geom_line(aes(y = lower_bound_net_rev, color = "Low"), size = 1) +
      geom_line(aes(y = net_rev, color = "Net Revenue"), size = 1) +
      geom_line(aes(y = upper_bound_net_rev, color = "High"), size = 1) +
      geom_point(aes(y = net_rev), color = "green", size = 3) +  # Add geom_point
      labs(title = "Net Revenue", x = "lime_tha", y = "net_rev") +
      theme(axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)) +
      scale_color_manual(values = c("red", "blue", "green")) +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
  
  output$plot_avcr <- renderPlot({
    # Calculate lower and upper bounds
    lower_bound_avcr <- (avcr_ci$low * (input$p_y - input$y_uncertainty)) / (avcr_ci$lime_tha * (input$p_x - input$x_uncertainty))
    upper_bound_avcr <- (avcr_ci$high * (input$p_y + input$y_uncertainty)) / (avcr_ci$lime_tha * (input$p_x + input$x_uncertainty))
    
    ggplot(avcr_ci, aes(x = lime_tha[1:7])) +
      geom_line(aes(y = lower_bound_avcr[1:7], color = "Low"), size = 1) +
      geom_line(aes(y = avcr_ci$avcr[1:7], color = "AVCR"), size = 1) +
      geom_line(aes(y = upper_bound_avcr[1:7], color = "High"), size = 1) +
      geom_point(aes(y = avcr_ci$avcr), color = "red", size = 3) +  # Add geom_point
      labs(title = "Average Variable Cost of Revenue", x = "lime_tha", y = "AVCR") +
      theme(axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)) +
      scale_color_manual(values = c("red", "blue", "green")) +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
  
  output$plot_mvcr <- renderPlot({
    lower_bound_mvcr <- (mvcr_ci$low * (input$p_y - input$y_uncertainty)) / (input$p_x - input$x_uncertainty)
    upper_bound_mvcr <- (mvcr_ci$high * (input$p_y + input$y_uncertainty)) / (input$p_x + input$x_uncertainty)
    
    ggplot(mvcr_ci, aes(x = lime_tha)) +
      geom_line(aes(y = lower_bound_mvcr, color = "Low"), size = 1) +
      geom_line(aes(y = mvcr_ci$mvcr, color = "MVCR"), size = 1) +
      geom_line(aes(y = upper_bound_mvcr, color = "High"), size = 1) +
      geom_point(aes(y = mvcr_ci$mvcr), color = "green", size = 3) +  # Add geom_point
      labs(title = "Marginal Variable Cost of Revenue", x = "lime_tha", y = "MVCR") +
      theme(axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)) +
      scale_color_manual(values = c("red", "blue", "green")) +
      theme(legend.position = "bottom", legend.text = element_text(size = 16), legend.title = element_blank())
  })
}

# Run the Shiny app
shinyApp(ui, server)
