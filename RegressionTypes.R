library(shiny)
library(dplyr)
library(ggplot2)
library(caret)

# Définition de l'interface utilisateur
ui <- fluidPage(
  titlePanel("Régression App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Importer le fichier CSV", accept = ".csv"),
      selectInput("regression_type", "Choisir le type de régression",
                  choices = c("Linear Regression", "Polynomial Regression", "Logistic Regression",
                              "Quantile Regression", "Ridge Regression", "Lasso Regression",
                              "Elastic Net Regression", "Principal Components Regression (PCR)",
                              "Partial Least Squares (PLS) Regression", "Support Vector Regression",
                              "Ordinal Regression", "Poisson Regression", "Negative Binomial Regression",
                              "Quasi Poisson Regression", "Cox Regression")),
      actionButton("perform_regression", "Effectuer la régression")
    ),
    mainPanel(
      plotOutput("regression_plot")
    )
  )
)

# Définition du serveur
server <- function(input, output) {

  # Importer les données
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE)
  })

  # Effectuer la régression sélectionnée
  regression_model <- reactive({
    req(input$perform_regression)

    # Sélectionner la régression en fonction du choix de l'utilisateur
    if (input$regression_type == "Linear Regression") {
      lm(formula = y ~ ., data = data())
    } else if (input$regression_type == "Polynomial Regression") {
      lm(formula = y ~ poly(x, degree = 2), data = data())
    } else if (input$regression_type == "Logistic Regression") {
      glm(formula = y ~ ., data = data(), family = binomial)
    } else if (input$regression_type == "Quantile Regression") {
      quantreg::rq(formula = y ~ ., data = data())
    } else if (input$regression_type == "Ridge Regression") {
      caret::train(form = y ~ ., data = data(), method = "ridge")
    } else if (input$regression_type == "Lasso Regression") {
      caret::train(form = y ~ ., data = data(), method = "lasso")
    } else if (input$regression_type == "Elastic Net Regression") {
      caret::train(form = y ~ ., data = data(), method = "enet")
    } else if (input$regression_type == "Principal Components Regression (PCR)") {
      caret::train(form = y ~ ., data = data(), method = "pcr")
    } else if (input$regression_type == "Partial Least Squares (PLS) Regression") {
      caret::train(form = y ~ ., data = data(), method = "pls")
    } else if (input$regression_type == "Support Vector Regression") {
      e1071::svm(formula = y ~ ., data = data())
    } else if (input$regression_type == "Ordinal Regression") {
      MASS::polr(formula = y ~ ., data = data(), method = "logistic")
    } else if (input$regression_type == "Poisson Regression") {
      glm(formula = y ~ ., data = data(), family = poisson)
    } else if (input$regression_type == "Negative Binomial Regression") {
      glm.nb(formula = y ~ ., data = data())
    } else if (input$regression_type == "Quasi Poisson Regression") {
      glm(formula = y ~ ., data = data(), family = quasipoisson)
    } else if (input$regression_type == "Cox Regression") {
      survival::coxph(formula = Surv(time, event) ~ ., data = data())
    }
  })

  # Afficher le graphique de régression
  output$regression_plot <- renderPlot({
    req(input$perform_regression)

    # Tracer les points de données
    ggplot(data(), aes(x = x, y = y)) +
      geom_point() +
      theme_minimal() +
      labs(x = "X", y = "Y")

    # Tracer la ligne de régression
    if (!is.null(regression_model())) {
      new_data <- data.frame(x = seq(min(data()$x), max(data()$x), length.out = 100))
      predicted_values <- predict(regression_model(), newdata = new_data)

      geom_line(data = new_data, aes(y = predicted_values), color = "red")
    }
  })
}

# Exécuter l'application Shiny
shinyApp(ui = ui, server = server)