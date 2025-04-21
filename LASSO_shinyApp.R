library(shiny)
library(glmnet)
library(MASS)

ui <- fluidPage(
  titlePanel("Lasso Coefficient Paths with Non-Nested Selection"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("numVars", "Number of Predictors (p):", min = 5, max = 50, value = 10, step = 1),
      sliderInput("numObs", "Number of Observations (n):", min = 50, max = 200, value = 100, step = 10),
      sliderInput("corr", "Correlation Between Predictors:", min = 0, max = 0.99, value = 0.8, step = 0.01),
      actionButton("resim", "Resimulate Data")
    ),
    
    mainPanel(
      plotOutput("lassoPlot"),
      verbatimTextOutput("selectedVars")
    )
  )
)

server <- function(input, output, session) {
  simulateData <- reactiveVal()
  
  observeEvent(input$resim, {
    p <- input$numVars
    n <- input$numObs
    rho <- input$corr
    
    Sigma <- rho ^ abs(outer(1:p, 1:p, "-"))
    X <- mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
    
    # Simulate true coefficients (first few non-zero)
    true_beta <- c(rep(3, min(3, p)), rep(0, p - min(3, p)))
    y <- X %*% true_beta + rnorm(n)
    
    simulateData(list(X = X, y = y))
  }, ignoreNULL = FALSE)
  
  fitLasso <- reactive({
    data <- simulateData()
    X <- data$X
    y <- data$y
    glmnet(X, y, alpha = 1)
  })
  
  output$lassoPlot <- renderPlot({
    fit <- fitLasso()
    plot(fit, xvar = "lambda", label = TRUE)
    title("Lasso Coefficient Paths")
  })
  
  output$selectedVars <- renderPrint({
    fit <- fitLasso()
    lambda_seq <- fit$lambda
    coeff_matrix <- as.matrix(coef(fit))[-1, ]  # remove intercept
    
    selected_sets <- lapply(1:length(lambda_seq), function(i) {
      which(coeff_matrix[, i] != 0)
    })
    
    cat("Selected variables at different lambda indices:\n")
    for (i in seq(10, length(lambda_seq), length.out = 3)) {
      idx <- round(i)
      cat(sprintf("\nLambda[%-3d] = %.4f: X%s\n", idx, lambda_seq[idx], 
                  if (length(selected_sets[[idx]]) > 0) 
                    paste(selected_sets[[idx]], collapse = ", ") 
                  else " (none)"))
    }
  })
}

shinyApp(ui, server)
