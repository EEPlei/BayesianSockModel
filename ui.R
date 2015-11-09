library(shiny)

total_priors = c("Poisson" = "pois", "Gamma" = "gam", "Negative Binomial" = "nbin")
prop_priors = c("Beta" = "beta", "Truncated Normal" = "tnorm")

  
shinyUI(
  fluidPage(
    titlePanel(
      "Socks ABC"
    ),
    sidebarPanel(
      numericInput("n_sims", h4("Simulations:"), value = 1000, min = 100, step = 1),
      hr(),
      h4("Data:"),
      sliderInput("n-drawn", "Number Drawn:", min = 1, max = 30, value = 9, step = 1), 
      sliderInput("n-drawn", "Number Black:", min = 1, max = 30, value = 3, step = 1), 
      hr(),
      h4("Priors:"), 
      selectInput("total_prior", "Prior for Total", total_priors), 
      selectInput("prop_prior", "Prior for Proportion", prop_priors), 
      hr(),
      h4("Hyperparameters:")
      
    ),
    mainPanel(
      "Main"
    )
  )
)