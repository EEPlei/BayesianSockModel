library(shiny)

total_priors = c("Poisson" = "pois", "Negative Binomial" = "nbinom")
prop_priors = c("Beta" = "beta", "Truncated Normal"="tnorm")

shinyUI(
  fluidPage(
    titlePanel(
      "Karl Broman's Socks"
    ),
    sidebarPanel(
      numericInput("n_sims", h4("Simulations:"),
                   value = 1000, min = 100, step = 1),
      hr(),
      h4("Data:"),
      sliderInput("n_pairs", "Number of picked sock pairs:", 
                  min=0, max=30, value=0, step=1),
      sliderInput("n_odd", "Number of picked odd socks:", 
                  min=0, max=30, value=11, step=1),
      hr(),
      h4("Priors:"),
      selectInput("total_prior", "Prior for total", total_priors),
      selectInput("prop_prior", "Prior for proportion", prop_priors),
      hr(),
      h4("Hyperparameters:"),
      conditionalPanel(
        condition="input.total_prior == 'pois'",
        sliderInput("total_lambda",HTML("Total prior - &lambda;"), 
                    svalue = 50, min=1, max=120)
      ),
      conditionalPanel(
        condition="input.total_prior == 'nbinom'",
        numericInput("total_r",HTML("Total prior - r"), 
                     value = 50, min=1, max=120),
        numericInput("total_p",HTML("Total prior - p"), 
                     value = 0.5, min=0, max=1)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'beta'",
        numericInput("prop_alpha",HTML("Total prior - &alpha;"), 
                     value = 1/3 , min=0, max=NA),
        numericInput("prop_beta",HTML("Total prior - &beta;"), 
                     value = 1, min=0, max=NA)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'tnorm'",
        numericInput("prop_mu",HTML("Proportion prior - &mu;"), 
                     value = 0.5, min=NA, max=NA),
        numericInput("prop_sigma",HTML("Proportion prior - &sigma;"), 
                     value = 0.1, min=0, max=NA)
      )
    ),
    mainPanel(
      h4("Results:"),
      #Total Socks in Laundry
      plotOutput("total_plot"), 
      br(),
      #Proportion of Socks in Pairs
      plotOutput("prop_plot") 
    )
  )
)
