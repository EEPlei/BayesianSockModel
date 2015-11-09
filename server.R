library(truncnorm)

shinyServer(
  function(input, output, session) 
  {
    observe(
      {
        updateSliderInput(session,"n_black", max = input$n_drawn)
      }
    )
    
    priors = reactive(
      {
        d_total = numeric()
        if (input$total_prior == "pois")
        {
          d_total = rpois(input$n_sims, input$total_lambda)  
        } else {
          d_total = rnbinom(input$n_sims,size = input$total_r, prob = input$total_p)
        }
        
        d_prop = numeric()
        if (input$prop_prior == "beta")
        {
          d_prop = rbeta(input$n_sims, input$prop_alpha, input$prop_beta)  
        } else {
          d_prop = rtruncnorm(input$n_sims,0,1,input$prop_mu,input$prop_sigma)
        }
        
        data.frame(total = d_total, prop = d_prop)
      }
    )
    
    sims = reactive(
      {
        gen_model = function(prior_N_total,prior_p_total_black)
        {
          n_black_bag = rbinom(1, prior_N_total, prior_p_total_black)
          p_black_bag = n_black_bag / prior_N_total
          n_black_hand = rbinom(1,input$n_drawn,p_black_bag)
          
          return(n_black_hand)
        }
        
        apply(priors(),1, function(x) gen_model(x[1],x[2]))
      }
    )
    
    posterior = reactive(
      {
        priors()[sims()==input$n_black,]    
      }
    )
    
    output$total_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,1], freq=FALSE, main="Total Jelly Beans")
        lines(density( priors()$total ),col='green',lwd=3)
      }
    )
    
    output$prop_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,2], freq=FALSE, main="Proportion of Black Jelly Beans")
        lines(density( priors()$prop ),col='blue',lwd=3)
      }
    )
  }
)
