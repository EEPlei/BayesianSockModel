library(truncnorm)

shinyServer(
  function(input, output, session) 
  {
    
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
        gen_model = function(prior_N_total,prior_prop_total)
        {
          n_picked <- input$n_odd + 2*input$n_pairs
          #Total socks in laundry
          n_socks <- prior_N_total
          #Proportion of socks in pairs
          prop_pairs <- prior_prop_total
          #number of sock pairs
          n_pairs <- round(floor(n_socks / 2) * prop_pairs)
          #number of odd socks
          n_odd <- n_socks - n_pairs * 2
          
          # Simulating picking out n_picked socks
          socks <- rep(seq_len(n_pairs + n_odd), rep(c(2, 1), c(n_pairs, n_odd)))
          picked_socks <- sample(socks, size =  min(n_picked, n_socks))
          sock_counts <- table(picked_socks)
          
          # Returning the parameters and counts of the number of matched 
          # and unique socks among those that were picked out.
          sock_sim <- sum(sock_counts == 1)
          
          return(sock_sim)
        }
        
        apply(priors(),1, function(x) gen_model(x[1],x[2]))
      }
    )
    
    posterior = reactive(
      {
        priors()[sims()==input$n_odd,]
      }
    )
    
    output$total_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,1], freq=FALSE, main="Total Socks in Laundry")
      }
    )
    
    output$prop_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,2], freq=FALSE, main="Proportion of Socks in Pairs")
      }
    )
  }
)