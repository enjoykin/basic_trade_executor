import rpy2.robjects as robjects

def run_test():
    print('start GARCH')
    forecast = robjects.r(r'''
    library(quantmod)
    library(zoo)
    library(fGarch)
    library(parallel)
    library(lattice)
    library(data.table)
    
    ###################################################################################
    # Get the daily/weekly/mothly data for a list of assets and save locally.
    # the function gets vector of strings-tickers e.g.
    # c("^GSPC", "MS", "JPM", "LVS", "WYNN") and from-date "2000-01-01"
    ###################################################################################
    
    get_data = function( data,
                         from,
                         depth,
                         data_dir)
    {
      for(i in data)
      {
        tryCatch({ticker_data = getSymbols(i, from=from, auto.assign=FALSE);
                  }, error = function(e) { print(e$message) })
        
        names(ticker_data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
        
        asst_dly_rtrn = diff( log( Cl(ticker_data)))
        asst_2yrs = tail( asst_dly_rtrn, depth)
        
        data_path = paste(data_dir, "\\", i, ".csv", sep = "")
        write.zoo(asst_2yrs, file = data_path, sep=",")
        
      }
    }
    
    get_data(
      data = c("GSPC", "MS", "JPM", "LVS", "WYNN"),
      from = "2000-01-01",
      depth = 500,
      data_dir = "D:\\Dima\\data"
    )
    
    ###################################################################################
    # create relevant xts objects using get_data() files
    ###################################################################################
    
    data = c("GSPC", "MS", "JPM", "LVS", "WYNN")
    data_dir = "D:\\Dima\\data"
    for(i in data)
    {
      path = paste(data_dir, "\\", i, ".csv", sep = "")
      var_name <- i
      assign(var_name, read.zoo(path, header = TRUE, sep =","))
    }
    
    ###################################################################################
    # fit GARCH model for single parameter set
    ###################################################################################
    
    garch_fit_auto = function(
      ll,
      data,
      forecast_length = 1,
      with_forecast = FALSE,
      ic = "AIC",
      garch_model = "garch")
    {
      formula = as.formula(paste(sep = "",
                                 "~ arma(", ll$order[1], ",", ll$order[2], ")+",
                                 garch_model,
                                 "(", ll$order[3], ",", ll$order[4], ")" ))
      fit = tryCatch( garchFit( formula=formula,
                                data=data,
                                trace=FALSE,
                                cond.dist=ll$dist),
                      error = function (err ) TRUE,
                      warning = function( warn) FALSE)
      pp = NULL
      
      if( !is.logical( fit) ) 
      {
        if( with_forecast ) 
        {
          pp = tryCatch( predict( fit,
                                  n.agead=forecast_length,
                                  doplot = FALSE ),
                         error = function (err ) TRUE,
                         warning = function( warn) FALSE)
          if( is.logical(pp))
          {
            fir = NULL
          }
        }
      } else {
        fit = NULL
      }
      
      return(fit)
                         
    }
    
    
    ###################################################################################
    # input parameters list construction for garch_cores
    # ARIMA(1,2) + GARCH(3,4) and "sged" parameter for cond.dist in garchFit
    ###################################################################################
    
     input_parameters = function (
       min.order,
       max.order,
       cond.dists,
       arma.sum
       )
     {
       models = list()
       for( dist in cond.dists )
        for( p in min.order[1]:max.order[1] )
          for( q in min.order[2]:max.order[2])
            for( r in min.order[3]:max.order[3])
             for( s in min.order[4]:max.order[4])
            {
              pq.sum = p + q
              if( pq.sum <= arma.sum[2] && pq.sum >= arma.sum[1] )
              {
                models[[length(models) + 1]] = list( order=c(p, q, r, s), dist = dist )
              }
            }
       return(models)
    }
    
    x = input_parameters(min.order = c(0,0,1,1), 
                         max.order = c(5,5,1,1),
                         cond.dists="sged",
                         arma.sum=c(0, 1e9))
    
    ###################################################################################
    # multi-core garch-fitting function
    ###################################################################################
    
    garch_cores = function(
      ts_data,
      parameters,
      with_forecast = TRUE,
      forecast_length=1,
      ic="AIC",
      garch_model="garch")
    {
      cl <- makeCluster(detectCores(logical = TRUE))
      
      clusterEvalQ(cl,
                   {
                     library(zoo)
                     library(fGarch)
                   })
      
      res = clusterApplyLB(cl,
                           parameters,
                           garch_fit_auto,
                           data = ts_data,
                           ic=ic,
                           garch_model=garch_model,
                           forecast_length=forecast_length,
                           with_forecast = TRUE)
      best.fit = NULL
      best.ic = 1e9
      for(rr in res)
      {
        if( !is.null(rr))
        {
          current.ic = rr@fit$ics[[ic]]
          if(current.ic < best.ic)
          {
            best.ic = current.ic
            best.fit = rr
          }
        }
      }
      
      stopCluster(cl)
      if( best.ic < 1e9 )
      {
        return(best.fit)
      }
      
      return( NULL)
    
    }
    
    z = garch_cores(MS, x)
    ff = predict(z, n.ahead = 1)$meanForecast
    
    if ( sign(ff) == -1 ) {
    	trade = 'sell'
    } else {
    	trade = 'buy'
    }

    ''')
    #print(forecast)
    print('end GARCH')
    #print(forecast)
    #print(str(forecast))
    #if forecast[5:9] == 'sell':
    #    return 'sell'
    #else:
    #    return 'buy'
        
run_test()
