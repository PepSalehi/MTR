library(tidyverse)
library(lubridate)
library(shiny)
library(shinythemes)
library(reshape2)
library(keras)
library(Metrics)
# https://stackoverflow.com/questions/45377186/increase-max-upload-file-size-in-shiny?noredirect=1&lq=1
options(shiny.maxRequestSize = 900000*1024^2)

server <- function(input, output, session) {
  # look up table for station ids 
  library(tidyverse)
  library(lubridate)
  library(shiny)
  library(shinythemes)
  library(reshape2)
  library(keras)
  library(Metrics)
  # https://stackoverflow.com/questions/45377186/increase-max-upload-file-size-in-shiny?noredirect=1&lq=1
  options(shiny.maxRequestSize = 900000*1024^2)
  look_up_table <- 
    tibble(
      names = c("CEN", "MOK", "PRE", "TSW", "WAC", "CAB", "TIH", "TIS"),
      ids = c(1,6,16,25,27,28,29,118)
    )
  
  values <- reactiveValues(df_data = NULL, station_id= NULL, station_name= NULL, station_data=NULL, processed_data=NULL,df=NULL)
  
  
  read_batch_with_progress = function(file_path,nrows,no_batches){
    progress = Progress$new(session, min = 1,max = no_batches)
    progress$set(message = "Reading ...")
    seq_length = ceiling(seq.int(from = 2, to = nrows-2,length.out = no_batches+1))
    seq_length = seq_length[-length(seq_length)]
    
    #read the first line
    df = read.csv(file_path,skip = 0,nrows = 1)
    col_names = colnames(df)
    
    for(i in seq_along(seq_length)){
      progress$set(value = i)
      if(i == no_batches) chunk_size = -1 else chunk_size = seq_length[i+1] - seq_length[i]
      
      df_temp = read.csv(file_path, skip = seq_length[i], nrows = chunk_size,header = FALSE,stringsAsFactors = FALSE)
      colnames(df_temp) = col_names
      df = rbind(df,df_temp)
    }
    
    progress$close()
    return(df)
  }
  
  # df = reactive({
  #   req(input$file1)
  #   n_rows = length(count.fields(input$file1$datapath))
  #   
  #   df_out = read_batch_with_progress(input$file1$datapath,n_rows,10)
  #   
  #   return(df_out)
  # })
  # 
  # observe({
  #   output$sum <- renderPrint({
  #     print(head(df(), 10))
  #   }) 
  # }) 
  # 
  # values$df_data <- df
  observeEvent(input$file1, {
    values$df_data <- read.csv(input$file1$datapath);
    
    output$sum <- renderPrint({
      print(head(values$df_data, 10))
    }) 
  }) 
  
  
  
  
  # 
  # observeEvent(input$file1, {
  #   values$df_data <- read.csv(input$file1$datapath);
  #   
  #   output$sum <- renderPrint({
  #     print(head(values$df_data, 10))
  #   }) 
  # }) 
  
  
  
  
  
  
  
  
  
  observeEvent(input$station, {
    values$station_name <- input$station;
    values$station_id <- look_up_table %>% filter(names == input$station ) %>% select(ids) %>% as.numeric();
    output$sum <- renderPrint({
      # summary(values$df_data)
      print(isolate(values$station_name));
      print(isolate(values$station_id));
      print(paste0(getwd()))
      print(paste0( system.file("ShinyApp", package = "MTRdemand")))
      setwd(system.file("ShinyApp", package = "MTRdemand"))
      print(paste0(here::here()))
      print(paste0(getwd()))
      # print(isolate(head(values$df_data)));
    })
  }, ignoreNULL=FALSE, ignoreInit=TRUE
  )
  
  
  # https://shiny.rstudio.com/articles/action-buttons.html
  # it keeps running this function whenever the selectInput is updated. That's why isolate is needed
  observeEvent(input$pri, {
    values$station_data <- values$df_data %>% filter(TRAIN_ENTRY_STN ==  values$station_id & TXN_TYPE_CO == "ENT") # & TXN_TYPE_CO == "ENT"
    output$sum <- renderPrint({
      # summary(values$df_data)
      # print(isolate(values$station_name));
      # print(isolate(values$station_id));
      print(isolate(head(values$station_data)));
      
    })
  })
  
  
  observeEvent(input$pre, {
    output$sum <- renderPrint({
      # print(isolate(head(values$df)));
      print(paste0(getwd()))
      # print(model %>% summary())
      
    })
    # to fill the missing values
    all_time_bins_template <- 
      tibble(
        bins = 1:96
      )
    
    # only work with 1 day 
    dd <- 
      values$station_data %>% 
      mutate(timestamp = dmy_hms(TXN_DT, tz= "Asia/Hong_Kong") ) %>% 
      mutate(dow = wday(timestamp, label=FALSE), DayOfMonth = day(timestamp)) %>% # monday is 2
      filter(!(dow %in% c(1,7))) %>% # filter to only include weekday
      group_by(DayOfMonth) %>% 
      tally(sort = T) %>% 
      slice(1) %>% 
      select(DayOfMonth) 
    
    # extract day of the week 
    dow <-
      values$station_data  %>% 
      mutate(timestamp = dmy_hms(TXN_DT, tz= "Asia/Hong_Kong") ) %>% 
      mutate(dow = wday(timestamp, label=FALSE), DayOfMonth = day(timestamp)) %>% # monday is 2
      slice(1) %>% 
      select(dow) %>% 
      as.numeric()
    
    # bin the data and counts 
    values$processed_data <- 
      values$station_data %>% 
      mutate(timestamp = dmy_hms(TXN_DT) ) %>% 
      mutate(dow = wday(timestamp, label=FALSE), DayOfMonth = day(timestamp)) %>% # monday is 2
      # only extract the time 
      mutate(time =  timestamp %>% format("%H:%M") %>% hm()) %>% 
      # bin it to be a number btw 0-96
      mutate(bin = ceiling((time %>% period_to_seconds())/900)) %>% 
      # Having intervals in a column breaks many dplyr verbs, https://github.com/tidyverse/lubridate/issues/635
      select(-time) %>% 
      # filter to only include weekday
      filter(!(dow %in% c(1,7))) %>% 
      filter(DayOfMonth == dd$DayOfMonth) %>%  
      # mutate(bin = as.numeric(cut(timestamp, breaks = "15 mins"))) %>% 
      group_by(bin, 
               add=FALSE)  %>% 
      summarise(coalesce = n())  %>% 
      right_join(all_time_bins_template, by=c("bin"= "bins")) %>%
      replace_na(list(coalesce= 1))
    
    # prepare for keras 
    
    df <- values$processed_data %>% 
      
      mutate(t1=lag(coalesce),
             t2=lag(t1),
             t3=lag(t2),
             t4=lag(t3),
             t5=lag(t4),
             t6=lag(t5),
             t7=lag(t6),
             t8=lag(t7)
      ) %>% 
      slice(-c(1:8)) 
    
    
    # dummy code day of week
    target_col <- (dow - 1) %>% unique()
    dow_oneHot <- data.frame(
      matrix(0, nrow(df), 5)
    )
    # dow_oneHot <- keras::to_categorical(df$dow) %>% as_tibble()
    names(dow_oneHot) <- c("M", "T", "W", "Th", "F")
    # fill the column for the test day
    dow_oneHot[,target_col] <- 1
    # keras expects categorical to start from 0
    df$dow <- dow -2
    values$df <- bind_cols(df[,-which(names(df) == "dow")], dow_oneHot)
    
    output$sum <- renderPrint({
      print(isolate(head(values$df)));
      
      
    })
    
    
  })
  
  
  observeEvent(input$pred, {
   
    ##############################
    # read the model and mean/std
    ##############################
    
 
    
    model_path <- paste0(here::here(),"/models/my_model_station_", as.character(values$station_id), ".h5")
    # model_path <- paste0("./models/my_model_station_", as.character(values$station_id), ".h5")
    model <- keras::load_model_hdf5(model_path)
    
    hist_avg <- readRDS(paste0(here::here(),"/models/station_", as.character(values$station_id),"_hist_avg.rds"))
    values$hist <-  hist_avg$demand[-c(1:5)]
    mean_std <- readRDS(paste0(here::here(),"/models/mean_std_", as.character(values$station_id),".rds"))
    
    training_mean <- mean_std$m
    training_std <- mean_std$std
    
    #############################
    # make predictions 
    #############################
    
    
    
    y.test = values$df$coalesce
    y.test = y.test[-c(1:17)]
    
    
    x.test <- values$df %>% select (-c( "bin", 'coalesce'))
    x.test <- as.matrix(x.test)
    # data preprocessing 
    
    
    test_data <- scale(x.test, center = training_mean, scale = training_std)
    test_targets <- y.test
    values$y <- y.test
    test_input_3D <- array_reshape(x = test_data, dim = c(dim(test_data), 1))
    
    values$results <- model %>% predict(test_input_3D) %>% as.integer()
    
    values$results <- values$results[-c(1:17)]
    
    output$sum <- renderPrint({
      # print(isolate(head(values$df)));
      print(isolate((values$results)));
      # print(model %>% summary())
      
    })
    
    
    output$myPlot <- renderPlot({
      
      
      # plot.ts(values$results,ylim=c(0,5000),col='red')
      # par(new=T)
      # plot.ts(hist_avg$demand[-c(1:8)],ylim=c(0,5000), col='springgreen4')
      # par(new=T)
      # plot.ts(y.test,ylim=c(0,5000),col='blue')
      
      
      start_day <- as.POSIXct("2010-01-01")
      start <- as.POSIXct("2010-01-01 06:00:00")
      interval <- 60
      end <- start_day + as.difftime(1, units="days")
      
      a <- seq(from=start, by=interval*120, to=end)
      x <- seq_along(values$results)
      
      plot_data <- as.data.frame(cbind(x = x,  results = values$results, expected=test_targets,
                                       historical_average=values$hist))
      
      d2 <- melt(plot_data, id="x")
      
      p <- ggplot(d2, aes(x, value, color=variable, linetype=variable))+
        geom_line() +
        scale_linetype_manual( values = c("solid","solid","dashed")) +
        
        labs(title="Demand Prediction") +
        scale_linetype_manual(labels=c("1 Step-ahead Predictions", "Observed", "Historical Average"),
                              values = c("solid","solid","dashed"))+
        scale_color_manual(labels=c("1 Step-ahead Predictions", "Observed", "Historical Average"),
                           values = c("#d7191c","#2b83ba","#4dac26")) + 
        # scale_size_manual(labels=c("1 Step-ahead Predictions", "Observed", "Historical Average"),
        #                   values=c(1, 1, .5, .75,.75))+
        
        ylab('Passengers per 15 minutes') +
        xlab('Time of day (one unit = 15 minutes)') +
        
        theme_bw() +
        theme(plot.title=element_text(size=rel(1.5), lineheight=.9, face="bold", colour="black", hjust = .5),
              plot.subtitle=element_text(  colour="black", hjust = .5),
              axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"),
              legend.title=element_blank(), legend.text = element_text(size=14)) 
      
      # if(length(results)==70){
      # p <- p + scale_x_continuous(breaks=seq(1,86, by = 8), labels= lapply(a, format,'%H:%M')) + xlab('Time of day')
      p
      # }
      
      
    })
    
    output$myTable <- renderTable({
      tibble(
        RMSE_Model = Metrics::rmse(values$results, y.test),
        MAD_Model =  Metrics::mae(values$results, y.test),
        RMSE_hist = Metrics::rmse(hist_avg$demand, y.test),
        MAD_hist =  Metrics::mae(hist_avg$demand, y.test)
      )
    })
    
  })
  
  
  
  # Create a download handler
  output$download_data <- downloadHandler(
    filename <- "predictions.csv",
    content = function(file) {
      data <- 
        values$results
      # Write the filtered data into a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Create a download handler
  output$download_hist <- downloadHandler(
    filename <- "hist_avg.csv",
    content = function(file) {
      data <- values$hist
      
      # Write the filtered data into a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
  output$download_y <- downloadHandler(
    filename <- "observed.csv",
    content = function(file) {
      data <- values$y
      
      # Write the filtered data into a CSV file
      write.csv(data, file, row.names = FALSE)
    }
  )
  
}