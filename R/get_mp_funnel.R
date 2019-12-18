#' Mixpanel Funnels
#'
#' This function retrieves funnel data from Mixpanel.
#'
#' @param api_secret The api secret from your Mixpanel account.
#' @param funnel_id The id of the funnel you want to retrieve.
#' @param unit The data can be retrieved by day, week, month, or year.
#' @param from_date The start date to retrieve the data.
#' @param to_date The end date to retrieve the data.
#' @return The funnel steps in order, by date.
#' @import lubridate
#' @import rjson
#' @export

# use the following to push to git, after Commit:
# git push --mirror https://github.com/daranjjohnson/mixpanelFunnels
# install on other computers:
# devtools::install_github("daranjjohnson/mixpanelFunnels")

get_mp_funnel <- function(api_secret, funnel_id, unit, from_date, to_date){
   
   # Check the api secret is passed - if not, exit.
   if(!is.null(api_secret)){
      
         url <- paste0('https://', api_secret, '@mixpanel.com/api/2.0/funnels/?')
      
   } else{return()}
   
   # Check the funnel id is passed - if not, exit.
   if(!is.null(funnel_id)){
      
         funnel_id <- paste0('funnel_id=', funnel_id)
      
   } else{return()}
   
   # Check the unit is passed - if not, use day.
   if(!is.null(unit)){
      
      if(unit %in% c('day', 'week', 'month', 'year')){
         
            unit <- paste0('unit=', unit)
         
      } else{ unit <- 'unit=day' }
      
   } else{ unit <- 'unit=day' }

   # Check the from date is passed - if not, use 30 days ago
   if(!is.null(from_date)){
      
         if(lubridate::is.Date(as.Date(from_date))){
            
               from_date <- paste0('from_date=', from_date)
            
         } else{ from_date <- Sys.Date() - 30}
      
   } else{ from_date <- Sys.Date() - 30}
   
   # Check the to date is passed - if not, use today.
   if(!is.null(to_date)){
      
         if(lubridate::is.Date(as.Date(to_date))){
         
               to_date <- paste0('to_date=', to_date)
         
         } else{ to_date <- Sys.Date() - 30}
      
   } else{ to_date <- Sys.Date()}
   
   # Bring the pieces together.
   full_url <- paste0(url, 
                      funnel_id, '&', 
                      unit, '&',
                      from_date, '&',
                      to_date)
   
   # Create a temp fiel to hold the data.
   filePath = paste("temp_", uuid::UUIDgenerate(), ".txt", sep="")
   
   # And remove the file when done.
   on.exit( { unlink(filePath) } )
   
   # Download the data.
   download.file(full_url, filePath, 'curl', quiet = TRUE)
   
   # Then read it and return.
   res = scan(filePath, what=character(), sep="\n", quiet=TRUE)
   
   # Next - transform to readable JSON.
   json_funnels <- fromJSON(res)
   
   # Create output variable.
   funnel_steps <- NULL
   
   i <- 1
   
   # Loop through each date.
   while(i <= length(json_funnels[[1]])){ # This is the dates
      
      date <- names(json_funnels[[1]][i]) # Get the date 'name' - it's the date.
      
      e <- 1
      
      # Loop through the steps to get the name and count.
      while(e <= length(json_funnels[[1]][[1]]$steps)){
         
         order <- e
         goal <- json_funnels[[1]][[1]][[1]][[e]]$step_label
         count <- json_funnels[[1]][[1]][[1]][[e]]$count
         
         # Create a temp table to hold the step data.
         funnel_steps_temp <- data.frame(order, goal, count, date)
         
         # If the funnel_steps is null - it's the first step.
         if(is.null(funnel_steps)){
            
            funnel_steps <- funnel_steps_temp
            
         } else{
            
            funnel_steps <- rbind(funnel_steps, funnel_steps_temp)
            
         }
         
         e <- e + 1
         
      }
      
      i <- i + 1
      
   }
   
   return(funnel_steps)
   
}





