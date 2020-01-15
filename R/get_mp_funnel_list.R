#' Mixpanel Funnels List
#'
#' This function retrieves funnel list from Mixpanel.
#'
#' @param api_secret The api secret from your Mixpanel account.
#' @return The funnel list.
#' @import rjson
#' @export

# call devtools::document() to document.
# use the following to push to git, after Commit:
# git push --mirror https://github.com/daranjjohnson/mixpanelFunnels
# install on other computers:
# devtools::install_github("daranjjohnson/mixpanelFunnels")

get_mp_funnel_list <- function(api_secret){
        
        # Check the api secret is passed - if not, exit.
        if(!is.null(api_secret)){
                
                full_url <- paste0('https://', api_secret, '@mixpanel.com/api/2.0/funnels/list/')
               
        } else{return()}
        
        # Create a temp fiel to hold the data.
        filePath = paste("temp_", uuid::UUIDgenerate(), ".txt", sep="")
        
        # And remove the file when done.
        on.exit( { unlink(filePath) } )
        
        # Download the data.
        download.file(full_url, filePath, 'curl', quiet = TRUE)
        
        # Then read it and return.
        res = scan(filePath, what=character(), sep="\n", quiet=TRUE)
        
        # Next - transform to readable JSON.
        json_funnel_list <- fromJSON(res)
        
        # Create output variable.
        funnel_list <- NULL
        
        i <- 1
        
        # Loop through each date.
        while(i <= length(json_funnel_list)){ # This is the dates
                
                funnel_name <- json_funnel_list[[i]][[2]]
                funnel_id <- json_funnel_list[[i]][[1]]
                
                funnel_list_temp <- data.frame(funnel_id, funnel_name)
                
                if(is.null(funnel_list)){
                        
                        funnel_list <- funnel_list_temp
                } else{
                        
                        funnel_list <- rbind(funnel_list, funnel_list_temp)
                }
                
                i <- i + 1

        }
        
        return(funnel_list)
        
}





