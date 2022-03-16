library(httr)
library(dplyr)
library(readr)
library(rlang)
library(purrr)

## Function to update datawrapper charts
republish_chart <- function(API_KEY, chart_id, data, subtitle = NULL, title = NULL, notes) {
  
  data_refresh <- PUT(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                  chart_id, "/data"),
                     add_headers(authorization = paste("Bearer", 
                                                       API_KEY, 
                                                       sep = " ")),
                     body = format_csv(data))
  
  call_back <- list(metadata = list())
  
  if (!is.null(title)) {
    call_back$title <- title
  }
  
  if (!is.null(subtitle)) {
    call_back$metadata$describe$intro <- subtitle   
  }
  
  call_back$metadata$annotate$notes <- notes
  
  notes_res <- PATCH(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                 chart_id),
                    add_headers(authorization = paste("Bearer", API_KEY, 
                                                      sep = " ")),
                    body = call_back,
                    encode = "json")
  
  publish_res <- POST(
    url = paste0("https://api.datawrapper.de/v3/charts/", 
                 chart_id, "/publish"),
    add_headers(authorization = paste("Bearer", 
                                      API_KEY, 
                                      sep = " "))
  )
  
  list(data_refresh, notes_res, publish_res) -> res_list
  
  if (any(map_lgl(res_list, http_error))) {
    which(map_lgl(res_list, http_error))[1] -> error_idx
    
    stop_for_status(res_list[[error_idx]], task = paste0("update step ",
                                                        error_idx, 
                                                       " of chart ", 
                                                       chart_id))
    
  } else {
    message(paste0("Chart ", chart_id, " updated successfully"))
  }
  
}

# Importing datawrapper api key and socrata password environment secrets
DW_API <- Sys.getenv("DW_API_KEY")
SCT_PW <- Sys.getenv("SCT_PW")

# Creating reference dataframe from base R state name and abbreviation vectors
state_ref <- tibble(
  full_name = c(state.name, "District of Columbia", 
                "Puerto Rico", "United States"),
  abbv = c(state.abb, "DC", "PR", "US")
)

# Importing population of various age groups by state
## TODO: Update this with new ACS 2020 5-year data
state_pops <- read_csv("./data/state_pops.csv", col_names = T, col_types = "ciiiii")

# Getting CDC vaccine data by state from the data.cdc.gov site via the
# socrata api: 
# https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc

cdc_fetch <- tryCatch(
  {
    res <- GET(url = "https://data.cdc.gov/resource/unsk-b7fc.csv?$limit=66",
               authenticate(user = "anesta@dotdash.com", 
                            password = SCT_PW))
    
    stop_for_status(res)
    
    content(res, encoding = "UTF-8", type = "text/csv",
            col_types = cols(.default = col_character())) %>% 
      mutate(date = base::as.Date(date)) %>% 
      filter(date == max(date)) -> covid_vax_by_state
    
  }, 
  error = function(cond) {
    e_full <- error_cnd(class = "cdc_fetch", 
                        message = paste(
                          "An error occured with fetching the CDC Data:", 
                                                              cond, 
                          "on", Sys.Date(), "\n"))
    
    message(e_full[["message"]])
    
    return(e_full)
    
  }
  
)

Sys.sleep(3)

if (class(cdc_fetch)[2] != "rlang_error") {
  
  # Creating Datawrapper subtitle note
  dw_subtitle <- paste0("COVID-19 vaccination data from the CDC as of ", 
                       format(max(covid_vax_by_state$date), "%m/%d/%Y"),
                       ".")
  
  # Below I am imputing the 5-11 year old, 12-17 year old, and under 18 raw
  # estimated number fully vaccinated from the [age group] plus categories
  covid_vax_by_state %>% 
    select(date, 
           location, 
           series_complete_yes, 
           series_complete_12plus, 
           series_complete_18plus, 
           series_complete_5plus) %>% 
    filter(date == max(date)) %>% 
    mutate(across(starts_with("series_complete"), as.integer)) %>%
    mutate(series_complete_5_11 = series_complete_5plus - series_complete_12plus,
           series_complete_12_17 = series_complete_12plus - series_complete_18plus,
           series_complete_U_18 = series_complete_yes - series_complete_18plus) %>% 
    inner_join(state_ref, by = c("location" = "abbv")) %>% 
    inner_join(state_pops, by = c("full_name" = "state")) %>% 
    filter(!(location %in% c("US", "PR"))) %>% 
    mutate(series_complete_5_11pop_pct = round((series_complete_5_11 / pop_5_11) * 100, 1),
           series_complete_12_17pop_pct = round((series_complete_12_17 / pop_12_17) * 100, 1),
           series_complete_U_18pop_pct = round((series_complete_U_18 / (total_pop - pop_18_plus)) * 100, 1)) %>% 
    select(date, location, full_name, series_complete_5_11pop_pct,
           series_complete_12_17pop_pct, series_complete_U_18pop_pct) %>% 
    arrange(full_name) -> kids_covid_19_vax_map
  
  write_csv(kids_covid_19_vax_map, "./data/kids_covid_19_vax_map.csv") 
  
  # Updating Datawrapper charts
  republish_chart(API_KEY = DW_API,
                  chart_id = "22ZPI",
                  data = kids_covid_19_vax_map,
                  notes = dw_subtitle
  )
  
  Sys.sleep(2)
  
  republish_chart(API_KEY = DW_API,
                  chart_id = "2VbWF",
                  data = kids_covid_19_vax_map,
                  notes = dw_subtitle
  )
  
  Sys.sleep(2)
  
  republish_chart(API_KEY = DW_API,
                  chart_id = "ZHO2q",
                  data = kids_covid_19_vax_map,
                  notes = dw_subtitle
  )

  }