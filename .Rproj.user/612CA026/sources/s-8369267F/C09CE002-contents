library(rlang)
library(rjson)
library(magrittr)
library(readr)
library(purrr)
library(dplyr)
library(tibble)
library(stringr)
library(zoo)
library(tidyr)
library(httr)

republishChart <- function(API_KEY, chartID, data, subtitle = NULL, title = NULL, notes) {
  
  dataRefresh <- PUT(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                  chartID, "/data"),
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
  
  notesRes <- PATCH(url = paste0("https://api.datawrapper.de/v3/charts/", 
                                 chartID),
                    add_headers(authorization = paste("Bearer", API_KEY, 
                                                      sep = " ")),
                    body = call_back,
                    encode = "json")
  
  publishRes <- POST(
    url = paste0("https://api.datawrapper.de/v3/charts/", 
                 chartID, "/publish"),
    add_headers(authorization = paste("Bearer", 
                                      API_KEY, 
                                      sep = " "))
  )
  
  list(dataRefresh, notesRes, publishRes) -> resList
  
  if (any(map_lgl(resList, http_error))) {
    which(map_lgl(resList, http_error))[1] -> errorIdx
    
    stop_for_status(resList[[errorIdx]], task = paste0("update step ",
                                                       errorIdx, 
                                                       " of chart ", 
                                                       chartID))
    
  } else {
    message(paste0("Chart ", chartID, " updated successfully"))
  }
  
}

DW_API <- Sys.getenv("DW_API_KEY")
SCT_PW <- Sys.getenv("SCT_PW")

statePops <- read_csv("./referenceData/populationEstimates.csv", col_types = "ciiici")

vaccineEligP1All <- read_csv("./referenceData/2021_02_23_vaccineEligibilityDataW1.csv", col_types = "ciiiiiiiiiiiiiici")

stateFIPS <- read_csv("./referenceData/stateFIPSCodes.csv", col_types = "cci")

Sys.sleep(3)

safe_extract <- function(l, wut) {
  res <- l[wut]
  null_here <- map_lgl(res, is.null)
  res[null_here] <- NA
  res
}


Sys.sleep(10)

cdcFetch <- tryCatch(
  {
    res <- GET(url = "https://data.cdc.gov/resource/unsk-b7fc.csv?$limit=50000",
               authenticate(user = "anesta@dotdash.com", password = SCT_PW))
    
    stop_for_status(res)
    
    Sys.sleep(10)
    
    content(res, encoding = "UTF-8") %>% 
      mutate(date = base::as.Date(date)) %>% 
      left_join(statePops, by = c("location" = "postal_code")) %>% 
      rename_with(.cols = everything(), .fn = ~str_replace_all(
        str_to_title(str_replace_all(.x, "_", " ")), " ", "_")
      ) %>% 
      rename(LongName = Geographic_Area_Name) -> cdcFullTableUpdated 
    
    cdcFullTableUpdated %>% 
      filter(Date == max(Date)) -> cdcTable 
  }, 
  error = function(cond) {
    condFull <- error_cnd(class = "cdcFetch", message = paste("An error occured with fetching the CDC Data:", 
                                                                        cond, "on", Sys.Date(), "\n"
    ))
    
    write(condFull[["message"]], "./errorLog.txt", append = T)
    
    return(condFull)
  }
  
)

if (class(cdcFetch)[2] == "rlang_error") {
  stop("CDC Fetch Error")
} else {
  finalResult <- tryCatch(
    {
      
      # cdcTable <- fromJSON(file = "https://raw.githubusercontent.com/anesta95/veryWellVaccineDataTracker/main/cdcVaccines.json") %>%
      #   extract2(2) %>%
      #   map_df(`[`) %>%
      #   mutate(Date = base::as.Date(Date)) %>%
      #   mutate(LongName = if_else(
      #     LongName == "New York State", "New York",
      #     LongName))
      
      
      # cdcFullTable <- read_csv("./chartData/cdcFullTable.csv")
      # 
      # if (unique(cdcTable$Date) == max(cdcFullTable$Date)) {
      #   cdcFullTableUpdated <- cdcFullTable
      # } else {
      #   cdcFullTableUpdated <- bind_rows(cdcTable, cdcFullTable)
      # }
      
      dwSubtitle <- paste0("Data updated as of ", format(max(cdcFullTableUpdated$Date), "%m/%d/%Y"))
      
      write_csv(cdcFullTableUpdated, "./chartData/cdcFullTable.csv")
      
      Sys.sleep(5)
      
      cdcWWWNontotal <- cdcTable %>% 
        filter(!(Location %in% c("BP2", "DD2", "IH2", "VA2", "LTC", "US"))) %>% 
        mutate(Complete_Vaccinations_Per_100K = Series_Complete_Pop_Pct * 1000,
               Administered_Dose1_Per_100K = Administered_Dose1_Pop_Pct * 1000) %>% 
        rename(Doses_Administered = Administered) %>% 
        select(LongName, 
               Doses_Administered,
               Administered_Dose1_Per_100K,
               Complete_Vaccinations_Per_100K
        ) %>% 
        arrange(desc(Complete_Vaccinations_Per_100K)) %>% 
        mutate(
          Doses_Administered = as.character(Doses_Administered),
          Administered_Dose1_Per_100K = as.character(Administered_Dose1_Per_100K),
          Complete_Vaccinations_Per_100K = as.character(Complete_Vaccinations_Per_100K)) %>% 
        filter(!is.na(LongName))
      
      cdcWWWTotal <- tibble_row(
        LongName = "U.S. Total",
        Doses_Administered = format(sum(as.integer(cdcWWWNontotal$Doses_Administered), na.rm = T), big.mark = ",", scientific = F),
        Administered_Dose1_Per_100K = format(round((pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose1_Pop_Pct"])) * 1000), big.mark = ",", scientific = F),
        Complete_Vaccinations_Per_100K = filter(cdcTable, Location == "US") %>% select(Series_Complete_Pop_Pct) %>% pull() %>% `*` (1000) %>% format(big.mark = ",", scientific = F)
        #   format(
        #   round(
        #     ((pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose2_Recip"]) + pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Janssen"])) / 331996199L) * 100000
        #   ), big.mark = ",", scientific = F
        # )
      )
      
      
      
      cdcWWWFormatted <- bind_rows(cdcWWWTotal, cdcWWWNontotal) %>% 
        rename(State = LongName,
               `Total Doses Administered` = Doses_Administered,
               `People with 1 Dose per 100k` = Administered_Dose1_Per_100K,
               `People Completely Vaccinated per 100k` = Complete_Vaccinations_Per_100K
        )
      
      
      cdcWWWFormatted %>% write_csv("./chartData/cdcWWWFormatted.csv")
      
      
      wwwTitle <- paste0(as.character(filter(cdcTable, Location == "US") %>% 
                                        select(Series_Complete_Pop_Pct) %>% 
                                        pull()), 
                         "% of the U.S. Population is Fully Immunized with the COVID-19 Vaccine")
      
      republishChart(API_KEY = DW_API, chartID = "5vLNq", data = cdcWWWFormatted, title = wwwTitle,
                     notes = dwSubtitle)
      
      
      Sys.sleep(5)
      
      cdcMap <- cdcTable %>% 
        # mutate(Complete_Vaccinations = round(Administered_Dose2_Recip + Administered_Janssen)) %>% 
        # inner_join(statePops, by = c("Location" = "postal_code")) %>% 
        mutate(`% of Currently Eligible Vaccinated` = round((Series_Complete_Yes / Census2019_5pluspop_2) * 100, digits = 1))%>% 
        select(LongName, `% of Currently Eligible Vaccinated`, 
               Census2019_5pluspop_2) %>% 
        rename(
          ID = LongName,
          `Total Currently Eligible` = Census2019_5pluspop_2
        ) %>% 
        filter(!is.na(ID) & ID != "Puerto Rico")
      
      cdcMap %>% write_csv("./chartData/cdcMap.csv")
      
      republishChart(API_KEY = DW_API, chartID = "3Jq4X",
                     data = cdcMap, notes = dwSubtitle)
      
      Sys.sleep(5)
      
      
      areWeThereYetNontotal <- cdcFullTableUpdated %>% 
        filter(Date %in% c(max(Date), max(Date) - 28)) %>%
        filter(!(Location %in% c("RP", "IH2", "VI", "MP", "BP2", "DD2",
                                 "GU", "FM", "MH", "VA2", "AS", "US"))) %>% 
        # filter(!(LongName %in% c("Bureau of Prisons", "Long Term Care", 
        #                          "Dept of Defense",
        #                          "Indian Health Svc",
        #                          "Veterans Health", "United States"))) %>%
        arrange(desc(Date), Location) %>% 
        mutate(`Doses administered in the last four weeks` = Series_Complete_Yes - lead(
          Series_Complete_Yes, n = 52)) %>% 
        # `1+ Doses adminstered in the last week` = Administered_Dose1_Recip - lead(
        #   Administered_Dose1_Recip, n = 59)
        filter(`Doses administered in the last four weeks` != 0) %>% 
        mutate(`Doses administered in the last four weeks` = if_else(
          `Doses administered in the last four weeks` < 0, 0, `Doses administered in the last four weeks`
        )) %>%
        # inner_join(statePops, by = c("Location" = "postal_code")) %>% 
        mutate(`70% of population` = .7 * Census2019_2,
               `Estimated to 70% Pop 2 Doses` = strftime(base::as.Date(
                 round(
                   (
                     (
                       (
                         (`70% of population` - Series_Complete_Yes) / 
                           `Doses administered in the last four weeks`)) * 28) + 28), 
                 origin = "1970-01-01") + as.integer(Sys.Date()), format = "%B %Y"),
               testSort = round(((((`70% of population` - Series_Complete_Yes) / 
                                     `Doses administered in the last four weeks`)) * 28) + 28)
               # Completely_Vaccinated_Pop_Pct = round(((Administered_Dose2_Recip + Administered_Janssen) / Census2019) * 100)
        ) %>%
        arrange(testSort) %>% 
        select(LongName, Series_Complete_Pop_Pct,
               `Doses administered in the last four weeks`, `Estimated to 70% Pop 2 Doses`) %>% 
        mutate(`Doses administered in the last four weeks` = format(`Doses administered in the last four weeks`, big.mark = ",", scientific = F)) %>% 
        rename(`% Population Fully Immunized` = Series_Complete_Pop_Pct, 
               State = LongName) %>% 
        mutate(`Estimated to 70% Pop 2 Doses` = if_else(`% Population Fully Immunized` >= 70, "Reached 70% fully vaccinated", `Estimated to 70% Pop 2 Doses`))
      
      # onePlusVaxLastWeek <- cdcFullTableUpdated %>% 
      #   filter(Date %in% c(max(Date), max(Date) - 7)) %>%
      #   filter(!(LongName %in% c("Bureau of Prisons", "Long Term Care",
      #                            "Dept of Defense",
      #                            "Indian Health Svc",
      #                            "Veterans Health", "United States"))) %>%
      #   mutate(`1+ Doses adminstered in the last week` = Administered_Dose1_Recip - lead(
      #     Administered_Dose1_Recip, n = 59)) %>% 
      #   filter(!is.na(`1+ Doses adminstered in the last week`)) %>% 
      #   pull(`1+ Doses adminstered in the last week`) %>% 
      #   sum()
      
      areWeThereYetTotal <- cdcFullTableUpdated %>% 
        filter(Date %in% c(max(Date), max(Date) - 28), Location == "US") %>%
        mutate(LongName = "United States") %>% 
        mutate(`Doses administered in the last four weeks` = Series_Complete_Yes - lead(
          Series_Complete_Yes, n = 1), Census2019_2 = 332008832L
          # `1+ Doses adminstered in the last week` = Administered_Dose1_Recip - lead(
          #   Administered_Dose1_Recip, n = 59)
        ) %>% 
        filter(!is.na(`Doses administered in the last four weeks`)) %>% 
        # inner_join(statePops, by = c("Location" = "postal_code")) %>% 
        mutate(`70% of population` = .7 * Census2019_2,
               `Estimated to 70% Pop 2 Doses` = strftime(base::as.Date(
                 round(
                   (
                     (
                       (
                         (`70% of population` - Series_Complete_Yes) / 
                           `Doses administered in the last four weeks`)) * 28) + 28), 
                 origin = "1970-01-01") + as.integer(Sys.Date()), format = "%B %Y"),
               testSort = round(((((`70% of population` - Series_Complete_Yes) / 
                                     `Doses administered in the last four weeks`)) * 28) + 28)
               # Completely_Vaccinated_Pop_Pct = round(((Administered_Dose2_Recip + Administered_Janssen) / Census2019) * 100)
        ) %>%
        arrange(testSort) %>% 
        select(LongName, Series_Complete_Pop_Pct,
               `Doses administered in the last four weeks`, `Estimated to 70% Pop 2 Doses`) %>% 
        mutate(`Doses administered in the last four weeks` = format(`Doses administered in the last four weeks`, big.mark = ",", scientific = F)) %>% 
        rename(`% Population Fully Immunized` = Series_Complete_Pop_Pct, 
               State = LongName) %>% 
        mutate(`Estimated to 70% Pop 2 Doses` = if_else(`% Population Fully Immunized` >= 70, "Reached 70% fully vaccinated", `Estimated to 70% Pop 2 Doses`))
      
      
      
      # areWeThereYetTotal <- tibble_row(
      #   State = "U.S. Total", 
      #   `% Population Fully Immunized` =
      #     round(
      #       ((pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose2_Recip"]) + pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Janssen"])) / 331996199L) * 100, 1
      #     ),
      #   `Doses administered in the last four weeks` = format(
      #     sum(
      #       as.integer(
      #         str_remove_all(
      #           str_trim(areWeThereYetNontotal$`Doses administered in the last four weeks`), ",")), na.rm = T
      #     ), big.mark = ",", scientific = F),
      #   `Estimated to 70% Pop 2 Doses` = strftime(
      #     base::as.Date(
      #       (round(
      #         (
      #           (
      #             (
      #               (331996199L * .7) - pull(cdcTable[which(cdcTable$Location == "US"), "Administered_Dose1_Recip"])) / 
      #               onePlusVaxLastWeek) * 7) + 28) + as.integer(Sys.Date())), origin = "1970-01-01"
      #     ), format = "%B %Y")
      # )
      
      # Prev US Pop Estimate 328580394L
      areWeThereYet <- bind_rows(areWeThereYetTotal, areWeThereYetNontotal)
      
      write_csv(areWeThereYet, "./chartData/areWeThereYet.csv")
      
      republishChart(API_KEY = DW_API, chartID = "NAj9u", data = areWeThereYet,
                     notes = dwSubtitle)
      
      Sys.sleep(5)
      # Don't need this anymore
      # supplyProjection <- cdcFullTableUpdated %>%
      #   filter(Date %in% c(max(Date), max(Date) - 7)) %>%
      #   filter(!(LongName %in% c("Bureau of Prisons", "Long Term Care",
      #                            "Dept of Defense",
      #                            "Indian Health Svc",
      #                            "Veterans Health"))) %>%
      #   arrange(desc(Date), LongName) %>%
      #   mutate(totalDosesDistrb = ((Distributed_Moderna / 2) + (Distributed_Pfizer / 2) + Distributed_Janssen),
      #          Complete_Vaccinations_Per_100K = round(((Administered_Dose2_Recip + Administered_Janssen) / Census2019) * 100000),
      #          `Doses delivered in the last week` = totalDosesDistrb - lead(
      #            totalDosesDistrb, n = 60
      #          )) %>%
      #   filter(`Doses delivered in the last week` != 0) %>%
      #   inner_join(statePops, by = c("Location" = "postal_code")) %>%
      #   mutate(`100% Vaccine Availability Est. Date` = strftime(base::as.Date(
      #     round(
      #       (
      #         (
      #           (
      #             (Census2019_18PlusPop_2 - totalDosesDistrb) /
      #               `Doses delivered in the last week`)) * 7)),
      #     origin = "1970-01-01") + as.integer(Sys.Date()))) %>%
      #   mutate(`How far behind` = case_when(`100% Vaccine Availability Est. Date` > base::as.Date("2021-05-31") & `100% Vaccine Availability Est. Date` < base::as.Date("2021-06-30") ~ "Less than 1 month behind",
      #                                       `100% Vaccine Availability Est. Date` >= base::as.Date("2021-06-30") & `100% Vaccine Availability Est. Date` < base::as.Date("2021-07-31") ~ "1-2 months behind",
      #                                       `100% Vaccine Availability Est. Date` >= base::as.Date("2021-07-31") & `100% Vaccine Availability Est. Date` < base::as.Date("2021-08-31") ~ "2-3 months behind",
      #                                       `100% Vaccine Availability Est. Date` >= base::as.Date("2021-08-31") & `100% Vaccine Availability Est. Date` < base::as.Date("2021-09-30") ~ "3-4 months behind",
      #                                       `100% Vaccine Availability Est. Date` >= base::as.Date("2021-09-30") & `100% Vaccine Availability Est. Date` < base::as.Date("2021-10-31") ~ "4-5 months behind",
      #                                       `100% Vaccine Availability Est. Date` >= base::as.Date("2021-10-31") & `100% Vaccine Availability Est. Date` < base::as.Date("2021-11-30") ~ "5-6 months behind",
      #                                       `100% Vaccine Availability Est. Date` >= base::as.Date("2021-11-30") & `100% Vaccine Availability Est. Date` < base::as.Date("2022-05-31") ~ "6 months - 1 year behind",
      #                                       `100% Vaccine Availability Est. Date` > base::as.Date("2022-05-31") ~ "1 year+",
      #                                       `100% Vaccine Availability Est. Date` <= base::as.Date("2021-05-31") ~ "Ahead of schedule")) %>% 
      #   select(LongName, Complete_Vaccinations_Per_100K, Census2019_18PlusPop_2, 
      #          `How far behind`, `100% Vaccine Availability Est. Date`) %>%
      #   rename(State = LongName, 
      #          Population = Census2019_18PlusPop_2,
      #          `People Vaccinated per 100K` = Complete_Vaccinations_Per_100K) %>%
      #   arrange(`100% Vaccine Availability Est. Date`)
      # 
      # write_csv(supplyProjection, "./chartData/supplyProjection.csv")
      
      Sys.sleep(5)
      
      firstVsSecondGrowth <- cdcFullTableUpdated %>% 
        filter(Location == "US") %>% 
        select(Date, Administered_Dose1_Recip, Series_Complete_Yes) %>% 
        mutate(dtdDose1Growth = Administered_Dose1_Recip - lead(Administered_Dose1_Recip, n = 1),
               dtdDose2Growth = Series_Complete_Yes - lead(Series_Complete_Yes, n = 1),
               `First Dose Daily Avg` = rollmean(dtdDose1Growth, k = 7, fill = NA, align = "left"),
               `Second Dose Daily Avg` = rollmean(dtdDose2Growth, k = 7, fill = NA, align = "left")) %>% 
        select(Date, `First Dose Daily Avg`, `Second Dose Daily Avg`) %>% 
        filter(!is.na(`First Dose Daily Avg`))
      
      write_csv(firstVsSecondGrowth, "./chartData/firstVsSecondGrowth.csv")
      
      republishChart(API_KEY = DW_API, chartID = "ZOj6S", data = firstVsSecondGrowth,
                     notes = dwSubtitle)
      
      # CDC by county map
      
      cdcVaxByCounty <- fromJSON(file = "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data") %>% 
        extract2("vaccination_county_condensed_data") %>%   
        map_df(safe_extract) %>% 
        select(Date, FIPS, StateName, StateAbbr, County, Series_Complete_Pop_Pct,
               Administered_Dose1_Pop_Pct) %>% 
        filter(FIPS != "UNK", !is.na(Series_Complete_Pop_Pct)) %>% 
        mutate(StateAbbr = str_trim(StateAbbr)) %>% 
        rename(`FIPS-Code` = FIPS)
      
      write_csv(cdcVaxByCounty, "./chartData/cdcVaxByCounty.csv")
      
      republishChart(API_KEY = DW_API, chartID = "Tko5S", data = cdcVaxByCounty,
                     notes = paste0("Data updated as of ", format(
                       max(base::as.Date(cdcVaxByCounty$Date)), "%m/%d/%Y")))
      
      # Vaccines by age
      cdcVaxByAgeHist <- read_csv("./chartData/cdcFullVaxByAge.csv",
                                  col_types = "Dddddddddd")
      
      cdcVaxByAge <- read_csv(file = "https://data.cdc.gov/resource/km4m-vcsb.csv",
                              col_types = "Tciddidddd") %>% 
        mutate(date = base::as.Date(date))
      
      if(max(cdcVaxByAge$date, na.rm = T) > max(cdcVaxByAgeHist$Date, na.rm = T)) {
        
        
        cdcVaxByAge %>% 
          filter(date >= max(cdcVaxByAgeHist$Date, na.rm = T)) %>%
          select(date, demographic_category, series_complete_yes) %>% 
          filter(demographic_category %in% c("Ages_<12yrs",
                                             "Ages_12-15_yrs",
                                             "Ages_16-17_yrs",
                                             "Ages_18-24_yrs",
                                             "Ages_25-39_yrs",
                                             "Ages_40-49_yrs",
                                             "Ages_50-64_yrs",
                                             "Ages_65-74_yrs",
                                             "Ages_75+_yrs")) %>% 
          arrange(demographic_category, desc(date)) %>% 
          group_by(demographic_category) %>% 
          mutate(dailyDiff = series_complete_yes - lead(series_complete_yes)) %>% 
          ungroup() %>% 
          filter(!is.na(dailyDiff)) %>% 
          group_by(date) %>% 
          mutate(dailyDoses = sum(dailyDiff)) %>% 
          ungroup() %>% 
          mutate(ageProp = (dailyDiff / dailyDoses) * 100) %>% 
          select(date, demographic_category, ageProp) %>% 
          pivot_wider(id_cols = date, names_from = demographic_category, values_from = ageProp) %>% 
          rename_with(~str_remove_all(.x, "_"), .cols = 2:10) %>%
          rename_with(~str_extract(.x, "<*\\d{2}[-\\+]*\\d*"), .cols = 2:10) %>%
          rename(Date = date) %>% 
          arrange(Date) -> cdcVaxByAgeNewest
        
        cdcVaxByAgeUpdated <- bind_rows(cdcVaxByAgeHist, cdcVaxByAgeNewest)
        
        write_csv(cdcVaxByAgeUpdated, "./chartData/cdcFullVaxByAge.csv")
        
        republishChart(API_KEY = DW_API, chartID = "f9Zc0",
                       data = cdcVaxByAgeUpdated, notes = paste0("Data updated as of ", format(
                         max(base::as.Date(cdcVaxByAgeUpdated$Date)), "%m/%d/%Y")))
        
      } else {
        republishChart(API_KEY = DW_API, chartID = "f9Zc0",
                       data = cdcVaxByAgeHist, notes = paste0("Data updated as of ", format(
                         max(base::as.Date(cdcVaxByAgeHist$Date)), "%m/%d/%Y")))
      }
      
      cdcTable %>% 
        select(LongName, Location, Additional_Doses_Vax_Pct) %>% 
        filter(Location %in% c(state.abb, "DC")) -> boosterMap
      
      
      write_csv(boosterMap, "./chartData/boosterMap.csv")
      
      republishChart(API_KEY = DW_API, chartID = "tQiJ5",
                     data = boosterMap, notes = dwSubtitle)
      
      
      
    }, error = function(cond) {
      condFull <- error_cnd(class = "vwDataTrackerError", message = paste("An error occured with the update:", 
                                                                          cond, "on", Sys.Date(), "\n"
      ))
      
      write(condFull[["message"]], "./errorLog.txt", append = T)
      
      return(condFull)
    }
  )
}








