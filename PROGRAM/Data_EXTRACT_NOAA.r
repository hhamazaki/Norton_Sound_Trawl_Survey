# install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(dplyr)

# link to the API
api_link <- "https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey/"

res <- httr::GET(url = paste0(api_link, '?q={"year":2023,"srvy":"NBS"}offset=0&limit=10000'))
data_catch <- jsonlite::fromJSON(base::rawToChar(res$content))$items %>% 
  dplyr::select(year,srvy,haul,vessel_id,date_time,station,species_code,common_name,cpue_kgkm2) 
  


# res # Test connection
data <- jsonlite::fromJSON(base::rawToChar(res$content))
# names(data)
tibble::as_tibble(data$items) %>% 
  dplyr::mutate_if(is.character, type.convert, as.is = TRUE) %>%
  dplyr::mutate(across(where(is.numeric), round, 3)) %>%
  head(3) %>%
  flextable::flextable() %>%
  flextable::theme_zebra() %>%
  flextable::colformat_num(x = ., j = c("year", "cruise", "species_code", "tsn", "ak_survey_id"), big.mark = "")
