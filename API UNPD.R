library(jsonlite)
library(httr)
library(jsonlite)
library(httr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(data.table)
library(openxlsx)
library(tidyverse)

callAPI <- function(relative_path, topics_list=FALSE){
  base_url <- "https://population.un.org/dataportalapi/api/v1"
  target <- paste0(base_url, relative_path)
  response <- fromJSON(target)
  # Checks if response was a flat file or a list (indicating pagination)
  # If response is a list, we may need to loop through the pages to get all of the data
  if (class(response)=="list"){
    # Create a dataframe from the first page of the response using the `data` attribute
    df <- response$data
    while (!is.null(response$nextPage)){
      response <- fromJSON(response$nextPage)
      df_temp <- response$data
      df <- rbind(df, df_temp)
    }
    return(df)}
  # Otherwise, we will simply load the data directly from the API into a dataframe
  else{
    if (topics_list==TRUE){
      df <- fromJSON(target, flatten = TRUE)
      return(df[[5]][[1]])
    }
    else{
      df <- fromJSON(target)        
      return(df)
    }
  }
}

# Declares the base url for calling the API
base_url <- "https://population.un.org/dataportalapi/api/v1"

# Creates the target URL, indicators, in this instance
target <- paste0(base_url, "/indicators/")

# Get the response, which includes data as well as information on pagination and number of records
response <- fromJSON(target)

# Get the first page of data
df_indicators <- response$data

# Loop until there are new pages with data
while (!is.null(response$nextPage)){
  
  #call the API for the next page
  response <- fromJSON(response$nextPage)
  
  #add the data of the new page to the data.frame with the data of the precious pages
  df_indicators <- rbind(df_indicators, response$data)
  
}



target <- paste0(base_url, "/locations/")

# Call the API
response <- fromJSON(target)

# Get the first page of data
df_countries <- response$data

# Get the other pages with data
while (!is.null(response$nextPage)){
  
  response <- fromJSON(response$nextPage)
  df_countries <- rbind(df_countries, response$data)
  
}

ESA_countries<- df_countries[df_countries$name %in% c("Angola"
                                                ,"Botswana"
                                                ,"Burundi"
                                                ,"Comoros"
                                                ,"Eritrea"
                                                ,"Eswatini"
                                                ,"Ethiopia"
                                                ,"Kenya"
                                                ,"Lesotho"
                                                ,"Madagascar"
                                                ,"Malawi"
                                                ,"Mozambique"
                                                ,"Namibia"
                                                ,"Rwanda"
                                                ,"Somalia"
                                                ,"South Africa"
                                                ,"South Sudan"
                                                ,"United Republic of Tanzania"
                                                ,"Uganda"
                                                ,"Zambia"
                                                ,"Zimbabwe"
                                                ,"Africa"
                                                ,"Sub-Saharan Africa"
                                                ,"World"),]
#identify countries
ESA_countries_id <- as.character(ESA_countries[,"id"])
ESA_countries_id<- paste(ESA_countries_id, collapse = ",")

#south africa only
#RSA<-df_countries[df_countries$name %in% c("South Africa"),]
#RSA_id <- as.character(RSA[,"id"])


#identify indicators
#df_pop_topics <- df_indicators[df_indicators$topicShortName=="Pop", "id"]
df_pop_topics<- c('70')

df_pop_indicatorCodes<- as.character(df_pop_topics)
df_pop_indicatorCodes <- paste(df_pop_indicatorCodes, collapse = ",")

#choose indicators one by one and time lines -> total pop by sex indicator code=49

#Test ESAR populattion
target <- paste0("/data/indicators/",df_pop_topics,"/locations/",24,"?startYear=2022&endYear=2022&startAge=0&endAge=17&variants=4&sexes=3&format=json")
esaTot_pop <- callAPI(target)

# loop countries for downloading and filtering
esar_population<- data.frame()
for (x in ESA_countries_id){
  target <- paste0("/data/indicators/",df_pop_topics,"/locations/",x,"?startYear=2010&endYear=2050&startAge=0&endAge=17&variants=4&sexes=3&format=json")
  temp<- callAPI(target)
  temp<- temp %>% filter(ageId==46 | ageId==188)
  
  esar_population<-rbind(esar_population,temp)
  
}

esar_population %>% select(location,ageLabel,value,timeLabel)%>% rename(popsize=value) ->pop

pop<- pop %>% left_join(centroids, by = c("location" = "region"))

write.csv(pop,"data/pop.csv",row.names = FALSE)




#ESAR populattion change
target <- paste0("/data/indicators/","51","/locations/",ESA_countries_id,"/start/2022/end/2022")
esaTot_pop_change <- callAPI(target)
esaTot_pop_change %>% filter(variantId==4) %>% select(locationId,value) %>% rename(popchange=value)->popchange

#ESAR populattion median
target <- paste0("/data/indicators/","67","/locations/",ESA_countries_id,"/start/2022/end/2022")
esaTot_pop_median <- callAPI(target)
esaTot_pop_median %>% filter(variantId==4) %>% select(locationId,value)%>% rename(medianage=value) ->popmedian


#ESAR populattion by age
target <- paste0("/data/indicators/","70","/locations/",ESA_countries_id,"/start/2022/end/2022")
esaTot_pop_age <- callAPI(target)
esaTot_pop_age %>% filter(variantId==4) %>% filter(ageId==46) %>% filter(sexId==3) %>% select(locationId,value) %>% rename(children=value) ->children

pop_list<-list(pop,popchange,popmedian,children)
pop_list %>% reduce(full_join, by='locationId') ->printpoptable
write.xlsx(printpoptable,paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\5. Data\\14. UNPD\\ESAR Country profile - ",Sys.Date(),".xlsx", sep=""))

write.xlsx(ESAR_pop,paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\5. Data\\14. UNPD\\ESAR population - ",Sys.Date(),".xlsx", sep=""))
write.xlsx(SA2_pop,paste("C:\\Users\\hwannis\\OneDrive - UNICEF\\ESARO\\5. Data\\14. UNPD\\SA2050 population - ",Sys.Date(),".xlsx", sep=""))


