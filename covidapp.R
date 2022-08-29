# update data with automated script
#source("jhu_data_daily_cases.R") # option to update daily cases
source("jhu_data_weekly_cases.R") # run locally to update numbers, but not live on Rstudio server /Users/epp11/Dropbox (VERG)/GitHub/nCoV_tracker/app.R(to avoid possible errors on auto-updates)
source("ny_data_us.R") # run locally to update numbers, but not live on Rstudio server (to avoid possible errors on auto-updates)

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_other_col = "#662506"
sars_col = "#045a8d"
h1n1_col = "#4d004b"
ebola_col = "#016c59"

# import data
cv_cases = read.csv("input_data/coronavirus.csv")
sars_cases = read.csv("input_data/sars.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
ebola_cases = read.csv("input_data/ebola.csv")
h1n1_cases = read.csv("input_data/h1n1.csv")
worldcountry = geojson_read("input_data/50m.geojson", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")
cv_states = read.csv("input_data/coronavirus_states.csv")

### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("Cumulative cases") +  xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new, colour = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    # geom_bar(position="stack", stat="identity") + 
    ylab("New cases (weekly)") + xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# test function
#cumulative_plot(cv_aggregated, current_date)
#new_cases_plot(cv_aggregated, current_date)

# function to plot cumulative sars cases by date
sars_cumulative_plot = function(sars_aggregated, sars_date) {
  plot_df = subset(sars_aggregated, date<=as.Date(sars_date, format="%Y-%m-%d"))
  ggplot(plot_df, aes(x = date, y = cases)) + geom_line(colour = sars_col) + geom_point(size = 1, alpha = 0.8, colour = sars_col) +
    ylab("Cumulative cases") + xlab("Date") + theme_bw() + 
    scale_colour_manual(values=c(sars_col)) + scale_x_date(date_labels = "%b", limits=c(sars_min_date,sars_max_date)) +
    scale_y_continuous(limits=c(0,10000), labels = function(l) {trans = l / 1000000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 5, 5, 5))
}

# function to plot new cases by date
sars_new_cases_plot = function(sars_aggregated, plot_date) {
  plot_df_new = subset(sars_aggregated, date<=plot_date)
  ggplot(plot_df_new, aes(x = date, y = new)) + 
    geom_bar(position="stack", stat="identity", fill = sars_col) + 
    ylab("New cases") +  xlab("Date") + theme_bw() + ylim(0,2000) + 
    scale_fill_manual(values=c(sars_col)) +
    xlim(c(sars_min_date,sars_max_date)) + scale_x_date(date_labels = "%b", limits=c(sars_min_date,sars_max_date)) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 5, 5, 5))
}

# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Date", "Week of 100th confirmed case", "Week of 10th death"), plot_start_date) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(plot_start_date,(current_date+5))) + xlab("Date")
  }
  
  if (start_point=="Week of 100th confirmed case") {
    cv_cases = subset(cv_cases, weeks_since_case100>0)
    g = ggplot(cv_cases, aes(x = weeks_since_case100, y = new_outcome, fill = region, group = 1,
                             text = paste0("Week ",weeks_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Weeks since 100th confirmed case") #+ xlim(c(plot_start_date,(current_date+5))) 
  }
  
  if (start_point=="Week of 10th death") {
    cv_cases = subset(cv_cases, weeks_since_death10>0)
    g = ggplot(cv_cases, aes(x = weeks_since_death10, y = new_outcome, fill = region, group = 1,
                             text = paste0("Week ",weeks_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Weeks since 10th death") #+ xlim(c(plot_start_date,(current_date+5))) 
  }
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("New (weekly)") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, start_point=c("Date", "Week of 100th confirmed case", "Week of 10th death"), plot_start_date) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
  }
  
  if (start_point=="Week of 100th confirmed case") {
    cv_cases = subset(cv_cases, weeks_since_case100>0)
    g = ggplot(cv_cases, aes(x = weeks_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Week ", weeks_since_case100,"\n", region, ": ",outcome))) +
      xlab("Weeks since 100th confirmed case")
  }
  
  if (start_point=="Week of 10th death") {
    cv_cases = subset(cv_cases, weeks_since_death10>0)
    g = ggplot(cv_cases, aes(x = weeks_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Week ", weeks_since_death10,"\n", region, ": ",outcome))) +
      xlab("Weeks since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Week of 100th confirmed case", "Week of 10th death"), plot_start_date)  {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(plot_start_date,(current_date+1))) + xlab("Date")
  }
  
  if (start_point=="Week of 100th confirmed case") {
    cv_cases = subset(cv_cases, weeks_since_case100>0)
    g = ggplot(cv_cases, aes(x = weeks_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Week ",weeks_since_case100, "\n", region, ": ",outcome))) +
      xlab("Weeks since 100th confirmed case")
  }
  
  if (start_point=="Week of 10th death") {
    cv_cases = subset(cv_cases, weeks_since_death10>0)
    g = ggplot(cv_cases, aes(x = weeks_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Week ",weeks_since_death10, "\n", region, ": ",outcome))) +
      xlab("Weeks since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("Cumulative (log10)") + theme_bw() +
    scale_y_continuous(trans="log10") +
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to render plotly of epidemic comparison depending on selected outcome
comparison_plot = function(epi_comp, comparison) {
  epi_comp$outcome = epi_comp[,comparison] 
  epi_comp = epi_comp[order(epi_comp$outcome),]
  epi_comp$outbreak = factor(epi_comp$outbreak, levels=epi_comp$outbreak)
  
  p1 <- ggplot(epi_comp, aes(x = outbreak, y = outcome, fill=outbreak, text = paste0(outbreak, ": ",outcome))) + geom_bar(alpha = 0.8, stat="identity") +
    ylab("N") + xlab("") + theme_bw() + 
    scale_fill_manual(values=c("2019-COVID"=covid_col, "2003-SARS"=sars_col, "2014-Ebola"=ebola_col,"2009-H1N1 (swine flu)"=h1n1_col)) +
    theme(legend.position = "")
  
 # if(comparison == "cfr") { p1 = p1 + ylab("%") }
  if(comparison == "deaths") { p1 = p1 + scale_y_continuous(trans='log10', limits = c(1,1e7), breaks=c(1,1000,1e6), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  if(comparison == "cases") { p1 = p1 + scale_y_continuous(trans='log10', limits = c(1,1e10), breaks=c(1,1000,1e6,1e9), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  ggplotly(p1 + coord_flip(), tooltip = c("text")) %>% layout(showlegend = FALSE)
}

### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$cases_per_million = as.numeric(format(round(cv_cases$cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$new_cases_per_million = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)
cv_cases$deaths_per_million = as.numeric(format(round(cv_cases$deaths/(cv_cases$population/1000000),1),nsmall=1))
cv_cases$new_deaths_per_million = as.numeric(format(round(cv_cases$new_deaths/(cv_cases$population/1000000),1),nsmall=1))

# add variable for weeks since 100th case and 10th death
cv_cases$weeks_since_case100 = cv_cases$weeks_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$weeks_since_case100[country_db$cases>=100] = 0:(sum(country_db$cases>=100)-1)
  country_db$weeks_since_death10[country_db$deaths>=10] = 0:(sum(country_db$deaths>=10)-1)
  cv_cases$weeks_since_case100[cv_cases$country==country_name] = country_db$weeks_since_case100
  cv_cases$weeks_since_death10[cv_cases$country==country_name] = country_db$weeks_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset of state data for today's data
if (any(grepl("/", cv_states$date))) { 
  cv_states$date = format(as.Date(cv_states$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_states$date = as.Date(cv_states$date, format="%Y-%m-%d") }
cv_states_today = subset(cv_states, date==max(cv_states$date))

# create subset for countries with at least 1000 cases
cv_today_reduced = subset(cv_today, cases>=1000)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                cases_per_million, new_cases_per_million,
                                deaths_per_million, new_deaths_per_million,
                                weeks_since_case100, weeks_since_death10)), "input_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for weeks since 100th case and 10th death
cv_cases_continent$weeks_since_case100 = cv_cases_continent$weeks_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
  continent_name = as.character(unique(cv_cases_continent$continent))[i]
  continent_db = subset(cv_cases_continent, continent==continent_name)
  continent_db$weeks_since_case100[continent_db$cases>=100] = 0:(sum(continent_db$cases>=100)-1)
  continent_db$weeks_since_death10[continent_db$deaths>=10] = 0:(sum(continent_db$deaths>=10)-1)
  cv_cases_continent$weeks_since_case100[cv_cases_continent$continent==continent_name] = continent_db$weeks_since_case100
  cv_cases_continent$weeks_since_death10[cv_cases_continent$continent==continent_name] = continent_db$weeks_since_death10
}

# add continent populations
cv_cases_continent$pop = NA
cv_cases_continent$pop[cv_cases_continent$continent=="Africa"] = 1.2e9
cv_cases_continent$pop[cv_cases_continent$continent=="Asia"] = 4.5e9
cv_cases_continent$pop[cv_cases_continent$continent=="Europe"] = 7.4e8
cv_cases_continent$pop[cv_cases_continent$continent=="North America"] = 5.8e8
cv_cases_continent$pop[cv_cases_continent$continent=="Oceania"] = 3.8e7
cv_cases_continent$pop[cv_cases_continent$continent=="South America"] = 4.2e8

# add normalised counts
cv_cases_continent$cases_per_million =  as.numeric(format(round(cv_cases_continent$cases/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$new_cases_per_million =  as.numeric(format(round(cv_cases_continent$new_cases/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$deaths_per_million =  as.numeric(format(round(cv_cases_continent$deaths/(cv_cases_continent$pop/1000000),1),nsmall=1))
cv_cases_continent$new_deaths_per_million =  as.numeric(format(round(cv_cases_continent$new_deaths/(cv_cases_continent$pop/1000000),1),nsmall=1))
write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$weeks_since_case100 = cv_cases_global$weeks_since_death10 = 0:(nrow(cv_cases_global)-1)

# add normalised counts
cv_cases_global$pop = 7.6e9
cv_cases_global$cases_per_million =  as.numeric(format(round(cv_cases_global$cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$new_cases_per_million =  as.numeric(format(round(cv_cases_global$new_cases/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$deaths_per_million =  as.numeric(format(round(cv_cases_global$deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
cv_cases_global$new_deaths_per_million =  as.numeric(format(round(cv_cases_global$new_deaths/(cv_cases_global$pop/1000000),1),nsmall=1))
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$ADM0_A3)
if (all(cv_large_countries$alpha3 %in% worldcountry$ADM0_A3)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,10,50,100,500,1000,Inf)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$cases_per_million, bins = bins)
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% cv_large_countries$alpha3, ]

# creat cv base map 
# basemap = leaflet(plot_map) %>% 
#   addTiles() %>% 
#   addLayersControl(
#     position = "bottomright",
#     overlayGroups = c("2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
#     options = layersControlOptions(collapsed = FALSE)) %>% 
#   hideGroup(c("2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola")) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   fitBounds(~-100,-60,~60,70) %>%
#   addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$deaths_per_million,
#             title = "<small>Deaths per million</small>") 

basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (new)", "2019-COVID (cumulative)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (cumulative)")) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-60,~60,70) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$deaths_per_million,
            title = "<small>Deaths per million</small>") 
# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 7 days
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),4)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)), as.character(unique(cv_states$state)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names

# Shiny UI
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 tracker</a>'), id="nav",
             windowTitle = "COVID-19 tracker",
             
             tabPanel("COVID-19 World Mapper",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Reported cases are subject to significant variation in testing policy and capacity between countries.")), style="color:#045a8d"),
                                        h3(textOutput("reactive_case_count"), align = "right"),
                                        h4(textOutput("reactive_death_count"), align = "right"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        h6(textOutput("reactive_country_count"), align = "right"),
                                        plotOutput("epi_curve", height="130px", width="100%"),
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        sliderTextInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        choices = format(unique(cv_cases$date), "%d %b %y"),
                                                        selected = format(current_date, "%d %b %y"),
                                                        grid = FALSE,
                                                        animate=animationOptions(interval = 3000, loop = FALSE))
                                        
                          ),
                          
                          # absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                          #               tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                          
                          # absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                          #               actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                          #                            onclick = sprintf("window.open('%s')", 
                          #                                              "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='http://covid19datasource.com/', tags$img(src='usclogo.png',height='60',width='180')))
                          
                          
                      )
             )
             
  )          
)

### SHINY SERVER ###

server = function(input, output, session) {
  
  # covid tab 
  formatted_date = reactive({
    format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(formatted_date()),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == formatted_date())
  })
  
  reactive_db_last7d = reactive({
    cv_cases %>% filter(date == formatted_date() & new_cases>0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    #large_countries = reactive %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    worldcountry_subset = worldcountry[worldcountry$ADM0_A3 %in% large_countries$alpha3, ]
    large_countries = large_countries[match(worldcountry_subset$ADM0_A3, large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last7d = reactive({
    large_countries = reactive_db_last7d() %>% filter(alpha3 %in% worldcountry$ADM0_A3)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large()$alpha3, ]
  })
  
  reactive_polygons_last7d = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive_db_large_last7d()$alpha3, ]
  })
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$deaths), big.mark=","), " deaths")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
  })
  
  output$reactive_new_cases_7d <- renderText({
    paste0(round((cv_aggregated %>% filter(date == formatted_date() & region=="Global"))$new/7,0), " 7-day average")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5.5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                       label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths, reactive_db()$cases_per_million, reactive_db()$deaths_per_million) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) %>%  
      
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$deaths_per_million)) %>%
      
      addCircleMarkers(data = reactive_db_last7d(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5.5), 
                       fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                       label = sprintf("<strong>%s (7-day average)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per million: %g<br/>Deaths per million: %g", reactive_db_last7d()$country, round(reactive_db_last7d()$new_cases/7,0), round(reactive_db_last7d()$new_deaths/7,0), round(reactive_db_last7d()$new_cases_per_million/7,1), round(reactive_db_last7d()$new_deaths_per_million/7,1)) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "15px", direction = "auto")) 
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(cv_aggregated, formatted_date())
  })
  
  output$epi_curve <- renderPlot({
    new_cases_plot(cv_aggregated, formatted_date())
  })
  
  }


shinyApp(ui, server)


