rm(list = ls()) 

library(leaflet)
library(sf)
library(dplyr)
library(htmlwidgets)

USA_shape <- st_read("1.0-shapefile-codebook/usa/usa.shp")

cejst_data <- read.csv("1.0-communities.csv")

#california only for community data
ca_cejst_data <- cejst_data %>%
  filter(State.Territory == 'California') %>%
  mutate(`Census.tract.2010.ID` = as.character(`Census.tract.2010.ID`))

#convert all variable names with greater.than... which are the basic classifiers for disadvanted communities to logicals
ca_cejst_data <- ca_cejst_data %>%
  mutate_at(vars(contains('Greater.than.or.equal.to.the.90th')), as.logical)

#need to convert to logicals first
# ca_cejst_data <- ca_cejst_data %>%
#   mutate(Climate_Change = case_when(
#     Greater.than.or.equal.to.the.90th.percentile.for.expected.population.loss.rate.and.is.low.income. |
#       Greater.than.or.equal.to.the.90th.percentile.for.share.of.properties.at.risk.of.flood.in.30.years.and.is.low.income. |
#       Greater.than.or.equal.to.the.90th.percentile.for.share.of.properties.at.risk.of.fire.in.30.years.and.is.low.income. |
#       Greater.than.or.equal.to.the.90th.percentile.for.expected.agriculture.loss.rate.and.is.low.income. |
#       Greater.than.or.equal.to.the.90th.percentile.for.expected.building.loss.rate.and.is.low.income. == TRUE, TRUE, FALSE),
#     
#     Energy = case_when(
#       Greater.than.or.equal.to.the.90th.percentile.for.PM2.5.exposure.and.is.low.income. |
#         Greater.than.or.equal.to.the.90th.percentile.for.energy.burden.and.is.low.income. |
#         Greater.than.or.equal.to.the.90th.percentile.for.diesel.particulate.matter.and.is.low.income. == TRUE, TRUE, FALSE),
#     
#     Transportation = case_when(
#       Greater.than.or.equal.to.the.90th.percentile.for.traffic.proximity.and.is.low.income. |
#         Greater.than.or.equal.to.the.90th.percentile.for.DOT.transit.barriers.and.is.low.income. == TRUE, TRUE, FALSE),
#     
#     Housing = case_when(
#       Greater.than.or.equal.to.the.90th.percentile.for.lead.paint..the.median.house.value.is.less.than.90th.percentile.and.is.low.income. |
#         Greater.than.or.equal.to.the.90th.percentile.for.share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent.and.is.low.income. == TRUE, TRUE, FALSE),
#     
#     Legacy_Pollution = case_when(
#       Greater.than.or.equal.to.the.90th.percentile.for.proximity.to.hazardous.waste.facilities.and.is.low.income. |
#         Greater.than.or.equal.to.the.90th.percentile.for.proximity.to.superfund.sites.and.is.low.income. |
#         Greater.than.or.equal.to.the.90th.percentile.for.proximity.to.RMP.sites.and.is.low.income. == TRUE, TRUE, FALSE),
#     
#     Health = case_when(
#       Greater.than.or.equal.to.the.90th.percentile.for.asthma.and.is.low.income. |
#         Greater.than.or.equal.to.the.90th.percentile.for.diabetes.and.is.low.income. |
#         Greater.than.or.equal.to.the.90th.percentile.for.heart.disease.and.is.low.income. |
#         Greater.than.or.equal.to.the.90th.percentile.for.low.life.expectancy.and.is.low.income. == TRUE, TRUE, FALSE),
#     
#     Workforce_Development = case_when(
#       Greater.than.or.equal.to.the.90th.percentile.for.low.median.household.income.as.a.percent.of.area.median.income.and.has.low.HS.attainment. == TRUE, TRUE, FALSE)
#   )
# 
# ca_cejst_data <- ca_cejst_data %>%
#   mutate(
#     Climate_Change = case_when(
#       (Greater.than.or.equal.to.the.90th.percentile.for.expected.population.loss.rate.and.is.low.income.) |
#         (Greater.than.or.equal.to.the.90th.percentile.for.share.of.properties.at.risk.of.flood.in.30.years.and.is.low.income.) |
#         (Greater.than.or.equal.to.the.90th.percentile.for.share.of.properties.at.risk.of.fire.in.30.years.and.is.low.income.) |
#         (Greater.than.or.equal.to.the.90th.percentile.for.expected.agriculture.loss.rate.and.is.low.income.) |
#         (Greater.than.or.equal.to.the.90th.percentile.for.expected.building.loss.rate.and.is.low.income.), TRUE, FALSE)
#   )
# 
# ca_cejst_data <- ca_cejst_data %>%
#   mutate(
#     Energy = case_when(
#       (
#         Greater.than.or.equal.to.the.90th.percentile.for.PM2.5.exposure.and.is.low.income. == TRUE |
#           Greater.than.or.equal.to.the.90th.percentile.for.energy.burden.and.is.low.income. == TRUE |
#           Greater.than.or.equal.to.the.90th.percentile.for.diesel.particulate.matter.and.is.low.income.== TRUE),
#       TRUE, FALSE)
#   )

#california only for shape file
ca_shape <- USA_shape %>%
  filter(SF == 'California') %>%
  rename('Census.tract.2010.ID' = 'GEOID10')

#adjust census tract so that it matches the same values exclusing the first zero
ca_shape <- ca_shape %>%
  mutate(`Census.tract.2010.ID` = substr(`Census.tract.2010.ID`, 2, 11))

#join shape file and community information, adjust logicals
ca_community_data <- left_join(ca_cejst_data, ca_shape, by = 'Census.tract.2010.ID') %>%
  mutate(`Identified.as.disadvantaged` = as.logical(`Identified.as.disadvantaged`))

#switch from data frame to shape frame containing geometric data
ca_community_data_sf <- st_as_sf(ca_community_data)

# ca_cejst_data <- ca_cejst_data %>%
#   mutate(disadvantage_variable = case_when(
#     Greater.than.or.equal.to.the.90th.percentile.for.expected.population.loss.rate.and.is.low.income. == TRUE ~ "Climate Change",
#     Greater.than.or.equal.to.the.90th.percentile.for.share.of.properties.at.risk.of.flood.in.30.years.and.is.low.income. == TRUE ~ "Climate Change",
#     Greater.than.or.equal.to.the.90th.percentile.for.share.of.properties.at.risk.of.fire.in.30.years.and.is.low.income. == TRUE ~ "Climate Change",
#     Greater.than.or.equal.to.the.90th.percentile.for.expected.agriculture.loss.rate.and.is.low.income. == TRUE ~ 'Climate Change',
#     Greater.than.or.equal.to.the.90th.percentile.for.expected.building.loss.rate.and.is.low.income. == TRUE ~ 'Climate Change',
#     Greater.than.or.equal.to.the.90th.percentile.for.PM2.5.exposure.and.is.low.income. == TRUE ~ "Energy",
#     Greater.than.or.equal.to.the.90th.percentile.for.energy.burden.and.is.low.income. == TRUE ~ "Energy",
#     Greater.than.or.equal.to.the.90th.percentile.for.diesel.particulate.matter.and.is.low.income. == TRUE ~ "Transportation",
#     Greater.than.or.equal.to.the.90th.percentile.for.traffic.proximity.and.is.low.income. == TRUE ~ "Transportation",
#     Greater.than.or.equal.to.the.90th.percentile.for.DOT.transit.barriers.and.is.low.income. == TRUE ~ "Transportation",
#     Greater.than.or.equal.to.the.90th.percentile.for.lead.paint..the.median.house.value.is.less.than.90th.percentile.and.is.low.income. == TRUE ~ "Housing",
#     Greater.than.or.equal.to.the.90th.percentile.for.share.of.the.tract.s.land.area.that.is.covered.by.impervious.surface.or.cropland.as.a.percent.and.is.low.income. == TRUE ~ "Housing",
#     Greater.than.or.equal.to.the.90th.percentile.for.proximity.to.hazardous.waste.facilities.and.is.low.income. == TRUE ~ "Legacy Pollution",
#     Greater.than.or.equal.to.the.90th.percentile.for.proximity.to.superfund.sites.and.is.low.income. == TRUE ~ "Legacy Pollution",
#     Greater.than.or.equal.to.the.90th.percentile.for.proximity.to.RMP.sites.and.is.low.income. == TRUE ~ "Legacy Pollution",
#     Greater.than.or.equal.to.the.90th.percentile.for.asthma.and.is.low.income. == TRUE ~ "Health",
#     Greater.than.or.equal.to.the.90th.percentile.for.diabetes.and.is.low.income. == TRUE ~ "Health",
#     Greater.than.or.equal.to.the.90th.percentile.for.heart.disease.and.is.low.income. == TRUE ~ "Health",
#     Greater.than.or.equal.to.the.90th.percentile.for.low.life.expectancy.and.is.low.income. == TRUE ~ "Health",
#     Greater.than.or.equal.to.the.90th.percentile.for.low.median.household.income.as.a.percent.of.area.median.income.and.has.low.HS.attainment. == TRUE ~ "Workforce Development"
#   )
#   )


#setting up color palate
pal <- colorNumeric("Reds", domain = ca_community_data$Identified.as.disadvantaged)

# Create leaflet map
map <- leaflet(data = ca_community_data_sf) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(Identified.as.disadvantaged),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste("Disadvantaged: ", ifelse(Identified.as.disadvantaged, "True", "False"))
  )

