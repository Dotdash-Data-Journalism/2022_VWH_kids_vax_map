# Verywell Health Kids COVID-19 Vaccination Maps

This repository contains an [R](https://www.r-project.org/) script 
`verywell_health_kids_covid_vax_map.R` that is run via a [GitHub Action](https://docs.github.com/en/actions)
every day at approximated 3:03am UTC. The script fetches the latest 
[COVID-19 vaccination](https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdi/unsk-b7fc) 
data by US state from the CDC. It then creates a CSV `kids_covid_19_vax_map.csv` in the `visualizations` folder of the
latest day's vaccination rate for children 5 - 11 years old, children 12 - 17 years old and children under 18 years 
old for each US state & Washington DC. The CSV is then sent via [API](https://developer.datawrapper.de/reference/introduction) 
to update three [Datawrapper](https://www.datawrapper.de/) chloropleth maps of the COVID-19 vaccination rates by state for
 [Verywell health](https://www.verywellhealth.com/).




