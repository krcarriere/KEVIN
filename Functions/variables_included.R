variable_list <- c(
  # identity
"VoterId", "FirstName", "LastName", "Name",
"FirstNameUpper", "LastNameUpper", "RowID",
# address / geo
"Precinct", "Street",
"StreetName", "StreetNameUpper",
"StreetSuffix", "StreetSuffixUpper",
"StreetNum",
# voter attributes the UI displays
"Age", "Party", "VoterScore", "VoterStatus", "MayHaveMoved",
"NSSE2026Voter", "nsse_2026_voter",
# routing
"Route", "RouteName", "StopOrder", "Scenario",
# canvassing context produced upstream
"WhoKnows", "Additional", "DonateText", "TimesSpoken", "Gender",
# voting history — used when filter_history is on
"Primary2020", "General2020", 
"Primary2022", "General2022",
"Primary2024", "General2024",
# computed inside voter_data()
"LatestSupport",
"OnTargetList", "DisplayAddress"
)