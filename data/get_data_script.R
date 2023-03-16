Competitions <- StatsBombR::FreeCompetitions()

usethis::use_data(Competitions, overwrite = T)

AllMatches <- StatsBombR::FreeMatches(StatsBombR::FreeCompetitions())

usethis::use_data(AllMatches, overwrite = T)
