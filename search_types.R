

phases = c(
  "Offense",
  "Defense",
  "Special Teams"
)


teams = tibble(
  "name" = c(
    "Patriots", "Jets", "Dolphins", "Bills",
    "Browns", "Steelers", "Ravens", "Bengals",
    "Titans", "Texans", "Colts", "Jaguars",
    "Chiefs", "Chargers", "Broncos", "Raiders",
    "Giants", "Eagles", "Redskins", "Cowboys",
    "Vikings", "Bears", "Packers", "Lions",
    "Saints", "Falcons", "Buccaneers", "Panthers",
    "Seahawks", "Rams", "Cardinals", "49ers"
  ),
  "abbr" = c(
    "NWE", "NYJ", "MIA", "BUF",
    "CLE", "PIT", "BAL", "CIN",
    "TEN", "HOU", "IND", "JAX",
    "KC" , "LAC", "DEN", "LVR",
    "NYG", "PHI", "WAS", "DAL",
    "MIN", "CHI", "GB" , "DET",
    "NO" , "ATL", "TB" , "CAR",
    "SEA", "LAR", "ARZ", "SF"
  ),
  "conf" = c(
    "AFC","AFC","AFC","AFC",
    "AFC","AFC","AFC","AFC",
    "AFC","AFC","AFC","AFC",
    "AFC","AFC","AFC","AFC",
    "NFC","NFC","NFC","NFC",
    "NFC","NFC","NFC","NFC",
    "NFC","NFC","NFC","NFC",
    "NFC","NFC","NFC","NFC"
  ),
  "div" = c(
    "East","East","East","East",
    "North","North","North","North",
    "South","South","South","South",
    "West","West","West","West",
    "East","East","East","East",
    "North","North","North","North",
    "South","South","South","South",
    "West","West","West","West"
  )
)