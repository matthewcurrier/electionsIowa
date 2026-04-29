library(purrr)
library(here)
library(fs)
library(httr)

BASE_URL <- "https://sos.iowa.gov/elections/pdf/precinctresults/2020general/"
OUT_DIR <- here("inst", "data", "2020", "general")

# Names = local save key, values = exact URL slug from the SOS page
# Scott is a PDF and has no xlsx — excluded with a note
counties <- c(
  adair = "adair",
  adams = "adams",
  allamakee = "allamakee",
  appanoose = "appanoose",
  audubon = "audubon",
  benton = "benton",
  blackhawk = "black%20hawk",
  boone = "boone",
  bremer = "bremer",
  buchanan = "buchanan",
  buenavista = "buena%20vista",
  butler = "butler",
  calhoun = "calhoun",
  carroll = "carroll",
  cass = "cass",
  cedar = "cedar",
  cerrogordo = "cerro%20gordo",
  cherokee = "cherokee",
  chickasaw = "chickasaw",
  clarke = "clarke",
  clay = "clay",
  clayton = "clayton",
  clinton = "clinton",
  crawford = "crawford",
  dallas = "dallas",
  davis = "davis",
  decatur = "decatur",
  delaware = "delaware",
  desmoines = "des%20moines",
  dickinson = "dickinson",
  dubuque = "dubuque",
  emmet = "emmet",
  fayette = "fayette",
  floyd = "floyd",
  franklin = "franklin",
  fremont = "fremont",
  greene = "greene",
  grundy = "grundy",
  guthrie = "guthrie",
  hamilton = "hamilton",
  hancock = "hancock",
  hardin = "hardin",
  harrison = "harrison",
  henry = "henry",
  howard = "howard",
  humboldt = "humboldt",
  ida = "ida",
  iowa = "iowa",
  jackson = "jackson",
  jasper = "jasper",
  jefferson = "jefferson",
  johnson = "johnson",
  jones = "jones",
  keokuk = "keokuk",
  kossuth = "kossuth",
  lee = "lee",
  linn = "linn",
  louisa = "louisa",
  lucas = "lucas",
  lyon = "lyon",
  madison = "madison",
  mahaska = "mahaska",
  marion = "marion",
  marshall = "marshall",
  mills = "mills",
  mitchell = "mitchell",
  monona = "monona",
  monroe = "monroe",
  montgomery = "montgomery",
  muscatine = "muscatine",
  obrien = "o'brien",
  osceola = "osceola",
  page = "page",
  paloalto = "palo%20alto",
  plymouth = "plymouth",
  pocahontas = "pocahontas",
  polk = "polk",
  pottawattamie = "pottawattamie",
  poweshiek = "poweshiek",
  ringgold = "ringgold",
  sac = "sac",
  # scott omitted — SOS only has a PDF for 2020 general, no xlsx
  shelby = "shelby",
  sioux = "sioux",
  story = "story",
  tama = "tama",
  taylor = "taylor",
  union = "union",
  vanburen = "van%20buren",
  wapello = "wapello",
  warren = "warren",
  washington = "washington",
  wayne = "wayne",
  webster = "webster",
  winnebago = "winnebago",
  winneshiek = "winneshiek",
  woodbury = "woodbury",
  worth = "worth",
  wright = "wright"
)

message(
  "Note: Scott County excluded — SOS only published a PDF for 2020 general."
)

dir_create(OUT_DIR)

results <- imap(counties, function(slug, key) {
  url <- paste0(BASE_URL, slug, ".xlsx")
  out_path <- path(OUT_DIR, paste0(key, ".xlsx"))

  tryCatch(
    {
      resp <- GET(
        url,
        user_agent(
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
        ),
        write_disk(out_path, overwrite = TRUE),
        timeout(60)
      )

      if (http_error(resp)) {
        message("FAIL: ", key, " — HTTP ", status_code(resp))
        list(county = key, status = "failed", code = status_code(resp))
      } else {
        message("OK:   ", key)
        list(county = key, status = "ok", code = status_code(resp))
      }
    },
    error = function(e) {
      message("FAIL: ", key, " — ", conditionMessage(e))
      list(county = key, status = "failed", code = NA)
    }
  )
})

statuses <- map_chr(results, "status")
failed <- names(counties)[statuses == "failed"]

message(
  "\n",
  sum(statuses == "ok"),
  " downloaded, ",
  length(failed),
  " failed."
)

if (length(failed) > 0) {
  message("Failed counties: ", paste(failed, collapse = ", "))
}
