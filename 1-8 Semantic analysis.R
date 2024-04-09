library(xml2)

# First, load all semcor files which match the verb type
semcor_path <- "data/dutsemcor/*.v.xml"
semcor <- data.frame(path = Sys.glob(semcor_path, dirmark = FALSE))
# Then, extract the verb lemma from the filename
semcor$lemma <- gsub(".*/(.*).v.xml", "\\1", semcor$path, ignore.case=T)

# Read the elastic net output, with lemmas
df <- read.csv("output/RoodGroenAnthe_coefficients_infused.csv")

# Only keep lemmas which have a DutchSemCor entry
semcor <- merge(x=df, y=semcor, by="lemma")

# Yes, parsing XML with regex is bad
# But the XML files are very simple, and some DutchSemCor files are HUGE
# so: this is a memory safe solution. don't @ me
get_senses_memory_safe <- function(filepath) {
  senses <- c()
  con = file(filepath, "r")
  while (TRUE) {
    line = readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) {
      break
    }
    sense <- gsub(".*sense=\"(.*?)\".*", "\\1", line)
    if (substring(sense, 1, 1) == "<") {
      next
    } else {
      senses <- append(senses, sense)
    }
  }
  
  close(con)
  
  return(senses)
}

semcor$path %>% lapply(function(path) {
  get_senses_memory_safe(path) %>% table %>% sort %>% names %>% first
})

read_xml(semcor$path) %>% xml_find_all("/tokens/token") %>% xml_attr("sense")

employee_data <- read_xml("data/dutsemcor/afspelen.v.xml")
xml_find_all(employee_data, "/tokens/token") %>% xml_attr("sense") %>% table
