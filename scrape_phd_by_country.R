library(rvest)


url <- 'https://en.wikipedia.org/wiki/List_of_countries_by_number_of_doctorates_awarded'
webpage <- read_html(url)
#Using CSS selectors to scrape the rankings section
country_html <- html_nodes(webpage,'#mw-content-text a , .headerSort:nth-child(1)')
#Converting the ranking data to text
Entity <- html_text(country_html)
#clear clutter
Entity <- Entity[8:22]
Entity <- gsub("\\*", "", Entity)
Entity <- gsub(" ", "_", Entity)
Entity <- gsub("Great_Britain", "United_Kingdom", Entity)




phd_awards_html <- html_nodes(webpage, 'td+ td')
phd_awards <- html_text(phd_awards_html)
# fix names
phd_awards <- phd_awards[-1]
phd_awards <- gsub("\n", "", x = phd_awards)
phd_awards <- gsub(",", "", x = phd_awards)
phd_awards <- as.numeric(phd_awards)

phd_by_country <- data.frame(Entity, phd_awards)

write.csv(phd_by_country, "cvss/phd_awards.csv")

