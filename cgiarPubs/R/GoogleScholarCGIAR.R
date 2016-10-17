#####################################################################################
# Title: Scrape CGIAR authors from Google Scholar
# Date: December 2015
# Project:  HarvestChoice, CRP Mapping
# Author: Bacou, Melanie <mel@mbacou.com>
#####################################################################################

library(data.table)
library(rvest)
library(stringr)
library(igraph)
library(networkD3)
memory.limit(6000)

# Aim is to create a dynamic network visualization of CGIAR authors and co-authors
# show citation details and sub-communities (key actors and links) with relationship 
# strenghts representing number of co-authored papers (?)
# GS does not yet provide an API yet (no official Google product).
# Found 2 general-purpose scraping libraries `rvest` and `scrapeR` 
# scrapeR uses straight xpath, rvest supports %>% piping.
# Inspiration at http://datascienceplus.com/google-scholar-scraping-with-rvest/
# `networkD3` and `igraph` may be used to generate dynamic network graphs with
# tutorial at http://kateto.net/network-visualization
# `igraph` needs 2 data.frames:
# links - describes the edges of the network. Its first two columns are the IDs of the 
# source and the target node for each edge. The following columns are edge attributes 
# (weight, type, label, or anything else).
# nodes - starts with a column of node IDs. Any following columns are interpreted 
# as node attributes.

setwd("~/Dropbox (IFPRI)/SDA/Projects/2015-CRP-Mapping")

# Load data snapshot
load("./temp/GoogleScholarCGIAR.RData")

# Get GS IDs and attributes of CGIAR authors over 2000-2015 period
# Note there are 
gs.url <- "https://scholar.google.com/citations?view_op=view_org&hl=en&org=5225706230162989193&before_author="
gs.url.author <- "https://scholar.google.com/citations?hl=en&user="
gs.url.colleagues <- "https://scholar.google.com/citations?view_op=list_colleagues&hl=en&user="
gs.top.author <- "WiY9Ku0AAAAJ"

# Get IDs and attributes of all CGIAR co-authors
page <- read_html(paste0(gs.url, gs.last.author))
cg.author <- data.table()

scrape.org <- function(id) {
  page <- read_html(paste0(gs.url, id))
  cg.author <- page %>% html_nodes(css=".gsc_1usr_name") %>% html_text()
  cg.id <-  page %>% html_nodes(xpath="//h3[@class='gsc_1usr_name']/a/@href") %>% html_text()
  cg.id <- str_replace(str_replace(cg.id, fixed("/citations?user="), ""), "&hl=en&oe=ASCII", "")
  cg.cited <- page %>% html_nodes(css=".gsc_1usr_cby") %>% html_text()
  cg.cited <- as.integer(str_replace(cg.cited, "Cited by ", ""))
  cg.title <- page %>% html_nodes(css=".gsc_1usr_aff") %>% html_text()
  cg.key <- page %>% html_nodes(css=".gsc_1usr_int")
  cg.key <- lapply(cg.key, html_nodes, css="a")
  cg.key <- lapply(cg.key, html_text)
  tmp <- data.table(id=cg.id, fullName=cg.author, cited=cg.cited, title=cg.title, keywords=cg.key)
  return(tmp)
}

scrape.author <- function(id) {
  page <- read_html(paste0(gs.url.author, id))
  cg.author <- page %>% html_node(css="#gsc_prf_in") %>% html_text()
  cg.cited <- page %>% html_node(css=".gsc_rsb_std") %>% html_text()
  cg.cited <- as.integer(cg.cited)
  cg.title <- page %>% html_node(css=".gsc_prf_il") %>% html_text()
  cg.key <- page %>% html_nodes(css=".gsc_prf_ila")
  cg.key <- sapply(cg.key, html_text)  
  cg.cgiar <- page %>% html_node(css="#gsc_prf_ivh") %>% html_text()
  cg.cgiar <- str_detect(cg.cgiar, "cgiar.org")
  tmp <- data.table(id=id, fullName=cg.author, cited=cg.cited, 
    title=cg.title, cgiar=cg.cgiar)
  tmp[, keywords := paste(tolower(cg.key), collapse="; ")]
  return(tmp)
}

scrape.coauthors <- function(id) {
  page <- read_html(paste0(gs.url.colleagues, id))
  cg.author <- page %>% html_nodes(css=".gsc_1usr_name") %>% html_text()
  cg.id <-  page %>% html_nodes(xpath="//h3[@class='gsc_1usr_name']/a/@href") %>% html_text()
  cg.id <- str_replace(str_replace(cg.id, fixed("/citations?user="), ""), "&hl=en&oe=ASCII", "")
  if (length(cg.id)>0) tmp <- data.table(id=id, ids=cg.id, coFullName=cg.author)
  else tmp <- data.table()
  return(tmp)
}

# Start scraping from top author down through CGIAR co-authors
#cg.author <- data.table()
#cg.coauthor <- data.table()
top.id <- scrape.coauthors(gs.top.author)$ids

for (i in 1:30) {
  j <- 1
  while (top.id[j] %in% cg.author$id) {
    # Don't pull existing authors
    if (j < length(top.id))  j <- j+1 else break
  }
  if (top.id[j] %in% cg.author$id) {
    top.id <<- cg.coauthor[!ids %in% cg.author$id, ids]
    j <- 1
  }
  cat(i, ":", top.id[j], "\n")
  tmp <- scrape.author(top.id[j])
  tmp1 <- scrape.coauthors(top.id[j])
  cg.coauthor <- rbind(cg.coauthor, tmp1, fill=T)
  setkey(cg.coauthor, id, ids)
  cg.coauthor <- unique(cg.coauthor)  
  tmp[, coauthors := nrow(tmp1)]
  cg.author <- rbind(cg.author, tmp, fill=T)
  setkey(cg.author, id)
  cg.author <- unique(cg.author)
  if (nrow(tmp1)>0) {
    # Rank co-authors
    tmp2 <- lapply(tmp1$ids, scrape.author)
    tmp2 <- rbindlist(tmp2)
    tmp2 <- tmp2[order(-cited)]
    # Process CGIAR co-authors first
    top.id <<- tmp2[cgiar==T, id]
    if (length(top.id)==0) top.id <<- tmp2$id
  }
}

# Remove duplicate authors
setkey(cg.author, id)
cg.author <- unique(cg.author)

# Clean list of keywords
cg.keywords <- paste(cg.author$keywords, collapse="; ")
cg.keywords <- unlist(str_split(cg.keywords, "; "))
cg.keywords <- unique(cg.keywords)
cg.keywords <- cg.keywords[!cg.keywords %in% c("", " ", NA)]


## Network graph
nodes <- cg.author
nodes[, ID := 0:(.N-1)]
nodes[, size := scale(cited, center=F)*4]
nodes[, group := factor(cgiar, labels=c("CGIAR", "Partners"))]
links <- cg.coauthor[id %in% nodes$id]
links <- links[ids %in% nodes$id]
setkey(links, id)
links$from <- nodes[links][, ID]
setkey(links, ids)
links$to <- nodes[links][, ID]
links[, value := 3]
setcolorder(nodes, c(8:10, 1:7))
setcolorder(links, c(4,5,6,1:3))
net <- graph.data.frame(links, nodes, directed=F)

# Network plots
plot(net, 
  edge.arrow.size=.2, 
  edge.color="orange",
  vertex.color=ifelse(V(net)$cgiar, "green", "orange"), 
  vertex.label.color="black",
  vertex.frame.color="white",
  vertex.size=log10(V(net)$cited),
  vertex.label=V(net)$fullName,
  vertex.label.cex=.5)

simpleNetwork(links, fontSize=8, zoom=T,
  Source="id", Target="fullName",
  linkColour="#666",
  #nodeColour=ifelse(V(gn)$cgiar, "green", "#3182bd"),
  #textColour=ifelse(V(gn)$cgiar, "gray50", "gray80"),
  nodeClickColour="#E34A33")

forceNetwork(links, nodes,
  Source="from", Target="to",
  NodeID="fullName", Nodesize="size",
  linkColour="#666", 
  fontSize=12, fontFamily="arial",
  Group="group", 
  colourScale=JS("d3.scale.category10()"),
  zoom=T)

# We also need to consolidate the list of organizations and keywords using the latest
# CGIAR ontologies.



#####################################################################################
# 2016.01.28 Find HarvestChoice citations in Google Scholar
#####################################################################################
# https://scholar.google.com/scholar?start=40&q=%22HarvestChoice%22+OR+%22Spatial+Production+Allocation+Model%22&hl=en&as_sdt=1,9

gs.url <- 'https://scholar.google.com/scholar?as_vis=1&q="HarvestChoice"+OR+"Spatial+Production+Allocation+Model"&hl=en&as_sdt=1,9&start='

scrape.hits <- function(url, start) {
  page <- read_html(paste0(gs.url, start), encoding="UTF-8")
  gs.title <- page %>% html_nodes(css="h3.gs_rt a") %>% html_text(trim=T)
  if (length(gs.title)>0) {
    cat("Page ", start, " : ", length(title), "\n")
    gs.href <- page %>% html_nodes(css="h3.gs_rt a") %>% html_attr("href")
    gs.author <- page %>% html_nodes(css=".gs_a") %>% html_text(trim=T)
    gs <- page %>% html_nodes(css=".gs_r")
    gs.cited <- lapply(gs, function(x) {
        x <- html_nodes(x, css=".gs_fl a") %>% html_text(trim=T)
        x <- ifelse(length(x[x %like% "Cited by"])>0, x, "0")
        x <- ifelse(x!="0", gsub("Cited by ", "", x, fixed=T), x)
      })
    gs.cited <- as.integer(unlist(gs.cited))
    gs.source <- lapply(gs, function(x) {
        x <- html_nodes(x, css=".gs_ggsS") %>% html_text(trim=T)
        x <- ifelse(length(x)>0, x, NA)
      })
    gs.source <- unlist(gs.source)
    tmp <- data.table(
      id=start:(start+9),
      title=gs.title, 
      authors=gs.author, 
      cited=gs.cited, 
      source=gs.source,
      href=gs.href)
  } else {
    cat("Page ", start, " : ", "no hit\n")
    tmp <- data.table()
  }
  return(tmp)
}

# Retrieve all hits for HarvestChoice
hc.title <- data.table()
for (i in 0:43) {
  tmp <- scrape.hits(gs.url, (i*10)+1)
  hc.title <- rbind(hc.title, tmp)
}

hc.title[is.na(cited), cited := 0]
hc.title[, title := toTitleCase(tolower(title))]
setorder(hc.title, -cited)
write.csv(hc.title, "./out/GoogleScholarHC.2016.01.29.csv", na="", row.names=F)

# Clean in XLS and reload
hc.title <- fread("./out/GoogleScholarHC.2016.01.29.csv")

# Extract date/year
hc.title[, year := str_locate(authors, fixed("20"))[,1]]
hc.title[, tmp := str_locate(authors, fixed("19"))[,1]]
hc.title[is.na(year), year := tmp]
hc.title[, tmp := NULL]
hc.title[, year := substr(authors, year, year+3)]

# Save snapshot
rm(list=ls()[ls() %like% "tmp"])
save.image("./temp/GoogleScholarCGIAR.RData")


