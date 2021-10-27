# Bibliometric - breast cancer Malaysia 28-05-2021

# Set wd ----
setwd("C:/Tengku/PhD/publication/bibliometric-BC-Malaysia/latest-24-05-2021/Data-Analysis")

# Packages
library(magrittr)
library(bibliometrix)
library(tidyverse)
library(ggplot2)
library(stringi)

# Read data
scopus_data <- convert2df(file = "scopus.bib", dbsource = "scopus", 
                          format = "bibtex")

# Check for duplicates ----
dim(scopus_data)

## In title
anyDuplicated(scopus_data$TI)
table(duplicated(scopus_data$TI))
scopus_data$TI[duplicated(scopus_data$TI)]
scopus_data %>% 
  select(TI, SO) %>% 
  filter(str_detect(TI, "LIFETIME PHYSICAL ACTIVITY AND BREAST CANCER:"))

### Remove duplicate in title
scopus_data1 <- scopus_data[!duplicated(scopus_data$TI),] 
dim(scopus_data1)

## In DOI
anyDuplicated(scopus_data1$DI)
table(duplicated(scopus_data1$DI))
scopus_data1[duplicated(scopus_data1$DI), c("SO", "DI")] # No duplicate in DOI


# Check for missing data ----
## In title
anyNA(scopus_data1$TI)

## In abstract
anyNA(scopus_data1$AB)
scopus_data1[!complete.cases(scopus_data1$AB), "TI"]
# No 1 and 2 not an article, proceeding or review
# No 3 is a review but truly has no abstract
# No 4, papers not found online, and so the abstract
# No 5, abstract exist

## remove 1 and 2
TI1 <- "RE: A RARE CASE OF BREAST CANCER PRESENTING AS TETANUS"                                        
TI2 <- "A NEW BRCA1 GERMLINE MUTATION (E879X) IN A MALAYSIAN BREAST CANCER PATIENT OF CHINESE DESCENT."
TI5 <- "PRIMARY CARCINOMA AND BENIGN TUMOURS OF THE FEMALE BREAST IN MALAYSIAN WOMEN."

scopus_data2 <- scopus_data1 %>% 
  filter(TI != c(TI1, TI2))
scopus_data2[scopus_data2$TI==TI5, "AB"] <- AB5  

dim(scopus_data2)

# Descriptive ----
scopus_data <- scopus_data2
dim(scopus_data)
result <- biblioAnalysis(scopus_data)
S <- summary(result, k=10)
plot(result, k=10)

## Plot paper per year ----
year <- scopus_data %>% 
  as_tibble() %>% 
  select(PY, DT) 

PaperPerYear <- 
  left_join(tibble(PY = 1982:2021), year, by = "PY") %>% 
  mutate(across(c(DT), factor)) %>% 
  ggplot(aes(PY, fill = DT)) + 
  geom_bar() + 
  theme_bw() + 
  ylab("Number of publications") + 
  xlab("Year of publication") + 
  scale_fill_brewer("Type:",
                    labels = c("Article", "Conference paper", "Review", "NA"),
                    na.translate=FALSE,
                    palette = 9) +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = seq(1982, 2021, by = 2),
                     limits = c(1981,2022), 
                     expand = c(0, 0))
PaperPerYear

## Top authors ----
result$Authors %>% 
  as_tibble() %>% 
  rename(authors = "AU", article_published = "n") %>% 
  slice(1:10) 

## Top-authors's productivity over time
auth_prod <- authorProdOverTime(scopus_data, k=10, graph = T) #need to run CopyOfauthorProdOverTime_nologo.R
head(auth_prod$dfAU, 10) # author's productivity per year

head(auth_prod$dfPapersAU, 10) # author's document list

auth_prod$graph + 
  labs(title = "") + 
  theme_bw()

## Top journals ----
result$Sources %>% 
  as_tibble() %>% 
  rename(journals = "SO", article_published = "n") %>% 
  top_n(15)

## Core journals ----
core_j <- bradford(scopus_data)
core_j <- core_j$table %>% as.data.frame()
zone_all <- core_j %>% 
  select(SO, Freq, Zone) %>% 
  group_by(Zone) %>% 
  summarise(journal = length(SO), article = sum(Freq)) 
zone_all

core_j %>% 
  filter(Zone == "Zone 1") %>%  # zone 1 journals
  mutate(Freq_percent = round(Freq/340 * 100, digits = 1), .after = Freq)

## Institution collab ----
NetMatrix <- biblioNetwork(scopus_data, 
                           analysis = "collaboration",  
                           network = "universities", 
                           sep = ";")
collab_uni <- networkPlot(NetMatrix, 
                          n = 20, 
                          Title = "", 
                          type = "circle", 
                          cluster = "none",
                          size = 15, 
                          size.cex = T, 
                          labelsize = 1.2, 
                          remove.isolates = T)
collab_uni$nodeDegree %>% 
  as_tibble(rownames = "string")

## Funded research ----
fund <- table(is.na(scopus_data$FU))
prop.table(fund)*100
fund # 78% not funded, 22% funded

## Author per paper ----
no_author <- stri_count_regex(scopus_data$AU, c(";"))
author <- data.frame(paper = scopus_data$TI, 
                     author = scopus_data$AU, 
                     no_auth = no_author+1, 
                     type = scopus_data$DT)
range(author$no_auth)
auth_paper <- author %>% 
  count(no_auth, type) 
sum(auth_paper$n)

author %>% 
  count(no_auth) %>% 
  arrange(desc(n)) %>% 
  mutate(Cum = (cumsum(n)/sum(n))*100)

### Plot
auth_paper %>%  
  mutate(across(c(no_auth, type), as_factor), 
         type = plyr::revalue(auth_paper$type, 
                              c(c("ARTICLE" = "Article", 
                                  "CONFERENCE PAPER" = "Conference paper",
                                  "REVIEW" = "Review")))) %>% 
  ggplot(aes(no_auth, n, fill = type)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(0,60, by=5)) + 
  xlab("Number of authors") +
  ylab("Frequency") + 
  scale_fill_brewer("Type:", palette = 9) +
  theme(legend.position = "top")

# Future trend ----
## Thematic map ----
Map <- thematicMap(scopus_data,
                   field = "DE", 
                   n = 100, 
                   minfreq = 8,
                   stemming = TRUE, 
                   size = 0.84, 
                   n.labels=5, 
                   repel = T)
P <- Map$map + 
  theme_bw() + 
  theme(legend.position = "none") 

P$layers[[6]] <- NULL #remove logo
P

Map$nclust
word_cluster <- Map$words

## Keyword trend ----
### Overall
es <- fieldByYear(scopus_data, 
                   field = "DE", 
                   timespan = c(2010,2020),
                   min.freq = 2, 
                   n.items = 5, 
                   graph = TRUE) 
es$graph +
  theme_bw()+
  ggtitle("")

es$df_graph %>% 
  arrange(desc(freq))

es$df_graph %>% 
  filter(freq > 5) %>% 
  arrange(desc(freq))

