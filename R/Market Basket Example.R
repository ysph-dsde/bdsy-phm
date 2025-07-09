# Market Basket Analysis, aka affinity analysis aka association rules mining is 
# an unsupervised machine learning technique which applies an algorithm (apriori 
# algorithm) to identify association rules in datasets.

# We'll be applying this algorithm to identify associations in a dataset containing 
# information about cases of perforative acute otitis media in children.

# We'll start with loading in our packages

#| message: FALSE
#| warning: FALSE

# NOTE: you might need to specify the source for the arules package:
# install.packages("arules", repos='http://cran.rstudio.com/')

suppressPackageStartupMessages({
  library("tidyverse")     # Collection of R packages for data science
  library("knitr")         # For dynamic report generation
  library("ggplot2")       # For creating static visualizations
  library("lubridate")     # For date and time manipulation
  library("arules")        # For mining association rules and frequent itemsets
  library("arulesViz")     # For visualizing association rules
  library("plyr")          # For data manipulation
  library("RColorBrewer")  # For color palettes
  library("plotly")        # For creating interactive web-based graphs
  library("httr")          # For downloading files from URLs
})

# Function to select "Not In"
'%!in%' <- function(x,y)!('%in%'(x,y))

# And our dataset

# Read in the cleaned data directly from the instructor's GitHub.
url <- "https://raw.githubusercontent.com/ysph-dsde/bdsy-phm/refs/heads/main/Data/pAOM.csv"
pAOM <- read_csv(url)


# We have 2137 rows representing 2137 cases of pAOM. Each row contains information 
# about PtID- study ID of patient
#    - SmokeNum- number of smokers in household
#    - PtDayCare- did patient attend daycare
#    - SibNum- number of siblings in household
#    - NumSibDaycare- number of siblings in daycare
#    - Pre_Post_PCV13- did the case occur before or after the child could have 
#      received PCV13? (PreVacc, PostVacc_yr1-5)
#    - PCV13_Serotype_AOM- of pneumococcal pAOM cases, were they from PCV13 
#      serotypes? (VT,NVTs)
#    - OtoPathogen- Strep_pneumo,Strep_pyogenes,Haem_inf, Morax_cat, Staph_aur, 
#      OthBact, PresViral
#    - Carriage1- otopathogens (c_Strep_pneumo,c_Strep_pyogenes,c_Haem_inf, 
#      c_Morax_cat, c_Staph_aur, c_None)found in NP carriage
#    - Carriage2- additional otopathogens found in NP carriage
#    - Carriage3- additional otopathogens found in NP carriage

# Before using our rule mining algorithm, we need to transform data from the data 
# frame format into transactions.

# Create a temporary file
temp_file <- tempfile()

# Download the data from GitHub and save it to the temporary file
GET(url, write_disk(temp_file, overwrite = TRUE))

# Read the transactions from the downloaded file
tr <- read.transactions(temp_file, format = 'basket', sep = ',')


print('Description of the transactions')
summary(tr)


# Let's see what items occur most frequently:


itemFrequencyPlot(tr,topN=25,type="absolute",col=brewer.pal(8,'Pastel2'), main="pAOM rules")

# a relative frequency plot
 

itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'),main="Relative frequency, pAOM")


## Create some rules
# We use the Apriori algorithm from the arules package to look for itemsets and 
# find support for rules. We pass supp=0.0001 and conf=0.8 to return all the 
# rules have a support of at least 0.1% and confidence of at least 80%. 

# We sort the rules by decreasing confidence. Here are the rules matching these 
# criteria:

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)


# We have 4093 rules, most are 4 or 5 items long. Let's inspect the top 10 rules 
# according to these parameters (supp 0.001, conf =0.8).

inspect(rules[1:10])

# And plot these top 10 rules, or 20, or 50.

topRules <- rules[1:10]
plot(rules)

# now with more colors

plot(rules, method = "two-key plot")

# now how about a network graph?

plot(topRules, method="graph")

# Now let's see an interactive map:

plot(topRules, method="graph", engine = 'interactive')

plot(topRules, method = "grouped")

plot(topRules, method = "graph",  engine = "htmlwidget")

# now a matrix plot

plot(topRules, method = "matrix", engine = "3d", measure = "lift")

# moving back a bit, let's check a different set of rules:

rules_b <- apriori(tr, parameter = list(supp=0.01, conf=1.0))
rules_b <- sort(rules_b, by='confidence', decreasing = TRUE)
summary(rules_b)

# these more stringent criteria mean that we're down to 115 rules to sort through.

inspect(rules_b[1:10])

# try to look for only for rules associated with Strep_pneumo


pneumo.rules<-sort(subset(rules_b, subset = rhs %in% "Strep_pneumo"))

inspect(pneumo.rules[1:10])


# now let's plot the pneumo rules:

plot(pneumo.rules, measure = c("support", "confidence"), shading = "lift")

plot(pneumo.rules[1:10], method="graph", engine = 'interactive')

plot(pneumo.rules[1:10], method= "paracoord", control=list(reorder=TRUE))


# reference: [R and Data Mining](http://www.rdatamining.com/examples/association-rules)
# there are other cool market basket analysis visualizations here: 