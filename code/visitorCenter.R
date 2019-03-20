## Visitor Center

# subset Visitor Center variables
vc <- subset(nvs2018, select = c(VISCEN:OTHERVISTXT))

# change missing values to NA
vc$OTHERVISTXT[vc$OTHERVISTXT=="99" | vc$OTHERVISTXT=="999"] <- NA

# trim whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
vc$OTHERVISTXT <- trim(vc$OTHERVISTXT)


# load packages
library(plyr)
count(vc$GIFTSHOP == 1)
count(vc$EXHIB == 1)
count(vc$ASKINFO == 1)
count(vc$PRESENT == 1)
count(vc$LAVUSE == 1)
count(vc$PERMIT == 1)
count(vc$OTHERVIS == 1)

# tables
table(vc$GIFTSHOP == 1)


# remove rows where OTHERHEARTXT="NA"
vcOther <- vc[!(is.na(vc$OTHERVISTXT)),]

# sort alphabetically
vcOther <- vc[order(vc$OTHERVISTXT),] 

list(vcOther$OTHERVISTXT)