#### this function ist used to clean and prepare the data

cleanKanu <- function(full){

# numeric to factors
full$Geschlecht <- factor(full$Geschlecht, 
						  levels = c(1,2), 
						  labels = c("männlich", "weiblich"))

full$BK <- factor(full$BK,
				  levels = c(0,1,2),
				  labels = c("NK1", "Nk1-PK","OK"))

full$ID <- factor(full$ID)

# replace zeros with NA
full[full == 0] <- NA

# data.table and reshape
full <- data.table(full)
vec1 <- names(full)[1:5]
vec2 <- names(full)[10:61]
kanu <- data.table::melt(full, id.vars = vec1, measure.vars = vec2)
kanu[,c("variable","session") := tstrsplit(variable, "_")]

# add year of measurement
testdate <- full[,c(2,6:9)]
testdate <- data.table::melt(testdate, id.vars = "ID")
testdate[, session := gsub('\\D+','', variable)]
testdate[,"variable"] <- NULL
setnames(testdate, "value", "testjahr")

setkey(kanu,ID, session)
setkey(testdate,ID, session)
kanu.long <- merge(kanu,testdate, all.x=TRUE)

# remove NA´s
kanu.long <- kanu.long[complete.cases(kanu.long$value),]

# add O-Zyklus

# table(is.na(kanu.long$testjahr))
# ids <- kanu.long[is.na(kanu.long$testjahr),]
# unique(ids$ID) # 27 Probanden ohne Testdatum
# kanu.long <- kanu.long[! ID %in% unique(ids$ID),]

kanu.long[, zyklus := cut(kanu.long$testjahr, breaks = 7 )]
kanu.long[, zyklus := gsub('^.','[', zyklus)]
kanu.long$zyklus <- factor(kanu.long$zyklus, 
						   ordered = TRUE,
						   levels = c("[1992,1996]",
									  "[1996,2000]",
									  "[2000,2004]",
									  "[2004,2007]",
									  "[2007,2011]",
									  "[2011,2015]",
									  "[2015,2019]",
									  "<NA>"),
						   labels = c("1992-1995",
									  "1996-1999",
									  "2000-2003",
									  "2004-2007",
									  "2008-2011",
									  "2012-2015",
									  "2016-2019",
									  "<NA>")
						   )

# remove Gesamt
kanu.long <- kanu.long[ variable != "Gesamt"]

# rename variables
kanu.long[variable == "X.30m"]$variable <- "Sprint_30m"
kanu.long[variable == "X.1500m"]$variable <- "Ausdauer_1500m"
kanu.long[variable == "X.800m"]$variable <- "Ausdauer_800m"

# variable to factor
kanu.long$variable <- factor(kanu.long$variable)

# rename Disziplin
kanu.long$Disziplin <- as.character(kanu.long$Disziplin)
kanu.long[Disziplin == "1"]$Disziplin <- "Kajak_m"
kanu.long[Disziplin == "2"]$Disziplin <- "Kajak_w"
kanu.long[Disziplin == "3"]$Disziplin <- "Canadier"

kanu.long$Disziplin <- factor(kanu.long$Disziplin)

return(kanu.long)
}


