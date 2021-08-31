#### load packages ----

library(clinfun)
library(splines)
library(data.table)
library(relaimpo)
library(semPlot)
library(lavaan)
library(paradox) 
library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(mlr3pipelines) # create ML pipelines
library(mlr3tuning) # tuning ML algorithms
library(mlr3measures)
library(xtable)
library(caret)
library(qgraph)
library(vcd)

#### read data ----

setwd("~/Documents/Projekte/FHSMP/R")
full <- read.csv("data/dataset_kanu.csv", sep = ";", dec = ",")

#### clean data ----

source("src/cleanKanu.R")
kanu.long <- cleanKanu(full)

#### summary ----

summary(kanu.long)

# anzahl der Probanden
length(unique(kanu.long$ID)) # Proband mit der ID 61325 hat keine Einträge, daher hier 918

# anzahl der Probanden nach Gruppen
x <- kanu.long[,  mean(value), by = .( ID, Disziplin, Geschlecht, BK)]
x[, .N , by = .(Disziplin, Geschlecht)]

# welchen Kaderstatus hatten die Athleten
kanu.long[,.(n = length(unique(ID))) , by = BK]

# wurden alle Kanuten viermal getestet?
ses <- kanu.long[,.(n = length(unique(session))), by = .(ID)]
table(ses$n, useNA = "always")

ses <- kanu.long[,.(n = length(unique(session))), by = .(ID, Disziplin)]
table(ses$n, ses$Disziplin, useNA = "always")

# Wie alt waren die Probanden?
table(kanu.long[,.(Alter = testjahr - Geburtsjahr), by = session])

# wieviel Zeitraum liegt zwischen den Tests

table(full$Test2 - full$Test1, useNA = "always")
table(full$Test3 - full$Test2, useNA = "always")
table(full$Test4 - full$Test3, useNA = "always")

# welche Geburtsjahre liegen innerhalb der zyklen
table(kanu.long$Geburtsjahr, kanu.long$zyklus)

#### Trend der Testleistungen ----

# kanu.long[,smooth := lm(value ~ns(Geburtsjahr, knots = c(1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019)))$fitted.values, by = list(variable, Disziplin,session)]
kanu.long[,smooth := lm(value ~ns(Geburtsjahr, df = 7 ))$fitted.values, by = list(variable, Disziplin,session)]
mylist <- split(kanu.long, by = c("variable","Disziplin"))
source("src/plot_splines.R")

pdf_file <- paste0("~/Desktop/Shortsave/FHSMP/R/fig/splines.pdf")
cairo_pdf(pdf_file, onefile = TRUE)
par(bty = "L", cex = 1, mfrow = c(1,1))
invisible(lapply(mylist, plot_splines))
dev.off()

mylist <- split(kanu.long, by = c("variable","Disziplin"))
mylist$Ausdauer_1500m.Kajak_w <- NULL
mylist$Ausdauer_800m.Canadier <- NULL
mylist$Ausdauer_800m.Kajak_m <- NULL
mylist$KnH.Kajak_m <- NULL
mylist$KnH.Kajak_w <- NULL
mylist$SH.Canadier <- NULL

source("src/boxplots.R")
pdf_file <- paste0("~/Desktop/Shortsave/FHSMP/R/fig/boxplots.pdf")
cairo_pdf(pdf_file, onefile=TRUE)
par(mfrow=c(1,1),
	bty = "L", cex = 1.2)
invisible(lapply(mylist,plots ))
dev.off()


source("src/jtTest.R")
erg <- lapply(mylist, jtTest)
res <- rbindlist(erg)
setorder(res, p.value.jt ) 
res1 <- res[p.value.jt < 0.05]

sink("~/Desktop/Shortsave/FHSMP/tex/tab/tab_pvalues.tex")
print(xtable(res1, auto = TRUE, caption = "Auszug der Ergebnisse des Trendtests nach Jonckheere (p.value.jt) und Rangkorrelationskoeefizienten (Kendall) mit dem dazugehörigem p-Wert. Abgebildet sind lediglich Disziplinen mit einem p.value.jt kleiner als 0.05"), booktabs = TRUE)
sink()

#### Leistungsstruktur builiding subsets ----

x <- dcast(kanu.long, ID + session + Disziplin ~ variable, value.var = "value")

Canadier <- x[Disziplin == "Canadier"]
colSums(is.na(Canadier))
Canadier$SH            <- NULL
Canadier$Ausdauer_800m <- NULL
Canadier               <- Canadier[complete.cases(Canadier),]

Kajak_m <- x[Disziplin == "Kajak_m"]
colSums(is.na(Kajak_m))
Kajak_m$KnH            <- NULL
Kajak_m$Ausdauer_800m <- NULL
Kajak_m               <- Kajak_m[complete.cases(Kajak_m),]

Kajak_w <- x[Disziplin == "Kajak_w"]
colSums(is.na(Kajak_w))
Kajak_w$KnH            <- NULL
Kajak_w$Ausdauer_1500m <- NULL
Kajak_w               <- Kajak_w[complete.cases(Kajak_w),]

#### Leistungsstruktur Canadier ----
C1 <- Canadier[session == 1]
C2 <- Canadier[session == 2]
C3 <- Canadier[session == 3]
C4 <- Canadier[session == 4]

mod1 <- lm(B250 ~ ASW + Ausdauer_1500m + KG + KH + KWW + KnH + Kraft + Sprint_30m, data = C1)
mod2 <- lm(B250 ~ ASW + Ausdauer_1500m + KG + KH + KWW + KnH + Kraft + Sprint_30m, data = C2)
mod3 <- lm(B250 ~ ASW + Ausdauer_1500m + KG + KH + KWW + KnH + Kraft + Sprint_30m, data = C3)
mod4 <- lm(B250 ~ ASW + Ausdauer_1500m + KG + KH + KWW + KnH + Kraft + Sprint_30m, data = C4)

mod1 <- step(mod1, direction = "both", trace = 0)
mod2 <- step(mod2, direction = "both", trace = 0)
mod3 <- step(mod3, direction = "both", trace = 0)
mod4 <- step(mod4, direction = "both", trace = 0)

relImportance1 <- calc.relimp(mod1, type = "lmg", rela = TRUE)
relImportance2 <- calc.relimp(mod2, type = "lmg", rela = TRUE)
relImportance3 <- calc.relimp(mod3, type = "lmg", rela = TRUE)
relImportance4 <- calc.relimp(mod4, type = "lmg", rela = TRUE)

x1 <- sort(relImportance1$lmg, decreasing=FALSE)
x2 <- sort(relImportance2$lmg, decreasing=FALSE)
x3 <- sort(relImportance3$lmg, decreasing=FALSE)
x4 <- sort(relImportance4$lmg, decreasing=FALSE)

par(mfrow = c(2,2), 
	bty = "L", 
	oma = c(2,0,3,0))
dotchart(x1)
mtext("Test 1",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance1@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance1@nobs),cex=0.6, side = 1, line = 2, adj = 1)

dotchart(x2)
mtext("Test 2",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance2@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance2@nobs),cex=0.5, side = 1, line = 2, adj = 1)
dotchart(x3)
mtext("Test 3",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance3@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance3@nobs),cex=0.5, side = 1, line = 2, adj = 1)

dotchart(x4)
mtext("Test 4",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance4@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance4@nobs),cex=0.5, side = 1, line = 2, adj = 1)

mtext("Bedeutung der Variablen für die 250m Zeit", outer = TRUE, cex = 1.5, adj = 0)
mtext("Canadier", line = -2, outer = TRUE, cex = 1.2, adj = 0, font = 3)
mtext("Metriken sind auf 100% normalisiert", side = 1, line = -1, adj = 1, outer = TRUE, cex = 0.7)

mod1 <- lm(B2000 ~ ASW + Ausdauer_1500m + KG + KH + KWW + KnH + Kraft + Sprint_30m, data = C1)
mod2 <- lm(B2000 ~ ASW + Ausdauer_1500m + KG + KH + KWW + KnH + Kraft + Sprint_30m, data = C2)
mod3 <- lm(B2000 ~ ASW + Ausdauer_1500m + KG + KH + KWW + KnH + Kraft + Sprint_30m, data = C3)
mod4 <- lm(B2000 ~ ASW + Ausdauer_1500m + KG + KH + KWW + KnH + Kraft + Sprint_30m, data = C4)

mod1 <- step(mod1, direction = "both", trace = 0)
mod2 <- step(mod2, direction = "both", trace = 0)
mod3 <- step(mod3, direction = "both", trace = 0)
mod4 <- step(mod4, direction = "both", trace = 0)

relImportance1 <- calc.relimp(mod1, type = "lmg", rela = TRUE)
relImportance2 <- calc.relimp(mod2, type = "lmg", rela = TRUE)
relImportance3 <- calc.relimp(mod3, type = "lmg", rela = TRUE)
relImportance4 <- calc.relimp(mod4, type = "lmg", rela = TRUE)

x1 <- sort(relImportance1$lmg, decreasing=FALSE)
x2 <- sort(relImportance2$lmg, decreasing=FALSE)
x3 <- sort(relImportance3$lmg, decreasing=FALSE)
x4 <- sort(relImportance4$lmg, decreasing=FALSE)

par(mfrow = c(2,2), 
	bty = "L", 
	oma = c(2,0,3,0))
dotchart(x1)
mtext("Test 1",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance1@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance1@nobs),cex=0.6, side = 1, line = 2, adj = 1)

dotchart(x2)
mtext("Test 2",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance2@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance2@nobs),cex=0.5, side = 1, line = 2, adj = 1)
dotchart(x3)
mtext("Test 3",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance3@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance3@nobs),cex=0.5, side = 1, line = 2, adj = 1)

dotchart(x4)
mtext("Test 4",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance4@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance4@nobs),cex=0.5, side = 1, line = 2, adj = 1)

mtext("Bedeutung der Variablen für die 2000m Zeit", outer = TRUE, cex = 1.5, adj = 0)
mtext("Canadier", line = -2, outer = TRUE, cex = 1.2, adj = 0, font = 3)
mtext("Metriken sind auf 100% normalisiert", side = 1, line = -1, adj = 1, outer = TRUE, cex = 0.7)


#### Leistungsstruktur  Kajak männlich ----
k1 <- Kajak_m[session == 1]
k2 <- Kajak_m[session == 2]
k3 <- Kajak_m[session == 3]
k4 <- Kajak_m[session == 4]

mod1 <- lm(B250 ~ ASW + Ausdauer_1500m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k1)
mod2 <- lm(B250 ~ ASW + Ausdauer_1500m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k2)
mod3 <- lm(B250 ~ ASW + Ausdauer_1500m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k3)
mod4 <- lm(B250 ~ ASW + Ausdauer_1500m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k4)

mod1 <- step(mod1, direction = "both", trace = 0)
mod2 <- step(mod2, direction = "both", trace = 0)
mod3 <- step(mod3, direction = "both", trace = 0)
mod4 <- step(mod4, direction = "both", trace = 0)

relImportance1 <- calc.relimp(mod1, type = "lmg", rela = TRUE)
relImportance2 <- calc.relimp(mod2, type = "lmg", rela = TRUE)
relImportance3 <- calc.relimp(mod3, type = "lmg", rela = TRUE)
relImportance4 <- calc.relimp(mod4, type = "lmg", rela = TRUE)

x1 <- sort(relImportance1$lmg, decreasing=FALSE)
x2 <- sort(relImportance2$lmg, decreasing=FALSE)
x3 <- sort(relImportance3$lmg, decreasing=FALSE)
x4 <- sort(relImportance4$lmg, decreasing=FALSE)

par(mfrow = c(2,2), 
	bty = "L", 
	oma = c(2,0,3,0))
dotchart(x1)
mtext("Test 1",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance1@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance1@nobs),cex=0.6, side = 1, line = 2, adj = 1)

dotchart(x2)
mtext("Test 2",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance2@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance2@nobs),cex=0.5, side = 1, line = 2, adj = 1)
dotchart(x3)
mtext("Test 3",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance3@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance3@nobs),cex=0.5, side = 1, line = 2, adj = 1)

dotchart(x4)
mtext("Test 4",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance4@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance4@nobs),cex=0.5, side = 1, line = 2, adj = 1)

mtext("Bedeutung der Variablen für die 250m Zeit", outer = TRUE, cex = 1.5, adj = 0)
mtext("Kajak männlich", line = -2, outer = TRUE, cex = 1.2, adj = 0, font = 3)
mtext("Metriken sind auf 100% normalisiert", side = 1, line = -1, adj = 1, outer = TRUE, cex = 0.7)

mod1 <- lm(B2000 ~ ASW + Ausdauer_1500m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k1)
mod2 <- lm(B2000 ~ ASW + Ausdauer_1500m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k2)
mod3 <- lm(B2000 ~ ASW + Ausdauer_1500m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k3)
mod4 <- lm(B2000 ~ ASW + Ausdauer_1500m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k4)

mod1 <- step(mod1, direction = "both", trace = 0)
mod2 <- step(mod2, direction = "both", trace = 0)
mod3 <- step(mod3, direction = "both", trace = 0)
mod4 <- step(mod4, direction = "both", trace = 0)

relImportance1 <- calc.relimp(mod1, type = "lmg", rela = TRUE)
relImportance2 <- calc.relimp(mod2, type = "lmg", rela = TRUE)
relImportance3 <- calc.relimp(mod3, type = "lmg", rela = TRUE)
relImportance4 <- calc.relimp(mod4, type = "lmg", rela = TRUE)

x1 <- sort(relImportance1$lmg, decreasing=FALSE)
x2 <- sort(relImportance2$lmg, decreasing=FALSE)
x3 <- sort(relImportance3$lmg, decreasing=FALSE)
x4 <- sort(relImportance4$lmg, decreasing=FALSE)

par(mfrow = c(2,2), 
	bty = "L", 
	oma = c(2,0,3,0))
dotchart(x1)
mtext("Test 1",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance1@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance1@nobs),cex=0.6, side = 1, line = 2, adj = 1)

dotchart(x2)
mtext("Test 2",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance2@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance2@nobs),cex=0.5, side = 1, line = 2, adj = 1)
dotchart(x3)
mtext("Test 3",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance3@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance3@nobs),cex=0.5, side = 1, line = 2, adj = 1)

dotchart(x4)
mtext("Test 4",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance4@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance4@nobs),cex=0.5, side = 1, line = 2, adj = 1)

mtext("Bedeutung der Variablen für die 2000m Zeit", outer = TRUE, cex = 1.5, adj = 0)
mtext("Kajak männlich", line = -2, outer = TRUE, cex = 1.2, adj = 0, font = 3)
mtext("Metriken sind auf 100% normalisiert", side = 1, line = -1, adj = 1, outer = TRUE, cex = 0.7)

#### Leistungsstruktur Kajak weiblich ----

k1 <- Kajak_w[session == 1]
k2 <- Kajak_w[session == 2]
k3 <- Kajak_w[session == 3]
k4 <- Kajak_w[session == 4]

mod1 <- lm(B250 ~ ASW + Ausdauer_800m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k1)
mod2 <- lm(B250 ~ ASW + Ausdauer_800m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k2)
mod3 <- lm(B250 ~ ASW + Ausdauer_800m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k3)
mod4 <- lm(B250 ~ ASW + Ausdauer_800m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k4)

mod1 <- step(mod1, direction = "both", trace = 0)
mod2 <- step(mod2, direction = "both", trace = 0)
mod3 <- step(mod3, direction = "both", trace = 0)
mod4 <- step(mod4, direction = "both", trace = 0)

relImportance1 <- calc.relimp(mod1, type = "lmg", rela = TRUE)
relImportance2 <- calc.relimp(mod2, type = "lmg", rela = TRUE)
relImportance3 <- calc.relimp(mod3, type = "lmg", rela = TRUE)
relImportance4 <- calc.relimp(mod4, type = "lmg", rela = TRUE)

x1 <- sort(relImportance1$lmg, decreasing=FALSE)
x2 <- sort(relImportance2$lmg, decreasing=FALSE)
x3 <- sort(relImportance3$lmg, decreasing=FALSE)
x4 <- sort(relImportance4$lmg, decreasing=FALSE)

par(mfrow = c(2,2), 
	bty = "L", 
	oma = c(2,0,3,0))
dotchart(x1)
mtext("Test 1",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance1@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance1@nobs),cex=0.6, side = 1, line = 2, adj = 1)

dotchart(x2)
mtext("Test 2",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance2@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance2@nobs),cex=0.5, side = 1, line = 2, adj = 1)
dotchart(x3)
mtext("Test 3",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance3@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance3@nobs),cex=0.5, side = 1, line = 2, adj = 1)

dotchart(x4)
mtext("Test 4",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance4@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance4@nobs),cex=0.5, side = 1, line = 2, adj = 1)

mtext("Bedeutung der Variablen für die 250m Zeit", outer = TRUE, cex = 1.5, adj = 0)
mtext("Kajak weiblich", line = -2, outer = TRUE, cex = 1.2, adj = 0, font = 3)
mtext("Metriken sind auf 100% normalisiert", side = 1, line = -1, adj = 1, outer = TRUE, cex = 0.7)


mod1 <- lm(B2000 ~ ASW + Ausdauer_800m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k1)
mod2 <- lm(B2000 ~ ASW + Ausdauer_800m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k2)
mod3 <- lm(B2000 ~ ASW + Ausdauer_800m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k3)
mod4 <- lm(B2000 ~ ASW + Ausdauer_800m + KG + KH + KWW + SH + Kraft + Sprint_30m, data = k4)

mod1 <- step(mod1, direction = "both", trace = 0)
mod2 <- step(mod2, direction = "both", trace = 0)
mod3 <- step(mod3, direction = "both", trace = 0)
mod4 <- step(mod4, direction = "both", trace = 0)

relImportance1 <- calc.relimp(mod1, type = "lmg", rela = TRUE)
relImportance2 <- calc.relimp(mod2, type = "lmg", rela = TRUE)
relImportance3 <- calc.relimp(mod3, type = "lmg", rela = TRUE)
relImportance4 <- calc.relimp(mod4, type = "lmg", rela = TRUE)

x1 <- sort(relImportance1$lmg, decreasing=FALSE)
x2 <- sort(relImportance2$lmg, decreasing=FALSE)
x3 <- sort(relImportance3$lmg, decreasing=FALSE)
x4 <- sort(relImportance4$lmg, decreasing=FALSE)


par(mfrow = c(2,2), 
	bty = "L", 
	oma = c(2,0,3,0))
dotchart(x1)
mtext("Test 1",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance1@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance1@nobs),cex=0.6, side = 1, line = 2, adj = 1)

dotchart(x2)
mtext("Test 2",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance2@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance2@nobs),cex=0.5, side = 1, line = 2, adj = 1)
dotchart(x3)
mtext("Test 3",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance3@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance3@nobs),cex=0.5, side = 1, line = 2, adj = 1)

dotchart(x4)
mtext("Test 4",cex=0.9, side = 3, line = 1)
mtext(bquote(r^2 == .(round(relImportance4@R2,2))),cex=0.9, side = 3, line = -0.4)
mtext(paste0("n=",relImportance4@nobs),cex=0.5, side = 1, line = 2, adj = 1)

mtext("Bedeutung der Variablen für die 2000m Zeit", outer = TRUE, cex = 1.5, adj = 0)
mtext("Kajak weiblich", line = -2, outer = TRUE, cex = 1.2, adj = 0, font = 3)
mtext("Metriken sind auf 100% normalisiert", side = 1, line = -1, adj = 1, outer = TRUE, cex = 0.7)

#### Leistungstruktur SEM

myModel <- '
unsp_kraft =~ Kraft + Sprint_30m + KWW
unsp_Ausdauer =~ Ausdauer_1500m
Körper =~ ASW + KG + KH + KnH

B250 ~ Kraft + unsp_Ausdauer
'

erg <- sem(model = myModel, 
			data = Canadier,
			group = "session"
			)
summary(erg, 
		fit.measures=TRUE, 
		standardized=TRUE, 
		rsquare = TRUE)
fitMeasures(erg,c("srmr", "rmsea", "tli", "cfa"))

semPaths(erg,"std")

#### mlr3 Benchmark ----
# https://mlr3gallery.mlr-org.com/posts/2020-03-30-imbalanced-data/

test <- kanu.long
test[variable == "Ausdauer_1500m"]$variable <- "Ausd"
test[variable == "Ausdauer_800m"]$variable <- "Ausd"
test[variable == "SH"]$variable <- "h_unten"
test[variable == "KnH"]$variable <- "h_unten"

kanu <- dcast(test, ID + session + Disziplin + BK ~ variable, value.var = "value")
kanu <- kanu[complete.cases(kanu),]
kanu <- droplevels(kanu)
kanu$BK <- ifelse(kanu$BK == "NK1", "Landeskader", "Bundeskader")
kanu$BK <- as.factor(kanu$BK)
kanu$session <- as.factor(kanu$session)
kanu$ID <- NULL

task    <- TaskClassif$new("kanu", backend = kanu, target = "BK")
# check original class balance
table(task$truth())

opb = po("classbalancing")
opb$param_set$values = list(ratio = 2.5, reference = "minor",
  adjust = "minor", shuffle = FALSE)
task = opb$train(list(task))[[1L]]
table(task$truth())

train <- sample(task$nrow, 0.8 * task$nrow)
test     <- setdiff(seq_len(task$nrow), train)

learners <- c("classif.ranger", "classif.rpart", "classif.log_reg", "classif.naive_bayes", "classif.featureless")
learners <- lapply(learners, lrn,
				   predict_type = "prob", predict_sets = c("train", "test"))
resamplings <- rsmp("cv", folds = 10)
design <- benchmark_grid(task, learners, resamplings)
bmr <- benchmark(design, store_models = TRUE)
measures <- list(
  msr("classif.acc"),
  msr("classif.auc"),
  msr("classif.precision"),
  msr("classif.logloss"),
  msr("classif.sensitivity"),
  msr("classif.specificity")
)

res <- bmr$aggregate(measures)
res <- data.frame(method = res$learner_id, 
				  resampling = res$resampling_id,
				  Accuracy = res$classif.acc, 
				  Auc = res$classif.auc,
				  logloss = res$classif.logloss, 
				  sensivity = res$classif.sensitivity, 
				  specifity = res$classif.specificity)
res$method <- gsub("classif.", "", res$method)

sink("~/Desktop/Shortsave/FHSMP/tex/tab/models.tex")
print(xtable(res,
			 auto = TRUE, 
			 caption = "Vergleich verschiedener Modelle auf der Basis des Testsets (split 70/30)",
			 digits = 2), 
	  booktabs = TRUE,
	  include.rownames = FALSE
)
sink()

rr = res$resample_result[[1]]
test <- as.data.table(rr$prediction())

sink("~/Desktop/Shortsave/FHSMP/tex/tab/pred.tex")
print(xtable(head(test),
			 auto = TRUE, 
			 caption = "Vorhersagen im Testset",
			 digits = 2), 
	  booktabs = TRUE,
	  include.rownames = FALSE
)
sink()

mat <- rr$prediction()$confusion
sink("~/Desktop/Shortsave/FHSMP/tex/tab/confmat.tex")
print(xtable(mat,
			 auto = TRUE, 
			 caption = "Konfusionsmatrix für die erste Iteration (10 fold cross validation) per random forest",
			 digits = 2), 
	  booktabs = TRUE,
	  include.rownames = FALSE
)
sink()

confusionMatrix(data = test$response, reference = test$truth)

set.seed(1)
dor(test$truth, test$response, positive = "Bundeskader")


#### predict newdata ----

task    <- TaskClassif$new("kanu", backend = kanu, target = "BK")
# check original class balance
table(task$truth())

opb = po("classbalancing")
opb$param_set$values = list(ratio = 2.5, reference = "minor",
                            adjust = "minor", shuffle = FALSE)
task = opb$train(list(task))[[1L]]
table(task$truth())

learner$predict_type <- "prob"
learner$train(task)

newdata <- data.frame(session = 4,
                      Disziplin = "Canadier",
                      ASW = 120,
                      X2 = 118,
                      B2000 = 70,
                      B250 = 50,
                      KG = 65,
                      X1 = 110,
                      KWW = 72,
                      KH = 144,
                      Kraft = 90, 
                      Sprint_30m = 22)
learner$predict_newdata(newdata)



#### networkgraphs Canadiers ----

test <- kanu.long[Disziplin == "Canadier"]
test1 <- test[session == 1]
test2 <- test[session == 2]
test3 <- test[session == 3]
test4 <- test[session == 4]

test1 <- dcast(test1, ID ~ variable, value.var = "value")
test2 <- dcast(test2, ID ~ variable, value.var = "value")
test3 <- dcast(test3, ID ~ variable, value.var = "value")
test4 <- dcast(test4, ID ~ variable, value.var = "value")

CorMat1 <- cor_auto(test1[,2:11])
CorMat2 <- cor_auto(test2[,2:11])
CorMat3 <- cor_auto(test3[,2:11])
CorMat4 <- cor_auto(test4[,2:11])

pdf_datei <- "~/Desktop/Shortsave/FHSMP/tex/fig/cormat_Can.pdf"
cairo_pdf(pdf_datei, width = 8, height = 8)
par(mfrow = c(2,2), oma = c(2,0,3,0))
p1 <- qgraph(CorMat1, graph = "cor", sampleSize = nrow(test1), layout = "circle", title = "13-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
p2 <- qgraph(CorMat2, graph = "cor", sampleSize = nrow(test2), layout = "circle", title = "14-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
p3 <- qgraph(CorMat3, graph = "cor", sampleSize = nrow(test3), layout = "circle", title = "15-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
p4 <- qgraph(CorMat4, graph = "cor", sampleSize = nrow(test4), layout = "circle", title = "16-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
mtext("Korrelationsstruktur Canadier", 
	  side = 3,
	  line = 0, outer = TRUE)
mtext("Edge labels sind Pearsons Korrelationskoeffizienten nach Bonferronie Korrektur", 
	  side = 1,
	  line = 0, 
	  outer = TRUE, adj = 1, cex = 0.5)
dev.off()

#### networkgraphs Kanuten weiblich ----

test <- kanu.long[Disziplin == "Kajak_w"]
test1 <- test[session == 1]
test2 <- test[session == 2]
test3 <- test[session == 3]
test4 <- test[session == 4]

test1 <- dcast(test1, ID ~ variable, value.var = "value")
test2 <- dcast(test2, ID ~ variable, value.var = "value")
test3 <- dcast(test3, ID ~ variable, value.var = "value")
test4 <- dcast(test4, ID ~ variable, value.var = "value")

CorMat1 <- cor_auto(test1[,2:11])
CorMat2 <- cor_auto(test2[,2:11])
CorMat3 <- cor_auto(test3[,2:11])
CorMat4 <- cor_auto(test4[,2:11])

pdf_datei <- "~/Desktop/Shortsave/FHSMP/tex/fig/cormat_kajak_w.pdf"
cairo_pdf(pdf_datei, width = 8, height = 8)
par(mfrow = c(2,2), oma = c(2,0,3,0))
p1 <- qgraph(CorMat1, graph = "cor", sampleSize = nrow(test1), layout = "circle", title = "13-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
p2 <- qgraph(CorMat2, graph = "cor", sampleSize = nrow(test2), layout = "circle", title = "14-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
p3 <- qgraph(CorMat3, graph = "cor", sampleSize = nrow(test3), layout = "circle", title = "15-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
p4 <- qgraph(CorMat4, graph = "cor", sampleSize = nrow(test4), layout = "circle", title = "16-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
mtext("Korrelationsstruktur Kajak weiblich", 
	  side = 3,
	  line = 0, outer = TRUE)
mtext("Edge labels sind Pearsons Korrelationskoeffizienten nach Bonferronie Korrektur", 
	  side = 1,
	  line = 0, 
	  outer = TRUE, adj = 1, cex = 0.5)
dev.off()

#### networkgraphs Kanuten männlich----

test <- kanu.long[Disziplin == "Kajak_m"]
test1 <- test[session == 1]
test2 <- test[session == 2]
test3 <- test[session == 3]
test4 <- test[session == 4]

test1 <- dcast(test1, ID ~ variable, value.var = "value")
test2 <- dcast(test2, ID ~ variable, value.var = "value")
test2[,KnH := NULL]
test3 <- dcast(test3, ID ~ variable, value.var = "value")
test4 <- dcast(test4, ID ~ variable, value.var = "value")

CorMat1 <- cor_auto(test1[,2:11])
CorMat2 <- cor_auto(test2[,2:11])
CorMat3 <- cor_auto(test3[,2:11])
CorMat4 <- cor_auto(test4[,2:11])

pdf_datei <- "~/Desktop/Shortsave/FHSMP/tex/fig/cormat_kajak_m.pdf"
cairo_pdf(pdf_datei, width = 8, height = 8)
par(mfrow = c(2,2), oma = c(2,0,3,0))
p1 <- qgraph(CorMat1, graph = "cor", sampleSize = nrow(test1), layout = "circle", title = "13-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
p2 <- qgraph(CorMat2, graph = "cor", sampleSize = nrow(test2), layout = "circle", title = "14-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
p3 <- qgraph(CorMat3, graph = "cor", sampleSize = nrow(test3), layout = "circle", title = "15-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
p4 <- qgraph(CorMat4, graph = "cor", sampleSize = nrow(test4), layout = "circle", title = "16-jährige", minimum = "sig", bonf = TRUE, edge.labels=TRUE)
mtext("Korrelationsstruktur Kajak männlich", 
	  side = 3,
	  line = 0, outer = TRUE)
mtext("Edge labels sind Pearsons Korrelationskoeffizienten nach Bonferronie Korrektur", 
	  side = 1,
	  line = 0, 
	  outer = TRUE, adj = 1, cex = 0.5)
dev.off()

#### run a logistic regression----

test <- kanu.long
test[variable == "Ausdauer_1500m"]$variable <- "X1"
test[variable == "Ausdauer_800m"]$variable <- "X1"
test[variable == "SH"]$variable <- "X2"
test[variable == "KnH"]$variable <- "X2"

kanu <- dcast(test, ID + session + Disziplin + BK ~ variable, value.var = "value")
kanu <- kanu[complete.cases(kanu),]
kanu <- droplevels(kanu)
kanu$BK <- ifelse(kanu$BK == "NK1", "Landeskader", "Bundeskader")
kanu$BK <- as.factor(kanu$BK)
kanu$session <- as.factor(kanu$session)
kanu$ID <- NULL

test <- kanu[session == 1]

mylogit <- glm(BK ~ ASW + B2000 + B250 + KG + KH + Kraft + KWW + Sprint_30m + X1 + X2, data = test, family = "binomial")
x <- step(mylogit)

summary(x)
source("~/Documents/RFunctions/LogReg_Rsquare.R")
logisticPseudoR2s(mylogit)

xtable(mylogit)

