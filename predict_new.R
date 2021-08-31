#### Predicitions on new data ----

# create task
task = TaskClassif$new(id = "BK", backend = kanu, target = "BK")

# upsampling
opb <- po("classbalancing")
opb$param_set$values = list(ratio = 2.5, reference = "minor",
  adjust = "minor", shuffle = FALSE)
task <- opb$train(list(task))[[1L]]
table(task$truth())

#c create learner
learner <- mlr_learners$get("classif.ranger")
learner$predict_type <- "prob"
learner$train(task)
learner$model
learner$predict(task)

# predict new cases
new       <- fread("~/Documents/Projekte/FHSMP/R/data/csv/newdata.csv", header = TRUE, sep = ";", dec = ",")
new$alter <- 2021 - new$alter
pred      <- learner$predict_newdata(new[,-c(1,2)])
pred      <- as.data.table(pred)
cols      <- c("prob.Bundeskader", "prob.Landeskader")
pred[,(cols) := round(.SD,2), .SDcols=cols]
new2 <- cbind(new, pred[,3:5])[order(-prob.Bundeskader)]

# write results as csv
write.csv(new2, "~/Desktop/predictions.csv")

#### parallel plot ----

library(MASS)

athlete <- new2[1,]
athlete <- athlete[,!c("Name", "Vorname", "prob.Bundeskader", "prob.Landeskader")]
setnames(athlete, "response", "BK")
athlete$BK <- "NA"
setcolorder(athlete, names(kanu))
kanu.new <- rbind(kanu, athlete)

kanu.new <- kanu.new[alter == 13 & Disziplin == "Kajak_m"]

colgrey <- rgb(105,105,105, alpha = 30, maxColorValue = 255)
colred1 <- rgb(220,13,0, alpha = 90, maxColorValue = 255)
isBK <- c(colred1,colgrey,"darkgreen")[kanu.new$BK]

pdf_datei <- "~/Desktop/kanu.paralell.pdf"
cairo_pdf(pdf_datei, width = 12, height = 8)
par(family = "Lato Light", xpd=TRUE, mar = c(6,4,6,6))
parcoord(kanu.new[,c("B2000", 
				"B250", 
				"Ausd",
				"KWW", 
				"Kraft", 
				"Sprint_30m",
				"KG",
				"KH")], col = isBK, var.label = TRUE, lwd = 1.9)
mtext("Sichtungsergebnisse", 
	side = 3,
	line = 2.5,
	adj = 0,
	font = 2,
	family = "Lato Black",
	cex = 1.6)
mtext("Kajak männlich (13 Jahre)", 
	side = 3,
	line = 1.2,
	adj = 0,
	font = 3,
	cex = 1.2)
legend(8,0.9, c("Landeskader","Bundeskader"), 
		horiz = FALSE,
		col=c(colgrey, colred1), 
		bty = "n",
		lwd=1.8)	
text(8.5,0.23, "Matwej Genrich", col = "darkgreen")
dev.off()



