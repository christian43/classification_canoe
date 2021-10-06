#### load packages ----

library(data.table)
library(paradox) 
library(mlr3)
library(mlr3learners)
library(mlr3filters)
library(mlr3pipelines) 
library(mlr3tuning) 
library(mlr3measures)
library("iml")
library(xtable)
library(caret)
library(patchwork)

#### read data ----

setwd("~/Documents/Projekte/FHSMP/R")
full <- read.csv("data/dataset_kanu.csv", sep = ";", dec = ",")

#### clean data ----

source("src/cleanKanu.R")
kanu.long <- cleanKanu(full)

#### process data ----

# rename variables
test <- kanu.long
test[variable == "Ausdauer_1500m"]$variable <- "Ausd"
test[variable == "Ausdauer_800m"]$variable <- "Ausd"
test[variable == "SH"]$variable <- "h_unten"
test[variable == "KnH"]$variable <- "h_unten"

# reshape data.frame and remove na´s 
kanu         <- dcast(test, ID + session + Geburtsjahr + testjahr + Disziplin + BK ~ variable, value.var = "value")
kanu$alter   <- kanu$testjahr - kanu$Geburtsjahr
kanu         <- kanu[complete.cases(kanu),]
kanu         <- droplevels(kanu)

# group team squats
kanu$BK      <- ifelse(kanu$BK == "NK1", "Landeskader", "Bundeskader")
kanu$BK      <- as.factor(kanu$BK)
kanu$session <- as.factor(kanu$session)
kanu$ID      <- NULL
kanu         <- data.table(kanu)
kanu         <- kanu[,!c("testjahr", "Geburtsjahr","session")]

# remove duplicate ID
kanu <- unique(kanu, by = "ID")
kanu$ID <- NULL

#### mlr3 Benchmark ----

# create task
task = TaskClassif$new(id = "BK", backend = kanu, target = "BK")

# check original class balance
table(task$truth())

# classbalancing
opb = po("classbalancing")
opb$param_set$values = list(ratio = 2.5, reference = "minor",
  adjust = "minor", shuffle = TRUE)
task = opb$train(list(task))[[1L]]
table(task$truth())

# create multiple learner
learners <- c("classif.ranger", "classif.rpart", "classif.log_reg", "classif.naive_bayes", "classif.lda", "classif.featureless")
learners <- lapply(learners, lrn,
				   predict_type = "prob", 
 				   predict_sets = c("train", "test")
)

# create list benchmark grid (task, learners and resampling strategy) and run
# models
resamplings <- rsmp("cv", folds = 10)
design      <- benchmark_grid(task, learners, resamplings)
bmr         <- benchmark(design, store_models = TRUE)

# specify model measures for model accuracy
measures <- list(
  msr("classif.acc"),
  msr("classif.auc"),
  msr("classif.precision"),
  msr("classif.logloss"),
  msr("classif.sensitivity"),
  msr("classif.specificity")
)

# run models
res <- bmr$aggregate(measures)


# print results as LaTeX table 
tab1 <- data.frame(method = res$learner_id, 
				  resampling = res$resampling_id,
				  Accuracy = res$classif.acc, 
				  Auc = res$classif.auc,
				  logloss = res$classif.logloss, 
				  sensivity = res$classif.sensitivity, 
				  specifity = res$classif.specificity)
tab1$method <- gsub("classif.", "", tab1$method)

# save results as table
sink("~/Desktop/models.tex")
print(xtable(tab1,
			 auto = TRUE, 
			 caption = "Vergleich verschiedener Modelle zur Klassifikation des Kaderstatuses (cv =
	10-fold-cross validation).",
			 digits = 2), 
	  booktabs = TRUE,
	  include.rownames = FALSE
)
sink()


#### learn and train  ----

# create task
task = TaskClassif$new(id = "BK", backend = kanu, target = "BK")

# upsampling
opb = po("classbalancing")
opb$param_set$values = list(ratio = 2.5, reference = "minor",
  adjust = "minor", shuffle = FALSE)
task = opb$train(list(task))[[1L]]
table(task$truth())

# run prefered model
learner <- mlr_learners$get("classif.ranger")
learner$predict_type <- "prob"
learner$train(task)
learner$model

# save model/learner
rf_mod <- learner
saveRDS(rf_mod, "./rf_mod.rds")

# print and save confusion matrix
sink("~/Desktop/Shortsave/FHSMP/tex/tab/confmat.tex")
print(xtable(tab2,
			 auto = TRUE, 
			 caption = "Konfusionsmatrix der klassifizierten Fälle für das Testset ",
			 digits = 2), 
	  booktabs = TRUE,
	  include.rownames = FALSE
)
sink()

prediction <- as.data.table(prediction)
caret::confusionMatrix(data = prediction$response, reference = prediction$truth)

sink("~/Desktop/Shortsave/FHSMP/tex/tab/pred.tex")
print(xtable(head(prediction),
			 auto = TRUE, 
			 caption = "Auszug aus den Vorhersagen am Testset",
			 digits = 2), 
	  booktabs = TRUE,
	  include.rownames = FALSE
)
sink()

#### model interpretation -----

# prepare model
x <- kanu[,!"BK"]
model = Predictor$new(learner, data = x, y = kanu$BK)

# feature effects
effect <- FeatureEffects$new(model)
plot(effect, features = names(x))
ggsave(filename = "fig/FeatureEffects.pdf",
	   width = 15,
	   height = 9)

# shapley
x.interest = data.frame(x[2, ])
shapley = Shapley$new(model, x.interest = x.interest)
# labels <- c(Bundeskader = "elite", Landeskader = "sub-elite")
plot(shapley) + theme(text = element_text(size = 20))
# 		+ facet_grid(. ~ class, labeller=labeller(class = labels)) 
ggsave(filename = "fig/shapley.pdf",
	   width = 15,
	   height = 9)

kanu[2,]
learner$predict(task)

# feature importance
effect <- FeatureImp$new(model, loss = "ce")
plot(effect, features = names(x))+ theme(text = element_text(size = 20)) 
ggsave(filename = "fig/featureimportance.pdf",
	   width = 15,
	   height = 9)
# FeatureImp computes feature importance for prediction models. The importance 
# is measured as the factor by which the model's prediction error increases 
# when the feature is shuffled.
