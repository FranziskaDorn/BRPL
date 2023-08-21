# tidy up
rm(list = ls())
gc(reset = TRUE)
# Set-Up
set.seed(5000)

load("data/total.Rda")
load("data/total1.Rda")

# Load all
devtools::load_all()

# Anwendung
# myecdf(data = total1, var = "INval")


bivqfun(data = total1, var1="NUval", var2="INval", tau = 0.25)

data("nutritionpoverty")
bivqfun(data = nutritionpoverty, var1="NUval", var2="INval", tau = 0.25)
