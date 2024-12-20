# tidy up
rm(list = ls())
gc(reset = TRUE)
# Set-Up
set.seed(5000)


# Load all
devtools::load_all()

data("nutritionpoverty")

# Anwendung
# myecdf(data = total1, var = "INval")
tt <- bivqfun(data = nutritionpoverty, var1="NUval", var2="INval", tau = 0.25)
plot(tt)

