# tidy up
rm(list = ls())
gc(reset = TRUE)
# Set-Up
set.seed(5000)


# Load all
devtools::load_all()

load("/Users/rkruse/Git/bivarte-discriminant-package/misc/nutripoverty.rda")
nutripoverty <- as.data.frame(nutripoverty)

# Anwendung
# myecdf(data = total1, var = "INval")
tt <- bivqfun(data = nutripoverty, var1="NUval", var2="INval", tau = 0.25)
# plot(tt)
summary(tt)
