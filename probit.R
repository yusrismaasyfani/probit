# Library
library(rmarkdown)
library(prettydoc)
library(tinytex)
library(readxl)
library(pscl)
library(ResourceSelection)
library(modEvA)
library(graphics)
library(car)
library(stats)


#memanggil data
data1 = read_excel("E:/skripsyen/data bab 4 fix/klaster1.xlsx")

# Normalisasi data menggunakan Z-Score Scaling
normalized_data <- data1
normalized_data[, -1] <- scale(normalized_data[, -1])
print(normalized_data)

print(normalized_data, n = nrow(normalized_data))

#model probit
Model_Probit <- glm(Y ~X1 + X2 + X3 + X4, data = normalized_data, family = binomial(link = "probit"))
summary(Model_Probit)


#uji parsial
summary(Model_Probit)
#tolak H0/ var berpengaruh p-value<alfa(0,1)
# Mengekstrak hasil uji parsial untuk variabel X1
summary(Model_Probit)$coefficients["X1", c("Estimate", "Std. Error", "z value", "Pr(>|z|)")]


# Jumlah observasi
n_obs <- nrow(data1)

# Jumlah parameter
n_param <- length(coef(Model_Probit)) - 1  # Exclude the intercept term

# Derajat kebebasan
df <- n_obs - n_param

# Menampilkan hasil
print(paste("Degrees of Freedom:", df))

#Uji Kecocokan  Model
hoslem.test(Model_Probit$y, fitted(Model_Probit))
qchisq(0.90, 13)
#tolak H0/ model sesuai chisquared<chitabel

# Menghitung prediktor linear (Z)
Z <- Model_Probit$linear.predictors

# Menghitung PDF distribusi normal pada Z
pdf_Z <- dnorm(Z)

# Membuat data frame untuk menyimpan hasil
results <- data.frame(Z = Z, pdf_Z = pdf_Z)

# Menghitung efek marginal untuk setiap variabel
for (var in names(Model_Probit$coefficients)[-1]) {  # Menghindari intercept
  coef_var <- coef(Model_Probit)[var]
  results[[paste("marginal_effects", var, sep = "_")]] <- coef_var * pdf_Z
}

# Menyertakan nilai asli variabel independen dalam hasil
for (var in names(Model_Probit$coefficients)[-1]) {  # Menghindari intercept
  results[[var]] <- normalized_data[[var]]
}

# Menampilkan hasil
print(results)

