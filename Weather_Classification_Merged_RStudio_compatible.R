# ============================================================
# RSTUDIO COMPATIBILITY ADDITIONS  (added only; original code kept)
# ============================================================
if (interactive()) {
  try(graphics.off(), silent = TRUE)
}

safe_rstudio_plot_device <- function(width = 14, height = 10, mar = c(4, 4, 2, 1), mfrow = NULL) {
  try({
    if (interactive()) {
      if (names(dev.cur()) == "RStudioGD") {
        grDevices::dev.new(width = width, height = height, noRStudioGD = TRUE)
      } else if (dev.cur() == 1) {
        grDevices::dev.new(width = width, height = height)
      }
    }
  }, silent = TRUE)
  try(graphics::par(mar = mar), silent = TRUE)
  if (!is.null(mfrow)) {
    try(graphics::par(mfrow = mfrow), silent = TRUE)
  }
  invisible(NULL)
}

safe_identify <- function(...) {
  tryCatch(
    graphics::identify(...),
    error = function(e) {
      message("identify() skipped in this graphics device: ", conditionMessage(e))
      invisible(NULL)
    }
  )
}

identify <- safe_identify
safe_rstudio_plot_device()

# ============================================================
# Weather Classification — Multivariate Analysis
# Merged Script
# ============================================================
# Notebook H  →  DA on the full dataset (WITH outliers)
# Notebook M  →  DA on the cleaned dataset (WITHOUT outliers)
# ============================================================



# ============================================================
# 0. PACKAGE INSTALLATION  (kept once)
# ============================================================
install.packages("robustX")
install.packages("robustbase")
install.packages("psych")
install.packages("MASS")
install.packages("nnet")



# ============================================================
# 1. DATA LOADING  (kept once)
# ============================================================
url <- "https://raw.githubusercontent.com/HagarSharkawy/Weather-Classification-Multivariate-Analysis/main/weather_classification_data.csv"

df <- read.csv(url)

head(df)
dim(df)

unique(df$"Weather.Type")

numerical_columns <- df[, -c(10, 8, 5, 11)]
numerical_columns



# ============================================================
# 2. EXPLORATION: PAIRWISE SCATTERPLOTS & NORMALITY
# ============================================================

# Pairwise Scatterplot
safe_rstudio_plot_device(width = 14, height = 10, mar = c(4, 4, 2, 1))
pairs(numerical_columns)

# BACON Code (custom bacon.R)
# source("bacon.R")
# Output <- bacon(numerical_columns, v=2, c=5, alpha=.05, bss=1,
#                 pr.opt=0, fname="OutputFile.txt")
# x: the input data (numeric & contains no missing values)
# v: Version 1 or 2
# c: multiplier for determining the size of the basic subset
# alpha: significance level of the test
# bss: 1 = building the basic subset from p+1 to m = c*p
#      otherwise m = c*p is chosen all at once
# known.out: if true outliers are known, Other values
#            indicate that the true outliers are unknown.
# pr.opt: 1 = print full output

# Pairs panels
library(psych)
safe_rstudio_plot_device(width = 14, height = 10, mar = c(4, 4, 2, 1))
pairs.panels(numerical_columns)
dev.new()

# Mahalanobis Q-Q Plot for Multivariate Normality (full dataset)
safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1))
x <- numerical_columns
md <- mahalanobis(x, center = colMeans(x), cov = cov(x))
qqplot(
  qchisq(ppoints(nrow(x)), df = ncol(x)),
  sort(md),
  xlab = "Chi-square quantiles",
  ylab = "Ordered Mahalanobis distances",
  main = "Q-Q Plot for Multivariate Normality"
)
abline(0, 1, col = "red", lty = 2)



# ============================================================
# 3. BACON — WHOLE DATASET
# ============================================================
require(robustX)
library(robustbase)

# Renamed from x / output / y  →  x_full / output_full / y_full
# to avoid collision with the per-class BACON loop variables below
x_full <- as.matrix(numerical_columns)
x_full <- scale(x_full)
output_full <- mvBACON(x_full)

safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1))
plot(x_full, pch=19, main = "Weather Classification data (n = 13200)")
points(x_full[ ! output_full$subset,], pch = 4, col = 2, cex = 1.5)
if (!exists("y_full")) {
  y_full <- cbind(1:nrow(x_full), output_full$dis)
  colnames(y_full) <- c("Index", "Distance")
}
identify(y_full[, 1], y_full[, 2], labels = y_full[, 1])

outlier_indices <- which(!output_full$subset)
cat("Number of outliers:", length(outlier_indices), "\n")
cat("Outlier row indices:", outlier_indices, "\n")
cat("BACON limit:", round(output_full$limit, 4), "\n")

y_full <- cbind(1:13200, output_full$dis)
colnames(y_full) <- c("Index", "Distance")

safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1))
plot(y_full, pch=19, main = "BACON Distances: Weather Classification")
abline(h=output_full$limit, col= "red", lty=2)
points(y_full[ ! output_full$subset, ], pch = 4, col = 2, cex = 1.5)

# Q-Q plot of squared BACON distances
bd_sq_full <- output_full$dis^2

qq_bd_x_full <- qchisq(ppoints(nrow(x_full)), df = ncol(x_full))
qq_bd_y_full <- sort(bd_sq_full)

safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1))
plot(qq_bd_x_full, qq_bd_y_full, pch = 19,
     main = "Q-Q plot of Squared BD vs. quantiles of Chisquare(p)",
     xlab = "qchisq(ppoints(n), df = p)",
     ylab = "BD")
abline(0, 1, col = "blue", lty = 2)
safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1))
tolEllipsePlot(x_full[, c("Temperature", "Humidity")], classic = TRUE)

# Mahalanobis distances (whole dataset, scaled)
md_full   <- mahalanobis(x_full, center = colMeans(x_full), cov = cov(x_full))
md_sq_full <- md_full

# Index plot of Mahalanobis distances
safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1))
plot(md_sq_full, pch = 19,
     main = "Index plot of Mahalanobis distances",
     xlab = "Index", ylab = "Mahalanobis Distance")
abline(h = qchisq(0.975, df = ncol(x_full)), col = "blue", lty = 2)
points(which(md_sq_full > qchisq(0.975, df = ncol(x_full))),
       md_sq_full[md_sq_full > qchisq(0.975, df = ncol(x_full))],
       pch = 4, col = 2, cex = 1.5)

# Q-Q plot of squared Mahalanobis distances vs Chi-square quantiles
qq_md_x_full <- qchisq(ppoints(nrow(x_full)), df = ncol(x_full))
qq_md_y_full <- sort(md_sq_full)

safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1))
plot(qq_md_x_full, qq_md_y_full, pch = 19,
     main = "Q-Q plot of Squared MD vs. quantiles of Chisquare(p)",
     xlab = "qchisq(ppoints(n), df = p)",
     ylab = "MD")
abline(0, 1, col = "blue", lty = 2)



# ============================================================
# 4. BACON — PER CLASS
# ============================================================

# Split dataset into 4 datasets by class
class_list <- split(df, df$Weather.Type)

# Apply: keep only numerical columns in each class
num_class_list <- lapply(class_list, function(x) {
  x[, sapply(x, is.numeric)]
})

# Numerical BACON outputs per class
bacon_results <- list()

for (class_name in names(num_class_list)) {

  x <- as.matrix(num_class_list[[class_name]])
  n <- nrow(x)
  p <- ncol(x)

  cat("\n============================\n")
  cat("Class:", class_name, "| n =", n, "\n")
  cat("============================\n")

  output <- mvBACON(x)

  bacon_results[[class_name]] <- list(
    x               = x,
    n               = n,
    p               = p,
    output          = output,
    outlier_indices = which(!output$subset),
    n_outliers      = sum(!output$subset),
    limit           = output$limit
  )

  cat("Number of outliers:", sum(!output$subset), "\n")
  cat("BACON limit:", round(output$limit, 4), "\n")
}

# Plots per class
for (class_name in names(bacon_results)) {

  x      <- bacon_results[[class_name]]$x
  n      <- bacon_results[[class_name]]$n
  p      <- bacon_results[[class_name]]$p
  output <- bacon_results[[class_name]]$output

  md_dist <- mahalanobis(x, colMeans(x), cov(x))
  bd_dist <- output$dis
  chisq_q <- qchisq(ppoints(n), df = p)

  safe_rstudio_plot_device(width = 14, height = 10, mar = c(4, 4, 2, 1), mfrow = c(2, 2))
  par(mfrow = c(2, 2))

  plot(sort(chisq_q), sort(md_dist), pch = 19, cex = 0.7,
       main = paste("Q-Q MD -", class_name),
       xlab = "qchisq(ppoints(n), df = p)", ylab = "MD")
  abline(0, 1, col = "blue")

  plot(1:n, md_dist, pch = 19, cex = 0.7,
       main = paste("Index Plot MD -", class_name),
       xlab = "Index", ylab = "MD")
  abline(h = sqrt(qchisq(0.975, df = p)), col = "blue", lty = 2)

  plot(sort(chisq_q), sort(bd_dist), pch = 19, cex = 0.7,
       main = paste("Q-Q BACON -", class_name),
       xlab = "qchisq(ppoints(n), df = p)", ylab = "BD")
  abline(0, 1, col = "blue")

  plot(1:n, bd_dist, pch = 19, cex = 0.7,
       main = paste("Index Plot BACON -", class_name),
       xlab = "Index", ylab = "BD")
  abline(h = output$limit, col = "red", lty = 2)
  points(which(!output$subset), bd_dist[!output$subset],
         pch = 4, col = 2, cex = 1.5)
          identify(1:n, bd_dist, labels = 1:n)

  par(mfrow = c(1, 1))
}


# ============================================================
# 5. OUTLIER REMOVAL
# ============================================================

# Initialize a list to store the datasets after outlier removal
cleaned_class_list <- list()

cat("=== Outlier Removal Summary ===\n")

for (class_name in names(bacon_results)) {
  # 1. Get the original full dataset for this specific class (includes Weather.Type)
  original_data <- class_list[[class_name]]

  # 2. Extract the 'subset' logical vector from BACON results
  # (TRUE = good observation, FALSE = outlier)
  good_data_indices <- bacon_results[[class_name]]$output$subset

  # 3. Filter the original data to keep only the non-outliers
  cleaned_class_list[[class_name]] <- original_data[good_data_indices, ]

  # Print a quick sanity check to ensure the math adds up
  cat("Class:", class_name,
      "| Original N:", nrow(original_data),
      "| Outliers Removed:", sum(!good_data_indices),
      "| New N:", nrow(cleaned_class_list[[class_name]]), "\n")
}
cat("===============================\n")

# 4. Combine all the cleaned class datasets back into one single dataframe
disc_data <- do.call(rbind, cleaned_class_list)

# 5. Reset the row names so they are sequential (1 to N) rather than messy inherited names
rownames(disc_data) <- NULL

# Verify the final dataset
cat("\nFinal combined dataset 'disc_data' has", nrow(disc_data), "rows and", ncol(disc_data), "columns.\n")

# Ensure disc_data is clean (no NA values before modeling)
disc_data_clean <- na.omit(disc_data)



# ============================================================
# 6. NORMALITY CHECKS (post-outlier-removal)
# ============================================================

# Isolate numeric columns
num_cols     <- sapply(disc_data_clean, is.numeric)
numeric_data <- disc_data_clean[, num_cols]

# Q-Q plots for each numeric variable
safe_rstudio_plot_device(width = 14, height = 10, mar = c(4, 4, 2, 1), mfrow = c(ceiling(ncol(numeric_data)/3), 3))
par(mfrow = c(ceiling(ncol(numeric_data)/3), 3))
for (col_name in names(numeric_data)) {
  qqnorm(numeric_data[[col_name]], main = paste("Q-Q Plot:", col_name), pch = 16, col = "darkgrey")
  qqline(numeric_data[[col_name]], col = "red", lwd = 2)
}
par(mfrow = c(1, 1))

# Pairs panels on cleaned numeric data
safe_rstudio_plot_device(width = 14, height = 10, mar = c(4, 4, 2, 1))
pairs.panels(numeric_data, method = "pearson", hist.col = "lightblue", density = TRUE)



# ============================================================
# 7. FUNCTION DEFINITIONS  (defined once; used in both sections below)
# ============================================================
library(MASS)

# Fisher Linear Discriminant Analysis — Internal Validation
flda = function(x, class) {
  cat("Fisher Linear Discriminant:\n")
  a = lda(x, class); d = predict(a)
  t = table(class, d$class); print(t)
  er = 100*(sum(t)-sum(diag(t)))/nrow(x)
  cat("Error Rate =", er, "%\n")
  return(d)
}

# Fisher Linear Discriminant Analysis — External Validation
flda_ext <- function(x_train, class_train, x_test, class_test) {
  cat("Fisher Linear Discriminant (External Validation):\n")
  a <- lda(x_train, class_train)
  d <- predict(a, newdata = x_test)
  t <- table(class_test, d$class); print(t)
  er <- 100 * (sum(t) - sum(diag(t))) / nrow(x_test)
  cat("Error Rate =", er, "%\n")
  return(d)
}

# FLDA for p = 2, k = 2
flda2 = function(x, class) {
  if(ncol(x)!=2) {cat("Data should be 2-dimensional\n"); return()}
  t=factor(class); level=levels(t);
  if(length(level)!=2) {cat("Data should have only two groups\n"); return()}
  y=x[class==level[1],]; x=x[class==level[2],]
  n1=nrow(x); n2=nrow(y); n=n1+n2; xcenter=colMeans(x); ycenter=colMeans(y)
  xcov=cov(x); ycov=cov(y); sp=(n1-1)*xcov+(n2-1)*ycov; sp=sp/(n-2)
  d=xcenter-ycenter; m=(xcenter+ycenter)/2; a=solve(sp)%*%d;
  class=c(rep(1,n1),rep(2,n2)); p=1;
  z=rbind(x,y); pred=z-matrix(m,ncol=2,nrow=n, byrow=T); pred=as.matrix(pred)
  pred=(pred%*%a)<log(p); C=(class!=pred+1); ce=sum(C)
  cat("--------------------------------------------------\n")
  cat(" Correct Incorrect\n Class Classification Classification Total\n")
  cat("--------------------------------------------------\n")
  cd1=n1-sum(C[1:n1]); cat(" 1 ",cd1," ", n1-cd1," ",n1,"\n")
  cd2=n2-sum(C[(n1+1):n]); cat(" 2 ",cd2," ", n2-cd2," ",n2,"\n")
  cat("--------------------------------------------------\n")
  cat(" Total: ",cd1+cd2," ", n-(cd1+cd2)," ",n,"\n")
  cat("--------------------------------------------------\n")
  cat("Error Rate = ",100*(ce/n),"%\n"); const=(sum(a*m)+log(p))/a[2]; slope=-a[1]/a[2]
  z=rbind(x,y); #print(rbind(xcenter[1:2],ycenter[1:2]))
  safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1))
  plot(z[,c(1,2)],col=class,pch=19); abline(const,slope, col = 'blue');
  points(rbind(xcenter[1:2],ycenter[1:2]),pch=19,col=3,cex=1.5);
  segments(xcenter[1], xcenter[2], ycenter[1], ycenter[2],col=3)
  list(xcenter=xcenter[2:1],ycenter=ycenter[2:1],xcov=xcov,ycov=ycov,sp=sp,a=a,slope=slope,
       const=const,ce=ce,m=m,z=z)
}



# ============================================================
# 8. DISCRIMINANT ANALYSIS — WITH OUTLIERS (Full Dataset)
# ============================================================
# Renamed from disc_data / disc_data_clean → disc_data_full / disc_data_full_clean
# to distinguish from the cleaned dataset (disc_data) built in Section 5

z <- scale(numerical_columns)
disc_data_full <- data.frame(z, Weather.Type = df$Weather.Type)
disc_data_full

any(is.na(disc_data_full))

disc_data_full_clean <- na.omit(disc_data_full)

# --- Internal Validation ---
rslt_flda_int_full <- flda(disc_data_full_clean[, 1:7], disc_data_full_clean[, 8])
rslt_flda_int_full

# --- FLDA2 (Snowy vs Sunny, p = 2) ---
rows    <- which(disc_data_full$"Weather.Type" == "Snowy" | disc_data_full$"Weather.Type" == "Sunny")
columns <- c(1, 2)
x       <- disc_data_full[rows, columns]
class   <- disc_data_full[rows, "Weather.Type"]
safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1))
plot(x, pch = 19, col = as.factor(class))
a <- flda2(x, class)
a$means

# --- External Validation ---
# Renamed: n → n_full, train_idx → train_idx_full
#          x_train/class_train/x_test/class_test → X_train_full/Y_train_full/X_test_full/Y_test_full
set.seed(123456)
n_full         <- nrow(disc_data_full)
train_idx_full <- sample(1:n_full, size = 0.8 * n_full)

X_train_full <- disc_data_full[ train_idx_full, 1:7]
Y_train_full <- disc_data_full[ train_idx_full, 8]
X_test_full  <- disc_data_full[-train_idx_full, 1:7]
Y_test_full  <- disc_data_full[-train_idx_full, 8]

cat("--- Internal Validation ---\n")
rslt_flda_int_full <- flda(disc_data_full[, 1:7], disc_data_full[, 8])

cat("\n--- External Validation ---\n")
rslt_flda_ext_full <- flda_ext(X_train_full, Y_train_full, X_test_full, Y_test_full)

# --- Multinomial: Internal Validation ---
# Renamed: mn → mn_int_full, results → results_mn_int_full, probs → probs_mn_int_full
#          t_int → t_mn_int_full, er_int → er_mn_int_full
library(nnet)

mn_int_full         <- multinom(disc_data_full[, "Weather.Type"] ~ ., data = disc_data_full)
results_mn_int_full <- predict(mn_int_full)
probs_mn_int_full   <- predict(mn_int_full, type = "probs")

t_mn_int_full  <- table(disc_data_full[, "Weather.Type"], results_mn_int_full)
print(t_mn_int_full)
er_mn_int_full <- 100 * (sum(t_mn_int_full) - sum(diag(t_mn_int_full))) / nrow(disc_data_full)
cat("Error Rate =", er_mn_int_full, "%\n")

# --- Multinomial: External Validation ---
# Renamed: train → train_full, test → test_full
#          mn_ext → mn_ext_full, results_ext → results_mn_ext_full
#          t_ext → t_mn_ext_full, er_ext → er_mn_ext_full
train_full <- disc_data_full[ train_idx_full, ]
test_full  <- disc_data_full[-train_idx_full, ]

mn_ext_full         <- multinom(Weather.Type ~ ., data = train_full)
results_mn_ext_full <- predict(mn_ext_full, newdata = test_full)

t_mn_ext_full  <- table(test_full$Weather.Type, results_mn_ext_full)
print(t_mn_ext_full)
er_mn_ext_full <- 100 * (sum(t_mn_ext_full) - sum(diag(t_mn_ext_full))) / nrow(test_full)
cat("External Error Rate =", er_mn_ext_full, "%\n")



# ============================================================
# 9. DISCRIMINANT ANALYSIS — WITHOUT OUTLIERS (Cleaned Dataset)
# ============================================================

# --- Scale the Data ---
disc_data_scaled              <- disc_data_clean
disc_data_scaled[, num_cols]  <- scale(numeric_data)

# Define X (predictors) and Y (class)
X <- disc_data_scaled[, num_cols]
Y <- disc_data_scaled$Weather.Type

# --- FLDA: Internal Validation ---
cat("\n--- FLDA: Internal Validation ---\n")
rslt_flda_int <- flda(X, Y)

# Split data 70/30 for External Validation
set.seed(123456)
n         <- nrow(disc_data_scaled)
train_idx <- sample(1:n, size = 0.8* n)

X_train <- X[ train_idx, ]
Y_train <- Y[ train_idx]
X_test  <- X[-train_idx, ]
Y_test  <- Y[-train_idx]

# --- FLDA: External Validation ---
cat("\n--- FLDA: External Validation ---\n")
rslt_flda_ext <- flda_ext(X_train, Y_train, X_test, Y_test)

# --- FLDA2 (p = 2, k = 2) ---
cat("\n--- FLDA2 (p=2, k=2) ---\n")

# Filter for exactly two classes (Snowy and Sunny)
flda2_rows <- which(disc_data_scaled$Weather.Type %in% c("Snowy", "Sunny"))

# Select exactly 2 numeric columns (first two numeric columns)
flda2_cols <- which(num_cols)[1:2]

X_flda2 <- disc_data_scaled[flda2_rows, flda2_cols]
Y_flda2 <- as.factor(disc_data_scaled$Weather.Type[flda2_rows])

# Dropping unused factor levels is crucial for FLDA2 to recognise exactly 2 levels
Y_flda2 <- droplevels(Y_flda2)

# Run FLDA2 and plot
safe_rstudio_plot_device(width = 12, height = 8, mar = c(4, 4, 2, 1), mfrow = c(1, 1))
par(mfrow = c(1, 1))
flda2_result <- flda2(X_flda2, Y_flda2)

# --- Multinomial: Internal Validation ---
cat("\n--- Multinomial: Internal Validation ---\n")

mn_int         <- multinom(Weather.Type ~ ., data = disc_data_scaled, trace = FALSE)
results_mn_int <- predict(mn_int)

t_mn_int  <- table(disc_data_scaled$Weather.Type, results_mn_int)
print(t_mn_int)
er_mn_int <- 100 * (sum(t_mn_int) - sum(diag(t_mn_int))) / nrow(disc_data_scaled)
cat("Multinomial Internal Error Rate =", er_mn_int, "%\n")

# --- Multinomial: External Validation ---
cat("\n--- Multinomial: External Validation ---\n")

train_data <- disc_data_scaled[ train_idx, ]
test_data  <- disc_data_scaled[-train_idx, ]

mn_ext         <- multinom(Weather.Type ~ ., data = train_data, trace = FALSE)
results_mn_ext <- predict(mn_ext, newdata = test_data)

t_mn_ext  <- table(test_data$Weather.Type, results_mn_ext)
print(t_mn_ext)
er_mn_ext <- 100 * (sum(t_mn_ext) - sum(diag(t_mn_ext))) / nrow(test_data)
cat("Multinomial External Error Rate =", er_mn_ext, "%\n")



# ============================================================
# 10. EXTRACTING OUTPUTS (Without-Outliers DA)
# ============================================================

# --- Internal Validation Outputs ---
conf_matrix_int <- table(Actual = Y, Predicted = rslt_flda_int$class)
cat("\n--- 1. Confusion Matrix (Internal) ---\n")
print(conf_matrix_int)

cat("\n--- 2. Predicted Classes (Internal, first 15) ---\n")
print(head(rslt_flda_int$class, 15))

cat("\n--- 3. Posterior Probabilities (Internal, first 15) ---\n")
print(head(rslt_flda_int$posterior, 15))

# --- External Validation Outputs ---
conf_matrix_ext <- table(Actual = Y_test, Predicted = rslt_flda_ext$class)
cat("\n--- 1. Confusion Matrix (External) ---\n")
print(conf_matrix_ext)

cat("\n--- 2. Predicted Classes (External, first 15) ---\n")
print(head(rslt_flda_ext$class, 15))

cat("\n--- 3. Posterior Probabilities (External, first 15) ---\n")
print(head(rslt_flda_ext$posterior, 15))


