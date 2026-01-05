# -------------------------------
# STEP 1: Check R is working
# -------------------------------
print("R is working")

# -------------------------------
# STEP 2: Load the dataset
# (Make sure output_file.csv is in the same folder)
# -------------------------------
df <- read.csv("output_file.csv")

# -------------------------------
# STEP 3: Inspect the data
# -------------------------------
head(df)
str(df)
summary(df)
colSums(is.na(df))

# -------------------------------
# STEP 4: Clean the target variable
# -------------------------------
# Remove rows where success is missing
df <- df[!is.na(df$success), ]

# Convert TRUE/FALSE to 1/0
df$success <- as.numeric(df$success)

# -------------------------------
# STEP 5: Install & load required package
# (install.packages runs only the first time)
# -------------------------------
install.packages("lubridate")   # run once, safe if already installed
library(lubridate)

# -------------------------------
# STEP 6: Handle date and create new feature
# -------------------------------
df$date_utc <- ymd_hms(df$date_utc)
df$launch_year <- year(df$date_utc)

# -------------------------------
# STEP 7: Handle missing values
# -------------------------------
df$window[is.na(df$window)] <- 0

# -------------------------------
# STEP 8: Create clean dataset
# -------------------------------
df_clean <- df[, c(
  "flight_number",
  "rocket",
  "launchpad",
  "launch_year",
  "window",
  "success"
)]

# -------------------------------
# STEP 9: Final check
# -------------------------------
str(df_clean)
head(df_clean)


install.packages("ggplot2")

# -------------------------------
# STEP 4: Exploratory Data Analysis
# -------------------------------

library(ggplot2)

# Overall success distribution
ggplot(df_clean, aes(x = factor(success))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Overall Launch Success Distribution",
    x = "Launch Outcome (0 = Fail, 1 = Success)",
    y = "Number of Launches"
  )



df_year <- aggregate(success ~ launch_year, df_clean, mean)

ggplot(df_year, aes(x = launch_year, y = success)) +
  geom_line(color = "darkgreen") +
  geom_point(size = 2) +
  labs(
    title = "Launch Success Rate Over Years",
    x = "Launch Year",
    y = "Success Rate"
  )





ggplot(df_clean, aes(x = launchpad, fill = factor(success))) +
  geom_bar(position = "fill") +
  labs(
    title = "Success Rate by Launchpad",
    x = "Launchpad",
    y = "Proportion",
    fill = "Success"
  )


  # Convert categorical variables to factors
df_model <- df_clean
df_model$rocket <- as.factor(df_model$rocket)
df_model$launchpad <- as.factor(df_model$launchpad)

set.seed(123)

train_index <- sample(seq_len(nrow(df_model)), size = 0.8 * nrow(df_model))

train_data <- df_model[train_index, ]
test_data  <- df_model[-train_index, ]




model <- glm(
  success ~ flight_number + rocket + launchpad + launch_year + window,
  data = train_data,
  family = binomial
)

summary(model)



pred_prob <- predict(model, test_data, type = "response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)



accuracy <- mean(pred_class == test_data$success)
accuracy
library(ggplot2)


ggplot(df_clean, aes(x = factor(success))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Overall Launch Success Distribution",
    x = "Launch Outcome",
    y = "Count"
  )
library(ggplot2)

p <- ggplot(df_clean, aes(x = factor(success))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Overall Launch Success Distribution",
    x = "Launch Outcome (0 = Failure, 1 = Success)",
    y = "Count"
  )
windows()


print(p)
library(ggplot2)

windows()   # FORCE plot window on Windows

p <- ggplot(df_clean, aes(x = factor(success))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Overall Launch Success Distribution",
    x = "Launch Outcome (0 = Failure, 1 = Success)",
    y = "Count"
  )

print(p)


View(df_clean)
