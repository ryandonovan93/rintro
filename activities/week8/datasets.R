

participant_id <- c(1:200)
group <- rep(c("Control", "Low Caffeine", "Moderate Caffeine", "High Caffeine"), each = 50)
mood <- round(rnorm(mean = 4, sd = 2, n = 200), 2)

mood <- sqrt(mood*mood)


example_data <- data.frame(
  participant_id,
  group,
  mood
)


summary(example_data)


write.csv(example_data, "caffeine_mood.csv", row.names = F)













# Set seed for reproducibility
set.seed(4311111)

# Define groups and sample size per group
groups <- c("Control", "Low Caffeine", "Moderate Caffeine", "High Caffeine")
n_per_group <- 50  # 50 participants per group
total_n <- n_per_group * length(groups)  # Total number of participants

# Define means to ensure significant differences
means <- c(4, 5.5, 6, 8.5)  # Higher caffeine levels lead to higher mood scores
std_dev <- 2  # Standard deviation

# Generate participant IDs
participant_id <- 1:total_n

# Generate mood scores with significant differences
mood <- rnorm(total_n, mean = rep(means, each = n_per_group), sd = std_dev)

# Ensure mood scores are all positive using absolute values
mood <- sqrt(abs(mood))

# Create corresponding group assignments
group <- rep(groups, each = n_per_group)

# Create data frame in long format
example_data <- data.frame(
  Participant_ID = participant_id,
  Group = factor(group, levels = groups),  # Ensure factor levels
  Mood = mood
)

# View summary
summary(example_data)



# Print first few rows to verify structure
head(example_data)

# Define function to replace outliers using the IQR method
replace_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  # Define bounds
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  # Replace outliers with the closest boundary value
  df[[column]] <- ifelse(df[[column]] < lower_bound, lower_bound,
                         ifelse(df[[column]] > upper_bound, upper_bound, df[[column]]))
  return(df)
}


# Remove outliers from Mood scores
example_data <- replace_outliers(example_data, "Mood")

# View new summary
summary(example_data)

descriptives(data = example_data, vars = "Mood", splitBy = "Group",
             box = TRUE, hist = TRUE, dens = TRUE)


example_data <- example_data %>%
  mutate(Group = case_when(Group == "Moderate Caffeine" ~ "High Caffeine",
                           Group == "High Caffeine" ~ "Moderate Caffeine",
                           TRUE ~ Group)) %>%
  mutate(Group = factor(Group, levels = c("Control", "Low Caffeine",
                                          "Moderate Caffeine", "High Caffeine"))) %>%
  arrange(Group)



# Save dataset
write.csv(example_data, "activities/week8/caffeine_mood.csv", row.names = FALSE)







anova_one <- aov_ez(id = "Participant_ID",
                    dv = "Mood",
                    between = "Group", #what our IV is
                    es = "pes",
                    type = 3,
                    include_aov = TRUE,
                    data = example_data_changed)


anova(anova_one)


qqPlot(anova_one$aov$residuals)
shapiro.test(anova_one$aov$residuals)
test_levene(anova_one)

AOV1_pairwise <-emmeans(anova_one, pairwise ~ Group,
                        adjust = "bonferroni")

AOV1_pairwise







# Factorial ANOVA ---------------------------------------------------------
# Load necessary packages
library(tidyverse)
library(afex)  # For ANOVA
library(emmeans)  # For post-hoc tests

# Set seed for reproducibility
set.seed(123)

# Define sample size
n_per_group <- 30  # 30 participants per therapy type per treatment length
n_total <- n_per_group * 3 * 2  # Total sample size (Mindfulness, CBT, DBT × 6 weeks, 12 weeks)

# Create data frame with participant ID, therapy type, and treatment length
df <- data.frame(
  ID = 1:n_total,
  Therapy = rep(c("Mindfulness", "CBT", "DBT"), each = n_per_group * 2),
  Length = rep(rep(c("6 Weeks", "12 Weeks"), each = n_per_group), times = 3)
)

# Generate Anxiety Scores
# Baseline Anxiety Scores (Pre-Treatment) - all start from around the same level
df$Anxiety_Pre <- rnorm(n_total, mean = 70, sd = 10)

# Define reductions in anxiety for each therapy type and length of treatment
mindfulness_6wk <- rnorm(n_per_group, mean = 10, sd = 5)  # Least effective
mindfulness_12wk <- rnorm(n_per_group, mean = 15, sd = 5)  # Somewhat better

cbt_6wk <- rnorm(n_per_group, mean = 20, sd = 5)  # Stronger effect
cbt_12wk <- rnorm(n_per_group, mean = 30, sd = 5)  # Much stronger effect

dbt_6wk <- rnorm(n_per_group, mean = 22, sd = 5)  # Similar to CBT
dbt_12wk <- rnorm(n_per_group, mean = 30, sd = 5)  # Similar to CBT

# Apply reductions based on therapy and treatment length
df$Anxiety_Post <- df$Anxiety_Pre  # Start with pre-treatment scores

df$Anxiety_Post[df$Therapy == "Mindfulness" & df$Length == "6 Weeks"] <-
  df$Anxiety_Pre[df$Therapy == "Mindfulness" & df$Length == "6 Weeks"] - mindfulness_6wk

df$Anxiety_Post[df$Therapy == "Mindfulness" & df$Length == "12 Weeks"] <-
  df$Anxiety_Pre[df$Therapy == "Mindfulness" & df$Length == "12 Weeks"] - mindfulness_12wk

df$Anxiety_Post[df$Therapy == "CBT" & df$Length == "6 Weeks"] <-
  df$Anxiety_Pre[df$Therapy == "CBT" & df$Length == "6 Weeks"] - cbt_6wk

df$Anxiety_Post[df$Therapy == "CBT" & df$Length == "12 Weeks"] <-
  df$Anxiety_Pre[df$Therapy == "CBT" & df$Length == "12 Weeks"] - cbt_12wk

df$Anxiety_Post[df$Therapy == "DBT" & df$Length == "6 Weeks"] <-
  df$Anxiety_Pre[df$Therapy == "DBT" & df$Length == "6 Weeks"] - dbt_6wk

df$Anxiety_Post[df$Therapy == "DBT" & df$Length == "12 Weeks"] <-
  df$Anxiety_Pre[df$Therapy == "DBT" & df$Length == "12 Weeks"] - dbt_12wk

# Ensure no negative anxiety scores
df$Anxiety_Post <- pmax(df$Anxiety_Post, 0)

# Convert variables to factors
df$Therapy <- factor(df$Therapy, levels = c("Mindfulness", "CBT", "DBT"))
df$Length <- factor(df$Length, levels = c("6 Weeks", "12 Weeks"))

# Conduct Factorial ANOVA
anova_model <- aov(Anxiety_Post ~ Therapy * Length, data = df)
summary(anova_model)

# Post-hoc tests using estimated marginal means (pairwise comparisons)
model <- aov_car(Anxiety_Post ~ Therapy * Length, data = df)
emmeans_results <- emmeans(anova_model, pairwise ~ Therapy, adjust = "bonferroni")
print(emmeans_results)

# Interaction effect visualization
df %>%
  group_by(Therapy, Length) %>%
  summarise(Mean_Anxiety = mean(Anxiety_Post), .groups = "drop") %>%
  ggplot(aes(x = Length, y = Mean_Anxiety, group = Therapy, color = Therapy)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Interaction Effect of Therapy Type and Treatment Length on Anxiety Reduction",
       x = "Length of Treatment",
       y = "Mean Anxiety Score") +
  theme_minimal()





# Factorial ANOVA Simplified ----------------------------------------------

# Load necessary packages
library(tidyverse)
library(afex)  # For ANOVA
library(emmeans)  # For post-hoc tests

# Set seed for reproducibility
set.seed(12)

# Define sample size
n_per_group <- 30  # 30 participants per therapy type per treatment Duration
n_total <- n_per_group * 3 * 2  # Total sample size (3 therapies × 2 durations)

# Create data frame with participant ID, therapy type, and treatment Duration
df <- data.frame(
  ID = 1:n_total,
  Therapy = rep(c("Mindfulness", "CBT", "DBT"), each = n_per_group * 2),
  Duration = rep(rep(c("6 Weeks", "12 Weeks"), each = n_per_group), times = 3)
)

# Generate Anxiety Scores
# Higher scores = More Anxiety, Lower scores = More Effective Treatment
# Define post-treatment anxiety levels
mindfulness_6wk <- rnorm(n_per_group, mean = 65, sd = 7)  # Least effective
mindfulness_12wk <- rnorm(n_per_group, mean = 60, sd = 7)  # Some improvement

cbt_6wk <- rnorm(n_per_group, mean = 50, sd = 7)  # Strong effect
cbt_12wk <- rnorm(n_per_group, mean = 40, sd = 7)  # Much stronger effect

dbt_6wk <- rnorm(n_per_group, mean = 48, sd = 7)  # Similar to CBT
dbt_12wk <- rnorm(n_per_group, mean = 39, sd = 7)  # Similar to CBT, slightly better

# Assign post-treatment anxiety scores based on therapy type and duration
df$Anxiety <- NA  # Initialize column

df$Anxiety[df$Therapy == "Mindfulness" & df$Duration == "6 Weeks"] <- mindfulness_6wk
df$Anxiety[df$Therapy == "Mindfulness" & df$Duration == "12 Weeks"] <- mindfulness_12wk
df$Anxiety[df$Therapy == "CBT" & df$Duration == "6 Weeks"] <- cbt_6wk
df$Anxiety[df$Therapy == "CBT" & df$Duration == "12 Weeks"] <- cbt_12wk
df$Anxiety[df$Therapy == "DBT" & df$Duration == "6 Weeks"] <- dbt_6wk
df$Anxiety[df$Therapy == "DBT" & df$Duration == "12 Weeks"] <- dbt_12wk

# Convert variables to factors
df$Therapy <- factor(df$Therapy, levels = c("Mindfulness", "CBT", "DBT"))
df$Duration <- factor(df$Duration, levels = c("6 Weeks", "12 Weeks"))

# Conduct Factorial ANOVA
anova_model <- aov(Anxiety ~ Therapy * Duration, data = df)
summary(anova_model)

# Post-hoc tests using estimated marginal means (pairwise comparisons)
emmeans_results <- emmeans(anova_model, pairwise ~ Therapy, adjust = "bonferroni")
print(emmeans_results)

# Visualization of interaction effect
df %>%
  group_by(Therapy, Duration) %>%
  summarise(Mean_Anxiety = mean(Anxiety), .groups = "drop") %>%
  ggplot(aes(x = Duration, y = Mean_Anxiety, group = Therapy, color = Therapy)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Interaction Effect of Therapy Type and Treatment Duration on Anxiety",
       x = "Duration of Treatment",
       y = "Mean Anxiety Score (Lower = Less Anxiety)") +
  theme_minimal()


df <- df %>%
  select(ID, Anxiety, Therapy, Duration)


descriptives(data = df,
             vars = Anxiety,
             splitBy = c("Therapy", "Duration"),
             min = F,
             max = F,
             box = T,
             hist = T,
             bar = T,
             dens = T)



write.csv(df, "anxiety_treatment.csv", row.names = F)




# Factorial ANOVA Exercise ------------------------------------------------

# Set seed for reproducibility
set.seed(123)

# Define sample size
n_per_group <- 30  # 30 participants per exercise and diet condition
n_total <- n_per_group * 3 * 2  # 3 Exercise Types × 2 Diet Types

# Create a data frame with Exercise Type and Diet Type
df <- data.frame(
  ID = 1:n_total,
  Exercise = rep(c("None", "Cardio", "Strength"), each = n_per_group * 2),
  Diet = rep(rep(c("Standard", "High-Protein"), each = n_per_group), times = 3)
)

# Generate Weight Loss (Dependent Variable)
# Baseline Mean Weight Loss by Exercise Type
none_weight_loss <- rnorm(n_per_group * 2, mean = 1, sd = 1)  # Minimal weight loss
cardio_weight_loss <- rnorm(n_per_group * 2, mean = 5, sd = 1)  # Higher weight loss
strength_weight_loss <- rnorm(n_per_group * 2, mean = 4.8, sd = 1)  # Similar to cardio

# Assign weight loss values based on exercise group (Diet has no effect)
df$Weight_Loss <- NA

df$Weight_Loss[df$Exercise == "None"] <- none_weight_loss
df$Weight_Loss[df$Exercise == "Cardio"] <- cardio_weight_loss
df$Weight_Loss[df$Exercise == "Strength"] <- strength_weight_loss



# Conduct Factorial ANOVA
anova_model <- aov_ez(id = "ID",
                      dv = "Weight_Loss",
                      between = c("Exercise", "Diet"),
                      data = df,
                      type = 3)

# Print ANOVA results
print(anova_model)

# Post-hoc tests
emmeans_results <- emmeans(anova_model, pairwise ~ Exercise, adjust = "bonferroni")
print(emmeans_results)

# Interaction plot
afex_plot(anova_model,
          x = "Diet",
          trace = "Exercise",
          error = "between",
          mapping = c("color", "linetype")) +
  theme_classic()

write.csv(df, "activities/week8/diet_exercise.csv", row.names = F)







