#title: "Konya_basin_research"
#date: "2025-09-24"

install.packages(c("xfun","knitr","rmarkdown"), dependencies = TRUE)

# If you use tinytex on the knitting side:
# install.packages("tinytex")

# Check:
packageVersion("xfun")  # should be >= '0.51'

# Current library paths
.libPaths()

# Where is xfun installed from?
find.package("xfun", .libPaths(), quiet = TRUE)

# Remove first, then reinstall
remove.packages("xfun")
install.packages("xfun", dependencies = TRUE)

install.packages("xfun", type = "binary")
install.packages("renv")

# Restore versions from lockfile
renv::restore()

# Load required libraries
library(tidyverse)
library(readr)

# Set working directory (folder containing data files)
setwd("E:/Konya_basin_research")

# Read all sub-basin files
Aksaray <- read_delim("Aksaray.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Altinekin <- read_delim("Altinekin.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Beysehir <- read_delim("Beysehir.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Cihanbeyli <- read_delim("Cihanbeyli.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Eregli <- read_delim("Eregli.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Karaman <- read_delim("Karaman.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Konya <- read_delim("Konya.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Melendiz <- read_delim("Melendiz.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Sereflikochisar <- read_delim("Sereflikochisar.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)


#====Exploratory analysis==========#

setwd("E:/Konya_basin_research")

# ==== Required libraries ====
library(tidyverse)   # includes dplyr, ggplot2, readr

# ==== Merge all data frames (sub-basin names in English) ====
df <- bind_rows(
  Aksaray %>% mutate(sub_basin = "Aksaray"),
  Altinekin %>% mutate(sub_basin = "Altinekin"),
  Beysehir %>% mutate(sub_basin = "Beysehir"),
  Cihanbeyli %>% mutate(sub_basin = "Cihanbeyli"),
  Eregli %>% mutate(sub_basin = "Eregli"),
  Karaman %>% mutate(sub_basin = "Karaman"),
  Konya %>% mutate(sub_basin = "Konya"),
  Melendiz %>% mutate(sub_basin = "Melendiz"),
  Sereflikochisar %>% mutate(sub_basin = "Sereflikochisar")
)

# ==== Check data structure ====
glimpse(df)

# ==== Basic summary statistics ====
cat("=== DATASET SUMMARY ===\n")
cat("Total observations:", nrow(df), "\n")
cat("Number of sub-basins:", length(unique(df$sub_basin)), "\n")
cat("Sub-basins:", paste(unique(df$sub_basin), collapse = ", "), "\n")
cat("Year range:", min(df$year), "-", max(df$year), "\n")
cat("Months:", paste(sort(unique(df$month)), collapse = ", "), "\n")
cat("Aspect classes:", paste(unique(df$aspect_class), collapse = ", "), "\n")
cat("Elevation classes:", paste(unique(df$elev_class), collapse = ", "), "\n")
cat("Land cover classes:", paste(unique(df$lc_class), collapse = ", "), "\n")

# ==== Type conversion + factor ordering ====
aspect_levels <- c("Flat","North","East","South","West")
elev_levels   <- c("900-1300m","1300-1700m","1700-2100m",
                   "2100-2500m","2500-2900m","2900-3400m")

df <- df %>%
  mutate(
    year         = as.factor(year),
    month        = as.factor(month),
    aspect_class = factor(aspect_class, levels = aspect_levels),
    elev_class   = factor(elev_class,   levels = elev_levels),
    lc_class     = as.factor(lc_class),
    sub_basin    = as.factor(sub_basin),
    median_LST_C = as.numeric(median_LST_C)
  )

# ==== Missing data check ====
cat("\n=== MISSING DATA CHECK ===\n")
missing_data <- sapply(df, function(x) sum(is.na(x)))
print(missing_data)

# ==== Basic LST statistics ====
cat("\n=== LST STATISTICS ===\n")
print(summary(df$median_LST_C))

# --- Common settings: safe labels and unified theme ---
ylab_degC      <- expression("Median LST ("*degree*C*")")
ylab_mean_degC <- expression("Mean LST ("*degree*C*")")

pub_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), # frame
    axis.text.x  = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# --- 1) Sub-basin boxplot ---
p1 <- ggplot(df, aes(x = sub_basin, y = median_LST_C, fill = sub_basin)) +
  geom_boxplot(alpha = 0.85, outlier.alpha = 0.6, width = 0.7) +
  labs(
    title = "LST Distribution by Sub-basin",
    subtitle = "All years and months combined",
    x = "Sub-basin",
    y = ylab_degC
  ) +
  pub_theme
print(p1)

# --- 2) Annual mean + trend (LM) with ASCII-safe annotation ---
annual_mean <- df %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(mean_lst = mean(median_LST_C, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(year_num = as.numeric(as.character(year)))

fit <- lm(mean_lst ~ year_num, data = annual_mean)
s   <- summary(fit)
slope <- coef(s)[2, 1]
pval  <- coef(s)[2, 4]
r2    <- s$r.squared
label_txt <- sprintf("Slope = %.3f deg C/yr | p = %.4f | R^2 = %.3f", slope, pval, r2)

p2 <- ggplot(annual_mean, aes(x = year_num, y = mean_lst)) +
  geom_line(color = "red", linewidth = 1.1) +
  geom_point(color = "darkred", size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Annual Mean LST Trend in Konya Basin (2003???2024)",
    x = "Year",
    y = ylab_mean_degC
  ) +
  annotate(
    "text",
    x = min(annual_mean$year_num) + 1,
    y = max(annual_mean$mean_lst),
    hjust = 0, vjust = 1,
    label = label_txt
  ) +
  pub_theme +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # keep year axis horizontal
print(p2)

# --- 3) Land cover boxplot ---
p3 <- ggplot(df, aes(x = lc_class, y = median_LST_C, fill = lc_class)) +
  geom_boxplot(outlier.alpha = 0.6, width = 0.7) +
  labs(
    title = "LST Distribution by Land Cover Class",
    x = "Land Cover Class",
    y = ylab_degC
  ) +
  pub_theme
print(p3)

# --- 4) Aspect boxplot (order preserved) ---
p4 <- ggplot(df, aes(x = aspect_class, y = median_LST_C, fill = aspect_class)) +
  geom_boxplot(outlier.alpha = 0.6, width = 0.7) +
  labs(
    title = "LST Distribution by Aspect",
    x = "Aspect Direction",
    y = ylab_degC
  ) +
  pub_theme
print(p4)

# --- 5) Elevation boxplot (order preserved) ---
p5 <- ggplot(df, aes(x = elev_class, y = median_LST_C, fill = elev_class)) +
  geom_boxplot(outlier.alpha = 0.6, width = 0.7) +
  labs(
    title = "LST Distribution by Elevation Class",
    x = "Elevation Class (m)",
    y = ylab_degC
  ) +
  pub_theme
print(p5)

# --- Save high-resolution figures (frame preserved) ---
ggsave("Fig_Subbasin_Boxplot.png", p1, width = 8, height = 5, dpi = 600, bg = "white")
ggsave("Fig_AnnualTrend.png",     p2, width = 8, height = 5, dpi = 600, bg = "white")
ggsave("Fig_LC_Boxplot.png",      p3, width = 8, height = 5, dpi = 600, bg = "white")
ggsave("Fig_Aspect_Boxplot.png",  p4, width = 8, height = 5, dpi = 600, bg = "white")
ggsave("Fig_Elev_Boxplot.png",    p5, width = 8, height = 5, dpi = 600, bg = "white")




#=======TREND ANALYSIS (Mann-Kendall + Theil-Sen)===========

# Required libraries
library(trend)
library(broom)

# 1. Calculate annual means (to remove seasonal effects)
annual_data <- df %>%
  group_by(year, sub_basin, lc_class, aspect_class, elev_class) %>%
  summarise(
    mean_lst = mean(median_LST_C, na.rm = TRUE),
    n_observations = n(),
    .groups = 'drop'
  ) %>%
  mutate(year_numeric = as.numeric(as.character(year)))

cat("=== ANNUAL DATA SUMMARY ===\n")
cat("Annual observations:", nrow(annual_data), "\n")
glimpse(annual_data)

# 2. Trend analysis function (corrected)
calculate_trend <- function(data) {
  if (nrow(data) < 2) {
    return(data.frame(
      p_value = NA,
      trend_direction = "Insufficient data",
      slope_per_year = NA,
      significance = "Insufficient data",
      n_years = nrow(data)
    ))
  }
  
  years <- data$year_numeric
  lst_values <- data$mean_lst
  
  # Mann-Kendall test for significance
  mk_test <- tryCatch(
    mk.test(lst_values),
    error = function(e) return(list(p.value = NA))
  )
  
  # Theil-Sen slope estimation
  sen_slope <- tryCatch(
    sens.slope(lst_values),
    error = function(e) return(list(estimates = NA))
  )
  
  # Results
  p_val <- ifelse(is.na(mk_test$p.value), NA, mk_test$p.value)
  slope <- ifelse(is.na(sen_slope$estimates), NA, sen_slope$estimates)
  
  result <- data.frame(
    p_value = p_val,
    trend_direction = ifelse(is.na(slope), "Undetermined",
                             ifelse(slope > 0, "Increasing", "Decreasing")),
    slope_per_year = slope,
    significance = ifelse(is.na(p_val), "Undetermined",
                          ifelse(p_val < 0.05, "Significant", "Not Significant")),
    n_years = nrow(data)
  )
  return(result)
}

# 3. Apply trend analyses - CORRECTED VERSION

# 3.1 Overall trend for each sub-basin (corrected)
overall_trends <- annual_data %>%
  group_by(sub_basin, year_numeric) %>%
  summarise(mean_lst = mean(mean_lst, na.rm = TRUE), .groups = 'drop') %>%
  group_by(sub_basin) %>%
  group_modify(~ calculate_trend(.x))

cat("\n=== OVERALL TRENDS BY SUB-BASIN ===\n")
print(overall_trends, n = Inf)

# 3.2 Trends by sub-basin and land cover (corrected)
trends_lc <- annual_data %>%
  group_by(sub_basin, lc_class, year_numeric) %>%
  summarise(mean_lst = mean(mean_lst, na.rm = TRUE), .groups = 'drop') %>%
  group_by(sub_basin, lc_class) %>%
  group_modify(~ calculate_trend(.x))

cat("\n=== TRENDS BY SUB-BASIN AND LAND COVER ===\n")
print(trends_lc, n = 20)

# 3.3 Simpler approach - direct grouping
# Prepare data first
trend_data_prep <- annual_data %>%
  group_by(sub_basin, lc_class, aspect_class, elev_class, year_numeric) %>%
  summarise(mean_lst = mean(mean_lst, na.rm = TRUE), .groups = 'drop')

# 4. ALTERNATIVE METHOD: Loop-based trend analysis (more robust)

# 4.1 Sub-basin trends
sub_basins <- unique(annual_data$sub_basin)
overall_results <- data.frame()

for (basin in sub_basins) {
  basin_data <- annual_data %>%
    filter(sub_basin == basin) %>%
    group_by(year_numeric) %>%
    summarise(mean_lst = mean(mean_lst, na.rm = TRUE))
  
  if (nrow(basin_data) >= 2) {
    trend_result <- calculate_trend(basin_data)
    overall_results <- rbind(overall_results,
                             data.frame(sub_basin = basin, trend_result))
  }
}

cat("\n=== OVERALL TRENDS BY SUB-BASIN (ALTERNATIVE METHOD) ===\n")
print(overall_results)

# 4.2 Sub-basin and land cover trends
lc_results <- data.frame()

for (basin in sub_basins) {
  lc_classes <- unique(annual_data$lc_class[annual_data$sub_basin == basin])
  
  for (lc in lc_classes) {
    lc_data <- annual_data %>%
      filter(sub_basin == basin, lc_class == lc) %>%
      group_by(year_numeric) %>%
      summarise(mean_lst = mean(mean_lst, na.rm = TRUE))
    
    if (nrow(lc_data) >= 2) {
      trend_result <- calculate_trend(lc_data)
      lc_results <- rbind(lc_results,
                          data.frame(sub_basin = basin, lc_class = lc, trend_result))
    }
  }
}

cat("\n=== TRENDS BY SUB-BASIN AND LAND COVER (ALTERNATIVE METHOD) ===\n")
print(lc_results, n = 20)

# ==== Significance stars function (p-value ??? symbol) ====
p_to_stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  if (p < 0.10)  return("+")  # optional: tendency
  ""
}

# ==== 1) BAR PLOT: Sub-basin Sen slopes (p6) ====
overall_results_star <- overall_results %>%
  mutate(stars = vapply(p_value, p_to_stars, character(1)))

p6 <- ggplot(overall_results_star,
             aes(x = sub_basin, y = slope_per_year, fill = trend_direction)) +
  geom_col() +
  geom_text(aes(label = round(slope_per_year, 3)),
            vjust = -0.5, size = 3) +
  geom_text(aes(label = stars,
                y = slope_per_year + 0.005 * sign(slope_per_year + 1e-9)),
            vjust = -0.5, size = 6, fontface = "bold") +
  labs(
    title = "Annual LST Trend Rates by Sub-basin (2003???2024)",
    subtitle = "Theil???Sen slope estimator (deg C per year)",
    x = "Sub-basin",
    y = expression("Trend Slope ("*degree*C*"/year)"),
    fill = "Trend Direction"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p6)

# ==== 2) HEATMAP: Land cover ?? Sub-basin (p7) ====
lc_results_star <- lc_results %>%
  filter(!is.na(slope_per_year)) %>%
  mutate(stars = vapply(p_value, p_to_stars, character(1)))

# Extract significant cells separately
sig_cells <- lc_results_star %>% filter(stars != "")

p7 <- ggplot(lc_results_star, aes(x = sub_basin, y = lc_class, fill = slope_per_year)) +
  geom_tile() +
  geom_text(aes(label = round(slope_per_year, 3)), size = 2.6) +
  geom_text(data = sig_cells, aes(label = stars),
            size = 5.2, fontface = "bold",
            vjust = -0.9) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0,
    name = expression("Slope ("*degree*C*"/year)")
  ) +
  labs(
    title = "LST Trends by Sub-basin and Land Cover",
    x = "Sub-basin",
    y = "Land Cover Class"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p7)

ggsave("Fig_Annual_LST_Trend_Rates_by_Sub-basin.png", p6, width = 8, height = 5, dpi = 600, bg = "white")
ggsave("Fig_LST_Trends_by_Sub-basin_and_Land_Cover.png",
       p7, width = 8, height = 5, dpi = 600, bg = "white")


#=========Linear Mixed Effects Model=================

# ===========================
# 0) PACKAGES
# ===========================
suppressPackageStartupMessages({
  library(readr)     # read_delim
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(tibble)
  library(nlme)      # LMM without lme4
  library(emmeans)   # Post-hoc, EMM
  library(readr)
})

# ===========================
# 1) READ & MERGE ALL BASINS
# ===========================
Aksaray         <- read_delim("Aksaray.csv",         delim = ";", escape_double = FALSE, trim_ws = TRUE)
Altinekin       <- read_delim("Altinekin.csv",       delim = ";", escape_double = FALSE, trim_ws = TRUE)
Beysehir        <- read_delim("Beysehir.csv",        delim = ";", escape_double = FALSE, trim_ws = TRUE)
Cihanbeyli      <- read_delim("Cihanbeyli.csv",      delim = ";", escape_double = FALSE, trim_ws = TRUE)
Eregli          <- read_delim("Eregli.csv",          delim = ";", escape_double = FALSE, trim_ws = TRUE)
Karaman         <- read_delim("Karaman.csv",         delim = ";", escape_double = FALSE, trim_ws = TRUE)
Konya           <- read_delim("Konya.csv",           delim = ";", escape_double = FALSE, trim_ws = TRUE)
Melendiz        <- read_delim("Melendiz.csv",        delim = ";", escape_double = FALSE, trim_ws = TRUE)
Sereflikochisar <- read_delim("Sereflikochisar.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

df <- bind_rows(
  Aksaray         %>% mutate(sub_basin = "Aksaray"),
  Altinekin       %>% mutate(sub_basin = "Altinekin"),
  Beysehir        %>% mutate(sub_basin = "Beysehir"),
  Cihanbeyli      %>% mutate(sub_basin = "Cihanbeyli"),
  Eregli          %>% mutate(sub_basin = "Eregli"),
  Karaman         %>% mutate(sub_basin = "Karaman"),
  Konya           %>% mutate(sub_basin = "Konya"),
  Melendiz        %>% mutate(sub_basin = "Melendiz"),
  Sereflikochisar %>% mutate(sub_basin = "Sereflikochisar")
)

cat("=== DATASET SUMMARY ===\n")
cat("Total observations:", nrow(df), "\n")
cat("Number of sub-basins:", length(unique(df$sub_basin)), "\n")
cat("Year range:", min(df$year), "-", max(df$year), "\n")
cat("Months:", paste(sort(unique(df$month)), collapse = ", "), "\n\n")

# ===========================
# 2) TYPES, FACTOR ORDERS, NA CLEANING
# ===========================
aspect_levels <- c("Flat","North","East","South","West")
elev_levels   <- c("900-1300m","1300-1700m","1700-2100m","2100-2500m","2500-2900m","2900-3400m")

df_lmm <- df %>%
  mutate(
    year         = as.integer(year),
    month        = as.integer(month),
    year_num     = as.numeric(year),
    aspect_class = factor(aspect_class, levels = aspect_levels),
    elev_class   = factor(elev_class,   levels = elev_levels),
    lc_class     = factor(lc_class),
    sub_basin    = factor(sub_basin),
    median_LST_C = as.numeric(median_LST_C)
  )

# NA check / cleaning (columns used in the model)
na_counts <- sapply(df_lmm[, c("median_LST_C","year_num","elev_class","aspect_class","lc_class","sub_basin")], function(x) sum(is.na(x)))
cat("=== NA COUNTS (pre-clean) ===\n"); print(na_counts)

df_lmm_clean <- df_lmm %>%
  filter(
    !is.na(median_LST_C),
    !is.na(year_num),
    !is.na(elev_class),
    !is.na(aspect_class),
    !is.na(lc_class),
    !is.na(sub_basin)
  )

cat("Rows before:", nrow(df_lmm), " | after NA-clean:", nrow(df_lmm_clean), "\n")

# ===========================
# 3) QUICK EDA (OPTIONAL)
# ===========================
ylab_degC      <- expression("Median LST ("*degree*C*")")
ylab_mean_degC <- expression("Mean LST ("*degree*C*")")

annual_mean <- df_lmm_clean %>%
  group_by(year) %>%
  summarise(mean_lst = mean(median_LST_C, na.rm = TRUE), .groups = "drop") %>%
  mutate(year_num = as.numeric(year))

fit <- lm(mean_lst ~ year_num, data = annual_mean)
s   <- summary(fit)
label_txt <- sprintf("Slope = %.3f deg C/yr | p = %.4f | R^2 = %.3f",
                     coef(s)[2,1], coef(s)[2,4], s$r.squared)

p_trend <- ggplot(annual_mean, aes(x = year_num, y = mean_lst)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(title = "Annual Mean LST Trend (2003???2024)", x = "Year", y = ylab_mean_degC) +
  annotate("text",
           x = min(annual_mean$year_num) + 1,
           y = max(annual_mean$mean_lst),
           hjust = 0, vjust = 1,
           label = label_txt) +
  theme_minimal()
print(p_trend)

# ===========================
# 4) LINEAR MIXED MODEL (nlme::lme)
# Fixed effects: year_num + elev_class + aspect_class + lc_class
# Random effects: (1 | sub_basin)
# ===========================
m_lme <- nlme::lme(
  fixed  = median_LST_C ~ year_num + elev_class + aspect_class + lc_class,
  random = ~ 1 | sub_basin,
  data   = df_lmm_clean,
  method = "REML",
  na.action = na.omit
)

cat("\n=== LMM SUMMARY (nlme::lme) ===\n")
print(summary(m_lme))

cat("\n=== LMM ANOVA (Fixed Effects) ===\n")
print(anova(m_lme))

# Fixed-effects coefficient table (publication-ready)
coef_tbl <- summary(m_lme)$tTable %>%
  as.data.frame() %>%
  rownames_to_column("Term")

names(coef_tbl) <- c("Term","Estimate","Std_Error","DF","t_value","p_value")
print(coef_tbl)

write_csv(coef_tbl, "LMM_FixedEffects_Coefficients.csv")
cat("Saved: LMM_FixedEffects_Coefficients.csv\n")

# ===========================
# 5) POST-HOC / EMM (emmeans)
# ===========================
emm_lc     <- emmeans(m_lme, ~ lc_class)
emm_elev   <- emmeans(m_lme, ~ elev_class)
emm_aspect <- emmeans(m_lme, ~ aspect_class)

cat("\n=== Tukey (lc_class) ===\n");  print(pairs(emm_lc,     adjust = "tukey"))
cat("\n=== Tukey (elev_class) ===\n"); print(pairs(emm_elev,   adjust = "tukey"))
cat("\n=== Tukey (aspect_class) ===\n"); print(pairs(emm_aspect, adjust = "tukey"))

# Save EMM tables
write_csv(as.data.frame(emm_lc),     "EMM_lc_class.csv")
write_csv(as.data.frame(emm_elev),   "EMM_elev_class.csv")
write_csv(as.data.frame(emm_aspect), "EMM_aspect_class.csv")
cat("Saved: EMM_lc_class.csv, EMM_elev_class.csv, EMM_aspect_class.csv\n")

# ===========================
# 6) EMM PLOTS (with error bars)
# ===========================
p_emm_lc <- as.data.frame(emm_lc) %>%
  ggplot(aes(x = lc_class, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  labs(title = "Estimated Mean LST by Land Cover",
       x = "Land Cover", y = ylab_degC) +
  theme_minimal() + theme(axis.text.x = element_text(angle=45, hjust=1))
print(p_emm_lc)

p_emm_elev <- as.data.frame(emm_elev) %>%
  ggplot(aes(x = elev_class, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  labs(title = "Estimated Mean LST by Elevation Class",
       x = "Elevation Class (m)", y = ylab_degC) +
  theme_minimal() + theme(axis.text.x = element_text(angle=45, hjust=1))
print(p_emm_elev)

p_emm_aspect <- as.data.frame(emm_aspect) %>%
  ggplot(aes(x = aspect_class, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  labs(title = "Estimated Mean LST by Aspect",
       x = "Aspect Direction", y = ylab_degC) +
  theme_minimal()
print(p_emm_aspect)

# (Optional) Save figures
# ggsave("Fig_AnnualTrend.png",    p_trend,     width=8, height=5, dpi=600, bg="white")
# ggsave("Fig_EMM_LandCover.png",  p_emm_lc,    width=8, height=5, dpi=600, bg="white")
# ggsave("Fig_EMM_Elevation.png",  p_emm_elev,  width=8, height=5, dpi=600, bg="white")
# ggsave("Fig_EMM_Aspect.png",     p_emm_aspect,width=8, height=5, dpi=600, bg="white")

# ===========================
# 7) SIMPLE ONE-WAY ANOVA + TUKEY (alternative / supplementary)
# ===========================
cat("\n=== One-way ANOVA: LST ~ lc_class ===\n")
model_aov_lc <- aov(median_LST_C ~ lc_class, data = df_lmm_clean)
print(summary(model_aov_lc))
print(TukeyHSD(model_aov_lc))

cat("\n=== One-way ANOVA: LST ~ elev_class ===\n")
model_aov_elev <- aov(median_LST_C ~ elev_class, data = df_lmm_clean)
print(summary(model_aov_elev))
print(TukeyHSD(model_aov_elev))

cat("\n=== One-way ANOVA: LST ~ aspect_class ===\n")
model_aov_aspect <- aov(median_LST_C ~ aspect_class, data = df_lmm_clean)
print(summary(model_aov_aspect))
print(TukeyHSD(model_aov_aspect))

cat("\n=== DONE ===\n")


# =========================================================
# A) FIXED EFFECTS FOREST PLOT (nlme::lme)
# =========================================================
library(dplyr)
library(tibble)
library(ggplot2)

# Extract 95% confidence intervals (fixed effects)
fx_ci <- intervals(m_lme, which = "fixed")$fixed
fx_df <- as.data.frame(fx_ci)
fx_df$Term <- rownames(fx_ci)
colnames(fx_df) <- c("Lower","Estimate","Upper","Term")

# Remove intercept and unnecessary levels for clarity
fx_keep <- fx_df %>%
  filter(Term != "(Intercept)")

# More readable labels (optional)
pretty_term <- function(x){
  x <- gsub("^year_num$", "Year (numeric)", x)
  x <- gsub("^elev_class", "Elev: ", x)
  x <- gsub("^aspect_class", "Aspect: ", x)
  x <- gsub("^lc_class", "LC: ", x)
  x
}
fx_keep$Label <- pretty_term(fx_keep$Term)

# Forest plot
p_forest <- ggplot(fx_keep, aes(y = reorder(Label, Estimate), x = Estimate)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(
    title = "Fixed Effects (LMM) with 95% CI",
    x = expression("Effect on LST ("*degree*C*")"),
    y = NULL
  ) +
  theme_minimal(base_size = 12)
print(p_forest)

# =========================================================
# B) EMMeans DOT + CI + CLD (letter groupings)
# =========================================================
library(emmeans)
ylab_degC <- expression("LST ("*degree*C*")")

# For CLD (letter groupings):
# install.packages("multcomp")  # if needed
library(multcomp)
library(multcompView)

# 1) Land cover
cld_lc <- multcomp::cld(emm_lc, Letters = letters, adjust = "tukey")
df_lc  <- as.data.frame(cld_lc)

p_emm_lc2 <- ggplot(df_lc, aes(x = lc_class, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  geom_text(aes(label = .group, y = upper.CL), vjust = -0.8, fontface = "bold") +
  labs(title = "Estimated Mean LST by Land Cover (with CLD)",
       x = "Land Cover", y = ylab_degC) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_emm_lc2)

# 2) Elevation
cld_elev <- multcomp::cld(emm_elev, Letters = letters, adjust = "tukey")
df_elev  <- as.data.frame(cld_elev)

p_emm_elev2 <- ggplot(df_elev, aes(x = elev_class, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  geom_text(aes(label = .group, y = upper.CL), vjust = -0.8, fontface = "bold") +
  labs(title = "Estimated Mean LST by Elevation Class (with CLD)",
       x = "Elevation Class (m)", y = ylab_degC) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_emm_elev2)

# 3) Aspect
cld_aspect <- multcomp::cld(emm_aspect, Letters = letters, adjust = "tukey")
df_aspect  <- as.data.frame(cld_aspect)

p_emm_aspect2 <- ggplot(df_aspect, aes(x = aspect_class, y = emmean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15) +
  geom_text(aes(label = .group, y = upper.CL), vjust = -0.8, fontface = "bold") +
  labs(title = "Estimated Mean LST by Aspect (with CLD)",
       x = "Aspect Direction", y = ylab_degC) +
  theme_minimal(base_size = 12)
print(p_emm_aspect2)

# =========================================================
# C) TUKEY HEATMAPS (pairwise differences & significance)
# =========================================================
# Helper: convert emmeans contrast table to heatmap format
plot_tukey_heatmap <- function(emm_obj, title_txt, xlab, ylab){
  contr <- as.data.frame(pairs(emm_obj, adjust = "tukey"))
  tmp <- tidyr::separate(contr, contrast, into = c("grp1","grp2"), sep = " - ")
  
  p <- ggplot(tmp, aes(x = grp1, y = grp2, fill = estimate)) +
    geom_tile() +
    geom_text(aes(label = ifelse(p.value < 0.05,
                                 sprintf("%.2f*", estimate),
                                 sprintf("%.2f", estimate))), size = 3) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                         name = expression(Delta*" LST ("*degree*C*")")) +
    labs(title = title_txt, x = xlab, y = ylab) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}

p_heat_lc    <- plot_tukey_heatmap(emm_lc,    "Tukey Pairwise Differences (LC)",         "Group 1", "Group 2")
p_heat_elev  <- plot_tukey_heatmap(emm_elev,  "Tukey Pairwise Differences (Elevation)",  "Group 1", "Group 2")
p_heat_aspec <- plot_tukey_heatmap(emm_aspect,"Tukey Pairwise Differences (Aspect)",     "Group 1", "Group 2")

print(p_heat_lc)
print(p_heat_elev)
print(p_heat_aspec)

# =========================================================
# D) SAVE FIGURES (600 dpi)
# =========================================================
ggsave("Fig_Forest_FixedEffects.png", p_forest,      width = 8, height = 6, dpi = 600, bg = "white")
ggsave("Fig_EMM_LC_CLD.png",          p_emm_lc2,     width = 8, height = 5, dpi = 600, bg = "white")
ggsave("Fig_EMM_Elev_CLD.png",        p_emm_elev2,   width = 8, height = 5, dpi = 600, bg = "white")
ggsave("Fig_EMM_Aspect_CLD.png",      p_emm_aspect2, width = 8, height = 5, dpi = 600, bg = "white")
ggsave("Fig_TukeyHeat_LC.png",        p_heat_lc,     width = 7, height = 6, dpi = 600, bg = "white")
ggsave("Fig_TukeyHeat_Elev.png",      p_heat_elev,   width = 7, height = 6, dpi = 600, bg = "white")
ggsave("Fig_TukeyHeat_Aspect.png",    p_heat_aspec,  width = 7, height = 6, dpi = 600, bg = "white")

