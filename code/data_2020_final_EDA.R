wd <- '/home/ntolley/Grad_School/DSI/data2020_final_project/data'
setwd(wd)
if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, kableExtra, RColorBrewer, usethis, janitor)

df <- read_csv('../data/beta_event_data.csv')
names(df) <- tolower(str_replace_all(names(df), ' ', '_')) # clean names

#### norm peak power v. frequency
fig1 <- df %>% 
  dplyr::select(peak_time, normalized_peak_power, peak_frequency) %>%
  mutate(`Peak Frequency` = peak_frequency) %>% 
  ggplot(aes(x = peak_time, y = normalized_peak_power, color = `Peak Frequency`)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(breaks = seq(from = 0, to = 2, by = 0.25)) +
  scale_y_log10() +
  labs(x = 'Peak Time', y = 'Normalized Peak Power') +
  theme_bw()

#ggsave('peak_time_norm_peak_power.png', plot = fig1, height = 5, width = 7, dpi = 500)

#### Feature correlations and models 
df <- df[, -1] # removes IDs
n <- length(colnames(df))
corr_matrix <- round(cor(df[,1:n], use = 'complete.obs'), digits = 1) # lots of incomplete rows, be careful

fig2 <- ggcorrplot(corr_matrix, hc.order = FALSE, lab = FALSE, outline.col = 'white',
           colors = c("#6D9EC1", "white", "#E46726"))
ggsave('feature_corr.jpg', plot = fig2, dpi = 500)

fit1 <- lm(normalized_peak_power ~ ., df) # full model
fit2 <- lm(normalized_peak_power ~ 1, df) # bare model
model_forward <- stepAIC(fit2, direction = 'forward', scope = list(upper = fit1, lower = fit2))



