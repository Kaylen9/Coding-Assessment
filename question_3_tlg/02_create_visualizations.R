# create the log file
sink("question_3_tlg/02_create_visualizations.log", split = TRUE)
cat("Running 02_create_visualizations.R\n")
cat("Execution started at:", Sys.time(), "\n\n")

# load the required packages 
library(dplyr)
library(gtsummary)

# load the data into R
adae <- pharmaverseadam::adae

# Keep only TEAEs
adae_y <- adae %>% filter(TRTEMFL == "Y")

# Create summary dataset (count of AE records by severity and treatment)
ae_sev_summary <- adae_y %>%
  group_by(ACTARM, AESEV) %>%
  summarise(n = n(), .groups = "drop")

# Create the stacked bar chart
plot1 <- ggplot(ae_sev_summary, aes(x = ACTARM, y = n, fill = AESEV)) +
  geom_bar(stat = "identity") +
  labs(
    title = "AE severity distribution by treatment",
    x = "Treatment Arm",
    y = "Count of AEs",
    fill = "Severity/Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold")
  )
# save the figure as PNG file
ggsave(
  filename = "question_3_tlg/plot1.png",
  plot = plot1,
  width = 8,
  height = 6,
  dpi = 300
)

# Calculate the total subjects
N_total <- adae %>% distinct(USUBJID) %>% nrow()

# Get the top 10 most frequent AEs 
top10 <- adae %>%
  filter(TRTEMFL == "Y") %>%
  distinct(USUBJID, AETERM) %>%
  count(AETERM, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  rowwise() %>%
  mutate(
    prop = n / N_total,
    lower = binom.test(n, N_total)$conf.int[1],
    upper = binom.test(n, N_total)$conf.int[2]
  ) %>%
  ungroup()

# order largest at top
top10 <- top10 %>%
  mutate(AETERM = factor(AETERM, levels = rev(AETERM)))

# make the plot
plot2 <- ggplot(top10, aes(y = AETERM)) +
  # Horizontal CI bars
  geom_errorbarh(
    aes(xmin = lower * 100, xmax = upper * 100),
    height = 0.15,
    size = 0.8,
    color = "black"
  ) +
  # Point estimate
  geom_point(
    aes(x = prop * 100),
    size = 3,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = c(5, 10, 15, 20, 25, 30),
    labels = function(x) paste0(x, "%"),
    limits = c(0, max(top10$upper * 100) * 1.05)
  ) +
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", N_total, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  theme_grey(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# save the figure as PNG file
ggsave(
  filename = "question_3_tlg/plot2.png",
  plot = plot2,
  width = 8,
  height = 6,
  dpi = 300
)

cat("\nExecution completed at:", Sys.time(), "\n")
sink()






