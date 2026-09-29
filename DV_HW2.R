library(AER)
library(ggplot2)

data("CPS1985", package = "AER")
dir.create("figures", showWarnings = FALSE)

married_levels <- levels(CPS1985$married)
married_colours <- setNames(
  c("#0072B2", "#D55E00"),
  married_levels
)

p1 <- ggplot(CPS1985, aes(x = age, y = wage)) +
  geom_point(alpha = 0.6) +
  scale_y_log10() +
  labs(
    title = "Hourly wage by age",
    x = "Age (years)",
    y = "Wage (dollars per hour, log scale)"
  )

p2 <- ggplot(
  CPS1985,
  aes(x = age, y = wage, colour = married)
) +
  geom_point(alpha = 0.65) +
  scale_y_log10() +
  scale_colour_manual(values = married_colours) +
  labs(
    title = "Hourly wage by age and marital status",
    x = "Age (years)",
    y = "Wage (dollars per hour, log scale)",
    colour = "Married"
  )

p3 <- ggplot(
  CPS1985,
  aes(x = reorder(ethnicity, wage, FUN = median), y = wage)
) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Hourly wages by ethnicity",
    x = "Ethnicity (ordered by median wage)",
    y = "Wage (dollars per hour)"
  )

p4_fixed <- p2 +
  facet_wrap(~ sector) +
  labs(title = "Wage by age and marital status, with a shared wage scale")

p4_free <- p2 +
  facet_wrap(~ sector, scales = "free_y") +
  labs(title = "Wage by age and marital status, with a separate wage scale")

p5_scale_limits <- ggplot(
  CPS1985,
  aes(x = education, y = wage)
) +
  geom_point(alpha = 0.45) +
  geom_smooth(method = "loess") +
  scale_y_continuous(limits = c(0, 20)) +
  labs(
    title = "Education and hourly wage: scale limits",
    x = "Education (years)",
    y = "Wage (dollars per hour)"
  )

p5_coord_zoom <- ggplot(
  CPS1985,
  aes(x = education, y = wage)
) +
  geom_point(alpha = 0.45) +
  geom_smooth(method = "loess") +
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    title = "Education and hourly wage: coordinate zoom",
    x = "Education (years)",
    y = "Wage (dollars per hour)"
  )

print(p1)
print(p2)
print(p3)
print(p4_fixed)
print(p4_free)
print(p5_scale_limits)  # Check and copy the warning shown in the Console
print(p5_coord_zoom)

ggsave("figures/q1_wage_by_age.png", p1, width = 7, height = 5, dpi = 300)
ggsave("figures/q2_wage_by_age_married.png", p2, width = 7, height = 5, dpi = 300)
ggsave("figures/q3_wage_by_ethnicity.png", p3, width = 7, height = 5, dpi = 300)
ggsave("figures/q4_facet_fixed_scale.png", p4_fixed, width = 9, height = 5, dpi = 300)
ggsave("figures/q4_facet_free_y.png", p4_free, width = 9, height = 5, dpi = 300)
ggsave("figures/q5_scale_limits.png", p5_scale_limits, width = 7, height = 5, dpi = 300)
ggsave("figures/q5_coord_cartesian.png", p5_coord_zoom, width = 7, height = 5, dpi = 300)

