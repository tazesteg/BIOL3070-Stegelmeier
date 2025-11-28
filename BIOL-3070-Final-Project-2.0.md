Opioid Therapy Response in European Cancer Patients
================
Taze Stegelmeier
2025-10-28

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [METHODS](#methods)
- [QUESTION](#question)
- [HYPOTHESIS](#hypothesis)
- [PREDICTION](#prediction)
- [RESULTS](#results)
- [DISCUSSION](#discussion)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

Opioid pain relief varies widely across Opioid pain relief varies widely
across patients, and EPOS (a human pain genetics database) was designed
to study how genetic variation contributes to that variability. In this
project, I asked whether genotype and carrier status (and their
interaction with sex) are associated with normalized opioid pain relief
in an EPOS-style simulated patient-level dataset (including genotype,
carrier status, sex, and country). I built two visuals: (1) a mean ± SE
bar plot of pain relief by genotype and carrier status and (2) a
carrier-by-sex boxplot of normalized pain relief. I also fit a linear
model (pain ~ genotype + carrier \* sex + country) to describe these
relationships. In these simulated data, GG genotypes show the highest
normalized pain relief, A-allele carriers show lower relief, and carrier
differences look similar in females and males, suggesting that genotype
and carrier status may contribute to variability in opioid response in
an EPOS-style setting.

# BACKGROUND

Opioid analgesics are a cornerstone of treatment for moderate-to-severe
pain, yet patients show wide variability in how much pain relief they
experience and in their risk for adverse effects and dose escalation
(Galvan et al., 2011). Understanding why opioid response differs across
individuals is clinically important, because it could help clinicians
choose safer and more effective therapies rather than relying on
trial-and-error prescribing. Prior human pain genetics work, including
the EPOS pain genetics database, suggests that genetic variants can
influence analgesic efficacy and contribute to this variability in
response. In this project, I focus on whether genotype and carrier
status are linked to differences in normalized opioid pain relief within
an EPOS-style dataset.

# METHODS

I analyzed a simulated EPOS-style patient-level dataset designed to
mimic a single-locus pain genetics study. Each row represents one
“patient” and includes a normalized pain relief score along with
genotype (AA, GA, GG), carrier status (A-allele carrier vs non-carrier),
sex, and country. The simulated data were constructed so that groups
overlap but show modest directional differences in pain relief, loosely
inspired by patterns reported in human pain genetics work (Galvan et
al., 2011) and generated with the help of ChatGPT (ChatGPT, 2025).

The pain outcome in the dataset is provided as a normalized pain relief
measure on a 0–100 scale. This normalized score was created during the
simulation process by rescaling underlying pain relief values so that 0
represents the lowest relief in the simulated population and 100
represents the highest. All analyses in this report use this normalized
0–100 pain relief variable directly.

Genotype, carrier status, sex, and country were treated as categorical
variables. Carrier status groups individuals into A-allele carriers (AA
or GA) versus non-carriers (GG).

I summarized group patterns descriptively using two figures:

Figure 1: A bar plot of mean normalized pain relief with standard error
bars, shown by genotype and carrier status.

Figure 2: Boxplots of normalized pain relief by carrier status, split by
sex, with group means overlaid.

No formal hypothesis tests or regression models were fit in this
analysis; instead, I focus on visual inspection of patterns across
genotypes, carrier groups, and sexes in this simulated EPOS-style
dataset.

``` r
# read + sanitize headers
dat <- read.csv("/cloud/project/BIOL3070-Stegelmeier/epos_style_rs12948783_simulated_large (1).csv")
names(dat) <- make.names(trimws(names(dat)), unique = TRUE)

# expect genotype, norm_pain_relief, carrier
stopifnot(all(c("genotype","norm_pain_relief","carrier") %in% names(dat)))
dat$genotype <- as.factor(dat$genotype)
dat$carrier  <- as.factor(dat$carrier)
dat$pain     <- suppressWarnings(as.numeric(dat$norm_pain_relief))
dat <- dat[is.finite(dat$pain), , drop = FALSE]

# summary by genotype x carrier
summ <- dat %>%
  dplyr::group_by(genotype, carrier) %>%
  dplyr::summarise(
    n  = dplyr::n(),
    mn = mean(pain, na.rm = TRUE),
    sd = sd(pain,   na.rm = TRUE),
    se = ifelse(n > 1, sd / sqrt(n), 0),
    .groups = "drop"
  )

# order genotypes by overall mean
geno_order <- summ %>%
  dplyr::group_by(genotype) %>%
  dplyr::summarise(overall = mean(mn, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(dplyr::desc(overall)) %>%
  dplyr::pull(genotype) %>%
  as.character()
summ$genotype <- factor(summ$genotype, levels = geno_order)

# plot
dodge_w <- 0.75
p <- ggplot(summ, aes(x = genotype, y = mn, fill = carrier)) +
  geom_col(position = position_dodge(width = dodge_w), width = 0.7) +
  geom_errorbar(aes(ymin = mn - se, ymax = mn + se),
                position = position_dodge(width = dodge_w), width = 0.15) +
  geom_text(aes(label = paste0("n=", n), y = mn + se),
            position = position_dodge(width = dodge_w),
            vjust = -0.4, size = 3) +
  labs(
    title = "Normalized pain relief (mean ± SE) by genotype and carrier",
    x = "Genotype", y = "Mean normalized pain relief", fill = "Carrier"
  ) +
  theme_minimal()
print(p)
```

![](BIOL-3070-Final-Project-2.0_files/figure-gfm/bar_mean_se_by_genotype_carrier-1.png)<!-- -->

<img src="BIOL-3070-Final-Project-2.0_files/Normalized pain relief (1).png" width="80%" />

# QUESTION

Do genotype and carrier status explain meaningful variation in
normalized pain relief in this simulated EPOS-style dataset, and is
there evidence that the carrier pattern differs by sex?

# HYPOTHESIS

Genotype groups will differ in mean normalized pain relief, and A-allele
carriers will tend to show lower relief than GG non-carriers. Any
carrier-by-sex interaction is expected to be modest (similar carrier
pattern in females and males, rather than a reversal).

# PREDICTION

In the genotype × carrier bar plot (Figure 1), ordering by genotype will
reveal a gradient in mean pain relief, with GG highest and AA lowest,
and a consistent gap between carriers and non-carriers. In the
carrier-by-sex boxplot (Figure 2), the direction of the carrier
difference will be similar in females and males, with only small shifts
in central tendency between sexes. The linear model will estimate
non-zero coefficients for genotype and carrier terms, with the carrier ×
sex interaction term smaller than the main effects.

``` r
# 1) Load CSV & sanitize headers (prevents zero-length-name issues)
df <- read.csv("/cloud/project/BIOL3070-Stegelmeier/epos_style_rs12948783_simulated_large (1).csv")
names(df) <- make.names(trimws(names(df)), unique = TRUE)

# 2) Light cleaning/coercions
df <- df %>%
  rename(pain = norm_pain_relief) %>%
  mutate(
    genotype = as.factor(genotype),
    carrier  = as.factor(carrier),
    sex      = as.factor(sex),
    country  = as.factor(country),
    pain     = suppressWarnings(as.numeric(pain))
  ) %>%
  filter(is.finite(pain))

# Order genotypes by median pain relief (descending)
geno_order <- df %>%
  group_by(genotype) %>%
  summarize(med = median(pain, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(med)) %>%
  pull(genotype) %>%
  as.character()
df$genotype <- factor(df$genotype, levels = geno_order)

# 3) FIGURE A — Pain relief by GENOTYPE
p1 <- ggplot(df, aes(genotype, pain)) +
  geom_violin(fill = "grey90", color = "grey30", trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 2.5, fill = "white") +
  stat_summary(
    fun.data = function(y) data.frame(y = max(y, na.rm = TRUE) + 0.02,
                                      label = paste0("n=", sum(is.finite(y)))),
    geom = "text", size = 3
  ) +
  labs(
    title = "Normalized pain relief by genotype",
    x = "Genotype (ordered by median pain relief)",
    y = "Normalized pain relief"
  ) +
  theme_minimal()

# 4) FIGURE B — Pain relief by CARRIER, split by SEX
p2 <- ggplot(df, aes(carrier, pain, fill = sex)) +
  geom_boxplot(outlier.alpha = 0.2, position = position_dodge(width = 0.8)) +
  stat_summary(aes(group = sex),
               fun = mean, geom = "point",
               position = position_dodge(width = 0.8),
               shape = 21, size = 2.5, fill = "white") +
  labs(
    title = "Normalized pain relief by carrier status (split by sex)",
    x = "Carrier status (0 = non-carrier, 1 = carrier)",
    y = "Normalized pain relief",
    fill = "Sex"
  ) +
  theme_minimal()
print(p2)
```

![](BIOL-3070-Final-Project-2.0_files/figure-gfm/fig_patient_level_from_csv_no_forcats-1.png)<!-- -->

``` r
# 5) Run the model silently (no printed output)
m <- lm(pain ~ genotype + carrier * sex + country, data = df)
invisible(capture.output(summary(m)))
```

<img src="BIOL-3070-Final-Project-2.0_files/Normalized pain relief by carrier status (2).png" width="80%" />

# RESULTS

Figure 1: genotype and carrier. Figure 1 shows mean normalized pain
relief (± SE) by genotype and carrier status. Descriptively, GG
individuals have the highest mean relief, while GA and AA genotypes tend
to have lower means, with AA appearing lowest and GA intermediate.
Within this pattern, A-allele carriers (AA_or_GA) are generally below
the GG non-carrier group, suggesting a separation in typical pain relief
between carriers and non-carriers.

Figure 2: carrier status and sex. Figure 2 shows boxplots of normalized
pain relief by carrier status, split by sex. GG non-carriers again
appear to have higher typical relief than A-allele carriers, and this
pattern is similar in both females and males. Within each carrier group,
male and female distributions overlap substantially, with only small
shifts in medians and means, indicating that sex differences in these
simulated data are modest relative to the carrier vs. non-carrier
contrast.

# DISCUSSION

Across these simulated EPOS-style data, the descriptive plots suggest
that genotype and carrier status are related to patterns in normalized
opioid pain relief. GG individuals tend to cluster at higher relief
values, while A-allele carriers (especially AA) tend to show lower
typical relief, with GA intermediate. The carrier-by-sex boxplots
indicate that this carrier pattern looks similar in males and females,
rather than showing a strong reversal or large sex-specific difference.

These patterns are consistent with a plausible biological scenario in
which genetic variation at a single locus contributes modestly to
variability in analgesic response, while many other clinical and genetic
factors (not modeled here) also play roles. Because the dataset is
simulated and I did not perform formal hypothesis tests, I interpret
these findings as descriptive patterns rather than definitive evidence
of causal effects.

# CONCLUSION

In this EPOS-style simulated dataset, descriptive bar plots and boxplots
show that GG non-carriers tend to have higher normalized opioid pain
relief than A-allele carriers, with AA lowest and GA intermediate, and
that this carrier pattern looks similar in females and males. These
results illustrate how patient-level pain-genetics data can be
visualized to explore potential relationships between genotype, carrier
status, sex, and opioid response, and they highlight patterns that could
be evaluated more rigorously with formal modeling and real EPOS data.

# REFERENCES

ChatGPT. OpenAI, version Jan 2025. Accessed r Sys.Date().

Galvan, A., et al. (2011). Clin Cancer Res, 17(13), 4581–4587.
<https://doi.org/10.1158/1078-0432.CCR-10-3028>
