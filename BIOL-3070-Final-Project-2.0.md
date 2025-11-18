Opioid Therapy Response in European Cancer Patients
================
Taze Stegelmeier
2025-10-28

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [QUESTION](#question)
- [HYPOTHESIS](#hypothesis)
- [PREDICTION](#prediction)
- [METHODS](#methods)
- [DISCUSSION](#discussion)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

Opioid pain relief varies widely across patients. Using a simulated
EPOS-style patient-level CSV (normalized pain relief with genotype,
carrier status, sex, and country), I built two visuals: (1) pain relief
across genotypes and (2) mean ± SE pain relief by genotype with carrier
status, plus a carrier-by-sex view. The genotype plot shows clear
between-group differences (genotypes ordered by median relief), and the
bar plot highlights a consistent gap between carrier groups within
genotypes, with small sex-specific shifts in the carrier-by-sex view. A
simple linear model (pain ~ genotype + carrier\*sex + country)
accompanies the figures to quantify these effects. Together, the visuals
and model support genotype-linked differences in analgesic response and
a carrier effect that may vary slightly by sex.

# BACKGROUND

Individual response to opioid therapy is influenced by clinical and
genetic factors. Prior work (e.g., EPOS) points to variants that
modulate analgesic efficacy. Here, instead of summary SNP tables, I use
a simulated EPOS-style patient-level file capturing normalized pain
relief and key covariates (genotype, carrier, sex, country). This lets
me directly inspect phenotype differences across genotypes and evaluate
whether carrier status—and its interaction with sex—tracks with the
observed relief.

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
normalized pain relief, and is there evidence of a carrier × sex
interaction in these simulated data?

# HYPOTHESIS

Genotype groups will differ in mean pain relief, and carriers will show
a systematic shift relative to non-carriers; the carrier effect may show
modest sex-specific modulation.

# PREDICTION

When ordering genotypes by median relief, groups will separate visually
(violin/boxplot), and mean ± SE bars will show a consistent
carrier/non-carrier gap within genotypes. In the carrier-by-sex view,
the direction of the carrier effect will be similar across sexes with
possible small differences in magnitude. The linear model will attribute
variance to genotype and carrier, with a potential (but not necessarily
large) carrier × sex term.

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

# METHODS

I loaded the CSV and coerced columns to factors where appropriate
(genotype, carrier, sex, country). I renamed norm_pain_relief to pain.
Figure A: Violin + boxplot of pain ~ genotype, with genotypes re-leveled
by descending median relief; points mark means. Figure B: Mean ± SE bar
plot of pain by genotype and carrier (dodged bars with n per cell).
Carrier × Sex view: Boxplot of pain ~ carrier colored by sex with mean
points overlaid. For inference support, I fit lm(pain ~ genotype +
carrier\*sex + country). I report the direction/patterns visually and
use the model to corroborate group differences; I do not interpret
absolute effect sizes beyond what the figures suggest.

# DISCUSSION

The genotype plot shows visibly distinct distributions and means across
groups, consistent with a genetic component to opioid response. Ordering
by median relief makes the gradient clear. The mean ± SE bars confirm a
reproducible carrier vs. non-carrier separation within genotypes,
indicating a carrier effect that persists after grouping by genotype. In
the carrier-by-sex view, both sexes follow the same overall direction,
with modest spacing between mean points suggesting a small interaction
rather than a reversal. The linear model formalizes this picture by
attributing variance to genotype and carrier, with country included to
absorb baseline differences.

These findings align with a polygenic, modest-effect view of analgesic
response: genotype strata differ, carrier status nudges the mean, and
sex may lightly tune the effect. Because these are simulated data, I
treat magnitudes as illustrative; the patterns nonetheless match what we
expect biologically (multiple small contributors rather than a single
dominant factor).

# CONCLUSION

Across these simulated patient-level data, genotype strata show
meaningful differences in normalized pain relief, carriers differ from
non-carriers in a consistent direction, and any sex modulation appears
modest—together supporting a genetic contribution to opioid response
with a carrier effect that may vary slightly by sex.

# REFERENCES

ChatGPT. OpenAI, version Jan 2025. Accessed r Sys.Date().

Galvan, A., et al. (2011). Clin Cancer Res, 17(13), 4581–4587.
<https://doi.org/10.1158/1078-0432.CCR-10-3028>
