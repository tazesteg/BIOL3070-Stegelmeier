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

![](BIOL-3070-Final-Project-2.0_files/figure-gfm/bar_mean_se_by_genotype_carrier-1.png)<!-- -->

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

![](BIOL-3070-Final-Project-2.0_files/figure-gfm/fig_patient_level_from_csv_no_forcats-1.png)<!-- -->

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
