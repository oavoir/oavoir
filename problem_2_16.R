# =============================================================================
# Problem 2.16 - Doll & Hill (1950) Lung Cancer and Smoking Case-Control Study
# Source: R. Doll and A. B. Hill, Br. Med. J., 739-748, September 30, 1950
#
# 20 hospitals in London, UK. Patients admitted with lung cancer were matched
# with noncancer control patients of same sex and 5-year age group.
# A smoker = person who smoked >= 1 cigarette/day for >= 1 year.
#
# Table 2.12:
#                  Lung Cancer
#                  Cases    Controls
# Have Smoked Yes    688       650
#             No      21        59
#             Total  709       709
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Data Setup - 2x2 Contingency Table
# -----------------------------------------------------------------------------

# Cell counts
cases_smoker    <- 688   # lung cancer cases who smoked
cases_nonsmoker <-  21   # lung cancer cases who did NOT smoke
ctrl_smoker     <- 650   # control patients who smoked
ctrl_nonsmoker  <-  59   # control patients who did NOT smoke

total_cases    <- cases_smoker + cases_nonsmoker   # 709
total_controls <- ctrl_smoker  + ctrl_nonsmoker    # 709

# Build the 2x2 matrix for easy display
tbl <- matrix(
  c(cases_smoker, cases_nonsmoker,
    ctrl_smoker,  ctrl_nonsmoker),
  nrow = 2, ncol = 2,
  dimnames = list(
    "Smoked"      = c("Yes", "No"),
    "Lung Cancer" = c("Cases", "Controls")
  )
)

cat("=== Table 2.12: Data for Problem 2.16 ===\n")
print(tbl)
cat(sprintf("\n  Total Cases:    %d\n", total_cases))
cat(sprintf("  Total Controls: %d\n\n", total_controls))


# =============================================================================
# (a) Response Variable and Explanatory Variable
# =============================================================================

cat("=============================================================\n")
cat("(a) Response Variable and Explanatory Variable\n")
cat("=============================================================\n\n")

cat("  Response Variable (outcome):\n")
cat("    Lung Cancer status -> Cases (lung cancer) vs Controls (no lung cancer)\n\n")

cat("  Explanatory Variable (exposure):\n")
cat("    Smoking status -> Have Smoked: Yes vs No\n\n")

cat("  Reasoning:\n")
cat("    The study investigates whether smoking (exposure) explains or predicts\n")
cat("    the occurrence of lung cancer (outcome). Researchers sampled based on\n")
cat("    the outcome (case-control design), then looked back at smoking history.\n\n")


# =============================================================================
# (b) Type of Study
# =============================================================================

cat("=============================================================\n")
cat("(b) Type of Study\n")
cat("=============================================================\n\n")

cat("  Study Type: CASE-CONTROL STUDY (Retrospective)\n\n")

cat("  Evidence:\n")
cat("  1) Sampling was done based on the OUTCOME (lung cancer status).\n")
cat("     - 709 cases  (patients WITH lung cancer) were selected.\n")
cat("     - 709 controls (patients WITHOUT lung cancer) were matched.\n\n")
cat("  2) The investigators then looked BACKWARD to assess past exposure\n")
cat("     (smoking history) for each participant.\n\n")
cat("  3) Both groups were sampled from the same hospitals, matching on\n")
cat("     sex and 5-year age group to control for confounding.\n\n")
cat("  Key feature: The totals in each column (709, 709) were FIXED by design.\n\n")


# =============================================================================
# (c) Can We Compare Proportions of Lung Cancer Between Smokers/Nonsmokers?
# =============================================================================

cat("=============================================================\n")
cat("(c) Can we compare proportions who suffered lung cancer?\n")
cat("=============================================================\n\n")

# Proportion of smokers among cases vs controls (what we CAN calculate)
prop_smoker_in_cases    <- cases_smoker / total_cases
prop_smoker_in_controls <- ctrl_smoker  / total_controls

cat("  What we CAN calculate (exposure proportions within each group):\n")
cat(sprintf("    Proportion of smokers among CASES:    %d/%d = %.4f (%.2f%%)\n",
            cases_smoker, total_cases,
            prop_smoker_in_cases, prop_smoker_in_cases * 100))
cat(sprintf("    Proportion of smokers among CONTROLS: %d/%d = %.4f (%.2f%%)\n\n",
            ctrl_smoker, total_controls,
            prop_smoker_in_controls, prop_smoker_in_controls * 100))

cat("  Answer: NO - we CANNOT estimate the proportion who SUFFERED lung cancer\n")
cat("          among smokers vs nonsmokers from this case-control data.\n\n")

cat("  Why not?\n")
cat("  - In a case-control study, the number of cases and controls is\n")
cat("    FIXED BY DESIGN (both set to 709 here).\n")
cat("  - These sampling fractions do NOT reflect the true prevalence\n")
cat("    of lung cancer in the population.\n\n")

# Illustrate the problem with a (wrong) attempt
wrong_prop_smoker    <- cases_smoker    / (cases_smoker + ctrl_smoker)
wrong_prop_nonsmoker <- cases_nonsmoker / (cases_nonsmoker + ctrl_nonsmoker)
cat("  Illustration of the problem:\n")
cat(sprintf("    'Proportion' with lung cancer among smokers:    %.4f\n",
            wrong_prop_smoker))
cat(sprintf("    'Proportion' with lung cancer among nonsmokers: %.4f\n",
            wrong_prop_nonsmoker))
cat("    These numbers are MEANINGLESS because the 1:1 case:control ratio\n")
cat("    was chosen by the researchers, not observed naturally.\n\n")

cat("  -> To compare exposure across two groups fixed by design, use the ODDS RATIO.\n\n")


# =============================================================================
# (d) Summarize the Association - Odds Ratio
# =============================================================================

cat("=============================================================\n")
cat("(d) Summarize the Association (Odds Ratio)\n")
cat("=============================================================\n\n")

# --- Odds of Smoking in Each Group ---
# Odds = P(exposure) / P(no exposure) within each outcome group

odds_smoking_cases    <- cases_smoker    / cases_nonsmoker
odds_smoking_controls <- ctrl_smoker     / ctrl_nonsmoker

cat("[ Odds of Smoking within Each Group ]\n")
cat(sprintf("  Odds of smoking | Cases:    %d / %d = %.4f\n",
            cases_smoker, cases_nonsmoker, odds_smoking_cases))
cat(sprintf("  Odds of smoking | Controls: %d / %d = %.4f\n\n",
            ctrl_smoker, ctrl_nonsmoker, odds_smoking_controls))


# --- Odds Ratio (OR) ---
# OR = (odds of exposure in cases) / (odds of exposure in controls)
# Equivalent cross-product formula: OR = (a*d) / (b*c)
#   a = cases_smoker, b = cases_nonsmoker
#   c = ctrl_smoker,  d = ctrl_nonsmoker

odds_ratio <- (cases_smoker * ctrl_nonsmoker) / (cases_nonsmoker * ctrl_smoker)

cat("[ Odds Ratio (OR) ]\n")
cat(sprintf("  Formula: (a * d) / (b * c)\n"))
cat(sprintf("  Calculation: (%d * %d) / (%d * %d)\n",
            cases_smoker, ctrl_nonsmoker, cases_nonsmoker, ctrl_smoker))
cat(sprintf("             = %d / %d\n",
            cases_smoker * ctrl_nonsmoker, cases_nonsmoker * ctrl_smoker))
cat(sprintf("             = %.4f\n\n", odds_ratio))

cat("[ Interpretation ]\n")
cat(sprintf("  OR = %.4f\n\n", odds_ratio))
cat("  The odds of having smoked are approximately 2.97 times HIGHER\n")
cat("  among lung cancer patients (cases) than among controls.\n\n")
cat("  Equivalently: smokers have roughly 3 times the odds of developing\n")
cat("  lung cancer compared to nonsmokers.\n\n")
cat("  Since OR > 1, smoking is POSITIVELY ASSOCIATED with lung cancer.\n\n")

# Note on why OR is used here (not RR)
cat("[ Note: Why OR instead of RR? ]\n")
cat("  This is a case-control study with fixed outcome totals.\n")
cat("  Relative Risk (RR) cannot be validly calculated because the row\n")
cat("  totals (smokers vs nonsmokers) were not sampled independently.\n")
cat("  The Odds Ratio is the appropriate measure for case-control studies.\n\n")


# =============================================================================
# Visualization
# =============================================================================

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# --- Plot 1: Stacked bar chart of the 2x2 table ---
prop_tbl <- prop.table(tbl, margin = 2)  # column proportions

barplot(prop_tbl,
        col    = c("firebrick", "steelblue"),
        main   = "Smoking Proportion\nby Lung Cancer Status",
        ylab   = "Proportion",
        ylim   = c(0, 1.15),
        legend.text = c("Smoker", "Nonsmoker"),
        args.legend = list(x = "topright", bty = "n"),
        las    = 1)

text(x      = c(0.7, 1.9),
     y      = c(prop_tbl[1, 1] / 2, prop_tbl[1, 2] / 2),
     labels = sprintf("%.1f%%", prop_tbl[1, ] * 100),
     col    = "white", fontface = "bold", cex = 1.0)


# --- Plot 2: Odds Ratio visualization ---
or_vals   <- c(odds_ratio, 1)
or_labels <- c(sprintf("OR = %.2f", odds_ratio), "OR = 1\n(no association)")

barplot(or_vals,
        names.arg = or_labels,
        col       = c("darkorange", "gray70"),
        main      = "Odds Ratio\n(Smokers vs Nonsmokers)",
        ylab      = "Odds Ratio",
        ylim      = c(0, or_vals[1] * 1.3),
        las       = 1)

abline(h = 1, lty = 2, col = "black", lwd = 1.5)  # reference line at OR = 1

text(x      = 0.7,
     y      = odds_ratio + or_vals[1] * 0.04,
     labels = sprintf("%.4f", odds_ratio),
     cex    = 0.95)

par(mfrow = c(1, 1))


# =============================================================================
# Summary
# =============================================================================

cat("=============================================================\n")
cat("Summary\n")
cat("=============================================================\n")
cat(sprintf("  Proportion of smokers in Cases:    %.4f (%.2f%%)\n",
            prop_smoker_in_cases, prop_smoker_in_cases * 100))
cat(sprintf("  Proportion of smokers in Controls: %.4f (%.2f%%)\n",
            prop_smoker_in_controls, prop_smoker_in_controls * 100))
cat(sprintf("  Odds Ratio (OR):                   %.4f\n\n", odds_ratio))
cat("  (a) Response: lung cancer status | Explanatory: smoking status\n")
cat("  (b) Case-control (retrospective) study\n")
cat("  (c) Cannot compare disease proportions - case-control design\n")
cat("      fixes outcome totals, not exposure totals\n")
cat(sprintf("  (d) OR = %.4f -> smokers have ~%.1fx the odds of lung cancer\n",
            odds_ratio, odds_ratio))
