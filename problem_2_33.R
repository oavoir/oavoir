# =============================================================================
# 문제 2.33 - 플로리다 살인 재판 사형 선고 (1976-1977)
# 출처: M. Radelet, Am. Sociol. Rev., 46: 918-927, 1981
#
# 플로리다 20개 카운티, 3원 분할표:
#   - 피고인 인종 (Defendant's Race): White / Black
#   - 피해자 인종 (Victim's Race):    White / Black
#   - 사형 선고 여부 (Death Penalty): Yes / No
#
# 원자료:
#   White killed White: 19/151 사형
#   White killed Black:  0/9   사형
#   Black killed White: 11/63  사형
#   Black killed Black:  6/103 사형
# =============================================================================

# -----------------------------------------------------------------------------
# 0. 데이터 설정
# -----------------------------------------------------------------------------

# 셀별 빈도수 (사형 Yes / No)
# [피고인 인종, 피해자 인종] 순서

# 피해자 = White 층 (Layer 1)
def_white_vic_white_yes <- 19;  def_white_vic_white_no <- 132   # 151명 중 19명 사형
def_black_vic_white_yes <- 11;  def_black_vic_white_no <-  52   # 63명 중 11명 사형

# 피해자 = Black 층 (Layer 2)
def_white_vic_black_yes <-  0;  def_white_vic_black_no <-   9   # 9명 중 0명 사형
def_black_vic_black_yes <-  6;  def_black_vic_black_no <-  97   # 103명 중 6명 사형


# =============================================================================
# (a) 3원 분할표 (Three-Way Contingency Table)
# =============================================================================

cat("=============================================================\n")
cat("(a) 3원 분할표: 피고인 인종 × 사형 선고 × 피해자 인종\n")
cat("=============================================================\n\n")

# 3차원 배열 생성: [피고인 인종, 사형 여부, 피해자 인종]
three_way_table <- array(
  data = c(
    # 피해자 = White
    def_white_vic_white_yes, def_black_vic_white_yes,   # Death = Yes
    def_white_vic_white_no,  def_black_vic_white_no,    # Death = No
    # 피해자 = Black
    def_white_vic_black_yes, def_black_vic_black_yes,   # Death = Yes
    def_white_vic_black_no,  def_black_vic_black_no     # Death = No
  ),
  dim = c(2, 2, 2),
  dimnames = list(
    "Defendant" = c("White", "Black"),
    "Death Penalty" = c("Yes", "No"),
    "Victim" = c("White", "Black")
  )
)

cat("  [ 피해자 인종 = White ]\n")
print(three_way_table[, , "White"])
cat(sprintf("  행 합계: White def=%d, Black def=%d\n\n",
            sum(three_way_table[1,,"White"]), sum(three_way_table[2,,"White"])))

cat("  [ 피해자 인종 = Black ]\n")
print(three_way_table[, , "Black"])
cat(sprintf("  행 합계: White def=%d, Black def=%d\n\n",
            sum(three_way_table[1,,"Black"]), sum(three_way_table[2,,"Black"])))

# 사형 비율 요약
cat("  [ 사형 선고 비율 요약 ]\n")
cat(sprintf("  White def, White victim: %d/%d = %.4f (%.1f%%)\n",
            def_white_vic_white_yes, def_white_vic_white_yes+def_white_vic_white_no,
            def_white_vic_white_yes/(def_white_vic_white_yes+def_white_vic_white_no),
            100*def_white_vic_white_yes/(def_white_vic_white_yes+def_white_vic_white_no)))
cat(sprintf("  White def, Black victim:  %d/%d  = %.4f (%.1f%%)\n",
            def_white_vic_black_yes, def_white_vic_black_yes+def_white_vic_black_no,
            def_white_vic_black_yes/(def_white_vic_black_yes+def_white_vic_black_no),
            100*def_white_vic_black_yes/(def_white_vic_black_yes+def_white_vic_black_no)))
cat(sprintf("  Black def, White victim: %d/%d = %.4f (%.1f%%)\n",
            def_black_vic_white_yes, def_black_vic_white_yes+def_black_vic_white_no,
            def_black_vic_white_yes/(def_black_vic_white_yes+def_black_vic_white_no),
            100*def_black_vic_white_yes/(def_black_vic_white_yes+def_black_vic_white_no)))
cat(sprintf("  Black def, Black victim:  %d/%d = %.4f (%.1f%%)\n\n",
            def_black_vic_black_yes, def_black_vic_black_yes+def_black_vic_black_no,
            def_black_vic_black_yes/(def_black_vic_black_yes+def_black_vic_black_no),
            100*def_black_vic_black_yes/(def_black_vic_black_yes+def_black_vic_black_no)))


# =============================================================================
# (b) 조건부 오즈비 (Conditional Odds Ratios)
#     피해자 인종별 부분표 — 0 셀 보정: 각 셀에 +0.5 추가
# =============================================================================

cat("=============================================================\n")
cat("(b) 조건부 오즈비 (피해자 인종으로 층화)\n")
cat("=============================================================\n\n")

adj <- 0.5   # 0 셀 보정값 (각 셀에 더해주는 값)

cat(sprintf("  * 0 셀 처리: 각 셀에 %.1f을 더해 계산\n\n", adj))

# --- 층 1: 피해자 = White ---
# 부분표:
#            Death Yes    Death No
# White def    19           132
# Black def    11            52
a1 <- def_white_vic_white_yes + adj   # White def, Yes
b1 <- def_white_vic_white_no  + adj   # White def, No
c1 <- def_black_vic_white_yes + adj   # Black def, Yes
d1 <- def_black_vic_white_no  + adj   # Black def, No

or_white_victim <- (a1 * d1) / (b1 * c1)

cat("  [ 부분표 1: 피해자 = White (+0.5 보정 후) ]\n")
cat("                   Death Yes   Death No\n")
cat(sprintf("  White defendant     %5.1f      %5.1f\n", a1, b1))
cat(sprintf("  Black defendant     %5.1f      %5.1f\n", c1, d1))
cat(sprintf("\n  조건부 OR = (%.1f × %.1f) / (%.1f × %.1f)\n", a1, d1, b1, c1))
cat(sprintf("            = %.2f / %.2f\n", a1*d1, b1*c1))
cat(sprintf("            = %.4f\n\n", or_white_victim))
cat("  [해석] 피해자가 White일 때, 백인 피고인이 흑인 피고인보다\n")
cat(sprintf("         사형 선고 오즈가 약 %.2f배 → 백인 피고인에게 유리\n\n", or_white_victim))


# --- 층 2: 피해자 = Black ---
# 부분표:
#            Death Yes    Death No
# White def     0             9
# Black def     6            97
a2 <- def_white_vic_black_yes + adj   # White def, Yes
b2 <- def_white_vic_black_no  + adj   # White def, No
c2 <- def_black_vic_black_yes + adj   # Black def, Yes
d2 <- def_black_vic_black_no  + adj   # Black def, No

or_black_victim <- (a2 * d2) / (b2 * c2)

cat("  [ 부분표 2: 피해자 = Black (+0.5 보정 후) ]\n")
cat("                   Death Yes   Death No\n")
cat(sprintf("  White defendant     %5.1f      %5.1f\n", a2, b2))
cat(sprintf("  Black defendant     %5.1f      %5.1f\n", c2, d2))
cat(sprintf("\n  조건부 OR = (%.1f × %.1f) / (%.1f × %.1f)\n", a2, d2, b2, c2))
cat(sprintf("            = %.4f / %.4f\n", a2*d2, b2*c2))
cat(sprintf("            = %.4f\n\n", or_black_victim))
cat("  [해석] 피해자가 Black일 때, 백인 피고인이 흑인 피고인보다\n")
cat(sprintf("         사형 선고 오즈가 약 %.2f배 → 백인 피고인에게 유리\n\n", or_black_victim))


cat("  [ 조건부 OR 요약 ]\n")
cat(sprintf("  피해자 White: OR = %.4f (< 1, 백인 피고에게 유리)\n", or_white_victim))
cat(sprintf("  피해자 Black: OR = %.4f (< 1, 백인 피고에게 유리)\n\n", or_black_victim))
cat("  → 두 층 모두 OR < 1: 피해자 인종을 통제하면,\n")
cat("    백인 피고인이 흑인 피고인보다 사형 선고를 덜 받는다.\n\n")


# =============================================================================
# (c) 주변 오즈비 (Marginal OR) & 심슨의 역설
# =============================================================================

cat("=============================================================\n")
cat("(c) 주변 오즈비 & 심슨의 역설(Simpson's Paradox)\n")
cat("=============================================================\n\n")

# 피해자 인종을 무시하고 합산한 주변표
marg_white_yes <- def_white_vic_white_yes + def_white_vic_black_yes   # 19+0 = 19
marg_white_no  <- def_white_vic_white_no  + def_white_vic_black_no    # 132+9 = 141
marg_black_yes <- def_black_vic_white_yes + def_black_vic_black_yes   # 11+6 = 17
marg_black_no  <- def_black_vic_white_no  + def_black_vic_black_no    # 52+97 = 149

cat("  [ 주변표 (피해자 인종 합산) ]\n")
cat("                   Death Yes   Death No   합계\n")
cat(sprintf("  White defendant     %3d         %3d       %3d\n",
            marg_white_yes, marg_white_no, marg_white_yes+marg_white_no))
cat(sprintf("  Black defendant     %3d         %3d       %3d\n\n",
            marg_black_yes, marg_black_no, marg_black_yes+marg_black_no))

# 주변 OR (0 셀 없으므로 보정 없이 계산)
marginal_or <- (marg_white_yes * marg_black_no) / (marg_white_no * marg_black_yes)

cat("  [ 주변 오즈비 ]\n")
cat(sprintf("  OR = (%d × %d) / (%d × %d)\n",
            marg_white_yes, marg_black_no, marg_white_no, marg_black_yes))
cat(sprintf("     = %d / %d\n",
            marg_white_yes*marg_black_no, marg_white_no*marg_black_yes))
cat(sprintf("     = %.4f\n\n", marginal_or))

cat(sprintf("  [해석] 주변 OR = %.4f > 1:\n", marginal_or))
cat("  피해자 인종을 무시하면, 백인 피고인이 흑인 피고인보다\n")
cat("  사형 선고 오즈가 더 높아 보인다.\n\n")


# --- 심슨의 역설 판단 ---
cat("  [ 심슨의 역설(Simpson's Paradox) 판단 ]\n\n")

cat("  ┌─────────────────────────────────────────────────────────┐\n")
cat("  │  조건부 OR (피해자 White): 0.67 < 1 → 백인 피고 유리 │\n")
cat("  │  조건부 OR (피해자 Black): 0.79 < 1 → 백인 피고 유리 │\n")
cat("  │  주변 OR (피해자 인종 무시): > 1   → 백인 피고 불리  │\n")
cat("  └─────────────────────────────────────────────────────────┘\n\n")

cat("  → 심슨의 역설 성립!\n\n")

cat("  [ 발생 원인 ]\n")
cat("  1) 피해자가 White인 사건은 전체의 대다수(214건 중)이며\n")
cat("     사형 선고율이 전반적으로 높다.\n")
cat("  2) 백인 피고인은 주로 White 피해자 사건(151건)에 집중되어 있고,\n")
cat("     흑인 피고인도 White 피해자 사건(63건)이 있지만\n")
cat("     Black 피해자 사건(103건)이 더 많다.\n")
cat("  3) 즉, 피해자 인종이 교란변수(confounding variable)로 작용하여\n")
cat("     층화 전후로 OR의 방향이 역전된다.\n\n")

cat("  결론: 피해자 인종을 통제하면 백인 피고인이 유리하지만,\n")
cat("  피해자 인종을 무시한 주변 OR은 반대 결과를 보여준다.\n")
cat("  이는 교란변수를 고려하지 않을 때 발생하는 심슨의 역설의 전형적 예이다.\n\n")


# =============================================================================
# 시각화
# =============================================================================

par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))

# --- 그래프 1: 층별 사형 선고 비율 ---
death_rates <- c(
  def_white_vic_white_yes / (def_white_vic_white_yes + def_white_vic_white_no),
  def_black_vic_white_yes / (def_black_vic_white_yes + def_black_vic_white_no),
  def_white_vic_black_yes / (def_white_vic_black_yes + def_white_vic_black_no),
  def_black_vic_black_yes / (def_black_vic_black_yes + def_black_vic_black_no)
)
bar_labels <- c("White def\nWhite vic", "Black def\nWhite vic",
                "White def\nBlack vic", "Black def\nBlack vic")
bar_cols   <- c("firebrick", "steelblue", "tomato", "cornflowerblue")

mp1 <- barplot(death_rates,
               names.arg = bar_labels,
               col       = bar_cols,
               main      = "Death Penalty Rate\nby Defendant & Victim Race",
               ylab      = "Proportion Sentenced to Death",
               ylim      = c(0, 0.25),
               las       = 1,
               cex.names = 0.8)
text(mp1, death_rates + 0.008,
     labels = sprintf("%.3f", death_rates), cex = 0.85)


# --- 그래프 2: 조건부 OR vs 주변 OR ---
or_vals   <- c(or_white_victim, or_black_victim, marginal_or)
or_labels <- c("Conditional OR\n(Victim=White)",
                "Conditional OR\n(Victim=Black)",
                "Marginal OR\n(Combined)")
or_cols   <- c("steelblue", "steelblue", "firebrick")

mp2 <- barplot(or_vals,
               names.arg = or_labels,
               col       = or_cols,
               main      = "Conditional vs Marginal\nOdds Ratios",
               ylab      = "Odds Ratio",
               ylim      = c(0, max(or_vals) * 1.3),
               las       = 1,
               cex.names = 0.75)
abline(h = 1, lty = 2, col = "black", lwd = 1.5)   # OR=1 기준선
text(mp2, or_vals + 0.03,
     labels = round(or_vals, 4), cex = 0.85)
legend("topright",
       legend = c("Conditional OR (< 1: White def favored)",
                  "Marginal OR (> 1: Black def favored)",
                  "OR = 1 (no association)"),
       fill   = c("steelblue", "firebrick", NA),
       lty    = c(NA, NA, 2),
       col    = c(NA, NA, "black"),
       bty    = "n", cex = 0.65)


# --- 그래프 3: 주변표 비율 비교 ---
marg_rates <- c(marg_white_yes / (marg_white_yes + marg_white_no),
                marg_black_yes / (marg_black_yes + marg_black_no))

mp3 <- barplot(marg_rates,
               names.arg = c("White\nDefendant", "Black\nDefendant"),
               col       = c("firebrick", "steelblue"),
               main      = "Marginal Death Penalty Rate\n(Victim Race Ignored)",
               ylab      = "Proportion Sentenced to Death",
               ylim      = c(0, max(marg_rates) * 1.4),
               las       = 1)
text(mp3, marg_rates + 0.003,
     labels = sprintf("%.3f", marg_rates), cex = 0.9)

par(mfrow = c(1, 1))


# =============================================================================
# 최종 요약
# =============================================================================

cat("=============================================================\n")
cat("최종 요약\n")
cat("=============================================================\n")
cat(sprintf("  조건부 OR (피해자 White, +0.5 보정): %.4f\n", or_white_victim))
cat(sprintf("  조건부 OR (피해자 Black, +0.5 보정): %.4f\n", or_black_victim))
cat(sprintf("  주변 OR   (피해자 인종 합산):         %.4f\n\n", marginal_or))
cat("  (a) 3원 분할표: 피고인 인종 × 사형 여부 × 피해자 인종\n")
cat("  (b) 두 층 모두 OR < 1 → 피해자 인종 통제 시 백인 피고에게 유리\n")
cat("  (c) 주변 OR > 1 → 심슨의 역설 성립\n")
cat("      원인: 피해자 인종이 교란변수로 작용\n")
