# =============================================================================
# 문제 2.23 - General Social Survey: 교육 수준과 종교적 신념의 연관성
# 출처: Table 2.16, General Social Survey
#
# 교육 수준(최고 학력)과 종교적 신념(근본주의/온건/자유주의)의
# 교차분류표. X² = 69.2 (문제에서 주어진 값)
#
# 표 2.16:
#                              Fundamentalist  Moderate  Liberal
# Less than high school            178(4.5)   138(-2.6) 108(-1.9)
# High school or junior college    570         648(1.3)  442(-4.0)
# Bachelor or graduate             138(-6.8)  252(0.7)  252(6.3)
# =============================================================================

# -----------------------------------------------------------------------------
# 0. 데이터 설정 - 3×3 분할표
# -----------------------------------------------------------------------------

# 관측 빈도수 입력 (행: 교육수준, 열: 종교적 신념)
obs_counts <- matrix(
  c(178, 570, 138,   # Fundamentalist 열
    138, 648, 252,   # Moderate 열
    108, 442, 252),  # Liberal 열
  nrow = 3, ncol = 3,
  dimnames = list(
    "Highest Degree" = c("Less than HS", "HS or Junior College", "Bachelor or Graduate"),
    "Religious Beliefs" = c("Fundamentalist", "Moderate", "Liberal")
  )
)

cat("=== 표 2.16: 교육 수준 × 종교적 신념 분할표 ===\n")
print(obs_counts)

# 행/열/전체 합계
row_totals <- rowSums(obs_counts)
col_totals <- colSums(obs_counts)
grand_total <- sum(obs_counts)

cat(sprintf("\n  행 합계: %s\n", paste(row_totals, collapse = ", ")))
cat(sprintf("  열 합계: %s\n", paste(col_totals, collapse = ", ")))
cat(sprintf("  전체 합계: %d명\n\n", grand_total))


# =============================================================================
# 1. 기대 빈도수(Expected Frequencies) 계산
# =============================================================================

cat("=============================================================\n")
cat("1. 기대 빈도수\n")
cat("=============================================================\n\n")

# 기대 빈도: E(i,j) = (행 합계_i × 열 합계_j) / 전체 합계
expected_counts <- outer(row_totals, col_totals) / grand_total

cat("  기대 빈도수 행렬:\n")
print(round(expected_counts, 2))
cat("\n")


# =============================================================================
# 2. 카이제곱 검정 (Chi-Square Test of Independence)
# =============================================================================

cat("=============================================================\n")
cat("2. 카이제곱 독립성 검정\n")
cat("=============================================================\n\n")

# R의 chisq.test()로 검정 수행
# correct=FALSE: Yates 연속성 보정 없음 (표준 카이제곱)
chi_result <- chisq.test(obs_counts, correct = FALSE)

cat("  [ 가설 ]\n")
cat("  H0: 교육 수준과 종교적 신념은 독립이다 (연관성 없음)\n")
cat("  H1: 교육 수준과 종교적 신념은 독립이 아니다 (연관성 있음)\n\n")

cat("  [ 검정 결과 ]\n")
cat(sprintf("  카이제곱 통계량 (X²): %.4f\n", chi_result$statistic))
cat(sprintf("  자유도 (df):          %d  ← (행수-1)×(열수-1) = 2×2\n", chi_result$parameter))
cat(sprintf("  p-값:                 %.2e\n\n", chi_result$p.value))

# 문제에서 주어진 X² = 69.2 확인
cat(sprintf("  * 문제에서 주어진 X² = 69.2  (R 계산값: %.1f)\n\n", chi_result$statistic))

cat("  [ 결론 ]\n")
if (chi_result$p.value < 0.001) {
  cat("  p < 0.001 이므로 유의수준 0.05에서 H0 기각.\n")
  cat("  교육 수준과 종교적 신념 사이에 통계적으로 유의한 연관성이 있다.\n\n")
}


# =============================================================================
# 3. 표준화 잔차(Standardized Residuals) 분석
# =============================================================================

cat("=============================================================\n")
cat("3. 표준화 잔차 분석\n")
cat("=============================================================\n\n")

# 표준화 잔차 = (관측 - 기대) / sqrt(기대 × (1 - 행비율) × (1 - 열비율))
# chisq.test()의 $stdres 가 이 값을 제공
std_residuals <- chi_result$stdres

cat("  표준화 잔차 행렬:\n")
print(round(std_residuals, 1))

cat("\n  해석 기준: |표준화 잔차| > 2 이면 통계적으로 유의한 셀\n\n")

cat("  [ 셀별 해석 ]\n")
cat("  ┌──────────────────────────────────────────────────────────┐\n")
cat("  │ Less than HS  × Fundamentalist: +4.5  → 기대보다 많음  │\n")
cat("  │ Less than HS  × Moderate:       -2.6  → 기대보다 적음  │\n")
cat("  │ Less than HS  × Liberal:        -1.9  → 거의 기대 수준 │\n")
cat("  │ HS/JC         × Liberal:        -4.0  → 기대보다 적음  │\n")
cat("  │ Bachelor+     × Fundamentalist: -6.8  → 기대보다 훨씬 적음│\n")
cat("  │ Bachelor+     × Liberal:        +6.3  → 기대보다 훨씬 많음│\n")
cat("  └──────────────────────────────────────────────────────────┘\n\n")


# =============================================================================
# 4. 행 비율(Row Proportions) - 패턴 파악용
# =============================================================================

cat("=============================================================\n")
cat("4. 행 비율 (교육 수준별 종교 신념 분포)\n")
cat("=============================================================\n\n")

row_props <- prop.table(obs_counts, margin = 1)  # 행 기준 비율

cat("  교육 수준별 종교 신념 비율:\n")
print(round(row_props, 4))
cat("\n")

cat("  요약:\n")
cat(sprintf("  고졸 미만    → 근본주의 %.1f%%, 온건 %.1f%%, 자유 %.1f%%\n",
            row_props[1,1]*100, row_props[1,2]*100, row_props[1,3]*100))
cat(sprintf("  고졸/전문대  → 근본주의 %.1f%%, 온건 %.1f%%, 자유 %.1f%%\n",
            row_props[2,1]*100, row_props[2,2]*100, row_props[2,3]*100))
cat(sprintf("  학사 이상    → 근본주의 %.1f%%, 온건 %.1f%%, 자유 %.1f%%\n\n",
            row_props[3,1]*100, row_props[3,2]*100, row_props[3,3]*100))


# =============================================================================
# 5. 시각화
# =============================================================================

par(mfrow = c(1, 2), mar = c(6, 5, 4, 2))

# --- 그래프 1: 교육 수준별 종교 신념 누적 막대그래프 ---
bar_colors <- c("firebrick", "steelblue", "darkgreen")

barplot(t(row_props),
        col        = bar_colors,
        main       = "Religious Beliefs\nby Education Level",
        ylab       = "Proportion",
        names.arg  = c("< High\nSchool", "HS / JC", "Bachelor+"),
        ylim       = c(0, 1.2),
        legend.text = c("Fundamentalist", "Moderate", "Liberal"),
        args.legend = list(x = "topright", bty = "n", cex = 0.8),
        las        = 1)


# --- 그래프 2: 표준화 잔차 히트맵 스타일 막대그래프 ---
std_vec   <- as.vector(t(std_residuals))   # 행별로 풀기
cell_names <- paste0(
  rep(c("<HS", "HS/JC", "Bach+"), each = 3),
  "\n",
  rep(c("Fund.", "Mod.", "Lib."), times = 3)
)
bar_col <- ifelse(std_vec > 0, "tomato", "cornflowerblue")

mp <- barplot(std_vec,
              names.arg = cell_names,
              col       = bar_col,
              main      = "Standardized Residuals",
              ylab      = "Standardized Residual",
              ylim      = c(min(std_vec) - 1, max(std_vec) + 1),
              las       = 2,
              cex.names = 0.75)

abline(h =  2, lty = 2, col = "darkred",  lwd = 1.5)   # 양의 기준선
abline(h = -2, lty = 2, col = "darkblue", lwd = 1.5)   # 음의 기준선
abline(h =  0, lty = 1, col = "black",    lwd = 0.8)

text(mp, std_vec + sign(std_vec) * 0.3,
     labels = round(std_vec, 1), cex = 0.8)

legend("topright",
       legend = c("Positive (more than expected)",
                  "Negative (less than expected)",
                  "|z| = 2 threshold"),
       fill   = c("tomato", "cornflowerblue", NA),
       lty    = c(NA, NA, 2),
       col    = c(NA, NA, "darkred"),
       bty    = "n", cex = 0.7)

par(mfrow = c(1, 1))


# =============================================================================
# 6. 약 200단어 보고서 (Report)
# =============================================================================

cat("=============================================================\n")
cat("6. 보고서 (약 200단어)\n")
cat("=============================================================\n\n")

cat("[ Description ]\n")
cat("  General Social Survey 자료에서 2,726명의 교육 수준(고졸 미만,\n")
cat("  고졸/전문대, 학사 이상)과 종교적 신념(근본주의, 온건, 자유주의)의\n")
cat("  관계를 분석하였다.\n\n")

cat("  행 비율을 보면, 교육 수준이 높을수록 근본주의 비율은 감소하고\n")
cat("  자유주의 비율은 증가하는 경향이 뚜렷하다.\n")
cat(sprintf("  고졸 미만 집단에서는 근본주의 비율이 %.1f%%로 가장 높고,\n",
            row_props[1,1]*100))
cat(sprintf("  학사 이상 집단에서는 자유주의 비율이 %.1f%%로 가장 높다.\n\n",
            row_props[3,3]*100))

cat("[ Inference ]\n")
cat(sprintf("  카이제곱 독립성 검정 결과 X² = %.1f, df = 4, p < 0.001로\n",
            chi_result$statistic))
cat("  교육 수준과 종교적 신념 사이에 통계적으로 매우 유의한 연관성이\n")
cat("  있음이 확인되었다.\n\n")

cat("  표준화 잔차 분석에서 특히 두드러진 셀은 다음과 같다:\n")
cat("  - 학사 이상 × 근본주의 (z = -6.8): 기대보다 훨씬 적음\n")
cat("  - 학사 이상 × 자유주의  (z = +6.3): 기대보다 훨씬 많음\n")
cat("  - 고졸 미만  × 근본주의 (z = +4.5): 기대보다 많음\n")
cat("  - 고졸/전문대 × 자유주의 (z = -4.0): 기대보다 적음\n\n")

cat("  결론: 교육 수준이 높을수록 근본주의적 종교 신념은 약해지고\n")
cat("  자유주의적 종교 신념이 강해지는 유의한 연관성이 존재한다.\n")
cat("  이는 교육이 종교적 신념의 다양성 및 자유주의적 사고와 관련될\n")
cat("  수 있음을 시사한다.\n\n")


# =============================================================================
# 최종 요약
# =============================================================================

cat("=============================================================\n")
cat("최종 요약\n")
cat("=============================================================\n")
cat(sprintf("  전체 표본 수:        %d명\n", grand_total))
cat(sprintf("  카이제곱 통계량 X²:  %.1f\n", chi_result$statistic))
cat(sprintf("  자유도 (df):         %d\n", chi_result$parameter))
cat(sprintf("  p-값:                %.2e\n", chi_result$p.value))
cat("  결론: 교육 수준과 종교적 신념은 통계적으로 유의한 연관성이 있음\n")
cat("        (교육 수준 ↑ → 근본주의 ↓, 자유주의 ↑)\n")
