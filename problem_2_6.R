# =============================================================================
# 문제 2.6 - 폐암 사망 확률: 흡연자 vs 비흡연자
# 출처: Pagano & Gauvreau, Principles of Biostatistics (1993), p.134
#
# 미국에서 35세 이상 여성의 폐암으로 인한 연간 사망 확률:
#   - 현재 흡연자: 0.001304
#   - 비흡연자:    0.000121
# =============================================================================

# -----------------------------------------------------------------------------
# 0. 기본 데이터 설정
# -----------------------------------------------------------------------------

p_smoker    <- 0.001304   # 흡연자의 폐암 사망 확률 (π₁)
p_nonsmoker <- 0.000121   # 비흡연자의 폐암 사망 확률 (π₂)

cat("=== 기본 데이터 ===\n")
cat(sprintf("흡연자의 연간 폐암 사망 확률 (π₁): %.6f\n", p_smoker))
cat(sprintf("비흡연자의 연간 폐암 사망 확률 (π₂): %.6f\n\n", p_nonsmoker))


# =============================================================================
# (a) 비율 차이(Difference of Proportions)와 상대위험도(Relative Risk)
# =============================================================================

cat("=============================================================\n")
cat("(a) 비율 차이와 상대위험도\n")
cat("=============================================================\n\n")

# --- 비율 차이 (Difference of Proportions) ---
# 공식: π₁ - π₂
# 두 집단 간 사망 확률의 절대적 차이를 나타냄 (절대위험 감소/증가)

diff_proportions <- p_smoker - p_nonsmoker

cat("[ 비율 차이 (Difference of Proportions) ]\n")
cat(sprintf("  계산: π₁ - π₂ = %.6f - %.6f\n", p_smoker, p_nonsmoker))
cat(sprintf("  결과: %.6f\n", diff_proportions))
cat("\n  [해석] 흡연자는 비흡연자보다 폐암으로 사망할 확률이\n")
cat(sprintf("        약 %.6f (약 %.4f%%) 더 높다.\n", diff_proportions, diff_proportions * 100))
cat("        즉, 1만 명 기준으로 흡연자는 비흡연자보다 약")
cat(sprintf(" %.1f명 더 폐암으로 사망한다.\n\n", diff_proportions * 10000))


# --- 상대위험도 (Relative Risk, RR) ---
# 공식: π₁ / π₂
# 비흡연자 대비 흡연자의 사망 위험이 몇 배인지를 나타냄 (상대적 비교)

relative_risk <- p_smoker / p_nonsmoker

cat("[ 상대위험도 (Relative Risk, RR) ]\n")
cat(sprintf("  계산: π₁ / π₂ = %.6f / %.6f\n", p_smoker, p_nonsmoker))
cat(sprintf("  결과: %.4f\n", relative_risk))
cat("\n  [해석] 흡연자는 비흡연자에 비해 폐암으로 사망할 위험이\n")
cat(sprintf("        약 %.2f배 높다.\n\n", relative_risk))


# --- 어떤 것이 더 정보량이 많은가? ---
cat("[ 어느 측도가 더 유익한가? ]\n")
cat("  → 이 데이터에서는 '상대위험도(RR)'가 더 유익하다.\n")
cat("\n  이유:\n")
cat("  1) 비율 차이는 0.001183으로 매우 작아서,\n")
cat("     두 집단 간 차이가 거의 없는 것처럼 보일 수 있다.\n")
cat("     (두 확률 모두 절대적으로 매우 작기 때문)\n")
cat("\n  2) 반면 상대위험도는 약 10.78배로,\n")
cat("     흡연자의 위험이 비흡연자보다 현저히 높음을 명확하게 보여준다.\n")
cat("\n  3) 두 확률 모두 매우 희귀한 사건(rare event)이기 때문에\n")
cat("     절대적 차이보다 상대적 비율이 실질적 위험도를 더 잘 반영한다.\n\n")


# =============================================================================
# (b) 오즈비(Odds Ratio)와 상대위험도와 유사한 이유
# =============================================================================

cat("=============================================================\n")
cat("(b) 오즈비(Odds Ratio)와 상대위험도와의 관계\n")
cat("=============================================================\n\n")

# --- 오즈(Odds) 계산 ---
# 오즈 = 사건이 발생할 확률 / 사건이 발생하지 않을 확률
# 공식: π / (1 - π)

odds_smoker    <- p_smoker    / (1 - p_smoker)
odds_nonsmoker <- p_nonsmoker / (1 - p_nonsmoker)

cat("[ 각 집단의 오즈(Odds) ]\n")
cat(sprintf("  흡연자 오즈:    π₁ / (1-π₁) = %.6f / %.6f = %.8f\n",
            p_smoker, 1 - p_smoker, odds_smoker))
cat(sprintf("  비흡연자 오즈:  π₂ / (1-π₂) = %.6f / %.6f = %.8f\n\n",
            p_nonsmoker, 1 - p_nonsmoker, odds_nonsmoker))


# --- 오즈비 (Odds Ratio, OR) ---
# 공식: [π₁ / (1-π₁)] / [π₂ / (1-π₂)]
# 비흡연자 대비 흡연자의 오즈 비율

odds_ratio <- odds_smoker / odds_nonsmoker

cat("[ 오즈비 (Odds Ratio, OR) ]\n")
cat(sprintf("  계산: (%.8f) / (%.8f)\n", odds_smoker, odds_nonsmoker))
cat(sprintf("  결과: %.4f\n", odds_ratio))
cat("\n  [해석] 흡연자가 비흡연자에 비해 폐암으로 사망할\n")
cat(sprintf("        오즈가 약 %.2f배 높다.\n\n", odds_ratio))


# --- RR과 OR 비교 ---
cat("[ 상대위험도(RR) vs 오즈비(OR) 비교 ]\n")
cat(sprintf("  상대위험도 (RR): %.4f\n", relative_risk))
cat(sprintf("  오즈비      (OR): %.4f\n", odds_ratio))
cat(sprintf("  차이: %.6f\n\n", abs(odds_ratio - relative_risk)))


# --- 왜 RR과 OR이 유사한 값을 가지는가? ---
cat("[ RR과 OR이 유사한 이유 ]\n")
cat("  수학적 관계식:\n")
cat("    OR = RR × [(1 - π₂) / (1 - π₁)]\n\n")

correction_factor <- (1 - p_nonsmoker) / (1 - p_smoker)
cat(sprintf("  보정 인수: (1-π₂)/(1-π₁) = (1-%.6f)/(1-%.6f) = %.6f\n",
            p_nonsmoker, p_smoker, correction_factor))
cat(sprintf("  검증: RR × 보정인수 = %.4f × %.6f = %.4f ≈ OR(%.4f)\n\n",
            relative_risk, correction_factor,
            relative_risk * correction_factor, odds_ratio))

cat("  핵심 이유 - '희귀 사건 근사(Rare Disease Assumption)':\n")
cat(sprintf("    π₁ = %.6f << 1  →  (1 - π₁) ≈ 1\n", p_smoker))
cat(sprintf("    π₂ = %.6f << 1  →  (1 - π₂) ≈ 1\n", p_nonsmoker))
cat("\n  두 확률 모두 1에 비해 매우 작으므로:\n")
cat("    OR = [π₁/(1-π₁)] / [π₂/(1-π₂)]\n")
cat("       ≈ [π₁/1] / [π₂/1]\n")
cat("       = π₁/π₂\n")
cat("       = RR\n\n")
cat("  즉, 사건 발생 확률이 매우 낮을 때(희귀 사건일 때)\n")
cat("  오즈비는 상대위험도의 좋은 근사값이 된다.\n")
cat("  이 문제에서 두 확률 모두 0.1% 미만이므로 RR ≈ OR 이다.\n\n")


# =============================================================================
# 시각화: 두 측도 비교
# =============================================================================

cat("=============================================================\n")
cat("시각화\n")
cat("=============================================================\n\n")

# 그래프 저장 (선택사항: pdf나 png로 저장)
# pdf("problem_2_6_plot.pdf", width = 10, height = 7)

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# --- 그래프 1: 사망 확률 비교 막대그래프 ---
probs <- c(p_smoker, p_nonsmoker)
names(probs) <- c("Smoker", "Nonsmoker")

barplot(probs,
        col    = c("firebrick", "steelblue"),
        main   = "Annual Lung Cancer Mortality Probability",
        ylab   = "Probability of Death",
        ylim   = c(0, max(probs) * 1.3),
        las    = 1)

# 각 막대 위에 수치 표시
text(x      = c(0.7, 1.9),
     y      = probs + max(probs) * 0.03,
     labels = sprintf("%.6f", probs),
     cex    = 0.9)

# 비율 차이와 RR 정보 추가
mtext(sprintf("Diff = %.6f  |  RR = %.2f",
              diff_proportions, relative_risk),
      side = 1, line = 4, cex = 0.85)


# --- 그래프 2: RR과 OR 비교 막대그래프 ---
measures <- c(relative_risk, odds_ratio)
names(measures) <- c("Relative Risk\n(RR)", "Odds Ratio\n(OR)")

barplot(measures,
        col    = c("darkorange", "darkgreen"),
        main   = "Relative Risk vs Odds Ratio",
        ylab   = "Value",
        ylim   = c(0, max(measures) * 1.3),
        las    = 1)

text(x      = c(0.7, 1.9),
     y      = measures + max(measures) * 0.03,
     labels = sprintf("%.4f", measures),
     cex    = 0.9)

mtext("Values are nearly identical (rare disease approximation)",
      side = 1, line = 4, cex = 0.85)

par(mfrow = c(1, 1))  # 그래프 레이아웃 초기화
# dev.off()  # pdf 저장 시 활성화


# =============================================================================
# 최종 요약
# =============================================================================

cat("=============================================================\n")
cat("최종 요약\n")
cat("=============================================================\n")
cat(sprintf("  비율 차이 (Difference of Proportions): %.6f\n", diff_proportions))
cat(sprintf("  상대위험도 (Relative Risk, RR):        %.4f\n", relative_risk))
cat(sprintf("  오즈비     (Odds Ratio, OR):           %.4f\n", odds_ratio))
cat("\n  결론:\n")
cat("  - 비율 차이보다 상대위험도가 더 유익한 정보를 제공\n")
cat("  - RR(10.78)은 흡연이 폐암 사망 위험을 약 10.78배 높임을 명확히 보여줌\n")
cat("  - 확률이 매우 작으므로 OR ≈ RR (희귀 사건 근사 성립)\n")
