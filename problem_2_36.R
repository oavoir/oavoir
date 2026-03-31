# =============================================================================
# 문제 2.36 - 주변 연관(Marginal Association)은 있지만
#             조건부 독립(Conditional Independence)인 실제 사례
#
# 개념 정리:
#   - 주변 연관(Marginal Association): Z를 무시했을 때 X와 Y 사이에 연관성 있음
#   - 조건부 독립(Conditional Independence): Z를 통제하면 X와 Y 사이에 연관성 없음
#   - 이는 Z가 X와 Y 모두의 공통 원인(교란변수, Confounding Variable)일 때 발생
# =============================================================================

# -----------------------------------------------------------------------------
# 실제 사례 설명
# -----------------------------------------------------------------------------

cat("=============================================================\n")
cat("실제 사례: 아이스크림 판매량 vs 익사 사망자 수\n")
cat("=============================================================\n\n")

cat("  변수 설정:\n")
cat("  X = 아이스크림 판매량 (Ice cream sales)\n")
cat("  Y = 익사 사망자 수    (Drowning deaths)\n")
cat("  Z = 기온 / 계절       (Temperature / Season)\n\n")

cat("  관찰:\n")
cat("  - X와 Y의 주변 연관: 아이스크림이 많이 팔릴수록 익사 사고도 많다\n")
cat("    (양의 상관관계 관찰됨)\n\n")
cat("  - Z를 통제한 후 조건부 독립:\n")
cat("    기온(계절)이 같은 집단 내에서는 아이스크림 판매량과\n")
cat("    익사 사망자 수 사이에 연관성이 사라진다\n\n")
cat("  - 진짜 원인:\n")
cat("    기온이 높으면 → 아이스크림 소비 증가 (X 증가)\n")
cat("    기온이 높으면 → 수영/수상활동 증가 → 익사 사고 증가 (Y 증가)\n")
cat("    X → Y 직접 인과관계 없음. Z가 공통 원인(교란변수)!\n\n")

cat("  구조:\n")
cat("         Z (기온)\n")
cat("        ↙       ↘\n")
cat("    X (아이스크림)   Y (익사 사망)\n")
cat("       ← 직접 연관 없음 →\n\n")


# =============================================================================
# 데이터 시뮬레이션으로 개념 증명
# =============================================================================

cat("=============================================================\n")
cat("시뮬레이션으로 개념 증명\n")
cat("=============================================================\n\n")

set.seed(42)   # 재현 가능성을 위한 난수 고정
n <- 300       # 표본 수 (월 단위 300개 관측)

# Z: 기온 (정규분포, 평균 15도, 표준편차 8도)
temperature <- rnorm(n, mean = 15, sd = 8)

# X: 아이스크림 판매량 = 기온에 비례 + 랜덤 노이즈
#    Z → X (기온이 오르면 아이스크림 판매 증가)
ice_cream_sales <- 50 + 3 * temperature + rnorm(n, mean = 0, sd = 5)

# Y: 익사 사망자 수 = 기온에 비례 + 랜덤 노이즈
#    Z → Y (기온이 오르면 수상활동 증가 → 익사 증가)
#    X와 Y는 직접 연관 없음 (공통 원인 Z만 공유)
drowning_deaths <- 2 + 0.4 * temperature + rnorm(n, mean = 0, sd = 2)

cat("  시뮬레이션 설정:\n")
cat(sprintf("  표본 수 n = %d\n", n))
cat("  Z (기온) ~ N(15, 8²)\n")
cat("  X (아이스크림) = 50 + 3×Z + 노이즈\n")
cat("  Y (익사 사망)  =  2 + 0.4×Z + 노이즈\n")
cat("  → X와 Y는 Z만 공유, 직접 인과관계 없음\n\n")


# --- 1) 주변 상관: X와 Y의 단순 상관 (Z 무시) ---
marginal_cor <- cor.test(ice_cream_sales, drowning_deaths)

cat("  [ 1) 주변 연관 (Z 무시) ]\n")
cat(sprintf("  Pearson 상관계수 r = %.4f\n", marginal_cor$estimate))
cat(sprintf("  p-값 = %.2e\n", marginal_cor$p.value))
if (marginal_cor$p.value < 0.05) {
  cat("  → p < 0.05: X와 Y 사이에 유의한 양의 상관 관찰됨\n")
  cat("    (아이스크림↑ → 익사↑ 처럼 보임 — 허위 연관!)\n\n")
}


# --- 2) 조건부 연관: Z를 통제한 후 편상관 ---
# 방법: X와 Y 각각을 Z에 회귀한 잔차(residual)끼리 상관
resid_x <- residuals(lm(ice_cream_sales ~ temperature))   # Z 제거 후 X 잔차
resid_y <- residuals(lm(drowning_deaths  ~ temperature))  # Z 제거 후 Y 잔차

conditional_cor <- cor.test(resid_x, resid_y)

cat("  [ 2) 조건부 연관 (Z=기온 통제 후 편상관) ]\n")
cat("  방법: X, Y 각각을 Z에 회귀한 잔차끼리 상관\n")
cat(sprintf("  편상관계수 r = %.4f\n", conditional_cor$estimate))
cat(sprintf("  p-값 = %.4f\n", conditional_cor$p.value))
if (conditional_cor$p.value >= 0.05) {
  cat("  → p ≥ 0.05: Z 통제 후 X와 Y 사이에 유의한 연관 없음\n")
  cat("    (조건부 독립 확인!)\n\n")
}


# --- 3) 다중회귀로도 확인 ---
# Y ~ X + Z 모델에서 X의 계수가 유의하지 않으면 조건부 독립
multi_model <- lm(drowning_deaths ~ ice_cream_sales + temperature)

cat("  [ 3) 다중회귀 확인: Y ~ X + Z ]\n")
cat("  모델: 익사 사망 ~ 아이스크림 판매 + 기온\n\n")
print(summary(multi_model)$coefficients)
cat("\n")
x_pval <- summary(multi_model)$coefficients["ice_cream_sales", "Pr(>|t|)"]
z_pval <- summary(multi_model)$coefficients["temperature", "Pr(>|t|)"]
cat(sprintf("  X(아이스크림) p-값: %.4f %s\n",
            x_pval, ifelse(x_pval >= 0.05, "→ 유의하지 않음 (조건부 독립)", "→ 유의함")))
cat(sprintf("  Z(기온)       p-값: %.4f %s\n\n",
            z_pval, ifelse(z_pval < 0.05, "→ 유의함 (진짜 원인)", "→ 유의하지 않음")))


# =============================================================================
# 결론 요약
# =============================================================================

cat("=============================================================\n")
cat("결론\n")
cat("=============================================================\n\n")
cat("  주변 상관 (Z 무시):\n")
cat(sprintf("    r = %.4f, p < 0.001 → X와 Y 강한 양의 연관\n\n", marginal_cor$estimate))
cat("  조건부 상관 (Z 통제 후):\n")
cat(sprintf("    r ≈ %.4f, p = %.4f → X와 Y 연관 없음 (조건부 독립)\n\n",
            conditional_cor$estimate, conditional_cor$p.value))
cat("  핵심 교훈:\n")
cat("  '아이스크림이 사람을 익사시킨다'는 허위 결론을 내릴 수 있지만,\n")
cat("  실제로는 기온(Z)이 두 변수 모두의 공통 원인이다.\n")
cat("  교란변수를 통제하지 않으면 잘못된 인과관계를 추론할 수 있다.\n\n")
cat("  → 이것이 '상관관계 ≠ 인과관계'의 핵심 이유 중 하나이다.\n")


# =============================================================================
# 시각화
# =============================================================================

par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))

# --- 그래프 1: X vs Y 주변 산점도 (허위 연관) ---
plot(ice_cream_sales, drowning_deaths,
     main = "Marginal Association\n(Spurious - Z ignored)",
     xlab = "Ice Cream Sales (X)",
     ylab = "Drowning Deaths (Y)",
     pch  = 16, col = rgb(0.8, 0.2, 0.2, 0.4), cex = 0.7)
abline(lm(drowning_deaths ~ ice_cream_sales), col = "firebrick", lwd = 2)
legend("topleft",
       legend = sprintf("r = %.3f\np < 0.001", marginal_cor$estimate),
       bty = "n", cex = 0.9)


# --- 그래프 2: Z와 X, Y의 관계 (공통 원인) ---
plot(temperature, ice_cream_sales,
     main = "Common Cause Z\n(Temperature drives both)",
     xlab = "Temperature / Season (Z)",
     ylab = "Value",
     pch  = 16, col = rgb(0.8, 0.2, 0.2, 0.4), cex = 0.7,
     ylim = c(min(ice_cream_sales, drowning_deaths*10),
              max(ice_cream_sales)))
points(temperature, drowning_deaths * 10,   # Y를 스케일 맞춰 표시
       pch = 17, col = rgb(0.2, 0.2, 0.8, 0.4), cex = 0.7)
abline(lm(ice_cream_sales ~ temperature), col = "firebrick", lwd = 2)
abline(lm(I(drowning_deaths*10) ~ temperature), col = "steelblue", lwd = 2)
legend("topleft",
       legend = c("Ice Cream Sales (X)", "Drowning x10 (Y)"),
       col    = c("firebrick", "steelblue"),
       pch    = c(16, 17), lty = 1, bty = "n", cex = 0.8)


# --- 그래프 3: 잔차 산점도 (조건부 독립) ---
plot(resid_x, resid_y,
     main = "Conditional Independence\n(Z controlled - residuals)",
     xlab = "Residual of X | Z",
     ylab = "Residual of Y | Z",
     pch  = 16, col = rgb(0.2, 0.6, 0.2, 0.4), cex = 0.7)
abline(lm(resid_y ~ resid_x), col = "darkgreen", lwd = 2)
abline(h = 0, lty = 2, col = "gray")
abline(v = 0, lty = 2, col = "gray")
legend("topleft",
       legend = sprintf("r = %.3f\np = %.3f", conditional_cor$estimate, conditional_cor$p.value),
       bty = "n", cex = 0.9)

par(mfrow = c(1, 1))
