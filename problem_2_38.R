# =============================================================================
# 문제 2.38 - 3원 분할표에서 조건부 독립과 동질 연관의 관계
#
# 핵심 개념:
#   - 조건부 독립 (Conditional Independence):
#       Z의 각 수준에서 X와 Y의 조건부 OR = 1
#   - 동질 연관 (Homogeneous Association):
#       세 번째 변수의 수준에 관계없이 나머지 두 변수의 조건부 OR이 동일
#   - 로그선형 모형 관점:
#       포화 모형: log μ_ijk = λ + λ_i^X + λ_j^Y + λ_k^Z
#                            + λ_ij^XY + λ_ik^XZ + λ_jk^YZ + λ_ijk^XYZ
#       동질 연관  ↔ λ^XYZ = 0   (3원 상호작용 없음)  → 모형 [XY, XZ, YZ]
#       조건부 독립 ↔ λ^XY  = 0 이고 λ^XYZ = 0      → 모형 [XZ, YZ]  (더 강한 조건)
# =============================================================================

# 오즈비 계산 헬퍼 함수
calc_or <- function(a, b, c, d, adj = 0) {
  # OR = (a*d) / (b*c),  adj: 0 셀 보정값
  ((a + adj) * (d + adj)) / ((b + adj) * (c + adj))
}


# =============================================================================
# (a) 어떤 쌍이라도 조건부 독립이면 → 동질 연관이 성립한다
# =============================================================================

cat("=============================================================\n")
cat("(a) 조건부 독립 → 동질 연관\n")
cat("=============================================================\n\n")

cat("[ 이론적 설명 ]\n\n")

cat("  조건부 독립(X⊥Y|Z)의 의미:\n")
cat("  - Z의 모든 수준 k에서 OR(XY|Z=k) = 1\n")
cat("  - 로그선형 모형에서 λ^XY = 0 이고 λ^XYZ = 0 을 의미\n\n")

cat("  동질 연관의 의미:\n")
cat("  - 세 쌍 (XY|Z), (XZ|Y), (YZ|X) 각각의 조건부 OR이\n")
cat("    세 번째 변수의 수준에 관계없이 동일\n")
cat("  - 로그선형 모형에서 λ^XYZ = 0 을 의미 (3원 상호작용 없음)\n\n")

cat("  논리적 연결:\n")
cat("  조건부 독립(어느 한 쌍이라도) → λ^XYZ = 0 이 필요조건\n")
cat("  λ^XYZ = 0 ↔ 동질 연관\n")
cat("  ∴ 조건부 독립 → 동질 연관  (조건부 독립은 더 강한 조건)\n\n")

cat("  수식으로:\n")
cat("  OR(XY|Z=k) = exp(λ^XY + λ^XYZ_k)\n")
cat("  X⊥Y|Z 이면 모든 k에서 OR=1, 즉 λ^XY=0, λ^XYZ_k=0\n")
cat("  → OR(XZ|Y=j) = exp(λ^XZ + λ^XYZ_j) = exp(λ^XZ) : j에 무관 → 동질\n")
cat("  → OR(YZ|X=i) = exp(λ^YZ + λ^XYZ_i) = exp(λ^YZ) : i에 무관 → 동질\n\n")


# --- 수치 예시로 확인 ---
cat("[ 수치 예시: X⊥Y|Z 인 3원 분할표 ]\n\n")

# 모형 [XZ, YZ]: X와 Y는 조건부 독립 (직접 연관 없음)
# 3차원 배열: [X, Y, Z]

# Z=1 층: OR(XY|Z=1) = 1 이 되도록 설정
#        Y=1  Y=2
# X=1     10   20
# X=2     20   40
layer_z1 <- matrix(c(10, 20, 20, 40), nrow = 2,
                   dimnames = list(X = c("X=1","X=2"), Y = c("Y=1","Y=2")))

# Z=2 층: OR(XY|Z=2) = 1 이 되도록 설정
#        Y=1  Y=2
# X=1     30   15
# X=2     20   10
layer_z2 <- matrix(c(30, 20, 15, 10), nrow = 2,
                   dimnames = list(X = c("X=1","X=2"), Y = c("Y=1","Y=2")))

cat("  Z=1 층:\n"); print(layer_z1)
cat(sprintf("  OR(XY|Z=1) = (10×40)/(20×20) = %.4f\n\n",
            calc_or(10, 20, 20, 40)))

cat("  Z=2 층:\n"); print(layer_z2)
cat(sprintf("  OR(XY|Z=2) = (30×10)/(15×20) = %.4f\n\n",
            calc_or(30, 15, 20, 10)))

cat("  → OR(XY|Z=1) = OR(XY|Z=2) = 1: X⊥Y|Z 확인\n\n")

# 이제 다른 두 쌍의 동질 연관 확인
# OR(XZ|Y): Y 수준별로 X와 Z의 OR
# Y=1 부분표:
#        Z=1  Z=2
# X=1    10   30
# X=2    20   20
or_xz_y1 <- calc_or(10, 30, 20, 20)

# Y=2 부분표:
#        Z=1  Z=2
# X=1    20   15
# X=2    40   10
or_xz_y2 <- calc_or(20, 15, 40, 10)

cat("  동질 연관 확인 (XZ|Y):\n")
cat(sprintf("  OR(XZ|Y=1) = (10×20)/(30×20) = %.4f\n", or_xz_y1))
cat(sprintf("  OR(XZ|Y=2) = (20×10)/(15×40) = %.4f\n", or_xz_y2))
cat(sprintf("  → 동일한가: %s\n\n", ifelse(abs(or_xz_y1 - or_xz_y2) < 1e-6, "동일 → 동질 연관 ✓", "다름")))

# OR(YZ|X): X 수준별로 Y와 Z의 OR
# X=1 부분표:
#        Z=1  Z=2
# Y=1    10   30
# Y=2    20   15
or_yz_x1 <- calc_or(10, 30, 20, 15)

# X=2 부분표:
#        Z=1  Z=2
# Y=1    20   20
# Y=2    40   10
or_yz_x2 <- calc_or(20, 20, 40, 10)

cat("  동질 연관 확인 (YZ|X):\n")
cat(sprintf("  OR(YZ|X=1) = (10×15)/(30×20) = %.4f\n", or_yz_x1))
cat(sprintf("  OR(YZ|X=2) = (20×10)/(20×40) = %.4f\n", or_yz_x2))
cat(sprintf("  → 동일한가: %s\n\n", ifelse(abs(or_yz_x1 - or_yz_x2) < 1e-6, "동일 → 동질 연관 ✓", "다름")))

cat("  결론 (a):\n")
cat("  X⊥Y|Z (조건부 독립)이면 λ^XYZ = 0 이고,\n")
cat("  이는 모든 쌍의 조건부 OR이 세 번째 변수에 무관하게 일정함을 보장.\n")
cat("  즉, 조건부 독립 → 동질 연관 (자동으로 성립).\n\n")


# =============================================================================
# (b) 동질 연관이 아니면 → 어떤 쌍도 조건부 독립일 수 없다
# =============================================================================

cat("=============================================================\n")
cat("(b) 비동질 연관 → 어떤 쌍도 조건부 독립 불가\n")
cat("=============================================================\n\n")

cat("[ 이론적 설명 ]\n\n")

cat("  (a)의 대우명제:\n")
cat("  '조건부 독립 → 동질 연관'의 대우:\n")
cat("  '동질 연관이 아님 → 조건부 독립이 아님'\n\n")

cat("  동질 연관이 아님 ↔ λ^XYZ ≠ 0 (3원 상호작용 존재)\n\n")

cat("  어떤 쌍 (예: X,Y)이 조건부 독립이려면:\n")
cat("    OR(XY|Z=k) = exp(λ^XY + λ^XYZ_k) = 1  for all k\n")
cat("    → λ^XYZ_k = 0  for all k  (λ^XYZ = 0 필요)\n")
cat("  그런데 λ^XYZ ≠ 0 이므로 → 불가능\n\n")
cat("  마찬가지로 (X,Z) 쌍과 (Y,Z) 쌍도:\n")
cat("    OR(XZ|Y=j) = exp(λ^XZ + λ^XYZ_j): j에 따라 달라짐\n")
cat("    OR(YZ|X=i) = exp(λ^YZ + λ^XYZ_i): i에 따라 달라짐\n")
cat("  모두 λ^XYZ ≠ 0 이면 조건부 독립 불가.\n\n")

# --- 수치 예시로 확인 ---
cat("[ 수치 예시: 비동질 연관이 있는 3원 분할표 ]\n\n")

# 3원 상호작용이 있는 표
# Z=1 층:
#        Y=1  Y=2
# X=1    20   10
# X=2    10    5
layer2_z1 <- matrix(c(20, 10, 10, 5), nrow = 2,
                    dimnames = list(X = c("X=1","X=2"), Y = c("Y=1","Y=2")))

# Z=2 층:
#        Y=1  Y=2
# X=1    10   20
# X=2     5   40
layer2_z2 <- matrix(c(10, 5, 20, 40), nrow = 2,
                    dimnames = list(X = c("X=1","X=2"), Y = c("Y=1","Y=2")))

or_xy_z1 <- calc_or(20, 10, 10, 5)
or_xy_z2 <- calc_or(10, 20, 5, 40)

cat("  Z=1 층:\n"); print(layer2_z1)
cat(sprintf("  OR(XY|Z=1) = (20×5)/(10×10) = %.4f\n\n", or_xy_z1))

cat("  Z=2 층:\n"); print(layer2_z2)
cat(sprintf("  OR(XY|Z=2) = (10×40)/(20×5) = %.4f\n\n", or_xy_z2))

cat(sprintf("  OR(XY|Z=1) = %.4f ≠ OR(XY|Z=2) = %.4f\n", or_xy_z1, or_xy_z2))
cat("  → 비동질 연관 (XY|Z)\n\n")

# 다른 쌍들도 조건부 독립인지 확인
# XZ|Y
or_xz2_y1 <- calc_or(20, 10, 10, 5)   # Y=1: Z=1(20,10), Z=2(10,5)
or_xz2_y2 <- calc_or(10, 20, 5, 40)   # Y=2: Z=1(10,5), Z=2(20,40) -- 헷갈리니 직접 계산

# Y=1 부분표 (XZ|Y=1):
#        Z=1  Z=2
# X=1    20   10
# X=2    10    5
or_xz2_y1 <- calc_or(20, 10, 10, 5)

# Y=2 부분표 (XZ|Y=2):
#        Z=1  Z=2
# X=1    10   20
# X=2     5   40
or_xz2_y2 <- calc_or(10, 20, 5, 40)

cat("  XZ 쌍 조건부 독립 확인 (XZ|Y):\n")
cat(sprintf("  OR(XZ|Y=1) = %.4f\n", or_xz2_y1))
cat(sprintf("  OR(XZ|Y=2) = %.4f\n", or_xz2_y2))
cat(sprintf("  → 다름: 동질 연관 아님, 조건부 독립도 아님 ✓\n\n"))

# YZ|X
# X=1 부분표:
#        Z=1  Z=2
# Y=1    20   10
# Y=2    10   20
or_yz2_x1 <- calc_or(20, 10, 10, 20)

# X=2 부분표:
#        Z=1  Z=2
# Y=1    10    5
# Y=2     5   40
or_yz2_x2 <- calc_or(10, 5, 5, 40)

cat("  YZ 쌍 조건부 독립 확인 (YZ|X):\n")
cat(sprintf("  OR(YZ|X=1) = %.4f\n", or_yz2_x1))
cat(sprintf("  OR(YZ|X=2) = %.4f\n", or_yz2_x2))
cat(sprintf("  → 다름: 동질 연관 아님, 조건부 독립도 아님 ✓\n\n"))

cat("  결론 (b):\n")
cat("  비동질 연관 ↔ λ^XYZ ≠ 0 (3원 상호작용 존재)\n")
cat("  λ^XYZ ≠ 0 이면, 어떤 쌍의 조건부 OR도 모든 수준에서 1이 될 수 없으므로\n")
cat("  어떤 쌍도 조건부 독립이 될 수 없다.\n\n")


# =============================================================================
# 로그선형 모형으로 검증
# =============================================================================

cat("=============================================================\n")
cat("로그선형 모형 검증\n")
cat("=============================================================\n\n")

# 예시 (a) 데이터를 데이터프레임으로 변환
counts_a <- c(10, 20, 20, 40, 30, 20, 15, 10)
df_a <- expand.grid(X = c(1,2), Y = c(1,2), Z = c(1,2))
df_a$count <- counts_a

# 예시 (b) 데이터
counts_b <- c(20, 10, 10, 5, 10, 5, 20, 40)
df_b <- expand.grid(X = c(1,2), Y = c(1,2), Z = c(1,2))
df_b$count <- counts_b

# 포화 모형 (3원 상호작용 포함) vs [XY,XZ,YZ] 모형 비교
model_sat_a  <- glm(count ~ X*Y*Z,            data = df_a, family = poisson)
model_homo_a <- glm(count ~ X*Y + X*Z + Y*Z,  data = df_a, family = poisson)

model_sat_b  <- glm(count ~ X*Y*Z,            data = df_b, family = poisson)
model_homo_b <- glm(count ~ X*Y + X*Z + Y*Z,  data = df_b, family = poisson)

lrt_a <- anova(model_homo_a, model_sat_a, test = "LRT")
lrt_b <- anova(model_homo_b, model_sat_b, test = "LRT")

cat("  예시 (a): X⊥Y|Z 데이터 — [XY,XZ,YZ] vs 포화 모형 LRT\n")
cat(sprintf("  이탈도 차이: %.4f,  p-값: %.4f\n",
            lrt_a$Deviance[2], lrt_a$`Pr(>Chi)`[2]))
cat(sprintf("  → p > 0.05: λ^XYZ = 0 기각 못함 (동질 연관 모형 적합) ✓\n\n"))

cat("  예시 (b): 비동질 연관 데이터 — [XY,XZ,YZ] vs 포화 모형 LRT\n")
cat(sprintf("  이탈도 차이: %.4f,  p-값: %.4f\n",
            lrt_b$Deviance[2], lrt_b$`Pr(>Chi)`[2]))
cat(sprintf("  → p < 0.05: λ^XYZ ≠ 0 (3원 상호작용 유의, 비동질 연관) ✓\n\n"))


# =============================================================================
# 시각화
# =============================================================================

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# --- 그래프 1: (a) 동질 연관 — 층별 OR이 동일 ---
or_vals_a <- c(
  calc_or(10, 20, 20, 40),    # OR(XY|Z=1) = 1
  calc_or(30, 15, 20, 10),    # OR(XY|Z=2) = 1
  or_xz_y1, or_xz_y2,        # OR(XZ|Y=1), OR(XZ|Y=2)
  or_yz_x1, or_yz_x2         # OR(YZ|X=1), OR(YZ|X=2)
)
labels_a <- c("OR(XY|Z=1)","OR(XY|Z=2)",
               "OR(XZ|Y=1)","OR(XZ|Y=2)",
               "OR(YZ|X=1)","OR(YZ|X=2)")
cols_a <- c("firebrick","firebrick","steelblue","steelblue","darkgreen","darkgreen")

barplot(or_vals_a, names.arg = labels_a,
        col = cols_a, las = 2, cex.names = 0.7,
        main = "(a) Homogeneous Association\n(X indep Y | Z)",
        ylab = "Conditional Odds Ratio",
        ylim = c(0, max(or_vals_a) * 1.4))
abline(h = 1, lty = 2, lwd = 1.5)
legend("topright", legend = c("XY|Z","XZ|Y","YZ|X"),
       fill = c("firebrick","steelblue","darkgreen"), bty = "n", cex = 0.8)


# --- 그래프 2: (b) 비동질 연관 — 층별 OR이 다름 ---
or_vals_b <- c(
  or_xy_z1, or_xy_z2,
  or_xz2_y1, or_xz2_y2,
  or_yz2_x1, or_yz2_x2
)
labels_b <- c("OR(XY|Z=1)","OR(XY|Z=2)",
               "OR(XZ|Y=1)","OR(XZ|Y=2)",
               "OR(YZ|X=1)","OR(YZ|X=2)")
cols_b <- c("firebrick","firebrick","steelblue","steelblue","darkgreen","darkgreen")

barplot(or_vals_b, names.arg = labels_b,
        col = cols_b, las = 2, cex.names = 0.7,
        main = "(b) Non-Homogeneous Association\n(3-way interaction exists)",
        ylab = "Conditional Odds Ratio",
        ylim = c(0, max(or_vals_b) * 1.3))
abline(h = 1, lty = 2, lwd = 1.5)
legend("topright", legend = c("XY|Z","XZ|Y","YZ|X"),
       fill = c("firebrick","steelblue","darkgreen"), bty = "n", cex = 0.8)

par(mfrow = c(1, 1))


# =============================================================================
# 최종 요약
# =============================================================================

cat("=============================================================\n")
cat("최종 요약\n")
cat("=============================================================\n\n")
cat("  조건부 독립 ⊂ 동질 연관 (조건부 독립이 더 강한 조건)\n\n")
cat("  (a) 조건부 독립 → 동질 연관:\n")
cat("      어떤 쌍이 조건부 독립이면 λ^XYZ=0 필요\n")
cat("      λ^XYZ=0 ↔ 동질 연관 (자동 성립)\n\n")
cat("  (b) 비동질 연관 → 어떤 쌍도 조건부 독립 불가:\n")
cat("      비동질 연관 ↔ λ^XYZ≠0\n")
cat("      λ^XYZ≠0 이면 어떤 쌍도 조건부 OR=1(상수)이 될 수 없음\n")
