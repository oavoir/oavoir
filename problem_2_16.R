# =============================================================================
# 문제 2.16 - Doll & Hill (1950) 폐암과 흡연 환자-대조군 연구
# 출처: R. Doll and A. B. Hill, Br. Med. J., 739-748, September 30, 1950
#
# 런던 20개 병원에서 폐암 입원 환자를 케이스로 선정하고,
# 같은 성별·5세 연령군의 비암 환자를 대조군으로 매칭.
# 흡연자 정의: 하루 1개비 이상, 1년 이상 흡연 경험자
#
# 표 2.12:
#                  폐암(Lung Cancer)
#                  Cases    Controls
# Have Smoked Yes    688       650
#             No      21        59
#             Total  709       709
# =============================================================================

# -----------------------------------------------------------------------------
# 0. 데이터 설정 - 2×2 분할표
# -----------------------------------------------------------------------------

# 셀 빈도수 입력
cases_smoker    <- 688   # 폐암 환자(케이스) 중 흡연자
cases_nonsmoker <-  21   # 폐암 환자(케이스) 중 비흡연자
ctrl_smoker     <- 650   # 대조군 중 흡연자
ctrl_nonsmoker  <-  59   # 대조군 중 비흡연자

total_cases    <- cases_smoker + cases_nonsmoker   # 케이스 합계: 709
total_controls <- ctrl_smoker  + ctrl_nonsmoker    # 대조군 합계: 709

# 2×2 분할표 행렬 생성 (출력용)
tbl <- matrix(
  c(cases_smoker, cases_nonsmoker,
    ctrl_smoker,  ctrl_nonsmoker),
  nrow = 2, ncol = 2,
  dimnames = list(
    "Smoked"      = c("Yes", "No"),
    "Lung Cancer" = c("Cases", "Controls")
  )
)

cat("=== 표 2.12: 문제 2.16 데이터 ===\n")
print(tbl)
cat(sprintf("\n  케이스 합계:    %d명\n", total_cases))
cat(sprintf("  대조군 합계: %d명\n\n", total_controls))


# =============================================================================
# (a) 반응변수(Response Variable)와 설명변수(Explanatory Variable) 식별
# =============================================================================

cat("=============================================================\n")
cat("(a) 반응변수와 설명변수\n")
cat("=============================================================\n\n")

cat("  반응변수 (결과변수, Response Variable):\n")
cat("    폐암 여부 → 케이스(폐암 있음) vs 대조군(폐암 없음)\n\n")

cat("  설명변수 (노출변수, Explanatory Variable):\n")
cat("    흡연 여부 → 흡연 경험 있음(Yes) vs 없음(No)\n\n")

cat("  근거:\n")
cat("    이 연구는 흡연(노출)이 폐암(결과) 발생을 설명하거나 예측하는지 조사한다.\n")
cat("    연구자는 결과(폐암 유무)를 기준으로 대상자를 먼저 선정한 뒤,\n")
cat("    과거의 흡연 이력을 소급 조사하는 방식으로 설계되었다.\n\n")


# =============================================================================
# (b) 연구 유형 식별
# =============================================================================

cat("=============================================================\n")
cat("(b) 연구 유형\n")
cat("=============================================================\n\n")

cat("  연구 유형: 환자-대조군 연구 (Case-Control Study, 후향적 연구)\n\n")

cat("  근거:\n")
cat("  1) 표본 추출이 결과(폐암 여부)를 기준으로 이루어졌다.\n")
cat("     - 케이스 709명 (폐암 환자) 선정\n")
cat("     - 대조군 709명 (비암 환자) 매칭 선정\n\n")
cat("  2) 선정 후 과거(역방향)의 흡연 노출 여부를 조사했다.\n\n")
cat("  3) 성별·5세 연령군을 매칭하여 교란 변수를 통제했다.\n\n")
cat("  핵심: 각 열 합계(709, 709)가 연구 설계에 의해 미리 고정되었다.\n\n")


# =============================================================================
# (c) 흡연자/비흡연자 간 폐암 발생 비율 비교 가능 여부?
# =============================================================================

cat("=============================================================\n")
cat("(c) 흡연자와 비흡연자의 폐암 발생 비율을 비교할 수 있는가?\n")
cat("=============================================================\n\n")

# 계산 가능한 것: 각 집단(케이스/대조군) 내 흡연자 비율
prop_smoker_in_cases    <- cases_smoker / total_cases
prop_smoker_in_controls <- ctrl_smoker  / total_controls

cat("  계산 가능한 값 (각 집단 내 흡연자 비율):\n")
cat(sprintf("    케이스 중 흡연자 비율:    %d/%d = %.4f (%.2f%%)\n",
            cases_smoker, total_cases,
            prop_smoker_in_cases, prop_smoker_in_cases * 100))
cat(sprintf("    대조군 중 흡연자 비율: %d/%d = %.4f (%.2f%%)\n\n",
            ctrl_smoker, total_controls,
            prop_smoker_in_controls, prop_smoker_in_controls * 100))

cat("  결론: 아니오 - 이 환자-대조군 자료로는 흡연자/비흡연자 중\n")
cat("        폐암에 걸린 비율을 추정할 수 없다.\n\n")

cat("  이유:\n")
cat("  - 환자-대조군 연구에서는 케이스와 대조군 수가 설계에 의해 고정된다.\n")
cat("    (여기서는 각각 709명으로 고정)\n")
cat("  - 이 표본 구성은 실제 모집단에서의 폐암 유병률을 반영하지 않는다.\n\n")

# 잘못된 계산 예시 (문제점 시각화)
wrong_prop_smoker    <- cases_smoker    / (cases_smoker + ctrl_smoker)
wrong_prop_nonsmoker <- cases_nonsmoker / (cases_nonsmoker + ctrl_nonsmoker)

cat("  잘못된 계산 예시 (이렇게 하면 안 됨):\n")
cat(sprintf("    흡연자 중 폐암 비율 (잘못):    %.4f\n", wrong_prop_smoker))
cat(sprintf("    비흡연자 중 폐암 비율 (잘못): %.4f\n", wrong_prop_nonsmoker))
cat("    → 1:1 케이스:대조군 비율은 연구자가 임의로 정한 것이므로\n")
cat("      이 수치는 실제 발병 위험을 전혀 의미하지 않는다.\n\n")

cat("  → 설계상 고정된 두 집단을 비교할 때는 오즈비(OR)를 사용한다.\n\n")


# =============================================================================
# (d) 연관성 요약 - 오즈비(Odds Ratio) 계산 및 해석
# =============================================================================

cat("=============================================================\n")
cat("(d) 연관성 요약 (오즈비)\n")
cat("=============================================================\n\n")

# --- 각 집단 내 흡연 오즈(Odds) 계산 ---
# 오즈 = 노출 확률 / 비노출 확률 (각 결과 집단 내에서)
odds_smoking_cases    <- cases_smoker    / cases_nonsmoker
odds_smoking_controls <- ctrl_smoker     / ctrl_nonsmoker

cat("[ 각 집단의 흡연 오즈(Odds) ]\n")
cat(sprintf("  케이스(폐암) 내 흡연 오즈:    %d / %d = %.4f\n",
            cases_smoker, cases_nonsmoker, odds_smoking_cases))
cat(sprintf("  대조군(비암) 내 흡연 오즈: %d / %d = %.4f\n\n",
            ctrl_smoker, ctrl_nonsmoker, odds_smoking_controls))


# --- 오즈비(OR) 계산 ---
# OR = (케이스의 흡연 오즈) / (대조군의 흡연 오즈)
# 교차곱 공식: OR = (a * d) / (b * c)
#   a = cases_smoker,    b = cases_nonsmoker
#   c = ctrl_smoker,     d = ctrl_nonsmoker
odds_ratio <- (cases_smoker * ctrl_nonsmoker) / (cases_nonsmoker * ctrl_smoker)

cat("[ 오즈비 (Odds Ratio, OR) ]\n")
cat("  공식: OR = (a * d) / (b * c)  [교차곱]\n")
cat(sprintf("  계산: (%d × %d) / (%d × %d)\n",
            cases_smoker, ctrl_nonsmoker, cases_nonsmoker, ctrl_smoker))
cat(sprintf("      = %d / %d\n",
            cases_smoker * ctrl_nonsmoker, cases_nonsmoker * ctrl_smoker))
cat(sprintf("      = %.4f\n\n", odds_ratio))

cat("[ 해석 ]\n")
cat(sprintf("  OR = %.4f\n\n", odds_ratio))
cat("  폐암 환자(케이스)에서의 흡연 오즈가 대조군보다 약 2.97배 높다.\n\n")
cat("  즉, 흡연자는 비흡연자에 비해 폐암에 걸릴 오즈가\n")
cat("  약 2.97배 더 높다.\n\n")
cat("  OR > 1 이므로, 흡연과 폐암 사이에 양(+)의 연관성이 존재한다.\n\n")

# OR 대신 RR을 쓸 수 없는 이유 설명
cat("[ 참고: OR을 사용하는 이유 (RR 사용 불가) ]\n")
cat("  환자-대조군 연구는 결과(폐암 유무)를 기준으로 표본을 고정했기 때문에,\n")
cat("  흡연자/비흡연자 행 합계가 임의로 정해지지 않았다.\n")
cat("  따라서 상대위험도(RR)는 유효하게 계산할 수 없으며,\n")
cat("  환자-대조군 연구에서는 오즈비(OR)가 적절한 연관성 측도이다.\n\n")


# =============================================================================
# 시각화 (플롯 라벨은 맥 호환을 위해 영어 유지)
# =============================================================================

par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# --- 그래프 1: 집단별 흡연자 비율 누적 막대그래프 ---
prop_tbl <- prop.table(tbl, margin = 2)  # 열 기준 비율 계산

barplot(prop_tbl,
        col    = c("firebrick", "steelblue"),
        main   = "Smoking Proportion\nby Lung Cancer Status",
        ylab   = "Proportion",
        ylim   = c(0, 1.15),
        legend.text = c("Smoker", "Nonsmoker"),
        args.legend = list(x = "topright", bty = "n"),
        las    = 1)

# 막대 위에 흡연자 비율(%) 표시
text(x      = c(0.7, 1.9),
     y      = c(prop_tbl[1, 1] / 2, prop_tbl[1, 2] / 2),
     labels = sprintf("%.1f%%", prop_tbl[1, ] * 100),
     col    = "white", fontface = "bold", cex = 1.0)


# --- 그래프 2: 오즈비 시각화 (기준선 OR=1 포함) ---
or_vals   <- c(odds_ratio, 1)
or_labels <- c(sprintf("OR = %.2f", odds_ratio), "OR = 1\n(no association)")

barplot(or_vals,
        names.arg = or_labels,
        col       = c("darkorange", "gray70"),
        main      = "Odds Ratio\n(Smokers vs Nonsmokers)",
        ylab      = "Odds Ratio",
        ylim      = c(0, or_vals[1] * 1.3),
        las       = 1)

abline(h = 1, lty = 2, col = "black", lwd = 1.5)  # OR=1 기준 점선

text(x      = 0.7,
     y      = odds_ratio + or_vals[1] * 0.04,
     labels = sprintf("%.4f", odds_ratio),
     cex    = 0.95)

par(mfrow = c(1, 1))  # 그래프 레이아웃 초기화


# =============================================================================
# 최종 요약
# =============================================================================

cat("=============================================================\n")
cat("최종 요약\n")
cat("=============================================================\n")
cat(sprintf("  케이스 내 흡연자 비율:    %.4f (%.2f%%)\n",
            prop_smoker_in_cases, prop_smoker_in_cases * 100))
cat(sprintf("  대조군 내 흡연자 비율: %.4f (%.2f%%)\n",
            prop_smoker_in_controls, prop_smoker_in_controls * 100))
cat(sprintf("  오즈비 (OR):               %.4f\n\n", odds_ratio))
cat("  (a) 반응변수: 폐암 여부 | 설명변수: 흡연 여부\n")
cat("  (b) 환자-대조군 연구 (후향적 연구)\n")
cat("  (c) 폐암 발생 비율 비교 불가 - 케이스/대조군 수가 설계상 고정됨\n")
cat(sprintf("  (d) OR = %.4f → 흡연자는 비흡연자보다 폐암 오즈가 약 %.2f배 높음\n",
            odds_ratio, odds_ratio))
