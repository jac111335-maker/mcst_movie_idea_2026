
# ============================================================
# 부산국제영화제(BIFF) 방문객 현황 EDA
# ============================================================

# 필요 패키지 설치 및 로드
packages <- c("ggplot2", "dplyr", "tidyr", "scales", "ggthemes", "patchwork", "knitr")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)


# ============================================================
# 1. 데이터 입력
# ============================================================
biff <- data.frame(
  year       = 2019:2025,
  period     = c("10.3~10.12", "10.21~10.30", "10.6~10.15",
                 "10.5~10.14", "10.4~10.13", "10.2~10.11", "9.30~10.9"),
  biff_visitors = c(80021, 18311, 31078, 64508, 64919, 66827, 69743),
  annual_visitors = c(816118, 261599, 309587, 622540, 793027, 900085, 1491041)
)

# 파생 변수 생성
biff <- biff %>%
  mutate(
    biff_ratio   = biff_visitors / annual_visitors,          # BIFF 기간 방문객 비율
    yoy_biff     = c(NA, diff(biff_visitors)),               # BIFF 방문객 전년 대비 증감
    yoy_annual   = c(NA, diff(annual_visitors)),             # 연간 방문객 전년 대비 증감
    yoy_biff_pct = biff_visitors / lag(biff_visitors) - 1,  # 전년 대비 증감률
    yoy_ann_pct  = annual_visitors / lag(annual_visitors) - 1
  )


# ============================================================
# 2. 기초 통계 요약
# ============================================================
cat("=== 기초 통계 요약 ===\n")
cat("\nBIFF 기간 방문객 (명)\n")
print(summary(biff$biff_visitors))

cat("\n연간 방문객 (명)\n")
print(summary(biff$annual_visitors))

cat("\nBIFF 기간 방문객 비율 (BIFF / 연간)\n")
print(summary(biff$biff_ratio))

cat("\n전체 데이터 테이블\n")
print(knitr::kable(
  biff %>% select(year, biff_visitors, annual_visitors, biff_ratio, yoy_biff_pct, yoy_ann_pct),
  col.names = c("연도", "BIFF 방문객", "연간 방문객", "BIFF 비율", "BIFF 증감률", "연간 증감률"),
  digits = 3,
  format = "simple"
))


# ============================================================
# 3. 시각화 공통 테마
# ============================================================
theme_biff <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 11),
    axis.title    = element_text(size = 11),
    panel.grid.minor = element_blank(),
    plot.margin   = margin(12, 12, 12, 12)
  )

palette_main <- c("BIFF 기간 방문객" = "#1D9E75", "연간 방문객" = "#378ADD")


# ============================================================
# 4. 그래프 1 — 연도별 방문객 추이 (막대 + 꺾은선)
# ============================================================
biff_long <- biff %>%
  select(year, biff_visitors, annual_visitors) %>%
  pivot_longer(cols = -year,
               names_to = "type",
               values_to = "visitors") %>%
  mutate(type = recode(type,
                       biff_visitors    = "BIFF 기간 방문객",
                       annual_visitors  = "연간 방문객"))

p1 <- ggplot(biff_long, aes(x = factor(year), y = visitors, fill = type)) +
  geom_col(position = "dodge", width = 0.65, alpha = 0.85) +
  geom_text(aes(label = scales::comma(visitors, big.mark = ",")),
            position = position_dodge(width = 0.65),
            vjust = -0.4, size = 3, color = "gray30") +
  scale_y_continuous(labels = scales::comma_format(big.mark = ","),
                     expand = expansion(mult = c(0, 0.12))) +
  scale_fill_manual(values = palette_main) +
  labs(title = "연도별 BIFF 기간 방문객 vs 연간 방문객",
       subtitle = "2020년 코로나19 영향으로 방문객 급감, 이후 회복세",
       x = "연도", y = "방문객 수 (명)", fill = NULL) +
  theme_biff


# ============================================================
# 5. 그래프 2 — BIFF 방문객 비율 추이
# ============================================================
p2 <- ggplot(biff, aes(x = factor(year), y = biff_ratio)) +
  geom_col(fill = "#534AB7", alpha = 0.8, width = 0.55) +
  geom_text(aes(label = scales::percent(biff_ratio, accuracy = 0.1)),
            vjust = -0.4, size = 3.2, color = "gray30") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "BIFF 기간 방문객 ",
       subtitle = "BIFF 개최 기간이 연간 방문객에서 차지하는 비중",
       x = "연도", y = "비율 (%)") +
  theme_biff


# ============================================================
# 6. 그래프 3 — 전년 대비 증감률
# ============================================================
yoy_long <- biff %>%
  filter(!is.na(yoy_biff_pct)) %>%
  select(year, yoy_biff_pct, yoy_ann_pct) %>%
  pivot_longer(-year, names_to = "type", values_to = "pct") %>%
  mutate(type = recode(type,
                       yoy_biff_pct = "BIFF 기간 방문객",
                       yoy_ann_pct  = "연간 방문객"))

p3 <- ggplot(yoy_long, aes(x = factor(year), y = pct, fill = type)) +
  geom_col(position = "dodge", width = 0.65, alpha = 0.85) +
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)),
            position = position_dodge(width = 0.65),
            vjust = ifelse(yoy_long$pct >= 0, -0.4, 1.2),
            size = 3, color = "gray30") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0.12, 0.12))) +
  scale_fill_manual(values = palette_main) +
  labs(title = "연도별 전년 대비 증감률",
       subtitle = "2020 급락 후 등락 반복, 2025년 연간 방문객 큰 폭 증가",
       x = "연도", y = "증감률 (%)", fill = NULL) +
  theme_biff


# ============================================================
# 7. 그래프 4 — BIFF 방문객 꺾은선 + 추세선
# ============================================================
p4 <- ggplot(biff, aes(x = year, y = biff_visitors)) +
  geom_line(color = "#1D9E75", linewidth = 1.1) +
  geom_point(aes(color = ifelse(year == 2020, "최솟값", "일반")), size = 3.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#E24B4A", linewidth = 0.7,
              linetype = "dashed", alpha = 0.12) +
  scale_color_manual(values = c("일반" = "#1D9E75", "최솟값" = "#E24B4A"),
                     guide = "none") +
  scale_x_continuous(breaks = 2019:2025) +
  scale_y_continuous(labels = scales::comma_format(big.mark = ","),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.1))) +
  geom_text(aes(label = scales::comma(biff_visitors, big.mark = ",")),
            vjust = -0.8, size = 3, color = "gray30") +
  labs(title = "BIFF 기간 방문객 추세",
       subtitle = "점선: 선형 추세선 (2020 코로나 이상치 존재)",
       x = "연도", y = "방문객 수 (명)") +
  theme_biff


# ============================================================
# 8. 전체 플롯 출력 (patchwork)
# ============================================================
final_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title   = "부산국제영화제(BIFF) 방문객 현황 EDA",
    caption = "출처: BIFF 기간 방문객 현황 (2019~2025)",
    theme   = theme(plot.title = element_text(size = 16, face = "bold"))
  )

print(final_plot)

# 파일 저장 (선택)
ggsave("biff_eda.png", final_plot, width = 14, height = 10, dpi = 150)


# ============================================================
# 9. 상관 분석
# ============================================================
cat("\n=== 상관 분석 ===\n")
cor_val <- cor(biff$biff_visitors, biff$annual_visitors)
cat(sprintf("BIFF 방문객 vs 연간 방문객 상관계수: %.4f\n", cor_val))


# ============================================================
# 10. 이상치 확인 (Z-score 기반)
# ============================================================
cat("\n=== 이상치 확인 (Z-score) ===\n")
biff <- biff %>%
  mutate(
    z_biff   = scale(biff_visitors)[, 1],
    z_annual = scale(annual_visitors)[, 1]
  )

outliers <- biff %>% filter(abs(z_biff) > 1.5 | abs(z_annual) > 1.5)
if (nrow(outliers) > 0) {
  cat("이상치 의심 연도:\n")
  print(outliers %>% select(year, biff_visitors, annual_visitors, z_biff, z_annual))
} else {
  cat("뚜렷한 이상치 없음\n")
}

