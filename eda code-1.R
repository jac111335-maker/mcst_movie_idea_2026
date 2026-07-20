# ============================================================
# 영화의전당 연도별·월별 방문객 수 EDA
# 구조: 1행=제목(병합), 2행=헤더, 3행~=데이터
# ============================================================

# 필요 패키지 설치 (최초 1회)
packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "scales")
new_pkgs <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs, repos = "https://cran.r-project.org")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# ============================================================
# ★ 파일 경로만 수정하세요
# ============================================================
file_path <- "(재)영화의전당 연도별·월별 방문객 수 통계 외.xlsx"
# ============================================================

# ============================================================
# 1. 데이터 불러오기
#    - skip=1: 1행(제목) 건너뜀
#    - sheet=1: 첫 번째 시트 고정
# ============================================================
df <- read_excel(file_path, sheet = 1, skip = 1)

# 컬럼명 정리: "연도", "1월"~"12월", "연간합계"
colnames(df) <- c("연도", paste0(1:12, "월"), "연간합계")

# 연도를 factor로 변환
df <- df %>% mutate(연도 = as.factor(연도))

cat("✅ 데이터 로드 완료\n")
print(df)

# ============================================================
# 2. 기본 통계
# ============================================================
cat("\n── 기술통계 ────────────────────────────────\n")
print(summary(df))

# ============================================================
# 3. 연간합계 추이 (막대그래프)
# ============================================================
ggplot(df, aes(x = 연도, y = 연간합계, fill = 연도)) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_text(aes(label = comma(연간합계)), vjust = -0.5, size = 3.5, color="black") +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(title = "영화의 전당 연도별 방문객 수",
       x = "연도", y = "방문객 수 (명)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(color = "black"),  # 제목
        axis.title = element_text(color = "black"),  # x, y축 제목
        axis.text = element_text(color = "black") ) # x, y축 눈금 숫자)
ggsave("01_연간합계.png", width = 10, height = 6, dpi = 300)

# ============================================================
# 4. 월별 방문객 수 히트맵 (연도 × 월)
# ============================================================
month_cols <- paste0(1:12, "월")

df_long <- df %>%
  select(연도, all_of(month_cols)) %>%
  pivot_longer(cols = all_of(month_cols),
               names_to  = "월",
               values_to = "방문객수") %>%
  mutate(월 = factor(월, levels = month_cols))

ggplot(df_long, aes(x = 월, y = 연도, fill = 방문객수)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = comma(방문객수)), size = 2.8) +
  scale_fill_gradient(low = "#EFF3FF", high = "#2171B5", labels = comma) +
  labs(title = "연도 × 월별 방문객 수 히트맵",
       x = "월", y = "연도", fill = "방문객 수") +
  theme_minimal(base_size = 12)
ggsave("02_히트맵.png", width = 12, height = 5, dpi = 300)

# ============================================================
# 5. 월별 평균 방문객 수 (월 패턴 파악)
# ============================================================
month_avg <- df_long %>%
  group_by(월) %>%
  summarise(평균방문객 = mean(방문객수, na.rm = TRUE), .groups = "drop")

ggplot(month_avg, aes(x = 월, y = 평균방문객, group = 1)) +
  geom_line(color = "#2171B5", linewidth = 1.2) +
  geom_point(color = "#2171B5", size = 3) +
  geom_text(aes(label = comma(round(평균방문객))), vjust = -0.8, size = 3.2) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "월별 평균 방문객 수 (전 연도 평균)",
       x = "월", y = "평균 방문객 수 (명)") +
  theme_minimal(base_size = 12)
ggsave("03_월별평균.png", width = 10, height = 6, dpi = 300)

# ============================================================
# 6. 연도별 월별 추이 (라인 차트)
# ============================================================
ggplot(df_long, aes(x = 월, y = 방문객수, color = 연도, group = 연도)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  labs(title = "연도별 월별 방문객 수 추이",
       x = "월", y = "방문객 수 (명)", color = "연도") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("04_연도별추이.png", width = 10, height = 6, dpi = 300)

# ============================================================
# 7. 결측치 확인
# ============================================================
cat("\n── 결측치 확인 ─────────────────────────────\n")
missing <- colSums(is.na(df))
print(missing[missing > 0])
if (sum(missing) == 0) cat("결측치 없음\n")

cat("\n🎉 EDA 완료! PNG 4장 저장됨\n")
cat("   01_연간합계.png\n")
cat("   02_히트맵.png\n")
cat("   03_월별평균.png\n")
cat("   04_연도별추이.png\n")
