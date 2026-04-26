# ==========================================
# НАЛАШТУВАННЯ ТА ПІДГОТОВКА ДАНИХ
# ==========================================
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(treemapify)
library(plotly)

options(scipen = 999) 

# 1. Завантаження даних
data_2026 <- read.csv("EXP-KPK-NAT-2026-1-2.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "Windows-1251")
data_2025 <- read.csv("EXP-KPK-NAT-2025-1-12.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "Windows-1251")
data_2024 <- read.csv("EXP-KPK-NAT-2024-1-12.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "Windows-1251")
data_2023 <- read.csv("EXP-KPK-NAT-2023-1-12.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "Windows-1251")
data_2022 <- read.csv("EXP-KPK-NAT-2022-1-12.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "Windows-1251")
data_2021 <- read.csv("EXP-KPK-NAT-2021-1-12.csv", sep = ";", stringsAsFactors = FALSE, fileEncoding = "Windows-1251")

# 2. Універсальна функція очищення
clean_budget_data <- function(df, data_year) {
  colnames(df) <- as.character(df[1, ])
  df <- df[-1, ]
  
  df <- df |> 
    filter(programCode != "0000") |> 
    select(-fundType, -periodCorrectionPlan, 
           -openAllocPeriod, -directedAllocPeriod,
           -deviationToPeriodCorrectionPlan, -percentDoneToPeriodCorrectionPlan) |> 
    mutate(across(-c(programCode, programCodeName), as.numeric)) |> 
    mutate(year = data_year) |> 
    relocate(year)
  
  return(df)
}

# 3. Очищення та об'єднання всіх років
clean_2026 <- clean_budget_data(data_2026, 2026)
clean_2025 <- clean_budget_data(data_2025, 2025)
clean_2024 <- clean_budget_data(data_2024, 2024)
clean_2023 <- clean_budget_data(data_2023, 2023)
clean_2022 <- clean_budget_data(data_2022, 2022)
clean_2021 <- clean_budget_data(data_2021, 2021)

# 4. Фінальний файл, готовий для аналізу
all_years_data <- bind_rows(clean_2021, clean_2022, clean_2023, clean_2024, clean_2025, clean_2026)
View(all_years_data)

# ==========================================
# 1. ДИНАМІКА ФІНАНСУВАННЯ ТОП-5 СФЕР (LINE)
# ==========================================
top_5_spheres <- all_years_data |> 
  filter(grepl("0000$", programCode) & programCode != "0000") |> 
  summarise(total_sum = sum(yearCorrectionPlan, na.rm = TRUE), .by = programCodeName) |> 
  arrange(desc(total_sum)) |> 
  slice_head(n = 5) |> 
  pull(programCodeName)

chart1_data <- all_years_data |> 
  filter(programCodeName %in% top_5_spheres) |> 
  summarise(yearly_budget_bln = sum(yearCorrectionPlan, na.rm = TRUE) / 1e9, .by = c(year, programCodeName))

ggplot(chart1_data, aes(x = year, y = yearly_budget_bln, color = str_wrap(programCodeName, 50))) + 
  geom_line(linewidth = 1.5) + 
  geom_point(size = 3) + 
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red", linewidth = 1) +
  labs(
    title = "Динаміка фінансування Топ-5 макро-сфер (2021-2026)",
    subtitle = "Червона пунктирна лінія позначає початок повномасштабної війни",
    x = "Рік", y = "Уточнений річний план (Млрд грн)", color = "Сфера / Програма"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "vertical", plot.title = element_text(face = "bold", size = 16))


# ==========================================
# 2. ПОРІВНЯННЯ ПРІОРИТЕТІВ (ДВА ОКРЕМІ ГРАФІКИ: 2021 ТА 2025)
# ==========================================
macro_data <- all_years_data |> 
  filter(year %in% c(2021, 2025)) |> 
  filter(grepl("0000$", programCode) & programCode != "0000")

total_2021 <- sum(macro_data$yearCorrectionPlan[macro_data$year == 2021], na.rm = TRUE)
total_2025 <- sum(macro_data$yearCorrectionPlan[macro_data$year == 2025], na.rm = TRUE)

# Графік 2-А: Топ-10 у 2021
top10_2021 <- macro_data |> 
  filter(year == 2021) |> 
  summarise(budget_bln = sum(yearCorrectionPlan, na.rm = TRUE) / 1e9, .by = programCodeName) |> 
  mutate(share_percent = (budget_bln / (total_2021 / 1e9)) * 100) |> 
  arrange(desc(share_percent)) |> slice_head(n = 10)

ggplot(top10_2021, aes(x = reorder(str_wrap(programCodeName, 35), share_percent), y = share_percent)) + 
  geom_col(fill = "#4A90E2", width = 0.7) + coord_flip() + 
  geom_text(aes(label = paste0(round(share_percent, 1), "% (", round(budget_bln, 1), " млрд)")), hjust = -0.05, size = 3.5, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
  labs(
    title = "Топ-10 пріоритетів держави у 2021 році", 
    subtitle = "Відсоток від загального макро-бюджету та сума", 
    x = "", 
    y = "Частка у загальному бюджеті (%)") +
  theme_minimal() + theme(
    plot.title = element_text(face = "bold", size = 15), 
    axis.text.y = element_text(size = 10, lineheight = 0.8))

# Графік 2-Б: Топ-10 у 2025
top10_2025 <- macro_data |> 
  filter(year == 2025) |> 
  summarise(budget_bln = sum(yearCorrectionPlan, na.rm = TRUE) / 1e9, .by = programCodeName) |> 
  mutate(share_percent = (budget_bln / (total_2025 / 1e9)) * 100) |> 
  arrange(desc(share_percent)) |> slice_head(n = 10)

ggplot(top10_2025, aes(x = reorder(str_wrap(programCodeName, 35), share_percent), y = share_percent)) + 
  geom_col(fill = "#E24A4A", width = 0.7) + coord_flip() + 
  geom_text(aes(label = paste0(round(share_percent, 1), "% (", round(budget_bln, 1), " млрд)")), hjust = -0.05, size = 3.5, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
  labs(
    title = "Топ-10 пріоритетів держави у 2025 році", 
    subtitle = "Відсоток від загального макро-бюджету та сума", 
    x = "", 
    y = "Частка у загальному бюджеті (%)") +
  theme_minimal() + theme(
    plot.title = element_text(face = "bold", size = 15),
    axis.text.y = element_text(size = 10, lineheight = 0.8)
    )

# ==========================================
# 3. ВИКОНАННЯ БЮДЖЕТУ: ПЛАН VS ФАКТ (ДВА ОКРЕМІ ГРАФІКИ: 2021 ТА 2025)
# ==========================================
plot_execution <- function(data, target_year) {
  df_year <- data |> filter(year == target_year, grepl("0000$", programCode) & programCode != "0000")
  
  top_15 <- df_year |> 
    summarise(total = sum(yearCorrectionPlan, na.rm = TRUE), .by = programCodeName) |> 
    arrange(desc(total)) |> slice_head(n = 15) |> pull(programCodeName)
  
  chart_data <- df_year |> 
    filter(programCodeName %in% top_15) |> 
    summarise(
      `План` = sum(yearCorrectionPlan, na.rm = TRUE) / 1e9,
      `Фактично виконано` = sum(periodDone, na.rm = TRUE) / 1e9,
      .by = programCodeName
    ) |> 
    pivot_longer(cols = c(`План`, `Фактично виконано`), names_to = "Показник", values_to = "Сума")
  
  ggplot(chart_data, aes(x = reorder(str_wrap(programCodeName, 45), Сума, FUN = max), y = Сума, fill = Показник)) + 
    geom_col(position = "dodge") + coord_flip() + 
    labs(title = paste("Виконання бюджету-", target_year, ": Топ-15 сфер", sep=""), x = "", y = "Сума (Млрд грн)", fill = "") +
    scale_fill_manual(values = c("План" = "#d3d3d3", "Фактично виконано" = "#4A90E2")) +
    theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 15))
}

# Графік 3-A: Топ-15 у 2021
plot_execution(all_years_data, 2021) 

# Графік 3-B: Топ-15 у 2025
plot_execution(all_years_data, 2025) 

# ==========================================
# 3.1 НАЙБІЛЬШЕ НЕДОВИКОНАННЯ БЮДЖЕТУ (ДВА ОКРЕМІ ГРАФІКИ: 2021 ТА 2025)
# ==========================================
# Графік 3.1-A: Топ-10 у 2021
unspent_data_21 <- all_years_data |> 
  filter(year == 2021) |> 
  filter(grepl("0000$", programCode) & programCode != "0000") |>
  summarise(
    plan_bln = sum(yearCorrectionPlan, na.rm = TRUE) / 1e9,
    done_bln = sum(periodDone, na.rm = TRUE) / 1e9,
    .by = programCodeName
  ) |> 
  filter(plan_bln > 1) |> # 
  mutate(unspent_percent = ((plan_bln - done_bln) / plan_bln) * 100) |> 
  filter(unspent_percent > 0) |> 
  arrange(desc(unspent_percent)) |> slice_head(n = 10)

ggplot(unspent_data_21, aes(x = reorder(str_wrap(programCodeName, 35), unspent_percent), y = unspent_percent)) +
  geom_col(fill = "#4A90E2", width = 0.7) + coord_flip() + 
  geom_text(aes(label = paste0(round(unspent_percent, 1), "% (План: ", round(plan_bln, 1), " млрд)")), hjust = -0.05, size = 3.5, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  labs(
    title = "Топ-10 сфер з найбільшим недовиконанням (2021)", 
    subtitle = "Відсоток невитрачених коштів та їх початковий план", 
    x = "", 
    y = "Відсоток невиконання (%)",
    caption = "Openbudget.gov.ua: Видатки бюджету за 2021-2026 роки")+
  theme_minimal() + theme(plot.title = element_text(face = "bold", size = 14), axis.text.y = element_text(size = 9, lineheight = 0.8), plot.margin = margin(10, 20, 10, 10))

# Графік 3.1-B: Топ-10 у 2025
unspent_data_25 <- all_years_data |> 
  filter(year == 2025) |> 
  filter(grepl("0000$", programCode) & programCode != "0000") |> # Тільки макро-категорії
  summarise(
    plan_bln = sum(yearCorrectionPlan, na.rm = TRUE) / 1e9,
    done_bln = sum(periodDone, na.rm = TRUE) / 1e9,
    .by = programCodeName
  ) |> 
  filter(plan_bln > 1) |> # Тільки великі бюджети (> 1 млрд)
  mutate(unspent_percent = ((plan_bln - done_bln) / plan_bln) * 100) |> 
  filter(unspent_percent > 0) |> 
  arrange(desc(unspent_percent)) |> slice_head(n = 10)

ggplot(unspent_data_25, aes(x = reorder(str_wrap(programCodeName, 35), unspent_percent), y = unspent_percent)) +
  geom_col(fill = "#E24A4A", width = 0.7) + coord_flip() + 
  geom_text(aes(label = paste0(round(unspent_percent, 1), "% (План: ", round(plan_bln, 1), " млрд)")), hjust = -0.05, size = 3.5, color = "black") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  labs(
    title = "Топ-10 сфер з найбільшим недовиконанням (2025)", 
    subtitle = "Відсоток невитрачених коштів та їх початковий план", 
    x = "", 
    y = "Відсоток невиконання (%)",
    caption = "Openbudget.gov.ua: Видатки бюджету за 2021-2026 роки") +
  theme_minimal() + theme(
    plot.title = element_text(face = "bold", size = 14), 
    axis.text.y = element_text(size = 9, lineheight = 0.8), 
    plot.margin = margin(10, 20, 10, 10))

  # ==========================================
# 4. ДЕТАЛІ МІНОБОРОНИ 2025 (МІКРО-ПРОГРАМИ)
# ==========================================
mod_data <- all_years_data |> 
  filter(year == 2025, grepl("^21", programCode), !grepl("000$", programCode)) |> 
  summarise(budget_bln = sum(yearCorrectionPlan, na.rm = TRUE) / 1e9, .by = programCodeName) |> 
  arrange(desc(budget_bln)) |> slice_head(n = 5)

ggplot(mod_data, aes(
  x = reorder(str_wrap(programCodeName, 50), budget_bln), 
  y = budget_bln)
  ) + geom_col(fill = "#2C3E50", width = 0.7) + coord_flip() + 
  geom_text(aes(label = round(budget_bln, 1)), hjust = -0.2, size = 4, color = "black") +
  labs(
    title = "Структура витрат Міноборони 2025 (Топ-5 програм)", 
    x = "", 
    y = "Уточнений план (Млрд грн)",
    caption = "Openbudget.gov.ua: Видатки бюджету за 2021-2026 роки") +
  theme_minimal() + theme(
    plot.title = element_text(face = "bold", size = 14), 
    axis.text.y = element_text(size = 9, lineheight = 0.8, color = "black"), 
    plot.margin = margin(10, 30, 10, 10))

# ==========================================
# 5. ІНТЕРАКТИВНИЙ SCATTER PLOT (РІЗНИЦЯ 2021 vs 2025)
# ==========================================
winners_losers <- all_years_data |> 
  filter(year %in% c(2021, 2025), grepl("0000$", programCode), programCode != "0000") |> 
  summarise(total_budget_bln = sum(yearCorrectionPlan, na.rm = TRUE) / 1e9, .by = c(year, programCodeName)) |> 
  pivot_wider(names_from = year, values_from = total_budget_bln) |> 
  rename(budget_2021 = `2021`, budget_2025 = `2025`) |> 
  filter(!is.na(budget_2021), budget_2021 > 0, !is.na(budget_2025)) |> 
  mutate(growth_times = budget_2025 / budget_2021)

p_scatter <- ggplot(winners_losers, aes(
  x = budget_2021, y = growth_times, 
  text = paste("<b>", programCodeName, "</b><br>Бюджет 2021: ", round(budget_2021, 1), " млрд<br>Бюджет 2025: ", round(budget_2025, 1), " млрд<br>Зростання: ", round(growth_times, 1), " разів")
)) +
  geom_point(aes(size = budget_2025), color = "#8E44AD", alpha = 0.6) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") +
  labs(title = "Зміна бюджетів макро-сфер: 2021 vs 2025",
       subtitle = "",
       x = "Бюджет у 2021 (Млрд грн)", 
       y = "Зростання (у разах)",
       caption = "Openbudget.gov.ua: Видатки бюджету за 2021-2026 роки") +
  theme_minimal() + theme(legend.position = "none")

ggplotly(p_scatter, tooltip = "text")


# ==========================================
# 6. ІНТЕРАКТИВНИЙ TREEMAP 2025 (ДЕТАЛІЗАЦІЯ)
# ==========================================
total_2025_bln_tree <- sum(clean_2025$yearCorrectionPlan, na.rm = TRUE) / 1e9

tm_data_2025 <- clean_2025 |> 
  filter(!grepl("000$", programCode)) |> # Тільки мікро-програми
  summarise(budget_bln = sum(yearCorrectionPlan, na.rm = TRUE) / 1e9, .by = programCodeName) |> 
  filter(budget_bln > 2) |> 
  
  mutate(
    label_wrapped = str_replace_all(str_wrap(programCodeName, width = 20), "\n", "<br>"),
    percent_total = (budget_bln / total_2025_bln_tree) * 100,
    box_text = paste0(round(budget_bln, 1), " млрд"),
    hover_text = paste0("<b>", programCodeName, "</b><br>Бюджет: ", round(budget_bln, 1), " млрд грн<br>Частка: ", round(percent_total, 2), "%")
  )

plot_ly(
  data = tm_data_2025,
  type = "treemap",
  labels = ~label_wrapped, parents = "", values = ~budget_bln,
  text = ~box_text, textinfo = "label+text",
  hovertext = ~hover_text, hoverinfo = "text",
  marker = list(colorscale = "Blues", reversescale = TRUE),
  textfont = list(size = 13)
) |> 
  layout(
    title = list(text = paste0("Деталізований бюджет 2025 (програми понад 2 млрд)<br>Загальний бюджет: ", round(total_2025_bln_tree, 1), " млрд грн"), y = 0.98),
    margin = list(t = 60, b = 10, l = 10, r = 10)
  )
