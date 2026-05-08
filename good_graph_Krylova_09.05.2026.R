# Визуальный стиль графика был вдохновлён материалами курса,
# примерами дата-визуализаций с портала X.com,
# а также работой Nico Künel Peltze (@nico_kinel):
# https://twitter.com/nico_kinel/status/1838497997602042097
#
# В процессе подготовки кода, выбора типа визуализации,
# оформления графика и интерпретации статистического теста
# я также консультировалась с ChatGPT
#
# Код и визуализация были адаптированы под данные моего проекта:
# корпус текстов IELTS Writing Task 1



# IELTS Writing Task 1: длина ответов по типам заданий


library(tidyverse)
library(jsonlite)
library(patchwork)


# Загружаем данные с корпуса из файла lengths.json

data_path <- if (file.exists("lengths.json")) {
  "lengths.json"
} else if (file.exists("data/lengths.json")) {
  "data/lengths.json"
} else {
  file.choose()
}

lengths <- fromJSON(data_path)


# Препроцессинг данных для визуализации
# На этом этапе мы оставляем только нужные переменные,
# переводим длину текста в числовой формат,
# заменяем технические названия типов заданий на читаемые подписи
# и удаляем пропущенные значения

# chart_type - техническое название типа задания
# wc_custom - длина текста в словах
# prompt_type - аккуратное английское название типа задания для графика,
# которое будет использоваться ниже и в легенде

lengths_clean <- lengths %>%
  transmute(
    word_count = as.numeric(wc_custom),
    prompt_type = recode(
      chart_type,
      "bar_chart" = "Bar chart",
      "line_graph" = "Line graph",
      "pie_chart" = "Pie chart",
      "table" = "Table",
      "map" = "Map",
      "process_diagram" = "Process diagram",
      "mixed" = "Mixed task"
    )
  ) %>%
  drop_na(prompt_type, word_count)


# Создаём сводную таблицу
# Для каждого типа задания считаем:
# median_words - медианную длину текста

summary_tbl <- lengths_clean %>%
  group_by(prompt_type) %>%
  summarise(
    median_words = median(word_count),
    .groups = "drop"
  ) %>%
  arrange(desc(median_words))


# Упорядочиваем типы заданий:
# На графике типы заданий будут идти от самой высокой медианной длины 
# к самой низкой

summary_tbl <- summary_tbl %>%
  mutate(
    prompt_type = factor(prompt_type, levels = prompt_type)
  )

lengths_clean <- lengths_clean %>%
  mutate(
    prompt_type = factor(prompt_type, levels = levels(summary_tbl$prompt_type))
  )


# Найдём самый длинный и самый короткий тип по медиане
# longest_type - тип задания с самой высокой медианной длиной
# shortest_type - тип задания с самой низкой медианной длиной

longest_type <- as.character(summary_tbl$prompt_type[1])
shortest_type <- as.character(summary_tbl$prompt_type[nrow(summary_tbl)])


# Добавим переменную для цветового выделения, чтоб график не был слишком мрачным:
# Самый длинный тип будет выделен более ярким цветом (фиолетовым)
# Самый короткий тип будет выделен более бледным цветом (желтоватым)
# Остальные типы будут светло-серыми

summary_tbl <- summary_tbl %>%
  mutate(
    highlight = case_when(
      prompt_type == longest_type ~ "Longest median",
      prompt_type == shortest_type ~ "Shortest median",
      TRUE ~ "Other types"
    )
  )

lengths_clean <- lengths_clean %>%
  left_join(
    summary_tbl %>% select(prompt_type, highlight),
    by = "prompt_type"
  )


# Проводим статистический тест
# Тест Краскела–Уоллиса проверяет,
# различается ли длина ответов между типами заданий
#
# Он подходит для данного исследования, так как:
# - длина текста - числовая переменная
# - тип задания — категориальная переменная
# - типов задания больше двух

# - тест не требует нормального распределения данных
# (т.к. этот тест подходит даже тогда, когда длины текстов распределены 
#    не идеально симметрично)

kw_test <- kruskal.test(word_count ~ prompt_type, data = lengths_clean)

kw_label <- paste0(
  "Kruskal-Wallis test: p = ",
  format.pval(kw_test$p.value, digits = 3, eps = 0.001)
)


# Создаём текст для подзаголовка, который будет меняться в зависимости от данных

# Этот блок делает короткий вывод для графика
# Он сам находит:
# - какой тип задания имеет самые длинные тексты
# - какой тип задания имеет самые короткие тексты
# - сколько слов в них примерно по медиане
#
# Потом этот текст появится под главным заголовком графика

key_message <- paste0(
  longest_type,
  " shows the highest median response length (",
  round(summary_tbl$median_words[1]),
  " words), while ",
  shortest_type,
  " is shortest (",
  round(summary_tbl$median_words[nrow(summary_tbl)]),
  " words)."
)


# Задаём цветовую палитру

# Самый длинный тип выделяем оттенком фиолетового
# Самый короткий тип — светло-жёлтым
# Остальные типы — светло-серые
# Серая пунктирная линия слева показывает порог 150 слов

long_accent <-  "#6F42C1"   
short_accent <- "#F3D9A4"   
dark_text    <- "#1F2937"
light_grey   <- "#DDE1E7"
mid_grey     <- "#A8B0BD"
grid_grey    <- "#E8EBF0"
line_grey    <- "#7B8794"

fill_values <- c(
  "Shortest median" = short_accent,
  "Longest median"  = long_accent,
  "Other types"     = light_grey
)

color_values <- c(
  "Shortest median" = "#D9B56B",
  "Longest median"  = long_accent,
  "Other types"     = mid_grey
)

# Создаём общую тему оформления

# Тема сделана в стиле журнала и вдохновлена графиком 
#                                             (от @nico_kinel, ссылка выше):
# - белый фон
# - крупные читаемые подписи
# - минимум декоративных элементов, но
# - наличие ярких элементов у самого длинного/короткого типа

editorial_theme <- theme_minimal(base_size = 13, base_family = "sans") +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 13,
      colour = dark_text
    ),
    plot.subtitle = element_text(
      size = 11,
      colour = "grey35"
    ),
    axis.title = element_text(
      face = "bold",
      colour = dark_text
    ),
    axis.text = element_text(
      colour = dark_text,
      size = 11
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = grid_grey,
      linewidth = 0.5
    ),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.caption = element_text(
      size = 9.5,
      colour = "grey40",
      hjust = 0
    )
  )


# Верхний график: типичная длина ответа

# Здесь каждая точка показывает медианную длину текста
# для одного типа IELTS-задания
#
# Вертикальная пунктирная линия - это рекомендуемый минимум 
# для данного задания IELTS: 150 слов
#
# Серая линия от 150 до точки показывает,
# насколько типичная длина ответа выше этого минимума
# Например, если точка стоит на 200,
# значит медианная длина примерно на 50 слов выше минимума

p_top <- ggplot(summary_tbl, aes(x = median_words, y = prompt_type)) +
  
  geom_segment(
    aes(x = 150, xend = median_words, yend = prompt_type),
    colour = "#C9D0DA",
    linewidth = 2.2,
    lineend = "round"
  ) +
  
  geom_vline(
    xintercept = 150,
    linetype = "dashed",
    linewidth = 0.8,
    colour = line_grey
  ) +
  
  geom_point(
    aes(colour = highlight),
    size = 4.2
  ) +
  
  geom_text(
    aes(label = round(median_words)),
    nudge_x = 4,
    size = 3.6,
    colour = dark_text
  ) +
  
  scale_colour_manual(values = color_values, guide = "none") +
  
  scale_x_continuous(
    breaks = seq(150, 240, 10),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  
  coord_cartesian(
    xlim = c(150, 240),
    clip = "off"
  ) +
  
  labs(
    title = "MEDIAN RESPONSE LENGTH RELATIVE TO THE IELTS MINIMUM",
    x = "Words",
    y = NULL
  ) +
  
  editorial_theme +
  
  theme(
    axis.title.x = element_text(
      margin = margin(t = 14)
    ),
    plot.margin = margin(
      t = 5,
      r = 10,
      b = 18,
      l = 5
    ),
    axis.title.y = element_blank()
  )

# Нижний график: разброс длины внутри типов заданий

# (Этот график дополняет первый)
# Первый график показывает краткое резюме:
# где находится медианная, то есть типичная, длина ответа
#
# Нижний график показывает подробнее:
# насколько сильно различается длина текстов
# внутри каждого типа задания
#
# Boxplot читается так:
# - короткая вертикальная линия внутри коробки = медиана
# - сама коробка = центральные 50% текстов
# - длинная горизонтальная линия = общий разброс типичных значений


p_bottom <- ggplot(lengths_clean, aes(x = prompt_type, y = word_count, fill = highlight)) +
  
  geom_hline(
    yintercept = 150,
    linetype = "dashed",
    linewidth = 0.8,
    colour = line_grey
  ) +
  
  geom_boxplot(
    width = 0.58,
    linewidth = 0.7,
    colour = "grey30",
    outlier.shape = NA
  ) +
  
  scale_fill_manual(
    values = fill_values,
    breaks = c("Shortest median", "Longest median", "Other types")
  ) +
  
  scale_y_continuous(
    breaks = seq(150, 240, 10),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  
  coord_flip(
    ylim = c(150, 240),
    clip = "off"
  ) +
  
  labs(
    title = "RESPONSE-LENGTH VARIATION WITHIN EACH PROMPT TYPE",
    x = NULL,
    y = "Words"
  ) +
  
  editorial_theme +
  
  theme(
    axis.title.x = element_text(
      margin = margin(t = 16)
    ),
    axis.title.y = element_text(
      margin = margin(r = 10)
    ),
    plot.margin = margin(
      t = 20,
      r = 10,
      b = 5,
      l = 5
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = grid_grey,
      linewidth = 0.5
    )
  )


# Объединяем два графика в одно финальное изображение

# patchwork позволяет собрать два графика в одну композицию
# Сверху — медианы
# Снизу — разброс длины внутри типов

# plot_spacer() добавляет небольшой промежуток между графиками, 
# чтобы было визуально и эстетично лучше :)

final_plot <- p_top / patchwork::plot_spacer() / p_bottom +
  plot_layout(
    heights = c(1, 0.12, 1.55)
  ) +
  plot_annotation(
    title = "Some IELTS Writing Task 1 Prompt Types Elicit Longer Responses",
    subtitle = paste(
      "Based on a corpus of IELTS Writing Task 1 model responses.",
      key_message,
      sep = "\n"
    ),
    caption = paste(
      "Note: In the top panel, grey horizontal lines connect the IELTS 150-word minimum to each prompt type's median.",
      "In the bottom panel, boxplots show variation in response length within each prompt type.",
      kw_label,
      "Source: IELTS Writing Task 1 corpus project.",
      sep = "\n"
    ),
    theme = theme(
      plot.title = element_text(
        size = 20,
        face = "bold",
        colour = dark_text,
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = 11.5,
        colour = "grey30",
        lineheight = 1.25,
        hjust = 0.5,
        margin = margin(b = 16)
      ),
      plot.caption = element_text(
        size = 9.5,
        colour = "grey40",
        hjust = 0,
        lineheight = 1.2
      ),
      plot.margin = margin(
        t = 10,
        r = 10,
        b = 10,
        l = 10
      )
    )
  )


#  Вывод графика в RStudio

print(final_plot)


# Сохранение графика в отдельный файл

print(getwd())

output_file <- "good_graph_Krylova_09_05_2026.png"

ggsave(
  filename = output_file,
  plot = final_plot,
  width = 11,
  height = 10,
  dpi = 300,
  bg = "white"
)



# Выведем результат статистического теста в консоли

print(kw_test)


# Тест Краскела–Уоллиса показал статистически значимые различия 
# между типами заданий по длине текста: 

# p-value = 0.005131, 
# что меньше стандартного порога 0.05

# Это означает, что наблюдаемые различия в длине ответов 
# вряд ли являются случайными