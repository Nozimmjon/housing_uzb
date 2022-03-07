
# Table data
final_data_gt = final_data %>% 
  group_by(region) %>%
  mutate(med=median(price_m2)) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(med))) %>%
  arrange(rank)

price_list <- split(final_data_gt$price_m2, final_data_gt$rank)
price_rng <- range(final_data_gt$price_m2)


gt_plot <- function(table_data, column, plot_data, plot_fun, ...){
  text_transform(
    table_data,
    # note the use of {{}} here - this is tidy eval
    # that allows you to indicate specific columns
    locations = cells_body(columns = vars({{column}})),
    fn = function(x){
      plot <- map(plot_data, plot_fun, width = 300, height = 70, same_lim = TRUE, ...)
      plot_svg <- map(plot, "svg_text")
      map(plot_svg, gt::html)
    }
  )
}

# {gt} table
great_viz <- final_data_gt %>%
  group_by(region) %>%
  summarise(n=n(),
            average = round(mean(price_m2),0),
            min=round(min(price_m2),0) ,
            median = round(median(price_m2),0),
            max=round(max(price_m2),0),
            range= max-min,
            histogram=list(price_m2)) %>%
  arrange(desc(median)) %>%
  mutate(boxplot ="",
         n2 = n, 
         #rank = dense_rank(desc(avg))
  ) %>%
  select(region, n, median, histogram, min, average, max, boxplot) %>%
  gt() %>%
  gt_theme_538() %>%
  gt_sparkline(
    histogram,
    type = "histogram",
    line_color = "#66462c",
    fill_color = "#66462c",
    bw = .25,
    same_limit = TRUE
  ) %>%
  gt_plot(
    column = boxplot,  # column to create plot in 
    plot_data = price_list, # external data to reference
    plot_fun = spec_boxplot,  # which plot fun
    lim = price_rng, # range applied
  ) %>%
  gt_color_rows(columns = "median",
                palette = "ggsci::brown_material") %>%
  cols_align(columns = c("histogram", "boxplot", "median"),
             align="center") %>%
  cols_label(region = html("Ҳудудлар"),
             n=html("Эълонлар сони"),
             median = html("Медиан нарх"),
             histogram = html("Гистограмма"),
             average = html("Ўртача нарх")) %>%
  tab_spanner(label="Нархлар", 
              columns=c(median:boxplot)) %>%
  tab_header(title=md("<span style='color:#411d13'> Ҳудудлар марказларида уй-жой нархлари </span>"),
             subtitle=md("*2021 йил декабрь ойи*")) %>%
  tab_source_note(source_note = gt::html("<br>Очиқ интернет маълумотлари асосида ҳисобланган")) %>%
  # Adjust sub-title font
  tab_style(
    style = list(
      cell_text(
        weight="lighter"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  )  

gtsave(great_viz, 'final_graph.png', path = here("code"))
