box::use(
  dplyr[`%>%`, select],
  stats[aggregate],
  ggplot2[ggplot, aes, geom_rect, scale_fill_brewer,
          scale_color_brewer, coord_polar, unit,
          xlim, theme_void, theme, geom_text, element_blank],
  ggrepel[geom_text_repel],
  utils[head]
)

#' @export
create_sum_data <- function(title,
                            split_by,
                            count_by,
                            data) {

  data_sum <- data %>%
    select(as.name(count_by), as.name(split_by))
  data_sum <- aggregate(x = data_sum[[as.character(count_by)]] ~ data_sum[[as.character(split_by)]],
                        data = data_sum, FUN = sum)
  data_sum$ymax <- cumsum(data_sum[, 2])
  data_sum$ymin <- c(0, head(data_sum$ymax, n=-1))
  data_sum$label <- paste0(data_sum$data_sum[, 1], ": ", data_sum[, 2])
  data_sum$ylabelPosition <- (data_sum$ymax + data_sum$ymin) / 2
  colnames(data_sum) <- c(split_by, count_by, "ymax",
                          "ymin", "label", "ylabelPosition")

  return(data_sum)
}

#' @export
plot_sum_data <- function(title, split_by, count_by, data_sum) {
  
  ggplot(data_sum,
         aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = data_sum[, split_by])) +
    geom_rect() +
    geom_text_repel(size = 8, x = 1, aes(y = ylabelPosition,
                                         label = data_sum[, count_by], color = data_sum[, split_by]),
                             point.padding = unit(0.2, "lines"),
                             direction = "x") +
    scale_fill_brewer(palette="Dark2") +
    scale_color_brewer(palette="Dark2") +
    coord_polar(theta="y") +
    xlim(c(-1, 4)) +
    theme_void() +
    theme(legend.position = "top", legend.title=element_blank())

}
  

