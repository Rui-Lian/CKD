on_target_rate <- function(i) {
    # Select the columns from raw.
    # by name in the ith row of normal table. 
    tmp <- raw %>%
        select(matches(normal_table[i, 1]))
    
    # Cut intervals of the  columns. 
    tmp <- cut(tmp[[1]], 
               
               # Define cutting breaks by 
               # ith row, columns 3, 4, 5, 6 of normal table. 
               breaks = c(normal_table[i, 3], 
                          normal_table[i, 4], 
                          normal_table[i, 5], 
                          normal_table[i, 6]), 
               
               # Define breaks
               labels = c(paste0("<", 
                                 normal_table[i, 4], 
                                 normal_table[i, 2]), 
                          paste0(normal_table[i, 4], 
                                 "-",
                                 normal_table[i, 5], 
                                 normal_table[i, 2]), 
                          paste0(">", 
                                 normal_table[i, 5], 
                                 normal_table[i, 2])))
    
    
    tmp <- data.frame(Phase = raw$Phase, 
                      Province = raw$Province,
                      Target = tmp)
    
    total_target <- tmp %>%
        filter(!is.na(Target) & Phase %in% "T0") %>%
        group_by(Target) %>%
        summarise(n = n()) %>%
        mutate(pct = n/sum(n)) %>%
        ggplot() + 
        geom_col(aes(x = "", 
                     y = pct, 
                     fill = fct_rev(Target))) + 
        geom_text(aes(x = "", 
                      y = pct, 
                      label = percent(pct, 2)),
                  position = position_stack(vjust = 0.5)) +
        guides(fill = guide_legend(title = "Target")) + 
        scale_y_continuous(label = percent) + 
        labs(title = paste(normal_table[i, 1],"总体达标率"),
             x = "Total", 
             y = "患者比例%") + 
        scale_fill_manual(values = palette_OkabeIto) + 
        theme(legend.direction = "horizontal", 
              legend.position = "bottom")
    
    province_target <-tmp %>%
        filter(!is.na(Target) & Phase %in% "T0") %>%
        group_by(Province, Target) %>%
        summarise(n = n()) %>%
        mutate(pct = n/sum(n)) %>%
        ggplot() + 
        geom_col(aes(x = Province, 
                     y = pct, 
                     fill = fct_rev(Target))) + 
        geom_text(aes(x = Province, 
                      y = pct, 
                      label = percent(pct, 2)),
                  position = position_stack(vjust = 0.5)) +
        guides(fill = guide_legend(title = "Target")) + 
        scale_y_continuous(label = percent) + 
        labs(title = paste(normal_table[i, 1],"分省达标率"),
             x = "Province", 
             y = "患者比例%") + 
        scale_fill_manual(values = palette_OkabeIto) + 
        theme(legend.direction = "horizontal", 
              legend.position = "bottom")
    
    # patchwork, right plot for size 1:3
    # with space in-between. 
    # with a overall title. 
    total_target + plot_spacer()+ province_target + 
        plot_layout(widths = c(1, 0.1, 3)) + 
        plot_annotation(title = paste0(normal_table[i, 1], "达标"))
}
