library(tidyverse)
library(lubridate)
library(patchwork)

color_actual = "firebrick"
color_optimal = "springgreen4"
color_thompson = "gray70"


types = tibble(
    male = rep(c("Female", "Male"), length.out = 8),
    english = rep(
        c("no english", "english"),
        length.out = 8,
        each = 2
    ),
    adult = rep(
        c("not working age", "working age"),
        length.out = 8,
        each = 4
    ),
    type_name = paste(male, english, adult, sep = ", "),
    U = 1:8
) %>%
    select(type_name, U)

theta_calibrated =
    read_csv("Data_processed/theta_calibrated.csv") %>%
    select(U, V, theta) %>%
    mutate(
        Simulated_V = V,
        Simulated_theta = theta,
        Optimal_V = V,
        Optimal_theta = theta
    )


# Calculate annual average employment rates for each simulation replication ----
csv_list = paste0(
    "Simulation_output/",
    list.files(path = "Simulation_output/",
               pattern = "*allocation.csv")
)

welfare_trajectory = function(filename) {
    read_csv(filename) %>%
        filter(NUST == 1) %>%
        mutate(Year = year(as_date(Arrival))) %>%
        filter(Year < 2020) %>%
        left_join(
            theta_calibrated %>% select(U, Simulated_V, Simulated_theta),
            by = c("U", "Simulated_V")
        ) %>%
        left_join(theta_calibrated %>% select(U, Optimal_V, Optimal_theta),
                  by = c("U", "Optimal_V")) %>%
        group_by(Year) %>%
        summarize(
            n = n(),
            Mean_employment = mean(y),
            Mean_simulated_employment = mean(Simulated_Y),
            Mean_theta = mean(theta),
            Mean_simulated_theta = mean(Simulated_theta),
            Mean_optimal_theta = mean(Optimal_theta)
        ) %>%
        mutate(File = filename)
}

summaries = map(csv_list, welfare_trajectory) %>%
    bind_rows()

bind_rows()


summaries %>%
    group_by(Year) %>%
    summarise(
        Mean_actual = mean(Mean_theta),
        Mean_simulated = mean(Mean_simulated_theta)
    ) %>%
    write_csv("Figures/Aggregate_byyear.csv")

average_summaries = summaries %>%
    group_by(Year) %>%
    summarise(
        Mean_simulated_employment = mean(Mean_simulated_employment),
        Mean_simulated_theta = mean(Mean_simulated_theta)
    ) %>%
    mutate(File = NA)

n = nrow(summaries)
line_labels = tibble(
    theta = c(summaries[[n, "Mean_theta"]], summaries[[n, "Mean_simulated_theta"]], summaries[[n, "Mean_optimal_theta"]]),
    lab = c("Actual assignment", "Thompson", "Oracle optimum")
)



# Save plots for actual and expected employment, annually -----

p1a = ggplot(summaries,
             aes(x = Year, y = Mean_simulated_employment, group = File)) +
    geom_path(aes(y = Mean_employment), color = color_actual) +
    geom_path(alpha = .08) +
    geom_path(data = average_summaries) +
    theme_minimal() +
    scale_x_continuous(
        breaks = min(summaries$Year):max(summaries$Year),
        minor_breaks = NULL,
        expand = expansion(add = 0)
    ) +
    labs(y = "Mean employment")


p1b = summaries %>%
    filter(File == "Simulation_output/1_hias_simulated_allocation.csv") %>%
    ggplot(aes(x = Year, y = Mean_theta, group = File)) +
    geom_path(aes(y = Mean_employment), color = color_thompson) +
    geom_path(color = color_actual) +
    theme_minimal() +
    scale_x_continuous(
        breaks = min(summaries$Year):max(summaries$Year),
        minor_breaks = NULL,
        expand = expansion(add = 0)
    ) +
    scale_y_continuous(limits = c(.3, NA),
                       sec.axis = dup_axis(
                           breaks = c(summaries[[n, "Mean_theta"]], summaries[[n, "Mean_employment"]]),
                           labels = c("Estimated employment rate", "Observed employment rate"),
                           name = NULL
                       )) +
    labs(y = "Mean employment")

p2 = ggplot(summaries,
            aes(x = Year, y = Mean_simulated_theta, group = File)) +
    geom_path(aes(y = Mean_theta), color = color_actual) +
    geom_path(aes(y = Mean_optimal_theta), color = color_optimal) +
    geom_path(alpha = .08) +
    geom_path(data = average_summaries) +
    theme_minimal() +
    scale_x_continuous(
        breaks = min(summaries$Year):max(summaries$Year),
        minor_breaks = NULL,
        expand = expansion(add = 0)
    ) +
    scale_y_continuous(
        limits = c(.35, NA),
        sec.axis = dup_axis(breaks = line_labels$theta,
                            labels = line_labels$lab),
        name = NULL
    ) +
    labs(y = "Mean expected employment")



ggsave(p1a,
       filename = "Figures/Simulated_trajectories.png",
       width = 7,
       height = 3)

ggsave(p1b,
       filename = "Figures/Realized_and_expected_trajectories.png",
       width = 7,
       height = 3)


ggsave(p2,
       filename = "Figures/Simulated_expected_trajectories.png",
       width = 7,
       height = 3)



# Same analysis as before, but split by type ----
welfare_trajectory_bytype = function(filename) {
    read_csv(filename) %>%
        filter(NUST == 1) %>%
        mutate(Year = year(as_date(Arrival))) %>%
        filter(Year < 2020) %>%
        left_join(
            theta_calibrated %>% select(U, Simulated_V, Simulated_theta),
            by = c("U", "Simulated_V")
        ) %>%
        left_join(theta_calibrated %>% select(U, Optimal_V, Optimal_theta),
                  by = c("U", "Optimal_V")) %>%
        group_by(Year, U) %>%
        summarize(
            n = n(),
            Mean_employment = mean(y),
            Mean_simulated_employment = mean(Simulated_Y),
            Mean_theta = mean(theta),
            Mean_simulated_theta = mean(Simulated_theta),
            Mean_optimal_theta = mean(Optimal_theta)
        ) %>%
        mutate(File = filename)
}

summaries_bytype = map(csv_list, welfare_trajectory_bytype) %>%
    bind_rows() %>%
    left_join(types, by = "U")

average_summaries_bytype = summaries_bytype %>%
    group_by(Year, type_name) %>%
    summarise(
        Mean_simulated_employment = mean(Mean_simulated_employment),
        Mean_simulated_theta = mean(Mean_simulated_theta)
    ) %>%
    mutate(File = NA)

p3 = ggplot(summaries_bytype,
            aes(x = Year, y = Mean_simulated_theta, group = File)) +
    geom_path(aes(y = Mean_theta), color = color_actual) +
    geom_path(aes(y = Mean_optimal_theta), color = color_optimal) +
    geom_path(alpha = .08) +
    geom_path(data = average_summaries_bytype) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(min(summaries$Year), max(summaries$Year), by =
                                        2),
                       minor_breaks = seq(min(summaries$Year), max(summaries$Year))) +
    ylim(0, NA) +
    facet_wrap(~ type_name, ncol = 4) +
    labs(y = "Mean expected employment")

ggsave(p3,
       filename = "Figures/Simulated_expected_trajectories_bytype.png",
       width = 10,
       height = 3.5)


# Additional summary plots ----
aggregate_summaries = summaries_bytype %>%
    group_by(U) %>%
    summarize(
        n_total = sum(n),
        Mean_theta = weighted.mean(Mean_theta, n),
        Mean_simulated_theta = weighted.mean(Mean_simulated_theta, n),
        Mean_optimal_theta = weighted.mean(Mean_optimal_theta, n)
    ) %>%
    left_join(types, by = "U")


agg_labs = aggregate_summaries %>%
    filter(U == 6) %>%
    select(type_name,
           Mean_theta,
           Mean_simulated_theta,
           Mean_optimal_theta) %>%
    pivot_longer(cols = c(-type_name),
                 names_to = "lab",
                 values_to = "theta") %>%
    mutate(label = c("Actual", "Thompson", "Oracle"),
           nudge_y = c(-.02, -.03, 0))

p4 = aggregate_summaries %>%
    ggplot(aes(x = type_name, y = Mean_theta)) +
    geom_segment(aes(xend = type_name,
                     yend = Mean_optimal_theta), alpha = .5) +
    geom_point(aes(y = Mean_simulated_theta),
               color = color_thompson,
               size = 4) +
    geom_point(color = color_actual, size = 2) +
    geom_point(aes(y = Mean_optimal_theta),
               color = color_optimal, size = 2) +
    geom_text(
        data =  agg_labs,
        aes(
            y = theta + nudge_y,
            x = type_name,
            label = label
        ),
        nudge_x = .4,
        size = 3
    ) +
    theme_minimal() +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(x = "",
         y = "Mean expected employment")

p5 = aggregate_summaries %>%
    ggplot(aes(x = type_name, y = n_total)) +
    geom_col(width = .4, fill = color_thompson) +
    ylim(0, NA) +
    scale_x_discrete(labels = NULL) +
    theme_minimal() +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(x = "",
         y = "Number of cases")

ggsave(p4,
       filename = "Figures/Aggregate_bytype.png",
       width = 7,
       height = 3.5)

ggsave(
    p4 + p5 + plot_layout(widths = c(3, 2)),
    filename = "Figures/Aggregate_bytype_casecount.png",
    width = 10,
    height = 3.5
)


aggregate_summaries %>%
    write_csv("Figures/Aggregate_bytype.csv")

# Same analysis as before, but split by affiliate ----
welfare_trajectory_by_affiliate = function(filename) {
    read_csv(filename) %>%
        filter(NUST == 1) %>%
        mutate(Year = year(as_date(Arrival))) %>%
        filter(Year < 2020) %>%
        left_join(
            theta_calibrated %>% select(U, Simulated_V, Simulated_theta),
            by = c("U", "Simulated_V")
        ) %>%
        left_join(theta_calibrated %>% select(U, Optimal_V, Optimal_theta),
                  by = c("U", "Optimal_V")) %>%
        group_by(Year, V) %>%
        summarize(
            n = n(),
            Mean_employment = mean(y),
            Mean_simulated_employment = mean(Simulated_Y),
            Mean_theta = mean(theta),
            Mean_simulated_theta = mean(Simulated_theta),
            Mean_optimal_theta = mean(Optimal_theta)
        ) %>%
        mutate(File = filename)
}

summaries_by_affiliate = map(csv_list, welfare_trajectory_by_affiliate) %>%
    bind_rows()

aggregate_summaries_by_affiliate = summaries_by_affiliate %>%
    group_by(V) %>%
    summarize(
        n_total = sum(n),
        Mean_theta = weighted.mean(Mean_theta, n),
        Mean_simulated_theta = weighted.mean(Mean_simulated_theta, n),
        Mean_optimal_theta = weighted.mean(Mean_optimal_theta, n)
    )

p6 = aggregate_summaries_by_affiliate %>%
    ggplot(aes(x = factor(V), y = Mean_theta)) +
    geom_segment(aes(xend = factor(V),
                     yend = Mean_optimal_theta), alpha = .5) +
    geom_point(aes(y = Mean_simulated_theta),
               color = color_thompson,
               size = 4) +
    geom_point(color = color_actual, size = 2) +
    geom_point(aes(y = Mean_optimal_theta),
               color = color_optimal, size = 2) +
    theme_minimal() +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(x = "Affiliate",
         y = "Mean expected employment")

p7 = aggregate_summaries_by_affiliate %>%
    ggplot(aes(x = factor(V), y = n_total)) +
    geom_col(width = .4, fill = color_thompson) +
    ylim(0, NA) +
    scale_x_discrete(labels = NULL) +
    theme_minimal() +
    coord_flip() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(x = "Affiliate",
         y = "Number of cases",
         title = "Distribution of cases across affiliates")

ggsave(p6,
       filename = "Figures/Aggregate_by_affiliate.png",
       width = 7,
       height = 6)

ggsave(
    p6 + p7 + plot_layout(widths = c(3, 2)),
    filename = "Figures/Aggregate_by_affiliate_casecount.png",
    width = 10,
    height = 6
)
