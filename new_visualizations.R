# Load the ggplot2 library, which is used for creating elegant data visualizations.
library(ggplot2)
library(cowplot)
library(ggtext)
library(showtext)
library(ggrepel)
library(grid)
library(ragg)

library(dplyr)
library(tidyr)

library(psych)

color_palette <- c(
  "#4E342E", # Espresso
  "#3B6B35", # Forest Green
  "#3F5E78", # Denim Blue
  "#990000", # Deep Red
  "#A64B42", # Rust
  "#BC7A5A", # Terracotta
  "#636b2f", # Olive Green
  "#1E325C", # Rainbow Indigo
  "#E8D8C3", # Soft Beige
  "#4A4A4A" # Stone Gray
) 

fill_palette <- c(
  "#4E342EAA", # Espresso
  "#3B6B35AA", # Forest Green
  "#3F5E78AA", # Denim Blue
  "#990000AA", # Deep Red
  "#A64B42AA", # Rust
  "#BC7A5AAA", # Terracotta
  "#636b2fAA", # Olive Green
  "#1E325CAA", # Rainbow Indigo
  "#E8D8C3AA", # Soft Beige
  "#4A4A4AAA" # Stone Gray
) 

background_color <- "#F2ECD7"
font_add(
  family = "xetbook",
  regular = "XETBook/XETBook-Regular.otf",
  bold = "XETBook/XETBook-Bold.otf",
  italic = "XETBook/XETBook-Italic.otf",
  bolditalic = "XETBook/XETBook-BoldItalic.otf"
)
font_add_google("Puritan", family = "puritan")
showtext_auto()

# Create a data frame named 'project_dynamics_score'.
project_dynamics_score <- data.frame(
  # Create a column named 'domain' with repeating domain names.
  # The 'rep()' function is used to repeat each domain name four times.
  domain = c(
    rep("Contexts", 4),
    rep("Partnerships", 4),
    rep("Research", 4),
    rep("Learning", 4),
    rep("Outcomes", 4)
  ),
  # Create a column named 'dimension' with the names of different dimensions
  # associated with the domains.
  dimension = c(
    "Challenge", "Diversity", "Resources", "Trust",
    "Beneficence", "Decisions", "Reflection", "Tools",
    "Design", "Time", "Questions", "Voice",
    "Civic Learning", "Integration", "Learning Goals", "Reciprocity",
    "Capabilities", "Goals", "Outputs", "Sustainability"
  ),
  color_fill = c(
    rep(fill_palette[1], 4),
    rep(fill_palette[2], 4),
    rep(fill_palette[3], 4),
    rep(fill_palette[4], 4),
    rep(fill_palette[5], 4)
  ),
  color_stroke = c(
    rep(color_palette[1], 4),
    rep(color_palette[2], 4),
    rep(color_palette[3], 4),
    rep(color_palette[4], 4),
    rep(color_palette[5], 4)
  )
)

# Set the seed for reproducibility using the value of pi.
# This ensures that the random number generation is consistent across runs.
set.seed(Sys.time())

# Extract the unique dimension names from the 'dimension' column of the
# 'project_dynamics_score' data frame. This creates a vector containing
# each unique dimension only once.
unique_dimensions <- unique(project_dynamics_score$dimension)

# Generate a vector of random numbers from a uniform distribution.
# The number of random numbers generated is equal to the number of unique
# dimensions. Each random number will be between the specified minimum (0.156)
# and maximum (1) values.
dimension_random_values <- runif(n = length(unique_dimensions), min = 0.156, max = 1)

# Assign the unique dimension names to the 'dimension_random_values' vector.
# This creates a named vector where each dimension name is associated with
# a specific random number. This makes it easy to look up the random number
# corresponding to each dimension.
names(dimension_random_values) <- unique_dimensions

# Create a new column named 'dimension_score' in the 'project_dynamics_score'
# data frame. The values for this new column are assigned by looking up the
# 'dimension' value in each row and retrieving the corresponding random number
# from the 'dimension_random_values' named vector. This ensures that all rows
# with the same dimension get the same randomly generated score.
project_dynamics_score$dimension_score <- dimension_random_values[project_dynamics_score$dimension]

project_dynamics_score <- project_dynamics_score |>
  group_by(domain) |>
  mutate(domain_score = geometric.mean(dimension_score)) |>
  ungroup()

project_domain_scores <- project_dynamics_score |>
  distinct(domain, .keep_all = TRUE)
project_domain_scores$id <- seq(1, nrow(project_domain_scores))

# Calculate the overall score (e.g., geometric mean of domain scores)
# Use the previously created 'project_domain_scores' which has one row per domain
overall_dynamics_score <- geometric.mean(project_domain_scores$domain_score)

# Round the numerical value to 2 decimal places
overall_dynamics_score <- round(overall_dynamics_score, digits = 2)
overall_dynamics_label <- bquote(S[d] == .(overall_dynamics_score))

project_dynamics_score$dimension <- factor(project_dynamics_score$dimension,
  levels = unique(project_dynamics_score$dimension),
  ordered = TRUE
)

project_dynamics_visualization <- ggplot(
  data = project_dynamics_score,
  aes(
    x = dimension,
    y = domain_score,
    fill = color_fill
  )
) +
  geom_bar(stat = "identity", width = 1) +
  scale_y_continuous(
    limits = c(0, 1.2),
    expand = expansion(mult = c(0, 0.2)),
    breaks = c(0, 0.25, 0.5, 0.75, 1)
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_fill_identity() +
  geom_segment(
    x = 0.5, y = 0, xend = 0.5, yend = 1,
    color = "#E0E0E0", linewidth = 0.25
  ) +
  geom_segment(
    x = 20.5, y = 0, xend = 20.5, yend = 1,
    color = "#E0E0E0", linewidth = 0.25
  ) +
  geom_segment(
    data = project_dynamics_score, aes(
      x = dimension,
      y = 0,
      xend = dimension,
      yend = dimension_score,
      color = color_stroke
    ),
    linewidth = 1
  ) +
  geom_point(data = project_dynamics_score, aes(
    x = dimension, y = dimension_score, color = color_stroke
  ), size = 3) +
  geom_segment(y = 0, x = 0.5, yend = 0, xend = 4.5, color = "#4E342E", linewidth = 1) +
  geom_segment(y = 0, x = 4.5, yend = 0, xend = 8.5, color = "#3B6B35", linewidth = 1) +
  geom_segment(y = 0, x = 8.5, yend = 0, xend = 12.5, color = "#3F5E78", linewidth = 1) +
  geom_segment(y = 0, x = 12.5, yend = 0, xend = 16.5, color = "#990000", linewidth = 1) +
  geom_segment(y = 0, x = 16.5, yend = 0, xend = 20.5, color = "#A64B42", linewidth = 1) +
  geom_text(
    data = project_dynamics_score, aes(
      y = domain_score[1] + 0.2, color = color_stroke[1]
    ),
    family = "xetbook", fontface = "italic",
    x = 2.5, label = "Contexts", size = 14
  ) +
  geom_text(
    data = project_dynamics_score, aes(
      y = domain_score[5] + 0.2, color = color_stroke[5]
    ),
    family = "xetbook", fontface = "italic",
    x = 6.5, label = "Partnerships", size = 14
  ) +
  geom_text(
    data = project_dynamics_score, aes(
      y = domain_score[9] + 0.2, color = color_stroke[9]
    ),
    family = "xetbook", fontface = "italic",
    x = 10.5, label = "Research", size = 14
  ) +
  geom_text(
    data = project_dynamics_score, aes(
      y = domain_score[13] + 0.2, color = color_stroke[13]
    ),
    family = "xetbook", fontface = "italic",
    x = 14.5, label = "Learning", size = 14
  ) +
  geom_text(
    data = project_dynamics_score, aes(
      y = domain_score[17] + 0.2, color = color_stroke[17]
    ),
    family = "xetbook", fontface = "italic",
    x = 18.5, label = "Outcomes", size = 14
  ) +
  scale_color_identity() +
  coord_radial(
    inner.radius = 0.18, r.axis.inside = TRUE, rotate.angle = FALSE,
    expand = FALSE, end = 1.75 * pi, clip = "off"
  ) +
  guides(
    theta = guide_axis_theta(angle = 90),
    r     = guide_axis(angle = 0)
  ) +
  ggtitle("Project Dynamics") +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.title = element_text(size = 54, face = "bold.italic", color = "#4A4A4A", hjust = 0.5),
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 24, family = "puritan", face = "italic", color = "#4A4A4A"),
    axis.text.x = element_text(size = 28, face = "italic", color = "#4A4A4A"),
    panel.grid.major = element_line(linewidth = 0.5, color = "#E0E0E0"),
    panel.grid.major.x = element_line(linetype = "dashed"),
    text = element_text(family = "xetbook")
  )

 final_dynamics_visualization <- ggdraw(project_dynamics_visualization) +
   draw_grob(
     textGrob(
       label = overall_dynamics_label,
       x = 0.5, y = 0.5,
       hjust = 0.5, vjust = 0.5,
       gp = gpar(fontsize = 48, fontface = "bold", col = "#4A4A4A", fontfamily = "xetbook")
     )
   )

ggsave("project_dynamics.png", final_dynamics_visualization, units = "in", dpi = 300,
       device = ragg::agg_png, width = 9, height = 9)

# Project Alignment
set.seed(Sys.time())
project_alignment_score <- data.frame(
  alignment = c(
    rep("Goals", 8),
    rep("Values", 8),
    rep("Roles", 8),
    rep("Resources", 8),
    rep("Activities", 8),
    rep("Empowerment", 8),
    rep("Outputs", 8),
    rep("Outcomes", 8)
  ),
  role = rep(rep(c("researcher", "partner"), times = c(3, 5)), times = 8),
  rating = runif(64, 0.36, 1),
  color_bead = c(
    rep(color_palette[1], 8),
    rep(color_palette[2], 8),
    rep(color_palette[3], 8),
    rep(color_palette[4], 8),
    rep(color_palette[5], 8),
    rep(color_palette[6], 8),
    rep(color_palette[7], 8),
    rep(color_palette[8], 8)
  )
)

researcher_median_frame <- project_alignment_score |>
  filter(role == "researcher") |>
  group_by(alignment) |>
  summarize(int_median = round(interp.median(rating, w = 1), 2), .groups = "drop")

partner_median_frame <- project_alignment_score |>
  filter(role == "partner") |>
  group_by(alignment) |>
  summarize(int_median = round(interp.median(rating, w = 1), 2), .groups = "drop")

# Join medians and calculate the geometric mean median
median_frame <- researcher_median_frame |>
  left_join(partner_median_frame, by = "alignment", suffix = c(".researcher", ".partner")) |>
  group_by(alignment) |>
  mutate(overall = round(geometric.mean(c(int_median.researcher, int_median.partner), na.rm = TRUE), 2)) |>
  ungroup() |>
  left_join(distinct(select(project_alignment_score, alignment, color_bead)), by = "alignment") |>
  pivot_longer(
    cols = !c(alignment, color_bead),
    names_to = "group",
    values_to = "value"
  ) |>
  mutate(group = case_when(group == "int_median.researcher" ~ "Researchers",
                           group == "int_median.partner" ~ "Partners",
                           .default = "Overall")) |>
  mutate(group = factor(group, levels = c("Researchers", "Partners", "Overall")))

overall_alignment_frame <- median_frame |>
  filter(group == "Overall")

overall_alignment_score <- round(geometric.mean(overall_alignment_frame$value), digits = 2)

overall_alignment_label <- bquote(S[a] == .(overall_alignment_score))


# First, let's determine the first and last group levels
first_group <- levels(median_frame$group)[1]
last_group <- levels(median_frame$group)[length(levels(median_frame$group))]

# Modified plot with values instead of points
project_alignment_visualization <- ggplot(data = median_frame, aes(x = group, y = value, group = alignment)) +
  # Add lines
  geom_line(aes(color = alignment), linewidth = 0.5) +
  scale_x_discrete(position = "top") +
  geom_point(aes(color = alignment)) +
  geom_text(aes(label = value, color = alignment), 
            size = 10,
            nudge_y = 0.015,
            family = "puritan", 
            fontface = "bold") + 
  # Left side labels
  geom_text_repel(data = median_frame %>% filter(group == first_group), 
                  aes(label = alignment, color = alignment), 
                  family = "xetbook", 
                  fontface = "italic", 
                  size = 14,
                  nudge_x = -0.3,
                  direction = "y",
                  hjust = 1,
                  segment.size = 0.25,
                  segment.color = "#4A4A4A",
                  box.padding = 0.5) +
  # Right side labels
  geom_text_repel(data = median_frame %>% filter(group == last_group), 
                  aes(label = alignment, color = alignment), 
                  family = "xetbook", 
                  fontface = "italic", 
                  size = 14,
                  nudge_x = 0.3,
                  direction = "y",
                  hjust = 0,
                  segment.size = 0.25,
                  segment.color = "#4A4A4A",
                  box.padding = 0.5) +
  scale_color_manual(values = color_palette) +
  ggtitle("Project Alignment") +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = margin(30, 30, 30, 30),
    plot.title = element_text(size = 54, face = "bold.italic", color = "#4A4A4A", hjust = 0.5),
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 24, family = "puritan", face = "italic", color = "#4A4A4A"),
    axis.text.x = element_text(size = 48, face = "italic", color = "#4A4A4A"),
    panel.grid.major.y = element_line(linewidth = 0.25, color = "#E0E0E0"),
    panel.grid.major.x = element_blank(),
    text = element_text(family = "xetbook")
  )

final_alignment_visualization <- ggdraw(project_alignment_visualization) +
  draw_grob(
    grobTree(
      roundrectGrob(
        x = 0.85, y = 0.9, 
        width = 0.2, height = 0.1,
        r = unit(0.05, "snpc"),
        gp = gpar(fill = "#F0F0F0", col = "#4A4A4A", lwd = 1.5)
      ),
      textGrob(
        label = overall_alignment_label,
        x = 0.9, y = 0.9,
        hjust = 0.5, vjust = 0.5,
        gp = gpar(fontsize = 48, fontface = "bold", col = "#4A4A4A", fontfamily = "xetbook")
      )
    ),
    width = 0.25, height = 0.15,
    x = 0.85, y = 0.9
  )


final_alignment_visualization <- ggdraw(project_alignment_visualization) +
  draw_grob(
    textGrob(
      label = overall_alignment_label,
      x = 0.8, y = 0.92,
      hjust = 0.5, vjust = 0.5,
      gp = gpar(fontsize = 48, fontface = "bold", col = "#4A4A4A", fontfamily = "xetbook")
    )
  )

ggsave("project_alignment.png", final_alignment_visualization, units = "in", dpi = 300,
       device = ragg::agg_png, width = 16, height = 9)

