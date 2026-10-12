# Reproduce manuscript figures from exported Source Data
# R >= 4.1.0. Save this script beside the source_data/ directory.
#
# Install once (the script never installs packages automatically):
# install.packages(c("readxl", "dplyr", "tidyr", "tibble", "ggplot2",
#   "patchwork", "scales", "gridExtra", "cowplot", "ggrepel", "sf",
#   "rnaturalearth", "rnaturalearthdata"))
#
# Run from the command line:
# Rscript plot_all_figures.R
# Rscript plot_all_figures.R --figures=Fig1,Fig4,EDFig1,EDFig4
# Rscript plot_all_figures.R --data-dir=source_data --output-dir=figures
# Rscript plot_all_figures.R --allow-incomplete
#
# Or in R / RStudio:
# source("plot_all_figures.R")
# run_figures(root = ".")
#
# All statistical estimates, confidence intervals, density coordinates and
# medians are read from Excel. No regression, matching or DID is refitted.
# Only display transformations, factor ordering and decorative geometry remain.
# Existing output files are never overwritten: use a new output directory.
# Incomplete figures are not exported unless --allow-incomplete is explicit.
# Preview exports are named *_INCOMPLETE.pdf and carry a visible warning.
# Read plotting_validation.csv after every run. No missing values are invented.
#
# Supplementary plot inputs needed for an exact recreation of all panels:
# 2b_probability.xlsx: hierarchy, total_pct (one row per doctoral rank group).
# 2e.xlsx / Fig2e_left: correct gender labels, one row per gender and x value.
# 3de_slope_difference.xlsx: outcome (Publications or JIF), estimate,
#   conf_low, conf_high (one row per outcome; includes covariance-based CIs).
# ED2c_baseline.xlsx: pathway, baseline (adjusted overall percentage).
# ED3a.xlsx and ED3b.xlsx / display: display_max (original 99.5th-percentile
#   upper display coordinate on the log(1 + outcome) scale).
# ED5a.xlsx: label, unmatched, matched (absolute standardized mean differences).
# These inputs must be exported from the original analyses, not reconstructed
# from PDF positions or substituted with estimates from a different model.




check_packages <- function(figures) {
  required <- c("readxl", "dplyr", "tidyr", "tibble", "ggplot2", "patchwork",
                "scales", "gridExtra", "cowplot", "ggrepel")
  if ("Fig1" %in% figures) required <- c(required, "sf", "rnaturalearth", "rnaturalearthdata")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "),
    ". Install them explicitly with install.packages().", call. = FALSE)
  invisible(lapply(required, function(p) suppressPackageStartupMessages(
    library(p, character.only = TRUE))))
  if (packageVersion("ggplot2") < "3.4.0" || packageVersion("patchwork") < "1.2.0")
    stop("Use ggplot2 >= 3.4.0 and patchwork >= 1.2.0.", call. = FALSE)
}

read_source <- function(source_dir, file, sheet = 1, columns = NULL) {
  path <- file.path(source_dir, file)
  if (!file.exists(path)) stop("Source Data missing: ", file, call. = FALSE)
  data <- readxl::read_excel(path, sheet = sheet, .name_repair = "check_unique")
  absent <- setdiff(columns, names(data))
  if (length(absent)) stop(file, " / ", sheet, ": missing columns ",
                          paste(absent, collapse = ", "), call. = FALSE)
  if (!nrow(data)) stop(file, " / ", sheet, ": empty worksheet.", call. = FALSE)
  data
}

assert_ci <- function(d, estimate = "estimate", low = "conf_low", high = "conf_high") {
  stopifnot(all(c(estimate, low, high) %in% names(d)))
  x <- d[[estimate]]; l <- d[[low]]; h <- d[[high]]
  if (any(!is.finite(x) | !is.finite(l) | !is.finite(h) | l > x | h < x))
    stop("Invalid estimate / confidence interval in Source Data.", call. = FALSE)
  d
}

gender_display <- function(x) {
  out <- dplyr::recode(as.character(x), Female = "Women", Male = "Men")
  if (anyNA(out) || any(!out %in% c("Women", "Men")))
    stop("Unknown gender label in Source Data.", call. = FALSE)
  factor(out, levels = c("Women", "Men"))
}

route_levels <- c("Direct self-hire", "Internal retention", "External return")
hierarchy_levels <- c("Top 100", "101–500", "501–1,500", "1,501–2,500", "2,501–3,500", ">3,500")
pathway_levels <- c("Direct self-hiring", "Internal pre-entry pathway", "External pre-entry pathway")

check_levels <- function(x, levels, label) {
  if (anyNA(x) || any(!as.character(x) %in% levels))
    stop("Unknown category in ", label, ".", call. = FALSE)
  factor(x, levels = levels)
}

missing_panel <- function(title, text) {
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = text, size = 3.1,
                      colour = "#8B1A1A", lineheight = 1.15) +
    xlim(0,1) + ylim(0,1) + labs(title = title) + theme_void() +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0))
}






##########################
# Figure 1

make_fig1 <- function(source_dir) {
  issues <- character()
  issue <- function(x) { issues <<- c(issues, x); invisible(NULL) }
  read_sd <- function(file, sheet = 1, columns = NULL) read_source(source_dir, file, sheet, columns)

COL_F <- "#d62728"
COL_M <- "#1f77b4"
COL_GREY <- "#6B7280"
COL_LIGHT <- "#E6E6E6"
COL_DIFF <- "#4C9A9A"
COHORT_SHORT <- c("1990–94","1995–99","2000–04","2005–09","2010–14","2015–19","2020–24")
theme_fig <- theme_classic(base_size = 9, base_family = "Arial") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    axis.title = element_text(size = 9, colour = "black"),
    axis.text = element_text(size = 8.5, colour = "black"),
    axis.line = element_line(linewidth = 0.55, colour = "black"),
    axis.ticks = element_line(linewidth = 0.55, colour = "black"),
    panel.grid = element_blank(),
    plot.margin = margin(6, 8, 6, 6)
  )



# a. Read observed / adjusted probabilities before plotting.
a_segments <- read_sd("1a.xlsx", columns=c("type","y","Male","Female","diff_pp"))
a_points <- a_segments %>% pivot_longer(c(Male,Female), names_to="gender_final", values_to="estimate")
if(any(a_points$estimate < 0 | a_points$estimate > 1)) stop("Fig1a probabilities must be in [0,1].")
raw_diff <- a_segments$diff_pp[a_segments$type=="Observed"]
raw_mid <- a_segments %>% filter(type=="Observed") %>% summarise(x=mean(c(Male,Female))*100) %>% pull(x)
adj_mid <- a_segments %>% filter(type=="Adjusted") %>% summarise(x=mean(c(Male,Female))*100) %>% pull(x)
# The same primary contrast is exported in ED1a; its CI is rounded to 2 decimals.
rd_a <- read_sd("ED1a.xlsx", columns=c("primary","estimate_pp","conf_low_pp","conf_high_pp")) %>%
  filter(primary) %>% rename(conf.low_pp=conf_low_pp,conf.high_pp=conf_high_pp)
stopifnot(nrow(rd_a)==1L, abs(rd_a$estimate_pp-a_segments$diff_pp[a_segments$type=="Adjusted"]) < .01)



P_1A <- ggplot() +
  geom_segment(
    data = a_segments %>% filter(type == "Observed"),
    aes(x = Male * 100, xend = Female * 100, y = y, yend = y),
    colour = "grey55",
    linewidth = 0.65
  ) +
  geom_segment(
    data = a_segments %>% filter(type == "Adjusted"),
    aes(x = Male * 100, xend = Female * 100, y = y, yend = y),
    colour = "grey45",
    linewidth = 0.85
  ) +
  geom_point(
    data = a_points,
    aes(x = estimate * 100, y = y, colour = gender_final),
    size = 2.8
  ) +
  geom_text(
    data = a_points %>% filter(type == "Observed"),
    aes(
      x = estimate * 100,
      y = y + 0.17,
      colour = gender_final,
      label = sprintf("%.2f%%", estimate * 100)
    ),
    size = 3.0,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_text(
    data = a_points %>% filter(type == "Adjusted"),
    aes(
      x = estimate * 100,
      y = y + 0.16,
      colour = gender_final,
      label = sprintf("%.2f%%", estimate * 100)
    ),
    size = 3.0,
    fontface = "bold",
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = raw_mid,
    y = 1.83,
    label = sprintf("%+.2f pp", raw_diff),
    size = 3.0,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = adj_mid,
    y = 0.80,
    label = sprintf(
      "Women − men difference = %+.2f pp\n95%% CI [%.2f, %.2f]",
      rd_a$estimate_pp, rd_a$conf.low_pp, rd_a$conf.high_pp
    ),
    size = 3.0,
    fontface = "bold"
  ) +
  scale_colour_manual(
    values = c("Female" = COL_F, "Male" = COL_M),
    breaks = c("Female", "Male"),
    labels = c("Women", "Men"),
    name = NULL,
    guide = guide_legend(
      override.aes = list(size = 3),
      nrow = 1
    )
  ) +
  scale_x_continuous(
    limits = c(18, 21.5),
    breaks = seq(18, 21.5, 0.5),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0.66, 2.34),
    breaks = c(1, 2),
    labels = c(
      "Country- and domain-\nstandardized",
      "Observed self-hiring rate"
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "a",
    x = "Doctoral self-hiring (%)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_fig +
  theme(
    text = element_text(family = "sans"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(
      size = 9.5,
      face = "bold",
      margin = margin(r = 6)
    ),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "center",
    legend.text = element_text(size = 9.5),
    legend.key.size = unit(0.35, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    legend.margin = margin(t = 3, b = 0)
  )






# b. Read domain contrasts; the original plot object is named P_1C.
domain_rd <- read_sd("1b.xlsx")
assert_ci(domain_rd,"estimate_pp","conf.low_pp","conf.high_pp")

P_1C <- ggplot(domain_rd) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    colour = "grey65"
  ) +
  geom_errorbarh(
    aes(
      y = y,
      xmin = conf.low_pp,
      xmax = conf.high_pp
    ),
    height = 0.13,
    linewidth = 0.5,
    colour = "#4C9A9A"
  ) +
  geom_point(
    aes(
      x = estimate_pp,
      y = y
    ),
    size = 2.5,
    colour = "#4C9A9A"
  ) +
  geom_text(
    aes(
      x = estimate_pp + if_else(estimate_pp >= 0, 0.35, -0.35),
      y = y + 0.22,
      label = sprintf("%+.2f", estimate_pp),
      hjust = if_else(estimate_pp >= 0, 0, 1)
    ),
    size = 2.9,
    colour = "#4C9A9A",
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = -0.35,
    xend = -3.3,
    y = 6.62,
    yend = 6.62,
    colour = "black",
    linewidth = 0.65,
    arrow = arrow(length = unit(0.09, "inches"))
  ) +
  annotate(
    "segment",
    x = 0.35,
    xend = 3.3,
    y = 6.62,
    yend = 6.62,
    colour = "black",
    linewidth = 0.65,
    arrow = arrow(length = unit(0.09, "inches"))
  ) +
  annotate(
    "text",
    x = -3,
    y = 7,
    label = "Men higher",
    colour = "black",
    fontface = "bold",
    size = 3.0
  ) +
  annotate(
    "text",
    x = 4,
    y = 7,
    label = "Women higher",
    colour = "black",
    fontface = "bold",
    size = 3.0
  ) +
  scale_x_continuous(
    limits = c(-6, 6.3),
    breaks = seq(-6, 6, 2),
    labels = function(x) ifelse(
      x > 0,
      paste0("+", x),
      as.character(x)
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0.5, 7.0),
    breaks = domain_rd$y,
    labels = domain_rd$current_domain
  ) +
  labs(
    title = "b",
    x = "Women − men difference in doctoral self-hiring (pp)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_fig +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(
      size = 8.5,
      margin = margin(r = 5)
    )
  )





# c. Read faculty-entry cohort contrasts.
cohort_rd <- read_sd("1c.xlsx")
assert_ci(cohort_rd,"estimate_pp","conf.low_pp","conf.high_pp")

P_1D <- ggplot(
  cohort_rd,
  aes(x = x, y = estimate_pp)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    colour = "grey65"
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low_pp,
      ymax = conf.high_pp
    ),
    width = 0.12,
    linewidth = 0.55,
    colour = "#4C9A9A"
  ) +
  geom_line(
    colour = "#4C9A9A",
    linewidth = 0.85
  ) +
  geom_point(
    colour = "#4C9A9A",
    size = 2.5
  ) +
  geom_text(
    aes(label = sprintf("%.2f", estimate_pp)),
    nudge_x = 0.3,
    nudge_y = -0.3,
    colour = "#4C9A9A",
    size = 2.9,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = 1:7,
    labels = COHORT_SHORT,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(
    breaks = seq(0, 5, 1),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = "c",
    x = "Faculty-entry cohort",
    y = "Women − men difference in\ndoctoral self-hiring (pp)"
  ) +
  coord_cartesian(
    ylim = c(-0.25, 4.85),
    clip = "off"
  ) +
  theme_fig +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1,
      vjust = 1
    )
  )






# d. Read country estimates. Geographic polygons are packaged map data.
country_rd <- read_sd("1d.xlsx", "Fig1d") %>% rename(
 first_faculty_country=country,estimate_pp=gender_difference_pp,
 conf.low_pp=ci_lower_pp,conf.high_pp=ci_upper_pp) %>%
 mutate(direction=if_else(estimate_pp>=0,"Women higher","Men higher"))
assert_ci(country_rd,"estimate_pp","conf.low_pp","conf.high_pp")
world <- rnaturalearth::ne_countries(scale="medium",returnclass="sf") %>%
 mutate(iso3=if_else(iso_a3=="-99",adm0_a3,iso_a3))
map_fig1b <- left_join(world,country_rd,by="iso3")
n_supported <- nrow(country_rd)
n_positive <- sum(country_rd$estimate_pp>0)
median_rd <- median(country_rd$estimate_pp)

MAP_NEG_DARK  <- "#4A2C7A"
MAP_NEG_MID   <- "#8C6BB1"
MAP_NEG_LIGHT <- "#D6CDE5"
MAP_ZERO <- "#FFFFFF"
MAP_POS_LIGHT <- "#C7E3D6"
MAP_POS_MID   <- "#4F9E7C"
MAP_POS_DARK  <- "#005C42"
MAP_NA <- "#AFAFAF"
P_1B_map <- ggplot(map_fig1b) +
  geom_sf(
    aes(fill = estimate_pp),
    colour = "white",
    linewidth = 0.12
  ) +
  scale_fill_gradientn(
    colours = c(
      MAP_NEG_DARK,
      MAP_NEG_MID,
      MAP_NEG_LIGHT,
      MAP_ZERO,
      MAP_POS_LIGHT,
      MAP_POS_MID,
      MAP_POS_DARK
    ),
    values = scales::rescale(
      c(-15, -5, -1.5, 0, 1.5, 5, 15),
      from = c(-15, 15)
    ),
    limits = c(-15, 15),
    breaks = c(-15, -10, -5, 0, 5, 10, 15),
    labels = c("≤−15", "−10", "−5", "0", "+5", "+10", "≥+15"),
    oob = scales::squish,
    na.value = MAP_NA,
    name = "Women − men\ndifference (pp)"
  ) +
  coord_sf(
    crs = "+proj=robin",
    default_crs = st_crs(4326),
    expand = FALSE
  ) +
  labs(title = "d") +
  theme_void(base_size = 9) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "right",
    legend.title = element_text(size = 8.5),
    legend.text = element_text(size = 8),
    legend.key.height = unit(18, "mm"),
    legend.key.width = unit(3.2, "mm"),
    plot.margin = margin(6, 6, 0, 6)
  )

set.seed(1)
country_rd_plot <- country_rd %>%
  mutate(plot_y = runif(n(), -0.055, 0.055))
focus_countries <- c("China", "United States", "Japan", "United Kingdom")
label_df <- country_rd_plot %>%
  filter(first_faculty_country %in% focus_countries | estimate_pp > 30 | estimate_pp < -5) %>%
  distinct(first_faculty_country, .keep_all = TRUE) %>%
  mutate(
    label_name = case_when(
      TRUE ~ first_faculty_country
    ),
    label_nudge = if_else(estimate_pp >= 0, 0.11, -0.11)
  )
P_1B_inset <- ggplot(country_rd_plot, aes(x = estimate_pp, y = plot_y)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, colour = "grey55") +
  geom_point(
    data = country_rd_plot %>% filter(ci_excludes_zero),
    aes(colour = direction),
    size = 1.8
  ) +
  geom_point(
    data = country_rd_plot %>% filter(!ci_excludes_zero),
    aes(colour = direction),
    shape = 21,
    fill = "white",
    stroke = 0.7,
    size = 1.8
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = label_name, colour = direction),
    nudge_y = label_df$label_nudge,
    size = 2.7,
    fontface = "bold",
    box.padding = 0.40,
    point.padding = 0.25,
    force = 1.8,
    force_pull = 0.1,
    min.segment.length = 0,
    segment.color = "grey40",
    segment.size = 0.35,
    seed = 1,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("Men higher" = MAP_NEG_MID, "Women higher" = MAP_POS_MID),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(-15, 35),
    breaks = c(-10, 0, 10, 20, 30),
    labels = function(x) ifelse(x > 0, paste0("+", x), as.character(x)),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(limits = c(-0.18, 0.18), expand = c(0, 0)) +
  labs(x = "Women − men difference (pp)", y = NULL) +
  theme_classic(base_size = 8.5) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(size = 7.8, margin = margin(t = 2)),
    axis.title.x = element_text(size = 8.3, margin = margin(t = 3)),
    axis.line.x = element_line(linewidth = 0.5),
    axis.ticks.x = element_line(linewidth = 0.45),
    plot.margin = margin(t = 2, r = 8, b = 2, l = 4)
  )

P_1B_summary <- textGrob(
  sprintf(
    "Positive point estimates: %d/%d systems\nMedian difference: %+.2f pp\nSolid/open: 95%% CI excludes/includes zero",
    n_positive, n_supported, median_rd
  ),
  x = unit(0.05, "npc"),
  y = unit(0.55, "npc"),
  hjust = 0,
  vjust = 0.5,
  gp = gpar(fontsize = 8.5, col = "black", lineheight = 1.25)
)
legend_b <- cowplot::get_legend(
  P_1B_map +
    theme(
      legend.position = "right",
      legend.box.margin = margin(0, 0, 0, 0)
    )
)
P_1B_map_noleg <- P_1B_map +
  theme(legend.position = "none")
NA_KEY <- gridExtra::arrangeGrob(
  grid::rectGrob(gp=grid::gpar(fill=MAP_NA,col=NA),
                 width=grid::unit(3.5,"mm"),height=grid::unit(3.5,"mm")),
  grid::textGrob("No estimate",x=grid::unit(0,"npc"),hjust=0,
                 gp=grid::gpar(fontsize=7.5,col="grey25")),
  ncol=2,widths=grid::unit.c(grid::unit(0.55,"cm"),grid::unit(1.75,"cm"))
)
legend_b_full <- gridExtra::arrangeGrob(legend_b,NA_KEY,ncol=1,heights=c(6,0.7))
P_1B_bottom <- arrangeGrob(
  nullGrob(),
  P_1B_summary,
  P_1B_inset,
  ncol = 3,
  widths = c(0.30, 0.70, 2.00)
)
P_1B_all <- arrangeGrob(
  P_1B_map_noleg,
  legend_b_full,
  P_1B_bottom,
  nullGrob(),
  layout_matrix = rbind(
    c(1, 2),
    c(3, 4)
  ),
  widths = c(1, 0.12),
  heights = c(4, 1.3)
)
FIG1_top <- arrangeGrob(
  P_1A,
  nullGrob(),
  P_1C,
  nullGrob(),
  P_1D,
  ncol = 5,
  widths = c(1.2, 0.08, 1, 0.08, 1)
)
FIG1 <- arrangeGrob(
  FIG1_top,
  P_1B_all,
  ncol = 1,
  heights = c(0.45, 0.8)
)

list(plot = FIG1, width = 30, height = 24, units = "cm", issues = unique(issues))
}













###########################
# Figure 2

make_fig2 <- function(source_dir) {
  issues <- character()
  issue <- function(x) { issues <<- c(issues, x); invisible(NULL) }
  read_sd <- function(file, sheet = 1, columns = NULL) read_source(source_dir, file, sheet, columns)

COL_FEMALE <- "#D62728"; COL_MALE <- "#1F77B4"; COL_OVERALL <- "#7F7F7F"
PATH_LEVELS <- pathway_levels; HIER_LEVELS <- hierarchy_levels
STACK_LEVELS <- c(PATH_LEVELS,"Institution-unresolved pre-entry pathway")
PATH_COLORS <- c("Direct self-hiring"="#45B5AA","Internal pre-entry pathway"="#F2A65A",
 "External pre-entry pathway"="#9B77D5","Institution-unresolved pre-entry pathway"="#999999")








# a. Read pathway shares and Wilson intervals.
a_plot <- read_sd("2a.xlsx")
assert_ci(a_plot,"pct","low","high")
a_xmax <- ceiling((max(a_plot$high)+6)/10)*10



P_2A <- ggplot(a_plot) +
  geom_segment(
    aes(x=low,xend=high,y=y,yend=y,colour=group),
    linewidth=0.7
  ) +
  geom_point(
    aes(x=pct,y=y,colour=group),
    size=2.5
  ) +
  geom_text(
    aes(x=high+1.1,y=y,label=sprintf("%.1f",pct),colour=group),
    size=3.5,
    hjust=0,
    show.legend=FALSE
  ) +
  scale_colour_manual(
    values=c(
      "Female"=COL_FEMALE,
      "Male"=COL_MALE,
      "Overall"=COL_OVERALL
    )
  ) +
  scale_y_continuous(
    breaks=c(3,2,1),
    labels=c(
      "Direct self-hiring\n(A → A)",
      "Internal pre-entry pathway\n(A → A → A)",
      "External pre-entry pathway\n(A → B → A)"
    ),
    limits=c(0.55,3.45)
  ) +
  scale_x_continuous(
    limits=c(0,a_xmax),
    expand=expansion(mult=c(0,0.02))
  ) +
  labs(
    title="a",
    x="Share among pathway-identifiable self-hires (%)",
    y=NULL
  ) +
  theme_classic(base_size=10) +
  theme(
    axis.title=element_text(size=10),
    axis.text=element_text(size=10,colour="black"),
    axis.line=element_line(linewidth=0.35),
    axis.ticks=element_line(linewidth=0.35),
    axis.ticks.y=element_blank(),
    axis.line.y=element_blank(),
    legend.position="top",
    legend.title=element_blank(),
    legend.text=element_text(size=10),
    plot.title=element_text(size=13,face="bold",hjust=0),
    plot.title.position="plot"
  )






# b. Read exported stacked-bar geometry, then the probability table.
b_comp_plot <- read_sd("2b.xlsx")
if(file.exists(file.path(source_dir,"2b_probability.xlsx"))) {
 b_total <- read_sd("2b_probability.xlsx",columns=c("hierarchy","total_pct")) %>%
  mutate(hierarchy=check_levels(hierarchy,HIER_LEVELS,"Fig2b"),x=as.integer(hierarchy)) %>% arrange(x)
 stopifnot(nrow(b_total)==6L,!anyDuplicated(b_total$hierarchy),all(is.finite(b_total$total_pct)),
           all(b_total$total_pct>=0 & b_total$total_pct<=100))
} else {
 issue("Fig2b: 2b_probability.xlsx missing; 2b.xlsx has composition only, without all-entrant denominators.")
 b_total <- tibble(x=integer(),total_pct=double())
}
COMP_BOTTOM <- 0; COMP_TOP <- 45; SELF_BOTTOM <- 52; SELF_TOP <- 79; PLOT_TOP <- 87
SELF_MAX <- max(40,ceiling((max(c(0,b_total$total_pct))+5)/10)*10)

b_total_plot <- b_total %>%
  mutate(
    y=SELF_BOTTOM+
      total_pct/SELF_MAX*(SELF_TOP-SELF_BOTTOM)
  )
comp_ticks <- tibble(value=c(0,50,100)) %>%
  mutate(
    y=COMP_BOTTOM+
      value/100*(COMP_TOP-COMP_BOTTOM)
  )
self_ticks <- tibble(
  value=seq(0,SELF_MAX,by=10)
) %>%
  mutate(
    y=SELF_BOTTOM+
      value/SELF_MAX*(SELF_TOP-SELF_BOTTOM)
  )
HIER_LABELS <- c(
  "Top 100",
  "101–500",
  "501–\n1,500",
  "1,501–\n2,500",
  "2,501–\n3,500",
  ">3,500"
)
P_2B <- ggplot() +
  geom_rect(
    data=b_comp_plot,
    aes(
      xmin=xmin,xmax=xmax,
      ymin=ymin,ymax=ymax,
      fill=pathway
    ),
    colour="white",
    linewidth=0.45
  ) +
  geom_text(
    data=filter(b_comp_plot,label!=""),
    aes(
      x=x,y=ymid,
      label=label,
      colour=text_col
    ),
    size=3,
    fontface="plain",
    show.legend=FALSE
  ) +
  scale_colour_identity() +
  geom_line(
    data=b_total_plot,
    aes(x=x,y=y,group=1),
    colour="#303030",
    linewidth=0.75,
    lineend="round"
  ) +
  geom_point(
    data=b_total_plot,
    aes(x=x,y=y),
    shape=21,
    size=3,
    stroke=0.7,
    fill="white",
    colour="#303030"
  ) +
  geom_text(
    data=b_total_plot,
    aes(
      x=x,y=y,
      label=sprintf("%.1f",total_pct)
    ),
    nudge_y=4,
    size=3,
    colour="#303030"
  ) +
  annotate(
    "segment",
    x=0.60,xend=6.40,
    y=48.5,yend=48.5,
    linewidth=0.3,
    colour="#E0E0E0"
  ) +
  annotate(
    "segment",
    x=0.57,xend=0.57,
    y=COMP_BOTTOM,yend=COMP_TOP,
    linewidth=0.35,
    colour="#444444"
  ) +
  geom_segment(
    data=comp_ticks,
    aes(
      x=0.53,xend=0.57,
      y=y,yend=y
    ),
    linewidth=0.35,
    colour="#444444"
  ) +
  geom_text(
    data=comp_ticks,
    aes(x=0.48,y=y,label=value),
    hjust=1,
    size=3,
    colour="#444444"
  ) +
  annotate(
    "segment",
    x=0.57,xend=0.57,
    y=SELF_BOTTOM,yend=SELF_TOP,
    linewidth=0.35,
    colour="#444444"
  ) +
  geom_segment(
    data=self_ticks,
    aes(
      x=0.53,xend=0.57,
      y=y,yend=y
    ),
    linewidth=0.35,
    colour="#444444"
  ) +
  geom_text(
    data=self_ticks,
    aes(x=0.48,y=y,label=value),
    hjust=1,
    size=3,
    colour="#444444"
  ) +
  annotate(
    "text",
    x=0.78,
    y=SELF_TOP+5,
    label="Self-hire probability (%)",
    hjust=0,
    vjust=1,
    size=3,
    fontface="plain",
    colour="#303030"
  ) +
  annotate(
    "text",
    x=0.05,
    y=(COMP_BOTTOM+COMP_TOP)/2,
    label="Pathway composition\namong self-hires (%)",
    angle=90,
    size=3,
    lineheight=0.95,
    colour="#303030"
  ) +
  annotate(
    "segment",
    x=0.57,xend=6.42,
    y=0,yend=0,
    linewidth=0.35,
    colour="#444444"
  ) +
  scale_x_continuous(
    breaks=1:6,
    labels=HIER_LABELS,
    limits=c(0.05,6.43),
    expand=c(0,0)
  ) +
  scale_y_continuous(
    limits=c(0,PLOT_TOP),
    breaks=NULL,
    expand=c(0,0)
  ) +
  scale_fill_manual(
    values=PATH_COLORS,
    breaks=STACK_LEVELS,
    name=NULL
  ) +
  coord_cartesian(clip="off") +
  labs(
    title="b",
    x="Doctoral-institution hierarchy",
    y=NULL
  ) +
  guides(
    fill=guide_legend(
      nrow=2,
      byrow=TRUE,
      override.aes=list(colour=NA)
    )
  ) +
  theme_classic(base_size=9) +
  theme(
    axis.line=element_blank(),
    axis.ticks.x=element_line(
      colour="#444444",
      linewidth=0.35
    ),
    axis.ticks.y=element_blank(),
    axis.ticks.length=grid::unit(3,"pt"),
    axis.text.x=element_text(
      size=9,
      colour="#303030",
      lineheight=0.88,
      margin=margin(t=5)
    ),
    axis.text.y=element_blank(),
    axis.title.x=element_text(
      size=9,
      colour="#303030",
      margin=margin(t=8)
    ),
    axis.title.y=element_blank(),
    legend.position="bottom",
    legend.direction="horizontal",
    legend.justification="center",
    legend.box.just="center",
    legend.text=element_text(size=9),
    legend.key.height=grid::unit(7,"pt"),
    legend.key.width=grid::unit(11,"pt"),
    legend.spacing.x=grid::unit(7,"pt"),
    legend.spacing.y=grid::unit(2,"pt"),
    legend.margin=margin(t=5),
    plot.title=element_text(
      size=13,
      face="bold",
      hjust=0
    ),
    plot.title.position="plot",
    plot.margin=margin(t=5,r=10,b=3,l=16)
  )

if(!nrow(b_total)) P_2B <- P_2B + annotate("text",x=3.5,y=66,
 label="Self-hiring probability Source Data missing",colour="#8B1A1A",size=3)










# c. Read pathway enrichment; no enrichment statistic is recomputed.
c_plot <- read_sd("2c.xlsx") %>% mutate(hierarchy=check_levels(hierarchy,HIER_LEVELS,"Fig2c"),
 pathway=check_levels(pathway,rev(PATH_LEVELS),"Fig2c"))
c_limit <- max(.1,ceiling(max(abs(c_plot$log2_oe))*10)/10)
legend_df <- data.frame(x=seq(-c_limit,c_limit,length.out=400))
legend_step <- 2*c_limit/(nrow(legend_df)-1)
tick_df <- data.frame(x=c(-c_limit,-c_limit/2,0,c_limit/2,c_limit),
 label=sprintf("%.1f",c(-c_limit,-c_limit/2,0,c_limit/2,c_limit)))

P_2C_main <- ggplot(
  c_plot,
  aes(x=hierarchy,y=pathway,fill=log2_oe)
) +
  geom_tile(colour="white",linewidth=0.8) +
  geom_text(
    aes(label=sprintf("%.2f",log2_oe)),
    size=3,
    colour="#222222"
  ) +
  scale_fill_gradient2(
    low="#4A83D8",
    mid="white",
    high="#F26B62",
    midpoint=0,
    limits=c(-c_limit,c_limit),
    oob=squish
  ) +
  scale_x_discrete(labels=HIER_LABELS) +
  labs(
    title="c",
    x="Doctoral-institution hierarchy",
    y=NULL
  ) +
  theme_minimal(base_size=10) +
  theme(
    panel.grid=element_blank(),
    axis.text=element_text(size=10,colour="black"),
    axis.title.x=element_text(size=10),
    axis.ticks=element_blank(),
    legend.position="none",
    plot.title=element_text(size=13,face="bold",hjust=0),
    plot.title.position="plot",
    plot.margin=margin(5,7,0,5)
  )
legend_df <- data.frame(
  x=seq(-c_limit,c_limit,length.out=400)
)
legend_step <- 2*c_limit/(nrow(legend_df)-1)
tick_df <- data.frame(
  x=c(-c_limit,-c_limit/2,0,c_limit/2,c_limit),
  label=sprintf("%.1f",c(-c_limit,-c_limit/2,0,c_limit/2,c_limit))
)
P_2C_legend <- ggplot() +
  geom_tile(
    data=legend_df,
    aes(x=x,y=1,fill=x),
    width=legend_step*1.05,
    height=0.18
  ) +
  scale_fill_gradient2(
    low="#4A83D8",
    mid="white",
    high="#F26B62",
    midpoint=0,
    limits=c(-c_limit,c_limit),
    oob=squish
  ) +
  geom_segment(
    data=tick_df,
    aes(x=x,xend=x,y=0.90,yend=0.86),
    colour="#555555",
    linewidth=0.35
  ) +
  geom_text(
    data=tick_df,
    aes(x=x,y=0.80,label=label),
    size=3,
    colour="#333333"
  ) +
  annotate(
    "segment",
    x=-c_limit*1.10,
    xend=-c_limit*1.38,
    y=1,
    yend=1,
    colour="#2F6FD6",
    linewidth=0.65,
    arrow=arrow(length=unit(0.12,"cm"),type="closed")
  ) +
  annotate(
    "text",
    x=-c_limit*1.46,
    y=1,
    label="Below overall\npathway share",
    colour="#2F6FD6",
    size=3,
    hjust=1,
    lineheight=0.95
  ) +
  annotate(
    "segment",
    x=c_limit*1.10,
    xend=c_limit*1.38,
    y=1,
    yend=1,
    colour="#FF3344",
    linewidth=0.65,
    arrow=arrow(length=unit(0.12,"cm"),type="closed")
  ) +
  annotate(
    "text",
    x=c_limit*1.46,
    y=1,
    label="Above overall\npathway share",
    colour="#FF3344",
    size=3,
    hjust=0,
    lineheight=0.95
  ) +
  annotate(
    "text",
    x=0,
    y=0.68,
    label="log[2]*'(observed / expected share)'",
    parse=TRUE,
    size=3
  ) +
  scale_x_continuous(
    limits=c(-c_limit*2,c_limit*2),
    expand=c(0,0)
  ) +
  scale_y_continuous(
    limits=c(0.60,1.15),
    expand=c(0,0)
  ) +
  guides(fill="none") +
  coord_cartesian(clip="off") +
  theme_void() +
  theme(
    plot.margin=margin(0,10,0,10)
  )
P_2C_bottom <- (plot_spacer() | P_2C_legend | plot_spacer()) +
  plot_layout(widths=c(0,0.70,0.30))
P_2C <- P_2C_main / P_2C_bottom +
  plot_layout(heights=c(1,0.24))















# d. Read within-rank enrichment of women.
d_plot <- read_sd("2d.xlsx") %>% mutate(hierarchy=check_levels(hierarchy,HIER_LEVELS,"Fig2d"),
 pathway=check_levels(pathway,rev(PATH_LEVELS),"Fig2d"))
d_limit <- max(2,ceiling(max(abs(d_plot$female_enrichment_pp))))

P_2D_main <- ggplot(
  d_plot,
  aes(
    x=hierarchy,
    y=pathway,
    fill=female_enrichment_pp
  )
) +
  geom_tile(
    colour="white",
    linewidth=0.8
  ) +
  geom_text(
    aes(label=sprintf("%+.1f",female_enrichment_pp)),
    size=3,
    colour="#222222"
  ) +
  scale_fill_gradient2(
    low="#4A83D8",
    mid="white",
    high="#F26B62",
    midpoint=0,
    limits=c(-d_limit,d_limit),
    oob=squish
  ) +
  scale_x_discrete(labels=HIER_LABELS) +
  labs(
    title="d",
    x="Doctoral-institution hierarchy",
    y=NULL
  ) +
  theme_minimal(base_size=10) +
  theme(
    panel.grid=element_blank(),
    axis.text=element_text(size=10,colour="black"),
    axis.title.x=element_text(size=10),
    axis.ticks=element_blank(),
    legend.position="none",
    plot.title=element_text(size=13,face="bold",hjust=0),
    plot.title.position="plot",
    plot.margin=margin(5,7,0,5)
  )
legend_df_d <- data.frame(
  x=seq(-d_limit,d_limit,length.out=400)
)
legend_step_d <- 2*d_limit/(nrow(legend_df_d)-1)
tick_df_d <- data.frame(
  x=c(-d_limit,-d_limit/2,0,d_limit/2,d_limit),
  label=sprintf("%.1f",c(-d_limit,-d_limit/2,0,d_limit/2,d_limit))
)




P_2D_legend <- ggplot() +
  geom_tile(
    data=legend_df_d,
    aes(x=x,y=1,fill=x),
    width=legend_step_d*1.05,
    height=0.18
  ) +
  scale_fill_gradient2(
    low="#4A83D8",
    mid="white",
    high="#F26B62",
    midpoint=0,
    limits=c(-d_limit,d_limit),
    oob=squish
  ) +
  geom_segment(
    data=tick_df_d,
    aes(x=x,xend=x,y=0.90,yend=0.86),
    colour="#555555",
    linewidth=0.35
  ) +
  geom_text(
    data=tick_df_d,
    aes(x=x,y=0.80,label=label),
    size=3,
    colour="#333333"
  ) +
  annotate(
    "segment",
    x=-d_limit*1.10,
    xend=-d_limit*1.38,
    y=1,
    yend=1,
    colour="#2F6FD6",
    linewidth=0.65,
    arrow=arrow(length=unit(0.12,"cm"),type="closed")
  ) +
  annotate(
    "text",
    x=-d_limit*1.46,
    y=1,
    label="Below hierarchy\nfemale baseline",
    colour="#2F6FD6",
    size=3,
    hjust=1,
    lineheight=0.95
  ) +
  annotate(
    "segment",
    x=d_limit*1.10,
    xend=d_limit*1.38,
    y=1,
    yend=1,
    colour="#FF3344",
    linewidth=0.65,
    arrow=arrow(length=unit(0.12,"cm"),type="closed")
  ) +
  annotate(
    "text",
    x=d_limit*1.46,
    y=1,
    label="Above hierarchy\nfemale baseline",
    colour="#FF3344",
    size=3,
    hjust=0,
    lineheight=0.95
  ) +
  annotate(
    "text",
    x=0,
    y=0.68,
    label="Female enrichment (pp)",
    size=3,
    colour="#222222"
  ) +
  scale_x_continuous(
    limits=c(-d_limit*2,d_limit*2),
    expand=c(0,0)
  ) +
  scale_y_continuous(
    limits=c(0.60,1.15),
    expand=c(0,0)
  ) +
  guides(fill="none") +
  coord_cartesian(clip="off") +
  theme_void() +
  theme(
    plot.margin=margin(0,10,0,10)
  )
P_2D_bottom <- (plot_spacer() | P_2D_legend | plot_spacer()) +
  plot_layout(widths=c(0,0.70,0.30))
P_2D <- P_2D_main / P_2D_bottom +
  plot_layout(heights=c(1,0.24))













# e. Read precomputed density coordinates and category-share intervals.
density_df <- read_sd("2e.xlsx","Fig2e_left",c("gender","log_rank_movement","density"))
density_df$gender <- gender_display(density_df$gender)
density_ok <- setequal(as.character(unique(density_df$gender)),c("Women","Men")) &&
 !anyDuplicated(density_df[c("gender","log_rank_movement")]) &&
 all(is.finite(density_df$density)) && all(density_df$density>=0)
if(!density_ok) {
 issue("Fig2e density: gender labels do not identify two unique curves; re-export Fig2e_left.")
 P_2E_left <- missing_panel("External rank movement density",
  "Correct Women / Men curve labels
are required in Source Data")
} else {
 P_2E_left <- ggplot(density_df,aes(log_rank_movement,density,colour=gender,fill=gender,linetype=gender)) +
  geom_area(alpha=.08,position="identity",colour=NA) + geom_line(linewidth=.95) +
  geom_vline(xintercept=0,linetype="dashed",colour="grey55") +
  scale_colour_manual(values=c(Women=COL_FEMALE,Men=COL_MALE)) +
  scale_fill_manual(values=c(Women=COL_FEMALE,Men=COL_MALE),guide="none") +
  scale_linetype_manual(values=c(Women="solid",Men="dashed"),guide="none") +
  coord_cartesian(xlim=c(-2,2)) + labs(x="Rank change of external institution relative to doctoral institution (log10 scale)",
  y="Density",colour=NULL) + theme_classic(base_size=10) + theme(legend.position="bottom")
}
right_df <- read_sd("2e.xlsx","Fig2e_right") %>%
 rename(estimate=share_percent,conf_low=ci_lower,conf_high=ci_upper) %>%
 mutate(gender_final=if_else(as.character(gender_display(gender))=="Women","Female","Male"),
 movement_short=case_when(grepl("higher",movement)~"Higher-ranked",grepl("lower",movement)~"Lower-ranked",
 TRUE~"Similar-ranked"), y_base=match(movement_short,c("Lower-ranked","Similar-ranked","Higher-ranked")),
 y_plot=y_base+if_else(gender_final=="Female",.115,-.115))
assert_ci(right_df)
female_col <- COL_FEMALE; male_col <- COL_MALE
text_col <- "#183153"; axis_col <- "#505A66"; grid_col <- "#E6EBF0"
x_min <- floor(min(right_df$conf_low)/5)*5; x_max <- ceiling(max(right_df$conf_high)/5)*5
x_left <- max(0,x_min-2); x_right <- x_max+7

P_2E_right <- ggplot(
  right_df,
  aes(
    x=estimate,
    y=y_plot,
    colour=gender_final
  )
) +
  geom_hline(
    yintercept=c(1,2,3),
    colour=grid_col,
    linewidth=0.45
  ) +
  geom_errorbarh(
    aes(
      xmin=conf_low,
      xmax=conf_high
    ),
    height=0.10,
    linewidth=0.65
  ) +
  geom_point(
    size=3
  ) +
  geom_text(
    aes(
      x=conf_high+0.8,
      label=sprintf("%.1f",estimate)
    ),
    hjust=0,
    size=3,
    show.legend=FALSE
  ) +
  scale_colour_manual(
    values=c("Female"=female_col,"Male"=male_col),
    breaks=c("Female","Male"),
    labels=c("Women","Men")
  ) +
  scale_x_continuous(
    limits=c(x_left,x_right),
    breaks=seq(
      ceiling(x_left/10)*10,
      floor(x_max/10)*10,
      by=10
    ),
    labels=function(x) paste0(x,"%"),
    expand=c(0,0)
  ) +
  scale_y_continuous(
    limits=c(0.55,3.45),
    breaks=c(1,2,3),
    labels=c(
      "Lower-ranked",
      "Similar-ranked",
      "Higher-ranked"
    ),
    expand=c(0,0)
  ) +
  labs(
    title="Scholar-weighted share of external institutions (%)",
    x=NULL,
    y=NULL,
    colour=NULL
  ) +
  guides(
    colour=guide_legend(
      override.aes=list(
        shape=16,
        linetype=0,
        size=3
      )
    )
  ) +
  theme_classic(base_size=9) +
  theme(
    axis.text.x=element_text(
      size=10,
      colour=axis_col,
      margin=margin(t=3)
    ),
    axis.text.y=element_text(
      size=8.2,
      colour=text_col,
      margin=margin(r=7)
    ),
    axis.title=element_blank(),
    axis.ticks=element_blank(),
    axis.line.y=element_blank(),
    axis.line.x=element_line(
      colour="#A7AFB8",
      linewidth=0.35
    ),
    legend.position="bottom",
    legend.direction="horizontal",
    legend.justification="center",
    legend.title=element_blank(),
    legend.text=element_text(
      size=10,
      colour=text_col
    ),
    legend.key.width=unit(12,"pt"),
    legend.key.height=unit(8,"pt"),
    legend.spacing.x=unit(8,"pt"),
    legend.margin=margin(t=3,b=0),
    plot.title=element_text(
      size=10,
      face="plain",
      colour=text_col,
      hjust=0,
      margin=margin(b=7)
    ),
    plot.margin=margin(8,10,0,8)
  )



P_2E <- (
  P_2E_left + P_2E_right +
    plot_layout(widths=c(1.85,1.05))
) +
  plot_annotation(
    title="e   Hierarchical mobility during external experience among returnees",
    theme=theme(
      plot.title=element_text(
        size=11,
        face="bold",
        hjust=0,
        colour="#111827",
        margin=margin(b=6)
      )
    )
  )









#########################
# Figure 3

make_fig3 <- function(source_dir) {
  issues <- character()
  issue <- function(x) { issues <<- c(issues, x); invisible(NULL) }
  read_sd <- function(file, sheet = 1, columns = NULL) read_source(source_dir, file, sheet, columns)

COL_WOMEN <- "#D62728"
COL_MEN <- "#1F77B4"
COL_DIFF <- "#4C9A9A"

# a. Read adjusted means and intervals on original outcome scales.
a_pub <- read_sd("3a.xlsx","a_pub")
a_jif <- read_sd("3a.xlsx","a_jif")
prepare_a <- function(d) { d$gender <- gender_display(d$gender)
 d$route <- recode(as.character(d$route),`Non-self-hire`="External hire")
 d$route <- check_levels(d$route,c(route_levels,"External hire"),"Fig3a"); assert_ci(d) }
a_pub <- prepare_a(a_pub); a_jif <- prepare_a(a_jif)




p_a_pub <- ggplot(a_pub,aes(x=route,y=estimate,colour=gender)) +
  geom_errorbar(aes(ymin=conf_low,ymax=conf_high),width=.07,linewidth=.65,
                position=position_dodge(width=.40)) +
  geom_point(size=2.5,position=position_dodge(width=.40)) +
  scale_colour_manual(
    values=c("Women"=COL_WOMEN,"Men"=COL_MEN),
    breaks=c("Women","Men"),
    drop=FALSE
  ) +
  scale_x_discrete(labels=c(
    "Direct self-hire"="Direct\nself-hire",
    "Internal retention"="Internal\nretention",
    "External return"="External\nreturn",
    "External hire"="Non-self-hire"
  )) +
  labs(title="Publications",x=NULL,
       y="Adjusted mean number\nof publications",colour=NULL) +
  theme_classic(base_size=9) +
  theme(
    plot.title=element_text(face="bold",size=10.5,hjust=.5),
    legend.position="top",
    legend.text=element_text(size=8.3),
    axis.title=element_text(size=9),
    axis.text=element_text(size=8.2,colour="black"),
    axis.text.x=element_text(lineheight=.95),
    plot.margin=margin(4,6,4,4)
  )


p_a_jif <- ggplot(a_jif,aes(x=route,y=estimate,colour=gender)) +
  geom_errorbar(aes(ymin=conf_low,ymax=conf_high),width=.07,linewidth=.65,
                position=position_dodge(width=.40)) +
  geom_point(size=2.5,position=position_dodge(width=.40)) +
  scale_colour_manual(
    values=c("Women"=COL_WOMEN,"Men"=COL_MEN),
    breaks=c("Women","Men"),
    drop=FALSE
  ) +
  scale_x_discrete(labels=c(
    "Direct self-hire"="Direct\nself-hire",
    "Internal retention"="Internal\nretention",
    "External return"="External\nreturn",
    "External hire"="Non-self-hire"
  )) +
  labs(title="Mean journal JIF",x=NULL,y="Adjusted mean JIF",colour=NULL) +
  theme_classic(base_size=9) +
  theme(
    plot.title=element_text(face="bold",size=10.5,hjust=.5),
    legend.position="top",
    legend.text=element_text(size=8.3),
    axis.title=element_text(size=9),
    axis.text=element_text(size=8.2,colour="black"),
    axis.text.x=element_text(lineheight=.95),
    plot.margin=margin(4,6,4,4)
  )




p_a_pub <- p_a_pub +
  scale_y_continuous(breaks=seq(1.5,7.5,1)) +
  coord_cartesian(ylim=c(1.5,7.5))
p_a_jif <- p_a_jif +
  scale_y_continuous(breaks=seq(1.5,7.5,1)) +
  coord_cartesian(ylim=c(1.5,7.5))
panel_a <- (p_a_pub | p_a_jif) +
  plot_layout(guides="collect") +
  plot_annotation(
    title="a  Pre-entry scholarly profiles across entry pathways",
    theme=theme(plot.title=element_text(face="bold",size=12,hjust=0))
  ) &
  theme(
    legend.position="top",
    legend.justification="center",
    legend.box.just="center"
  )







# b. Read within-destination contrasts (do not refit the models).
prepare_b <- function(d) { d$gender <- gender_display(d$gender)
 d$route <- check_levels(d$route,rev(route_levels),"Fig3b"); assert_ci(d) }
b_pub <- prepare_b(read_sd("3b.xlsx","b_pub"))
b_jif <- prepare_b(read_sd("3b.xlsx","b_jif"))

p_b_pub <- ggplot(b_pub,aes(x=estimate,y=route,colour=gender)) +
  geom_vline(xintercept=0,linetype="dashed",linewidth=.45,colour="grey55") +
  geom_errorbarh(aes(xmin=conf_low,xmax=conf_high),
                 height=.10,linewidth=.7,
                 position=position_dodge(width=.38)) +
  geom_point(size=2.5,position=position_dodge(width=.38)) +
  scale_colour_manual(values=c("Women"=COL_WOMEN,"Men"=COL_MEN)) +
  labs(
    title="Publications",
    x="Self-hire pathway − non-self-hire\ndifference in publications",
    y=NULL,
    colour=NULL
  ) +
  theme_classic(base_size=9) +
  theme(
    plot.title=element_text(face="bold",size=10.5,hjust=.5),
    legend.position="top",
    legend.text=element_text(size=8.3),
    axis.title=element_text(size=9),
    axis.text=element_text(size=8.3,colour="black"),
    axis.ticks.y=element_blank(),
    plot.margin=margin(4,6,4,4)
  )
p_b_jif <- ggplot(b_jif,aes(x=estimate,y=route,colour=gender)) +
  geom_vline(xintercept=0,linetype="dashed",linewidth=.45,colour="grey55") +
  geom_errorbarh(aes(xmin=conf_low,xmax=conf_high),
                 height=.10,linewidth=.7,
                 position=position_dodge(width=.38)) +
  geom_point(size=2.5,position=position_dodge(width=.38)) +
  scale_colour_manual(values=c("Women"=COL_WOMEN,"Men"=COL_MEN)) +
  labs(
    title="Mean journal JIF",
    x="Self-hire pathway − non-self-hire\ndifference in mean JIF",
    y=NULL,
    colour=NULL
  ) +
  theme_classic(base_size=9) +
  theme(
    plot.title=element_text(face="bold",size=10.5,hjust=.5),
    legend.position="top",
    legend.text=element_text(size=8.3),
    axis.title=element_text(size=9),
    axis.text.x=element_text(size=8.3,colour="black"),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    plot.margin=margin(4,6,4,4)
  )
p_b_pub <- p_b_pub +
  scale_x_continuous(breaks=seq(-3,1,1)) +
  coord_cartesian(xlim=c(-3,1))
p_b_jif <- p_b_jif +
  scale_x_continuous(breaks=seq(-3,1,1)) +
  coord_cartesian(xlim=c(-3,1))
panel_b <- (p_b_pub | p_b_jif) +
  plot_layout(guides="collect") +
  plot_annotation(
    title="b  Within-destination pathway contrasts",
    theme=theme(
      plot.title=element_text(face="bold",size=12)
    )
  ) &
  theme(
    legend.position="top",
    legend.justification="center",
    legend.box.just="center"
  )








# c. Read similar-prestige institutional comparisons.
c_pub <- assert_ci(read_sd("3c.xlsx","c_pub"))
c_jif <- assert_ci(read_sd("3c.xlsx","c_jif"))

COL_C <- "#3A8F73"
make_boundary_plot <- function(dat,title,show_y_title=TRUE){
  dat <- dat %>%
    arrange(epsilon) %>%
    mutate(
      eps_plot=factor(
        paste0("≤ ",sprintf("%.3f",epsilon)),
        levels=paste0("≤ ",sprintf("%.3f",epsilon))
      )
    )
  ggplot(dat,aes(x=eps_plot,y=estimate)) +
    geom_hline(
      yintercept=0,
      linetype="dashed",
      linewidth=.45,
      colour="grey55"
    ) +
    geom_errorbar(
      aes(ymin=conf_low,ymax=conf_high),
      width=.10,
      linewidth=.7,
      colour=COL_C
    ) +
    geom_point(
      size=2.8,
      colour=COL_C
    ) +
    geom_text(
      aes(
        label=sprintf("%.2f",estimate),
        vjust=ifelse(estimate<0,1.6,-.7)
      ),
      nudge_x=.35,
      size=2.8,
      colour=COL_C
    ) +
    labs(
      title=title,
      x="Doctoral–faculty SpringRank difference threshold",
      y=if(show_y_title)
        "Adjusted self-hire − non-self-hire\ndifference"
      else NULL
    ) +
    scale_y_continuous(
      breaks=seq(-2,0.3,0.5)
    ) +
    coord_cartesian(ylim=c(-2,0.2)) +
    theme_classic(base_size=9) +
    theme(
      plot.title=element_text(
        face="bold",
        size=10.5,
        hjust=.5
      ),
      axis.title=element_text(size=9),
      axis.text=element_text(
        size=8,
        colour="black"
      ),
      axis.text.x=element_text(
        lineheight=.95,
        margin=margin(t=3)
      ),
      plot.margin=margin(5,7,5,5)
    )
}








p_c_pub <- make_boundary_plot(
  c_pub,
  "Pre-entry publications",
  show_y_title=TRUE
)
p_c_jif <- make_boundary_plot(
  c_jif,
  "Mean journal JIF",
  show_y_title=FALSE
)



panel_c <- (p_c_pub | p_c_jif) +
  plot_annotation(
    title="c  Similar-prestige institutional comparisons",
    theme=theme(
      plot.title=element_text(
        face="bold",
        size=12,
        hjust=0
      )
    )
  )






# d and e. Read prestige curves and their pointwise intervals.
curve_pub <- assert_ci(read_sd("3d.xlsx","d"))
curve_jif <- assert_ci(read_sd("3e.xlsx","e"))
curve_pub$gender <- gender_display(curve_pub$gender)
curve_jif$gender <- gender_display(curve_jif$gender)
prestige_range <- range(c(curve_pub$prestige_z,curve_jif$prestige_z))
cap_d <- cap_e <- ""
if(file.exists(file.path(source_dir,"3de_slope_difference.xlsx"))) {
 slopes <- assert_ci(read_sd("3de_slope_difference.xlsx",columns=c("outcome","estimate","conf_low","conf_high")))
 format_slope <- function(o) { z <- filter(slopes,outcome==o); stopifnot(nrow(z)==1L)
  sprintf("Women − Men prestige-slope difference = %.3f\n95%% CI %.3f to %.3f",z$estimate,z$conf_low,z$conf_high) }
 cap_d <- format_slope("Publications"); cap_e <- format_slope("JIF")
} else issue("Fig3d,e: slope-difference annotations and covariance-based CIs are not exported; add 3de_slope_difference.xlsx.")

prestige_breaks <- seq(
  ceiling(prestige_range[1]*2)/2,
  floor(prestige_range[2]*2)/2,
  by=0.5
)
prestige_labels <- ifelse(
  abs(prestige_breaks)<1e-8,
  "Mean",
  paste0(
    ifelse(prestige_breaks>0,"+",""),
    format(prestige_breaks,trim=TRUE,nsmall=1),
    " SD"
  )
)





p_d <- ggplot(curve_pub,aes(x=prestige_z,y=estimate,colour=gender,fill=gender,linetype=gender)) +
  geom_hline(yintercept=0,linetype="dashed",linewidth=.45,colour="grey55") +
  geom_ribbon(aes(ymin=conf_low,ymax=conf_high),alpha=.10,colour=NA,show.legend=FALSE) +
  geom_line(linewidth=.9) +
  scale_colour_manual(
    values=c("Women"=COL_WOMEN,"Men"=COL_MEN),
    breaks=c("Women","Men")
  ) +
  scale_fill_manual(
    values=c("Women"=COL_WOMEN,"Men"=COL_MEN),
    guide="none"
  ) +
  scale_linetype_manual(
    values=c("Women"="22","Men"="solid"),
    breaks=c("Women","Men")
  ) +
  scale_x_continuous(
    limits=prestige_range,
    breaks=prestige_breaks,
    labels=prestige_labels,
    expand=expansion(mult=c(.01,.01))
  ) +
  annotate(
    "text",
    x=prestige_range[1],
    y=-Inf,
    label=cap_d,
    hjust=0,
    vjust=-.7,
    size=2.7,
    colour="grey30"
  ) +
  labs(
    title="d  Publications across destination prestige",
    x="First-faculty institution prestige\n(standardized SpringRank; higher = more prestigious)",
    y="Adjusted self-hire − non-self-hire\ndifference in pre-entry publications",
    colour=NULL,
    linetype=NULL
  ) +
  scale_y_continuous(
    breaks=seq(-2.5,1,0.5)
  ) +
  coord_cartesian(ylim=c(-2.5,1)) +
  theme_classic(base_size=9) +
  theme(
    plot.title.position="plot",
    plot.title=element_text(face="bold",size=11.5,hjust=0),
    legend.position="top",
    legend.justification="center",
    legend.text=element_text(size=8.3),
    axis.title=element_text(size=9),
    axis.text=element_text(size=8.2,colour="black"),
    plot.margin=margin(5,8,5,5)
  )





p_e <- ggplot(curve_jif,aes(x=prestige_z,y=estimate,colour=gender,fill=gender,linetype=gender)) +
  geom_hline(yintercept=0,linetype="dashed",linewidth=.45,colour="grey55") +
  geom_ribbon(aes(ymin=conf_low,ymax=conf_high),alpha=.10,colour=NA,show.legend=FALSE) +
  geom_line(linewidth=.9) +
  scale_colour_manual(
    values=c("Women"=COL_WOMEN,"Men"=COL_MEN),
    breaks=c("Women","Men")
  ) +
  scale_fill_manual(
    values=c("Women"=COL_WOMEN,"Men"=COL_MEN),
    guide="none"
  ) +
  scale_linetype_manual(
    values=c("Women"="22","Men"="solid"),
    breaks=c("Women","Men")
  ) +
  scale_x_continuous(
    limits=prestige_range,
    breaks=prestige_breaks,
    labels=prestige_labels,
    expand=expansion(mult=c(.01,.01))
  ) +
  annotate(
    "text",
    x=prestige_range[1],
    y=-Inf,
    label=cap_e,
    hjust=0,
    vjust=-.7,
    size=2.7,
    colour="grey30"
  ) +
  labs(
    title="e  Journal impact across destination prestige",
    x="First-faculty institution prestige\n(standardized SpringRank; higher = more prestigious)",
    y="Adjusted self-hire − non-self-hire\ndifference in pre-entry mean JIF",
    colour=NULL,
    linetype=NULL
  ) +
  scale_y_continuous(
    breaks=seq(-2.5,1,0.5)
  ) +
  coord_cartesian(ylim=c(-2.5,1)) +
  theme_classic(base_size=9) +
  theme(
    plot.title.position="plot",
    plot.title=element_text(face="bold",size=11.5,hjust=0),
    legend.position="top",
    legend.justification="center",
    legend.text=element_text(size=8.3),
    axis.title=element_text(size=9),
    axis.text=element_text(size=8.2,colour="black"),
    plot.margin=margin(5,8,5,5)
  )












################################
# Figure 4

make_fig4 <- function(source_dir) {
  issues <- character()
  issue <- function(x) { issues <<- c(issues, x); invisible(NULL) }
  read_sd <- function(file, sheet = 1, columns = NULL) read_source(source_dir, file, sheet, columns)

FIG4_PANEL_TITLES <- c(
  a="Pathway gaps before and after faculty entry",
  b="Gender-specific matched post-entry trajectories",
  c="Disciplinary heterogeneity",
  d="Institutional hierarchy heterogeneity"
)
FIG4_OUTCOME_TITLES <- c(
  Publications="Publications",
  JIF="Journal impact (JIF)"
)
FIG4_AB_ASPECT <- 0.62








# a. Read pre/post pathway contrasts. Ellipses below are decorative,
# not confidence regions; no outcome model is estimated here.
quad_pub <- read_sd("4a.xlsx","quad_pub")
quad_jif <- read_sd("4a.xlsx","quad_jif")

plot_trajectory <- function(data,title){
  PERIODS <- c("0–2 years","3–5 years","6–8 years")
  ROUTES <- c("Direct self-hire","Internal retention","External return")
  GENDERS <- c("Women","Men")
  ROUTE_SHAPES <- c("Direct self-hire"=21,"Internal retention"=22,"External return"=24)
  PERIOD_SIZES <- c("0–2 years"=2.2,"3–5 years"=3.0,"6–8 years"=3.8)
  GENDER_COLORS <- c("Women"="#D62728","Men"="#1F77B4")
  data <- data %>%
    mutate(
      period=factor(period,levels=PERIODS),
      gender=factor(gender,levels=GENDERS),
      route=factor(route,levels=ROUTES),
      pre_gap_plot=pre_gap,
      point_fill=case_when(
        period=="0–2 years" & gender=="Women" ~ "white",
        period=="0–2 years" & gender=="Men" ~ "white",
        period=="3–5 years" & gender=="Women" ~ "#F2B5B5",
        period=="3–5 years" & gender=="Men" ~ "#B3D2E8",
        period=="6–8 years" & gender=="Women" ~ "#D62728",
        period=="6–8 years" & gender=="Men" ~ "#1F77B4",
        TRUE ~ "white"
      )
    ) %>%
    filter(
      !is.na(period),
      !is.na(gender),
      !is.na(route),
      is.finite(pre_gap_plot),
      is.finite(post_gap)
    )
  if(nrow(data)==0) stop("No valid trajectory observations.")
  line_data <- data %>% arrange(route,gender,period)
  point_data <- data %>% arrange(desc(as.integer(period)),route,gender)
  lim <- max(abs(c(data$pre_gap_plot,data$post_gap)),na.rm=TRUE)
  lim <- max(ceiling(lim/0.025)*0.025,if(identical(title,FIG4_OUTCOME_TITLES[["JIF"]])) 0.10 else 0.125)
  tick_max <- floor((lim+1e-8)/0.05)*0.05
  axis_breaks <- seq(-tick_max,tick_max,by=0.05)
  ellipse_data <- data %>%
    group_by(route) %>%
    filter(n()>=2) %>%
    group_modify(~{
      x0 <- mean(.x$pre_gap_plot)
      y0 <- mean(.x$post_gap)
      rx <- max(diff(range(.x$pre_gap_plot))/2*1.45,0.07*lim)
      ry <- max(diff(range(.x$post_gap))/2*1.45,0.07*lim)
      theta <- seq(0,2*pi,length.out=200)
      tibble::tibble(
        x=x0+rx*cos(theta),
        y=y0+ry*sin(theta)
      )
    }) %>%
    ungroup() %>%
    mutate(
      ellipse_fill=case_when(
        route=="Direct self-hire" ~ "#D9A6A6",
        route=="Internal retention" ~ "#9FBBD5",
        route=="External return" ~ "#A8CEB5"
      )
    )
  route_labels <- tibble::tibble(
    route=factor(ROUTES,levels=ROUTES),
    label_x=rep(0.20*lim,3),
    label_y=c(-0.50,-0.64,-0.78)*lim,
    label=ROUTES
  )
  ref_label <- tibble::tibble(
    label_x=0.20*lim,
    label_y=-0.92*lim,
    label="Non-self-hire reference"
  )
  ggplot(data,aes(x=pre_gap_plot,y=post_gap)) +
    geom_hline(
      yintercept=0,
      colour="grey75",
      linetype="dashed",
      linewidth=0.4
    ) +
    geom_vline(
      xintercept=0,
      colour="grey75",
      linetype="dashed",
      linewidth=0.4
    ) +
    geom_abline(
      intercept=0,
      slope=1,
      colour="grey85",
      linetype="dotted",
      linewidth=0.4
    ) +
    geom_polygon(
      data=ellipse_data,
      aes(x=x,y=y,group=route,fill=ellipse_fill),
      inherit.aes=FALSE,
      alpha=0.18,
      colour=NA,
      show.legend=FALSE
    ) +
    geom_line(
      data=line_data,
      aes(colour=gender,group=interaction(route,gender)),
      linewidth=0.68,
      alpha=0.72,
      lineend="round",
      show.legend=FALSE
    ) +
    geom_point(
      data=point_data,
      aes(colour=gender,shape=route,size=period,fill=point_fill),
      stroke=0.70,
      alpha=1,
      show.legend=c(colour=TRUE,shape=FALSE,size=TRUE,fill=FALSE)
    ) +
    annotate(
      "point",
      x=0,
      y=0,
      shape=23,
      size=3.7,
      stroke=0.9,
      fill="white",
      colour="grey15"
    ) +
    geom_point(
      data=route_labels,
      aes(x=label_x,y=label_y,shape=route),
      inherit.aes=FALSE,
      size=2.3,
      stroke=0.65,
      colour="grey35",
      fill="white",
      show.legend=FALSE
    ) +
    geom_text(
      data=route_labels,
      aes(x=label_x+0.055*lim,y=label_y,label=label),
      inherit.aes=FALSE,
      hjust=0,
      vjust=0.5,
      size=2.75,
      colour="grey20"
    ) +
    geom_point(
      data=ref_label,
      aes(x=label_x,y=label_y),
      inherit.aes=FALSE,
      shape=23,
      size=2.35,
      stroke=0.80,
      colour="grey20",
      fill="white",
      show.legend=FALSE
    ) +
    geom_text(
      data=ref_label,
      aes(x=label_x+0.055*lim,y=label_y,label=label),
      inherit.aes=FALSE,
      hjust=0,
      vjust=0.5,
      size=2.75,
      colour="grey20"
    ) +
    scale_colour_manual(
      values=GENDER_COLORS,
      limits=GENDERS,
      name=NULL
    ) +
    scale_shape_manual(
      values=ROUTE_SHAPES,
      limits=ROUTES,
      guide="none"
    ) +
    scale_size_manual(
      values=PERIOD_SIZES,
      limits=PERIODS,
      drop=FALSE,
      name=NULL
    ) +
    scale_fill_identity(guide="none") +
    scale_x_continuous(
      breaks=axis_breaks,
      labels=\(x) sprintf("%.2f",x),
      expand=expansion(mult=c(0.01,0.01))
    ) +
    scale_y_continuous(
      breaks=axis_breaks,
      labels=\(x) sprintf("%.2f",x),
      expand=expansion(mult=c(0.01,0.01))
    ) +
    coord_cartesian(
      xlim=c(-lim,lim),
      ylim=c(-lim,lim),
      clip="off"
    ) +
    labs(
      title=title,
      x="Adjusted pre-entry gap, years −3 to −1\n(Pathway − non-self-hire)",
      y="Adjusted post-entry gap\n(Pathway − non-self-hire)"
    ) +
    guides(
      colour=guide_legend(
        order=1,
        override.aes=list(
          shape=16,
          size=3.0,
          colour=unname(GENDER_COLORS),
          alpha=1
        )
      ),
      size=guide_legend(
        order=2,
        override.aes=list(
          shape=21,
          colour="grey35",
          fill=c("white","grey75","grey35"),
          size=unname(PERIOD_SIZES),
          stroke=0.65,
          alpha=1
        )
      )
    ) +
    theme_classic(base_size=11) +
    theme(
      plot.title=element_text(
        hjust=0.5,
        face="bold",
        size=12,
        margin=margin(b=5)
      ),
      axis.title.x=element_text(
        size=10.5,
        margin=margin(t=7)
      ),
      axis.title.y=element_text(
        size=10.5,
        margin=margin(r=7)
      ),
      axis.text=element_text(
        size=9.2,
        colour="black"
      ),
      axis.line=element_line(
        linewidth=0.45,
        colour="black"
      ),
      axis.ticks=element_line(
        linewidth=0.4,
        colour="black"
      ),
      axis.ticks.length=grid::unit(2.5,"pt"),
      legend.position="bottom",
      legend.direction="horizontal",
      legend.box="horizontal",
      legend.key.width=grid::unit(1.05,"lines"),
      legend.spacing.x=grid::unit(0.22,"cm"),
      legend.text=element_text(size=9.2),
      legend.margin=margin(t=-2),
      plot.margin=margin(8,10,8,8),
      aspect.ratio=FIG4_AB_ASPECT
    )
}



P_pub_trajectory <- plot_trajectory(quad_pub,FIG4_OUTCOME_TITLES[["Publications"]])
P_jif_trajectory <- plot_trajectory(quad_jif,FIG4_OUTCOME_TITLES[["JIF"]]) +
  theme(axis.title.y=element_blank())








P_a <- (P_pub_trajectory | P_jif_trajectory) +
  plot_layout(
    ncol=2,
    widths=c(1,1),
    guides="collect"
  ) +
  plot_annotation(
    title=paste0("a  ",FIG4_PANEL_TITLES[["a"]]),
    theme=theme(
      plot.title=element_text(
        hjust=0,
        face="bold",
        size=14,
        margin=margin(b=5)
      ),
      plot.title.position="plot"
    )
  ) &
  theme(legend.position="bottom")










# b. Read CEM-weighted event-study results.
pub_gender <- read_sd("4b.xlsx","pub_gender")
jif_gender <- read_sd("4b.xlsx","jif_gender")
assert_ci(pub_gender,"estimate","conf.low","conf.high")
assert_ci(jif_gender,"estimate","conf.low","conf.high")

plot_gender_did <- function(data,title){
  GENDER_COLORS <- c("Female"="#D62728","Male"="#1F77B4")
  OFFSET <- 0.10
  data <- data %>%
    mutate(
      gender=factor(gender,levels=c("Female","Male")),
      x_plot=event_time+case_when(
        gender=="Female" ~ -OFFSET,
        gender=="Male" ~ OFFSET,
        TRUE ~ 0
      )
    ) %>%
    filter(
      !is.na(gender),
      is.finite(x_plot),
      is.finite(estimate),
      is.finite(conf.low),
      is.finite(conf.high)
    ) %>%
    arrange(gender,event_time)
  ggplot(data,aes(x=x_plot,y=estimate,colour=gender,group=gender)) +
    geom_hline(
      yintercept=0,
      colour="grey72",
      linetype="dashed",
      linewidth=0.45
    ) +
    geom_vline(
      xintercept=0,
      colour="grey65",
      linetype="dashed",
      linewidth=0.50
    ) +
    geom_errorbar(
      aes(ymin=conf.low,ymax=conf.high),
      width=0.10,
      linewidth=0.38,
      alpha=0.60
    ) +
    geom_line(
      linewidth=0.80,
      alpha=0.90,
      lineend="round"
    ) +
    geom_point(
      size=2.5,
      stroke=0.30,
      alpha=0.95
    ) +
    scale_colour_manual(
      values=GENDER_COLORS,
      breaks=c("Female","Male"),
      labels=c("Women","Men"),
      name=NULL
    ) +
    scale_x_continuous(
      breaks=-5:5,
      limits=c(-5.5,5.5),
      expand=expansion(mult=c(0.02,0.02))
    ) +
    scale_y_continuous(
      breaks=seq(-0.15,0.10,by=0.05),
      labels=function(x) sprintf("%.2f",x),
      expand=expansion(mult=0)
    ) +
    coord_cartesian(ylim=c(-0.15,0.10)) +
    labs(
      title=title,
      x="Years relative to first faculty entry",
      y="Self-hire − non-self-hire difference\nrelative to year −1"
    ) +
    guides(
      colour=guide_legend(
        override.aes=list(
          linewidth=0.90,
          size=2.7,
          alpha=1
        )
      )
    ) +
    theme_classic(base_size=11) +
    theme(
      plot.title=element_text(hjust=0.5,face="bold",size=12,margin=margin(b=5)),
      axis.title.x=element_text(size=10.5,margin=margin(t=7)),
      axis.title.y=element_text(size=10.5,margin=margin(r=7)),
      axis.text=element_text(size=9.3,colour="black"),
      axis.line=element_line(colour="black",linewidth=0.45),
      axis.ticks=element_line(colour="black",linewidth=0.4),
      axis.ticks.length=grid::unit(2.5,"pt"),
      legend.position="bottom",
      legend.direction="horizontal",
      legend.box="horizontal",
      legend.key.width=grid::unit(1.2,"lines"),
      legend.spacing.x=grid::unit(0.25,"cm"),
      legend.text=element_text(size=9.5),
      legend.margin=margin(t=-2),
      plot.margin=margin(6,10,6,8),
      aspect.ratio=FIG4_AB_ASPECT
    )
}




P_pub_gender <- plot_gender_did(pub_gender,FIG4_OUTCOME_TITLES[["Publications"]])
P_jif_gender <- plot_gender_did(jif_gender,FIG4_OUTCOME_TITLES[["JIF"]]) +
  theme(axis.title.y=element_blank())




library(patchwork)
P_gender_did <- (P_pub_gender | P_jif_gender) +
  plot_layout(guides="collect") +
  plot_annotation(
    title=paste0("b  ",FIG4_PANEL_TITLES[["b"]]),
    theme=theme(
      plot.title=element_text(
        hjust=0,
        face="bold",
        size=14,
        margin=margin(b=10)
      ),
      plot.title.position="plot"
    )
  ) &
  theme(legend.position="bottom")













# c. Read domain-specific DID contrasts and published intervals.
results_domain <- read_sd("4c.xlsx","results_domain")
assert_ci(results_domain,"estimate","conf.low","conf.high")
FIELD_ORDER <- c("Engineering and technology","Agricultural and veterinary sciences",
 "Humanities and the arts","Natural sciences","Social sciences","Medical and health sciences")
field_labels <- c("Engineering and technology"="Engineering\nand technology",
 "Agricultural and veterinary sciences"="Agricultural and\nveterinary sciences",
 "Humanities and the arts"="Humanities\nand the arts","Natural sciences"="Natural\nsciences",
 "Social sciences"="Social\nsciences","Medical and health sciences"="Medical and\nhealth sciences")
GENDERS <- c("Female","Male"); COLORS <- c(Female="#D62728",Male="#1F77B4"); SHAPES <- c(Female=16,Male=17)

plot_domain_opt <- function(data,outcome,title,tag=NULL){
  d <- data %>%
    filter(.data$outcome==.env$outcome) %>%
    mutate(
      field_rev=factor(field,levels=rev(FIELD_ORDER)),
      y_base=as.numeric(field_rev),
      y=if_else(gender_final=="Female",y_base+0.14,y_base-0.14)
    )
  stripe_df <- d %>%
    distinct(y_base) %>%
    arrange(y_base) %>%
    mutate(
      ymin=y_base-0.5,
      ymax=y_base+0.5,
      odd=row_number() %% 2==1
    )
  xlim <- range(c(d$conf.low,d$conf.high),na.rm=TRUE)+c(-0.04,0.05)
  ggplot(d,aes(x=estimate,y=y,colour=gender_final,shape=gender_final)) +
    geom_rect(
      data=stripe_df %>% filter(odd),
      aes(xmin=-Inf,xmax=Inf,ymin=ymin,ymax=ymax),
      inherit.aes=FALSE,
      fill="grey97",
      colour=NA
    ) +
    geom_vline(
      xintercept=0,
      colour="grey55",
      linetype="dashed",
      linewidth=0.45
    ) +
    geom_segment(
      aes(x=conf.low,xend=conf.high,y=y,yend=y),
      linewidth=0.60,
      lineend="round"
    ) +
    geom_point(size=2.7) +
    scale_colour_manual(
      values=COLORS,
      breaks=GENDERS,
      labels=c("Women","Men"),
      name=NULL
    ) +
    scale_shape_manual(
      values=SHAPES,
      breaks=GENDERS,
      labels=c("Women","Men"),
      name=NULL
    ) +
    scale_y_continuous(
      breaks=seq_along(FIELD_ORDER),
      labels=unname(field_labels[rev(FIELD_ORDER)]),
      limits=c(0.5,length(FIELD_ORDER)+0.5)
    ) +
    scale_x_continuous(breaks=seq(-0.4,0.4,by=0.2)) +
    coord_cartesian(xlim=xlim) +
    labs(
      title=title,
      tag=tag,
      x="Post-entry DID estimate (log1p units)",
      y=NULL
    ) +
    guides(
      colour=guide_legend(override.aes=list(size=3)),
      shape=guide_legend(override.aes=list(size=3))
    ) +
    theme_classic(base_size=11) +
    theme(
      plot.title=element_text(size=12,face="bold",hjust=0.5),
      plot.tag=element_text(size=13,face="bold"),
      axis.title.x=element_text(size=10.5,margin=margin(t=7)),
      axis.text.x=element_text(size=9.5,colour="black"),
      axis.text.y=element_text(size=9.5,colour="black",lineheight=0.95),
      axis.line.y=element_blank(),
      axis.ticks.y=element_blank(),
      legend.position="bottom",
      legend.box="horizontal",
      legend.text=element_text(size=10),
      panel.grid.major.x=element_blank(),
      panel.grid.minor=element_blank(),
      plot.margin=margin(8,10,8,8)
    )
}




P_4C_PUB <- plot_domain_opt(
  results_domain,
  "Publications",
  FIG4_OUTCOME_TITLES[["Publications"]]
)
P_4C_JIF <- plot_domain_opt(
  results_domain,
  "JIF",
  FIG4_OUTCOME_TITLES[["JIF"]]
) +
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )






P_4C <- (P_4C_PUB | P_4C_JIF) +
  plot_layout(
    widths=c(1,1),
    guides="collect",
    axis_titles="collect_x"
  ) +
  plot_annotation(
    title=paste0("c  ",FIG4_PANEL_TITLES[["c"]]),
    theme=theme(
      plot.title=element_text(
        hjust=0,
        size=14,
        face="bold",
        margin=margin(b=5)
      ),
      plot.title.position="plot"
    )
  ) &
  theme(
    legend.position="bottom",
    axis.title.x=element_text(
      size=10.5,
      colour="black",
      margin=margin(t=7)
    )
  )











# d. Read institutional-rank DID contrasts.
RANK_LEVELS <- hierarchy_levels
results_hier <- read_sd("4d.xlsx","results_hier") %>%
 mutate(hierarchy=check_levels(hierarchy,RANK_LEVELS,"Fig4d"))
assert_ci(results_hier,"estimate","conf.low","conf.high")

plot_hier_opt <- function(data,outcome,title,xlim=NULL){
  d <- data %>%
    filter(.data$outcome==.env$outcome) %>%
    mutate(
      y_base=length(RANK_LEVELS)+1-as.integer(hierarchy),
      y=y_base+if_else(gender_final=="Female",0.14,-0.14)
    )
  stopifnot(nrow(d)==12,!anyDuplicated(d[c("hierarchy","gender_final")]))
  stripes <- tibble(y_base=seq_along(RANK_LEVELS)) %>%
    filter(y_base%%2==0)
  if(is.null(xlim)){
    xlim <- range(c(d$conf.low,d$conf.high),na.rm=TRUE)+c(-0.03,0.03)
  }
  ggplot() +
    geom_rect(
      data=stripes,
      aes(xmin=-Inf,xmax=Inf,ymin=y_base-0.5,ymax=y_base+0.5),
      fill="grey97",colour=NA
    ) +
    geom_vline(
      xintercept=0,
      colour="grey55",
      linetype="dashed",
      linewidth=0.45
    ) +
    geom_segment(
      data=d,
      aes(x=conf.low,xend=conf.high,y=y,yend=y,colour=gender_final),
      linewidth=0.60,
      lineend="round"
    ) +
    geom_point(
      data=d,
      aes(x=estimate,y=y,colour=gender_final,shape=gender_final),
      size=2.7
    ) +
    scale_colour_manual(
      values=COLORS,
      breaks=GENDERS,
      labels=c("Women","Men"),
      name=NULL
    ) +
    scale_shape_manual(
      values=SHAPES,
      breaks=GENDERS,
      labels=c("Women","Men"),
      name=NULL
    ) +
    scale_y_continuous(
      breaks=seq_along(RANK_LEVELS),
      labels=rev(RANK_LEVELS),
      limits=c(0.5,length(RANK_LEVELS)+0.5)
    ) +
    scale_x_continuous(breaks=scales::breaks_pretty(n=5)) +
    coord_cartesian(xlim=xlim) +
    labs(
      title=title,
      x="Post-entry DID estimate (log1p units)",
      y=NULL
    ) +
    guides(
      colour=guide_legend(override.aes=list(size=3)),
      shape=guide_legend(override.aes=list(size=3))
    ) +
    theme_classic(base_size=11) +
    theme(
      plot.title=element_text(size=12,face="bold",hjust=0.5),
      axis.title.x=element_text(size=10.5,margin=margin(t=7)),
      axis.text.x=element_text(size=9.5,colour="black"),
      axis.text.y=element_text(size=9.5,colour="black"),
      axis.line.y=element_blank(),
      axis.ticks.y=element_blank(),
      legend.position="bottom",
      legend.box="horizontal",
      legend.text=element_text(size=10),
      panel.grid.major.x=element_blank(),
      panel.grid.minor=element_blank(),
      plot.margin=margin(8,10,8,8)
    )
}




P_4D_PUB <- plot_hier_opt(
  results_hier,
  "Publications",
  FIG4_OUTCOME_TITLES[["Publications"]]
)
P_4D_JIF <- plot_hier_opt(
  results_hier,
  "JIF",
  FIG4_OUTCOME_TITLES[["JIF"]]
) +
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )



P_4D <- (P_4D_PUB | P_4D_JIF) +
  plot_layout(
    widths=c(1,1),
    guides="collect",
    axis_titles="collect_x"
  ) +
  plot_annotation(
    title=paste0("d  ",FIG4_PANEL_TITLES[["d"]]),
    theme=theme(
      plot.title=element_text(size=14,face="bold",hjust=0),
      plot.title.position="plot"
    )
  ) &
  theme(
    legend.position="bottom",
    axis.title.x=element_text(
      size=10.5,
      colour="black",
      margin=margin(t=7)
    )
  )













###########################
# Extended Data Figure 1

make_edfig1 <- function(source_dir) {
  issues <- character()
  issue <- function(x) { issues <<- c(issues, x); invisible(NULL) }
  read_sd <- function(file, sheet = 1, columns = NULL) read_source(source_dir, file, sheet, columns)

  
  
  
  
  
  
# a. Read the adjustment sequence and precomputed labels.
ed1a_plot_df <- read_sd("ED1a.xlsx")
assert_ci(ed1a_plot_df,"estimate_pp","conf_low_pp","conf_high_pp")
COL_DIFF <- "#4C9A9A"; COL_PRIMARY <- "#216869"

P_ED1A <- ggplot(ed1a_plot_df,aes(x=estimate_pp,y=y)) +
  geom_vline(xintercept=0,linetype="dashed",linewidth=0.45,colour="grey65") +
  geom_hline(yintercept=1.5,linetype="dashed",linewidth=0.40,colour="grey75") +
  geom_errorbar(
    data=ed1a_plot_df %>% filter(!primary),
    aes(xmin=conf_low_pp,xmax=conf_high_pp),
    orientation="y",width=0.10,linewidth=0.60,colour=COL_DIFF
  ) +
  geom_errorbar(
    data=ed1a_plot_df %>% filter(primary),
    aes(xmin=conf_low_pp,xmax=conf_high_pp),
    orientation="y",width=0.11,linewidth=0.80,colour=COL_PRIMARY
  ) +
  geom_point(
    data=ed1a_plot_df %>% filter(!primary),
    size=2.7,colour=COL_DIFF
  ) +
  geom_point(
    data=ed1a_plot_df %>% filter(primary),
    size=3.3,colour=COL_PRIMARY
  ) +
  geom_text(
    aes(x=conf_high_pp+0.10,label=value_label,fontface=if_else(primary,"bold","plain")),
    hjust=0,size=3.0,colour="black"
  ) +
  scale_x_continuous(
    breaks=c(0,1,2,3),
    labels=function(x) ifelse(x>0,paste0("+",x),as.character(x)),
    expand=expansion(mult=c(0,0))
  ) +
  scale_y_continuous(
    limits=c(0.5,6.5),
    breaks=ed1a_plot_df$y,
    labels=ed1a_plot_df$specification
  ) +
  labs(
    title="a  From observed to standardized gender difference",
    x="Women − men difference in doctoral self-hiring (pp)",
    y=NULL
  ) +
  coord_cartesian(xlim=c(-0.2,3.55),clip="off") +
  theme_classic(base_size=9,base_family="sans") +
  theme(
    plot.title.position="plot",
    plot.title=element_text(face="bold",size=12,hjust=0,margin=margin(b=10)),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_text(size=10,colour="black",margin=margin(r=9)),
    axis.text.x=element_text(size=10,colour="black"),
    axis.title.x=element_text(size=10,colour="black",margin=margin(t=6)),
    plot.margin=margin(t=6,r=78,b=6,l=6)
  )









# b. Read selected country estimates and ordering.
ed1b_selected <- read_sd("ED1b.xlsx","ed1b_selected")
assert_ci(ed1b_selected,"estimate_pp","conf.low_pp","conf.high_pp")
COL_MALE <- "#1F77B4"; COL_FEMALE <- "#D62728"; COL_ZERO <- "grey65"
continent_order <- c("Americas","Asia","Europe","Africa","Oceania")
ed1b_selected$continent <- factor(ed1b_selected$continent,levels=continent_order)
n_total <- nrow(ed1b_selected)

n_total <- nrow(ed1b_selected)
group_info <- ed1b_selected %>%
  group_by(continent) %>%
  summarise(ymin=min(y),ymax=max(y),ymid=mean(y),.groups="drop") %>%
  arrange(continent)
sep_df <- group_info %>%
  slice(-n()) %>%
  transmute(y_sep=ymin-0.5)
left_long <- ed1b_selected %>%
  select(first_faculty_country,y,male_prob,female_prob) %>%
  pivot_longer(
    cols=c(male_prob,female_prob),
    names_to="gender",
    values_to="prob"
  ) %>%
  mutate(gender=recode(gender,male_prob="Men",female_prob="Women"))



left_x_min <- min(ed1b_selected$male_prob,ed1b_selected$female_prob,na.rm=TRUE)
left_x_max <- max(ed1b_selected$male_prob,ed1b_selected$female_prob,na.rm=TRUE)
left_x_range <- left_x_max-left_x_min
right_x_min <- min(0,ed1b_selected$conf.low_pp,na.rm=TRUE)
right_x_max <- max(ed1b_selected$conf.high_pp,na.rm=TRUE)
right_x_range <- right_x_max-right_x_min
diff_label_x <- right_x_max+0.05*right_x_range




P_ED1B_left <- ggplot() +
  geom_segment(
    data=ed1b_selected,
    aes(x=male_prob,xend=female_prob,y=y,yend=y),
    linewidth=0.50,colour="grey75"
  ) +
  geom_point(
    data=left_long,
    aes(x=prob,y=y,colour=gender),
    size=2.35
  ) +
  geom_hline(
    data=sep_df,
    aes(yintercept=y_sep),
    linetype="dashed",linewidth=0.42,colour="grey78"
  ) +
  scale_colour_manual(
    values=c("Women"=COL_FEMALE,"Men"=COL_MALE),
    breaks=c("Women","Men"),
    name=NULL,
    guide=guide_legend(order=1,nrow=1,override.aes=list(size=2.6))
  ) +
  scale_x_continuous(
    breaks=pretty(c(left_x_min,left_x_max),n=5),
    expand=expansion(mult=c(0.02,0.04))
  ) +
  scale_y_continuous(
    breaks=ed1b_selected$y,
    labels=ed1b_selected$first_faculty_country,
    limits=c(0.5,n_total+0.5),
    expand=c(0,0)
  ) +
  coord_cartesian(
    xlim=c(left_x_min-0.04*left_x_range,left_x_max+0.05*left_x_range),
    clip="off"
  ) +
  labs(x="Doctoral self-hiring (%)",y=NULL) +
  theme_classic(base_size=10,base_family="sans") +
  theme(
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_text(size=10,colour="black"),
    axis.text.x=element_text(size=10,colour="black"),
    axis.title.x=element_text(size=10,colour="black",margin=margin(t=5)),
    legend.position="bottom",
    plot.margin=margin(t=4,r=8,b=5,l=4)
  )




P_ED1B_right <- ggplot(ed1b_selected,aes(x=estimate_pp,y=y)) +
  geom_vline(
    xintercept=0,linetype="dashed",
    linewidth=0.45,colour=COL_ZERO
  ) +
  geom_hline(
    data=sep_df,
    aes(yintercept=y_sep),
    inherit.aes=FALSE,
    linetype="dashed",linewidth=0.42,colour="grey78"
  ) +
  geom_errorbar(
    aes(xmin=conf.low_pp,xmax=conf.high_pp),
    orientation="y",width=0.09,
    linewidth=0.52,colour=COL_DIFF,
    show.legend=FALSE
  ) +
  geom_point(
    aes(colour="Women - men difference"),
    size=2.35
  ) +
  geom_text(
    aes(x=diff_label_x,label=diff_label),
    hjust=0,size=3,colour="black"
  ) +
  scale_colour_manual(
    values=c("Women - men difference"=COL_DIFF),
    name=NULL,
    guide=guide_legend(order=2,nrow=1,override.aes=list(size=2.6))
  ) +
  scale_x_continuous(
    breaks=pretty(c(right_x_min,right_x_max),n=5),
    labels=function(x) ifelse(x>0,paste0("+",x),as.character(x)),
    expand=expansion(mult=c(0.02,0.03))
  ) +
  scale_y_continuous(
    limits=c(0.5,n_total+0.5),
    expand=c(0,0)
  ) +
  coord_cartesian(
    xlim=c(right_x_min-0.04*right_x_range,right_x_max+0.09*right_x_range),
    clip="off"
  ) +
  labs(x="Women - men difference (pp)",y=NULL) +
  theme_classic(base_size=10,base_family="sans") +
  theme(
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(size=10,colour="black"),
    axis.title.x=element_text(size=10,colour="black",margin=margin(t=5)),
    legend.position="bottom",
    plot.margin=margin(t=4,r=58,b=5,l=2)
  )





P_ED1B <- P_ED1B_left + P_ED1B_right +
  plot_layout(widths=c(1.00,1.22),guides="collect") +
  plot_annotation(
    title="b  Country-level self-hiring rates and gender differences",
    theme=theme(
      plot.title.position="plot",
      plot.title=element_text(
        family="sans",face="bold",size=12,
        colour="black",hjust=0,margin=margin(b=8)
      )
    )
  ) &
  theme(
    legend.position="bottom",
    legend.justification="center",
    legend.direction="horizontal",
    legend.box="horizontal",
    legend.box.just="center",
    legend.text=element_text(family="sans",size=10,colour="black"),
    legend.key.width=unit(0.65,"cm"),
    legend.key.height=unit(0.30,"cm"),
    legend.spacing.x=unit(0.16,"cm"),
    legend.margin=margin(t=1,r=0,b=0,l=0)
  )













# c. Read displayed heat-map estimates (NA cells remain grey).
ed1c_plot_df <- read_sd("ED1c.xlsx","ed1c")
country_order_c <- ed1b_selected %>% select(first_faculty_country,continent,y)

COL_NEG_20 <- "#53338F"
COL_NEG_10 <- "#8B74B6"
COL_NEG_5 <- "#C8BDD9"
COL_ZERO <- "#FFFFFF"
COL_POS_5 <- "#C7E3D6"
COL_POS_10 <- "#63AA88"
COL_POS_20 <- "#0C6A4B"
COL_NA <- "#D9D9D9"
continent_order <- c("Americas","Asia","Europe","Africa","Oceania")
domain_order <- c(
  "Engineering and technology",
  "Agricultural and veterinary sciences",
  "Humanities and the arts",
  "Natural sciences",
  "Social sciences",
  "Medical and health sciences"
)
domain_plot_order <- c(domain_order,"Overall")
domain_labels <- c(
  "Engineering and technology"="Engineering",
  "Agricultural and veterinary sciences"="Agriculture",
  "Humanities and the arts"="Humanities",
  "Natural sciences"="Natural sciences",
  "Social sciences"="Social sciences",
  "Medical and health sciences"="Medical\n& health",
  "Overall"="Overall"
)
domain_x <- c(1,2,3,4,5,6,7.25)



group_info_c <- country_order_c %>%
  group_by(continent) %>%
  summarise(ymin=min(y),ymax=max(y),.groups="drop") %>%
  arrange(continent)
sep_df_c <- group_info_c %>%
  slice(-n()) %>%
  transmute(y_sep=ymin-0.5)
country_axis_c <- country_order_c %>% arrange(y)
P_ED1C <- ggplot(ed1c_plot_df,aes(x=x,y=y,fill=estimate_display)) +
  geom_tile(
    width=0.92,height=0.88,
    colour="grey94",linewidth=0.40
  ) +
  geom_hline(
    data=sep_df_c,
    aes(yintercept=y_sep),
    inherit.aes=FALSE,
    linetype="dashed",linewidth=0.40,colour="grey78"
  ) +
  scale_fill_gradientn(
    colours=c(
      COL_NEG_20,COL_NEG_10,COL_NEG_5,
      COL_ZERO,
      COL_POS_5,COL_POS_10,COL_POS_20
    ),
    values=scales::rescale(c(-20,-10,-5,0,5,10,20)),
    limits=c(-20,20),
    breaks=c(-20,-10,0,10,20),
    labels=c("<=-20","-10","0","+10",">=+20"),
    oob=scales::squish,
    na.value=COL_NA,
    name="Women - men\ndifference (pp)"
  ) +
  scale_x_continuous(
    breaks=domain_x,
    labels=unname(domain_labels[domain_plot_order]),
    expand=c(0,0),
    position="top"
  ) +
  scale_y_continuous(
    breaks=country_axis_c$y,
    labels=country_axis_c$first_faculty_country,
    limits=c(0.5,max(country_axis_c$y)+0.5),
    expand=c(0,0)
  ) +
  coord_cartesian(xlim=c(0.5,7.75),clip="off") +
  labs(
    title="c  Country × domain heterogeneity in gender differences",
    x=NULL,y=NULL
  ) +
  theme_classic(base_size=9,base_family="sans") +
  theme(
    plot.title.position="plot",
    plot.title=element_text(face="bold",size=12,colour="black",hjust=0),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.text.y=element_text(size=10,colour="black",margin=margin(r=5)),
    axis.text.x=element_text(size=10,colour="black",angle=30,hjust=0,vjust=0),
    legend.position="none",
    plot.margin=margin(t=8,r=6,b=4,l=4)
  )









# d. Read domain-by-cohort display estimates.
ed1d_plot_df <- read_sd("ED1d.xlsx","ed1d")

domain_order_d <- c(
  "Engineering and technology",
  "Agricultural and veterinary sciences",
  "Humanities and the arts",
  "Natural sciences",
  "Social sciences",
  "Medical and health sciences"
)
domain_labels_d <- c(
  "Engineering and technology"="Engineering",
  "Agricultural and veterinary sciences"="Agriculture",
  "Humanities and the arts"="Humanities",
  "Natural sciences"="Natural sciences",
  "Social sciences"="Social sciences",
  "Medical and health sciences"="Medical & health"
)
cohort_order <- c("1990-94","1995-99","2000-04","2005-09","2010-14","2015-19","2020-24")
cohort_plot_order <- c(cohort_order,"Overall")
cohort_x <- c(1,2,3,4,5,6,7,8.55)




P_ED1D <- ggplot(ed1d_plot_df,aes(x=x,y=y,fill=estimate_display)) +
  geom_tile(
    width=0.92,height=0.88,
    colour="grey94",linewidth=0.40
  ) +
  scale_fill_gradientn(
    colours=c(
      COL_NEG_20,COL_NEG_10,COL_NEG_5,
      COL_ZERO,
      COL_POS_5,COL_POS_10,COL_POS_20
    ),
    values=scales::rescale(c(-20,-10,-5,0,5,10,20)),
    limits=c(-20,20),
    breaks=c(-20,-10,0,10,20),
    labels=c("<=-20","-10","0","+10",">=+20"),
    oob=scales::squish,
    na.value=COL_NA,
    name="Women - men\ndifference (pp)"
  ) +
  scale_x_continuous(
    breaks=cohort_x,
    labels=c(
      "1990-94","1995-99","2000-04","2005-09",
      "2010-14","2015-19","2020-24","Overall"
    ),
    limits=c(0.5,9.05),
    expand=c(0,0),
    position="top"
  ) +
  scale_y_continuous(
    breaks=seq_along(domain_order_d),
    labels=rev(unname(domain_labels_d[domain_order_d])),
    limits=c(0.5,length(domain_order_d)+0.5),
    expand=c(0,0)
  ) +
  labs(
    title="d  Domain × cohort heterogeneity in gender differences",
    x=NULL,y=NULL
  ) +
  theme_classic(base_size=9,base_family="sans") +
  theme(
    plot.title.position="plot",
    plot.title=element_text(
      face="bold",size=12,colour="black",
      hjust=0,margin=margin(b=8)
    ),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.text.y=element_text(
      size=10,colour="black",margin=margin(r=5)
    ),
    axis.text.x=element_text(
      size=10,colour="black",angle=25,hjust=0,vjust=0
    ),
    legend.position="right",
    legend.title=element_text(size=10,lineheight=0.95),
    legend.text=element_text(size=10),
    legend.key.height=unit(1.05,"cm"),
    plot.margin=margin(t=8,r=8,b=4,l=6)
  )










####################
# Extended Data Figure 2

make_edfig2 <- function(source_dir) {
  issues <- character()
  issue <- function(x) { issues <<- c(issues, x); invisible(NULL) }
  read_sd <- function(file, sheet = 1, columns = NULL) read_source(source_dir, file, sheet, columns)

COL <- c("Women"="#D62728","Men"="#1F77B4")
PATHS <- c("Direct self-hiring","Internal pre-entry pathway","External pre-entry pathway")
PATH_COL <- c("Direct self-hiring"="#45B5AA","Internal pre-entry pathway"="#F2A65A","External pre-entry pathway"="#9B77D5","Institution-unresolved pre-entry pathway"="#999999","Total self-hiring"="#222222")
CUTS <- c(100,500,1500,2500,3500)



base_theme <- theme_classic(base_size=9) + theme(
  axis.title=element_text(size=9),axis.text=element_text(size=8,colour="#222222"),
  plot.title=element_text(size=11,face="bold",hjust=0),plot.title.position="plot",
  legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=8),
  plot.margin=margin(5,7,5,6))
rank_scale <- scale_x_log10(labels=scales::label_comma())








# a. Read additive contributions to the raw gender difference.
a_tab <- read_sd("ED2a.xlsx","P_ED2A") %>%
 mutate(pathway=check_levels(pathway,rev(c(PATHS,"Institution-unresolved pre-entry pathway","Total self-hiring")),"EDFig2a"))
assert_ci(a_tab,"estimate","low","high")

P_ED2A <- ggplot(a_tab,aes(estimate,pathway,colour=pathway)) +
  geom_vline(xintercept=0,linetype="dashed",colour="grey65") +
  geom_errorbarh(aes(xmin=low,xmax=high),height=.12,linewidth=.55) +
  geom_point(size=2.5) +
  geom_text(
    aes(label=sprintf("%+.2f",estimate)),
    hjust=-0.35,
    nudge_y=0.18,
    size=3.2,
    fontface="bold",
    show.legend=FALSE
  )+
  scale_colour_manual(values=PATH_COL,guide="none") +
  scale_x_continuous(
    expand=expansion(mult=c(0.08,0.18))
  ) +
  labs(
    title="a  Pathway-specific decomposition of gender gap",
    x="Women − men difference in self-hiring probability (pp)",
    y=NULL
  ) +
  coord_cartesian(clip="off") +
  base_theme








# b. Read standardized probability curves and gender differences.
b_curve <- read_sd("ED2b.xlsx","up"); b_gap <- read_sd("ED2b.xlsx","down")
b_curve$gender <- gender_display(b_curve$gender)
assert_ci(b_curve,"estimate","low","high"); assert_ci(b_gap,"estimate","low","high")

P_ED2B1 <- ggplot(b_curve,aes(rank,estimate,colour=gender,fill=gender)) +
  geom_vline(xintercept=CUTS,linetype="dotted",colour="grey75",linewidth=.3) +
  geom_ribbon(aes(ymin=low,ymax=high),alpha=.12,colour=NA) +
  geom_line(linewidth=.8) +
  rank_scale + scale_colour_manual(values=COL) +
  scale_fill_manual(values=COL,guide="none") +
  labs(x=NULL,y="Self-hiring probability (%)") + base_theme


P_ED2B2 <- ggplot(b_gap,aes(rank,estimate)) +
  geom_hline(yintercept=0,linetype="dashed",colour="grey65",linewidth=.4) +
  geom_ribbon(aes(ymin=low,ymax=high),fill="#4C9A9A",alpha=.13) +
  geom_line(colour="#4C9A9A",linewidth=.8) + rank_scale +
  labs(x="Doctoral-institution global rank (log scale)",
       y="Women − men (pp)") + base_theme



P_ED2B <- (P_ED2B1 / P_ED2B2) +
  plot_layout(heights=c(1.5,.75)) +
  plot_annotation(title="b  Adjusted self-hiring across continuous hierarchy",
                  theme=theme(plot.title=element_text(size=11,face="bold")))







# c. Read multinomial pathway curves; baseline is a separate input.
c_curves <- read_sd("ED2c.xlsx","c") %>% mutate(pathway=check_levels(pathway,PATHS,"EDFig2c"))
assert_ci(c_curves,"estimate","low","high")
if(file.exists(file.path(source_dir,"ED2c_baseline.xlsx"))) {
 c_base <- read_sd("ED2c_baseline.xlsx",columns=c("pathway","baseline")) %>%
  mutate(pathway=check_levels(pathway,PATHS,"EDFig2c baseline"))
 stopifnot(nrow(c_base)==3L,!anyDuplicated(c_base$pathway),all(is.finite(c_base$baseline)),
           all(c_base$baseline>=0 & c_base$baseline<=100))
} else {
 issue("EDFig2c: overall adjusted pathway baselines missing; add ED2c_baseline.xlsx.")
 c_base <- tibble(pathway=factor(character(),levels=PATHS),baseline=double())
}



P_ED2C <- ggplot(c_curves,aes(rank,estimate,colour=pathway,fill=pathway)) +
  geom_hline(data=c_base,aes(yintercept=baseline),inherit.aes=FALSE,
             linetype="dashed",colour="grey65",linewidth=.4) +
  geom_ribbon(aes(ymin=low,ymax=high),alpha=.12,colour=NA) +
  geom_line(linewidth=.85) +
  facet_wrap(~pathway,nrow=1) +
  scale_x_log10(labels=scales::label_comma(),breaks=c(10,100,1000,4000)) +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,20)) +
  scale_colour_manual(values=PATH_COL,guide="none") +
  scale_fill_manual(values=PATH_COL,guide="none") +
  labs(title="c  Adjusted pathway composition across doctoral rank",
       x="Doctoral-institution global rank (log scale)",
       y="Adjusted share among self-hires (%)") +
  base_theme + theme(strip.background=element_blank(),strip.text=element_text(size=9,face="bold"),
                     panel.spacing=grid::unit(.8,"lines"))







# d. Read model-sensitivity and threshold curves.
d_forest <- read_sd("ED2d.xlsx","d_left")
d_threshold <- read_sd("ED2d.xlsx","d_right")
assert_ci(d_forest,"estimate","low","high"); assert_ci(d_threshold,"estimate","low","high")
SIMILAR_MAIN <- .10
d_main_shares <- d_threshold %>% filter(abs(threshold-SIMILAR_MAIN)<1e-8)

d_lim <- range(c(d_forest$low,d_forest$high,0),finite=TRUE)
d_pad <- max(.035,diff(d_lim)*.13)
d_label_x <- d_lim[2]+d_pad*.55
d_stripes <- tibble(y=seq_len(nrow(d_forest))) %>%
  filter(y%%2==0)




P_ED2D1 <- ggplot(d_forest,aes(x=estimate,y=y)) +
  geom_rect(
    data=d_stripes,
    aes(xmin=-Inf,xmax=Inf,ymin=y-.5,ymax=y+.5),
    inherit.aes=FALSE,
    fill="grey97",
    colour=NA
  ) +
  geom_vline(
    xintercept=0,
    linetype="dashed",
    colour="grey65",
    linewidth=.45
  ) +
  geom_segment(
    aes(x=low,xend=high,yend=y),
    colour="#666666",
    linewidth=.65,
    lineend="round"
  ) +
  geom_point(
    colour="#4C9A9A",
    size=2.8
  ) +
  geom_text(
    aes(x=d_label_x,label=label),
    hjust=0,
    size=3,
    fontface="bold",
    colour="#333333"
  ) +
  scale_y_continuous(
    breaks=d_forest$y,
    labels=as.character(d_forest$specification),
    limits=c(.5,nrow(d_forest)+.5),
    expand=expansion(mult=0)
  ) +
  scale_x_continuous(
    breaks=scales::breaks_pretty(n=4)
  ) +
  coord_cartesian(
    xlim=c(d_lim[1]-d_pad,d_label_x+d_pad*1.2),
    clip="off"
  ) +
  labs(
    title="Measurement specifications",
    x="Women − men movement (log10 rank ratio)",
    y=NULL
  ) +
  base_theme +
  theme(
    plot.title=element_text(size=10,face="bold",hjust=.5),
    axis.text.y=element_text(size=8,colour="black",lineheight=.95),
    axis.text.x=element_text(size=8.5,colour="black"),
    axis.title.x=element_text(size=9,margin=margin(t=6)),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid=element_blank(),
    plot.margin=margin(6,12,8,6)
  )


DIRECTION_COL <- c(
  "Higher-ranked"="#7A6F9B",
  "Lower-ranked"="#4C9A9A"
)




P_ED2D2 <- ggplot(
  d_threshold,
  aes(x=threshold,y=estimate,colour=direction,fill=direction,group=direction)
) +
  geom_hline(
    yintercept=0,
    linetype="dashed",
    colour="grey65",
    linewidth=.45
  ) +
  geom_vline(
    xintercept=SIMILAR_MAIN,
    linetype="dotted",
    colour="grey45",
    linewidth=.55
  ) +
  geom_ribbon(
    aes(ymin=low,ymax=high),
    colour=NA,
    alpha=.10
  ) +
  geom_line(linewidth=.95) +
  geom_point(
    data=d_main_shares,
    size=2.6,
    stroke=.4,
    show.legend=FALSE
  ) +
  scale_colour_manual(
    values=DIRECTION_COL,
    breaks=c("Higher-ranked","Lower-ranked"),
    labels=c("Higher-ranked","Lower-ranked"),
    name=NULL
  ) +
  scale_fill_manual(values=DIRECTION_COL,guide="none") +
  scale_x_continuous(
    breaks=scales::breaks_pretty(n=5),
    expand=expansion(mult=c(.02,.03))
  ) +
  labs(
    title="Movement classification threshold",
    x="Similar-ranked tolerance: |log10 rank ratio|",
    y="Women − men share (pp)"
  ) +
  base_theme +
  theme(
    plot.title=element_text(size=10,face="bold",hjust=.5),
    axis.text=element_text(size=8.5,colour="black"),
    axis.title=element_text(size=9),
    legend.position="bottom",
    legend.direction="horizontal",
    legend.text=element_text(size=8.5),
    legend.key.width=grid::unit(1.2,"lines"),
    legend.margin=margin(0,0,0,0),
    panel.grid.minor=element_blank(),
    plot.margin=margin(6,10,5,6)
  )



P_ED2D <- (P_ED2D1 | P_ED2D2) +
  plot_layout(widths=c(1.2,1)) +
  plot_annotation(
    title="d  External-mobility measurement sensitivity",
    theme=theme(
      plot.title=element_text(size=11,face="bold",hjust=0),
      plot.title.position="plot"
    )
  )








# e. Read conditional external-rank predictions. Estimates are log10
# ranks; y-axis labels display 10^estimate, matching the original figure.
e_gender_curves <- read_sd("ED2e.xlsx","e_left")
e_curve <- read_sd("ED2e.xlsx","e_right")
e_gender_curves$gender <- gender_display(e_gender_curves$gender)
assert_ci(e_gender_curves,"estimate","low","high"); assert_ci(e_curve,"estimate","low","high")
e_equal <- tibble(rank=sort(unique(e_gender_curves$rank)),
 external_log_rank=log10(sort(unique(e_gender_curves$rank))))



P_ED2E1 <- ggplot(e_gender_curves,aes(rank,estimate,colour=gender,fill=gender)) +
  geom_line(data=e_equal,aes(x=rank,y=external_log_rank),inherit.aes=FALSE,
            linetype="dashed",colour="grey65",linewidth=.4) +
  geom_ribbon(aes(ymin=low,ymax=high),alpha=.12,colour=NA) +
  geom_line(linewidth=.85) +
  rank_scale + scale_y_continuous(labels=function(x) scales::label_comma()(10^x)) +
  scale_colour_manual(values=COL) + scale_fill_manual(values=COL,guide="none") +
  labs(title="Adjusted external rank at the same doctoral rank",x=NULL,
       y="External-institution rank (log scale)") +
  base_theme + theme(plot.title=element_text(size=9,face="plain"),
                     legend.position="bottom")



P_ED2E2 <- ggplot(e_curve,aes(rank,estimate)) +
  geom_hline(yintercept=0,linetype="dashed",colour="grey65",linewidth=.4) +
  geom_ribbon(aes(ymin=low,ymax=high),fill="#4C9A9A",alpha=.16) +
  geom_line(colour="#4C9A9A",linewidth=.9) + rank_scale +
  labs(title="Gender contrast at equal doctoral rank",
       x="Doctoral-institution global rank (log scale)",
       y="Women − men movement (log10 ratio)") +
  base_theme + theme(plot.title=element_text(size=9,face="plain"))





P_ED2E1 <- P_ED2E1 +
  theme(
    plot.title=element_text(size=10,face="bold",hjust=0.5)
  )
P_ED2E2 <- P_ED2E2 +
  theme(
    plot.title=element_text(size=10,face="bold",hjust=0.5)
  )
P_ED2E <- (P_ED2E1 | P_ED2E2) +
  plot_layout(widths=c(1,1)) +
  plot_annotation(
    title="e  Gender and doctoral hierarchy in external movement",
    theme=theme(
      plot.title=element_text(size=11,face="bold",hjust=0),
      plot.title.position="plot"
    )
  )










###########################
# Extended Data Figure 3

make_edfig3 <- function(source_dir) {
  issues <- character()
  issue <- function(x) { issues <<- c(issues, x); invisible(NULL) }
  read_sd <- function(file, sheet = 1, columns = NULL) read_source(source_dir, file, sheet, columns)

COL_WOMEN <- "#D62728"
COL_MEN <- "#1F77B4"
COL_PURPLE <- "#6A1B6A"
COL_ZERO <- "grey55"
eps_grid <- c(0.01,0.025,0.05,0.075,0.10,0.15,0.20)
theme_ed <- function(base_size=10.5){
  theme_classic(base_size=base_size)+
    theme(
      plot.title=element_text(face="bold",size=11.5,hjust=0),
      plot.subtitle=element_text(size=9.2,colour="grey30",margin=margin(b=5)),
      axis.title=element_text(size=9.5),
      axis.text=element_text(size=8.8,colour="black"),
      legend.title=element_blank(),
      legend.position="top",
      legend.text=element_text(size=8.8),
      strip.background=element_blank(),
      strip.text=element_text(face="bold",size=9.3),
      panel.spacing=unit(0.7,"lines"),
      plot.margin=margin(5,6,5,5)
    )
}




# a and b. Draw the exported density coordinates directly: geom_density()
# would incorrectly estimate a new density of these already-smoothed values.
plot_density_source <- function(file, prefix, xlab, title) {
 d <- read_sd(file,paste0(prefix,"_density"))
 med <- read_sd(file,paste0(prefix,"_median"))
 d$gender <- gender_display(d$gender); med$gender <- gender_display(med$gender)
 d$hire_status <- check_levels(d$hire_status,c("Self-hire","Non-self-hire"),file)
 med$hire_status <- check_levels(med$hire_status,c("Self-hire","Non-self-hire"),file)
 stopifnot(!anyDuplicated(d[c("hire_status","gender","log_value")]),
           all(is.finite(d$density)),all(d$density>=0))
 if("display" %in% readxl::excel_sheets(file.path(source_dir,file))) {
  xmax <- read_sd(file,"display",columns="display_max")$display_max
  stopifnot(length(xmax)==1L,is.finite(xmax),xmax>0)
 } else {
  issue(paste0(file,": original 99.5th-percentile display limit missing; add display sheet with display_max."))
  xmax <- max(d$log_value)
 }
 ggplot(d,aes(log_value,density,colour=gender,fill=gender)) +
  geom_area(alpha=.12,position="identity",colour=NA) + geom_line(linewidth=.8) +
  geom_vline(data=med,aes(xintercept=weighted_median,colour=gender),linetype="dashed",linewidth=.55,show.legend=FALSE) +
  facet_wrap(~hire_status,nrow=1) +
  scale_colour_manual(values=c(Women=COL_WOMEN,Men=COL_MEN),breaks=c("Women","Men")) +
  scale_fill_manual(values=c(Women=COL_WOMEN,Men=COL_MEN),guide="none") +
  coord_cartesian(xlim=c(0,xmax),ylim=c(0,1)) +
  labs(title=title,x=xlab,y="Density",colour=NULL) + theme_ed()
}


p_a <- plot_density_source("ED3a.xlsx","Fig_a","log(1 + pre-entry publications)","a   Distribution of pre-entry publications")
p_b <- plot_density_source("ED3b.xlsx","Fig_b","log(1 + pre-entry mean JIF)","b   Distribution of pre-entry mean JIF")
# c. Read the gender-by-pathway contrasts.
route_gender <- assert_ci(read_sd("ED3c.xlsx","c")) %>%
 mutate(route=check_levels(route,rev(route_levels),"EDFig3c"))





COL_DIFF <- "#4C9A9A"
make_gender_mod_plot <- function(dat,title,xlab){
  ggplot(dat,aes(x=estimate,y=route)) +
    geom_vline(xintercept=0,linetype="dashed",linewidth=.45,colour=COL_ZERO) +
    geom_segment(
      aes(x=conf_low,xend=conf_high,y=route,yend=route),
      linewidth=.75,colour=COL_DIFF
    ) +
    geom_point(size=2.6,colour=COL_DIFF) +
    geom_text(
      aes(label=sprintf("%.2f",estimate)),
      nudge_y=0.22,size=3,colour=COL_DIFF
    ) +
    scale_x_continuous(breaks=seq(-0.5,1.5,0.5)) +
    coord_cartesian(xlim=c(-0.5,1.6)) +
    scale_y_discrete(expand=expansion(add=0.45)) +
    labs(title=title,x=xlab,y=NULL) +
    theme_ed() +
    theme(legend.position="none")
}



p_c1 <- make_gender_mod_plot(
  filter(route_gender,outcome_label=="Pre-entry publications"),
  "Pre-entry publications",
  "Women − men difference\nin pathway contrast (publications)"
)
p_c2 <- make_gender_mod_plot(
  filter(route_gender,outcome_label=="Mean journal JIF"),
  "Mean journal JIF",
  "Women − men difference\nin pathway contrast (mean JIF)"
) +
  theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )




p_c <- (p_c1|p_c2)+
  plot_annotation(
    title="c  Gender differences in within-destination pathway contrasts",
    theme=theme(
      plot.title=element_text(face="bold",size=11.5),
      plot.subtitle=element_text(size=9.2,colour="grey30")
    )
  )







# d and e. Read sensitivity estimates and intervals.
boundary_grid <- assert_ci(read_sd("ED3d&3e.xlsx","d_e"))

make_bandwidth_plot <- function(dat,title,ylab){
  ggplot(dat,aes(x=epsilon,y=estimate))+
    geom_hline(
      yintercept=0,
      linetype="dashed",
      linewidth=.45,
      colour=COL_ZERO
    )+
    geom_ribbon(
      aes(ymin=conf_low,ymax=conf_high),
      fill=COL_PURPLE,
      alpha=.10
    )+
    geom_line(
      colour=COL_PURPLE,
      linewidth=.75
    )+
    geom_point(
      colour=COL_PURPLE,
      size=2.3
    )+
    scale_x_continuous(
      breaks=c(.025,.05,.10,.15,.20),
      labels=c(".025",".05",".10",".15",".20")
    )+
    scale_y_continuous(breaks=seq(-2,0.2,0.5)) +
    coord_cartesian(ylim=c(-2,0.2)) +
    labs(
      title=title,
      x="SpringRank difference threshold (epsilon)",
      y=ylab
    )+
    theme_ed()+
    theme(legend.position="none")
}




p_d <- make_bandwidth_plot(
  filter(boundary_grid,outcome=="pre3_pubs_primary"),
  "d   Hierarchy-neutral boundary contrast: publications",
  "Self-hire − non-self-hire\ndifference in publications"
)+
  theme(
    plot.title.position="plot",
    plot.title=element_text(
      face="bold",
      hjust=0
    )
  )
p_e <- make_bandwidth_plot(
  filter(boundary_grid,outcome=="pre3_primary_jif_mean"),
  "e   Hierarchy-neutral boundary contrast: mean JIF",
  "Self-hire − non-self-hire\ndifference in mean JIF"
)+
  theme(
    plot.title.position="plot",
    plot.title=element_text(
      face="bold",
      hjust=0
    )
  )









################
# Extended Data Figure 4

make_edfig4 <- function(source_dir) {
  issues <- character()
  issue <- function(x) { issues <<- c(issues, x); invisible(NULL) }
  read_sd <- function(file, sheet = 1, columns = NULL) read_source(source_dir, file, sheet, columns)

COL_WOMEN <- "#D62728"
COL_MEN <- "#1F77B4"
COL_PURPLE <- "#6A1B6A"
COL_BLUE <- COL_MEN
COL_ZERO <- "grey55"
theme_ed <- function(base_size=10.5){
  theme_classic(base_size=base_size)+
    theme(
      plot.title=element_text(face="bold",size=11.5,hjust=0),
      plot.subtitle=element_text(size=9.2,colour="grey30",margin=margin(b=5)),
      axis.title=element_text(size=9.5),
      axis.text=element_text(size=8.8,colour="black"),
      legend.title=element_blank(),
      legend.position="top",
      legend.text=element_text(size=8.8),
      strip.background=element_blank(),
      strip.text=element_text(face="bold",size=9.2),
      panel.spacing=unit(.7,"lines"),
      plot.margin=margin(5,6,5,5)
    )
}






# a. Read pooled publication-prestige sensitivity coefficients.
pub_spec <- assert_ci(read_sd("ED4a.xlsx")) %>%
 mutate(spec=factor(spec,levels=rev(unique(spec))))

p_a <- ggplot(pub_spec,aes(x=estimate,y=spec)) +
  geom_vline(
    xintercept=0,
    linetype="dashed",
    linewidth=.45,
    colour=COL_ZERO
  ) +
  geom_segment(
    aes(x=conf_low,xend=conf_high,y=spec,yend=spec),
    linewidth=.75,
    colour=COL_PURPLE
  ) +
  geom_point(
    size=2.6,
    colour=COL_PURPLE
  ) +
  geom_text(
    aes(label=sprintf("%.3f",estimate)),
    nudge_y=0.3,
    size=3,
    colour=COL_PURPLE
  ) +
  labs(
    title="a  Publication–prestige association: model sensitivity",
    x="Self-hire × first-faculty prestige coefficient",
    y=NULL
  ) +
  scale_x_continuous(
    breaks=seq(-1,0.5,0.25)
  ) +
  coord_cartesian(xlim=c(-1.1,0.6)) +
  theme_ed() +
  theme(
    plot.title.position="plot",
    plot.title=element_text(
      face="bold",
      size=11.5,
      hjust=0
    ),
    legend.position="none"
  )






# b. Read model-specific gender interactions; no cross-scale conversion.
pub_gender_rob <- assert_ci(read_sd("ED4b.xlsx")) %>%
 mutate(spec=factor(spec,levels=rev(unique(spec))))

p_b <- ggplot(pub_gender_rob,aes(x=estimate,y=spec)) +
  geom_vline(
    xintercept=0,
    linetype="dashed",
    linewidth=.45,
    colour=COL_ZERO
  ) +
  geom_segment(
    aes(x=conf_low,xend=conf_high,y=spec,yend=spec),
    linewidth=.75,
    colour=COL_WOMEN
  ) +
  geom_point(
    size=2.6,
    colour=COL_WOMEN
  ) +
  geom_text(
    aes(label=sprintf("%.3f",estimate)),
    nudge_y=0.22,
    size=3,
    colour=COL_WOMEN
  ) +
  labs(
    title="b  Gender difference in publication–prestige gradients",
    x="Women − men difference in\nself-hire × prestige slope",
    y=NULL
  ) +
  scale_x_continuous(
    breaks=seq(-1,0.5,0.25)
  ) +
  coord_cartesian(xlim=c(-1.1,0.6)) +
  theme_ed() +
  theme(
    plot.title.position="plot",
    plot.title=element_text(
      face="bold",
      size=11.5,
      hjust=0
    ),
    legend.position="none"
  )







# c. Read pooled and gender-interaction JIF coefficients.
# Workbook sheet names are reversed relative to the plotted subpanel position:
# RIGHT contains pooled coefficients, plotted on the left; LEFT contains
# women-minus-men slope differences, plotted on the right.
jif_pooled_rob <- assert_ci(read_sd("ED4c.xlsx","RIGHT")) %>%
 mutate(spec=factor(spec,levels=rev(unique(spec))))
jif_gender_rob <- assert_ci(read_sd("ED4c.xlsx","LEFT")) %>%
 mutate(spec=factor(spec,levels=rev(unique(spec))))
COL_DIFF <- "#4C9A9A"
xlim_c1 <- xlim_c2 <- c(-.4,.3)

p_c1 <- ggplot(jif_pooled_rob,aes(x=estimate,y=spec)) +
  geom_vline(xintercept=0,linetype="dashed",linewidth=.45,colour=COL_ZERO) +
  geom_segment(
    aes(x=conf_low,xend=conf_high,y=spec,yend=spec),
    linewidth=.75,colour=COL_PURPLE
  ) +
  geom_point(size=2.6,colour=COL_PURPLE) +
  geom_text(
    aes(label=sprintf("%.3f",estimate)),
    nudge_y=0.22,size=3,colour=COL_PURPLE
  ) +
  scale_x_continuous(breaks=seq(-0.4,0.3,0.1)) +
  scale_y_discrete(expand=expansion(add=0.45)) +
  coord_cartesian(xlim=xlim_c1) +
  labs(
    title="Pooled contrast",
    x="Self-hire × first-faculty\nprestige coefficient",
    y=NULL
  ) +
  theme_ed() +
  theme(legend.position="none")
p_c2 <- ggplot(jif_gender_rob,aes(x=estimate,y=spec)) +
  geom_vline(xintercept=0,linetype="dashed",linewidth=.45,colour=COL_ZERO) +
  geom_segment(
    aes(x=conf_low,xend=conf_high,y=spec,yend=spec),
    linewidth=.75,colour=COL_DIFF
  ) +
  geom_point(size=2.6,colour=COL_DIFF) +
  geom_text(
    aes(label=sprintf("%.3f",estimate)),
    nudge_y=0.22,size=3,colour=COL_DIFF
  ) +
  scale_x_continuous(breaks=seq(-0.4,0.3,0.1)) +
  scale_y_discrete(expand=expansion(add=0.45)) +
  coord_cartesian(xlim=xlim_c2) +
  labs(
    title="Women − men slope difference",
    x="Women − men difference in\nself-hire × prestige slope",
    y=NULL
  ) +
  theme_ed() +
  theme(
    legend.position="none",
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
  )
p_c <- (p_c1 | p_c2) +
  plot_annotation(
    title="c  Journal-impact prestige gradients: model sensitivity",
    theme=theme(
      plot.title=element_text(face="bold",size=11.5,hjust=0)
    )
  )








# d. Read average marginal gender differences after scholarly adjustment.
gap_plot <- read_sd("ED4d.xlsx")
assert_ci(gap_plot,"estimate_pp","conf_low_pp","conf_high_pp")
x_pad <- .08

p_f <- ggplot(gap_plot,aes(x=estimate_pp,y=y_pos)) +
  geom_vline(
    xintercept=0,
    linetype="dashed",
    linewidth=.45,
    colour=COL_ZERO
  ) +
  geom_hline(
    yintercept=5.5,
    linetype="dashed",
    linewidth=.45,
    colour="grey65"
  ) +
  geom_errorbarh(
    aes(xmin=conf_low_pp,xmax=conf_high_pp),
    height=.10,
    linewidth=.7,
    colour=COL_PURPLE
  ) +
  geom_point(
    size=3,
    colour=COL_PURPLE
  ) +
  geom_text(
    aes(
      x=conf_high_pp+x_pad,
      label=sprintf("%.2f",estimate_pp)
    ),
    hjust=0,
    size=3,
    colour=COL_PURPLE
  ) +
  annotate(
    "text",
    x=-Inf,y=6.38,
    label="Full analytical sample",
    hjust=-0.05,vjust=0,
    size=3,fontface="bold",colour="grey30"
  ) +
  annotate(
    "text",
    x=-Inf,y=5.38,
    label="Common scholarly sample",
    hjust=-0.05,vjust=0,
    size=3,fontface="bold",colour="grey30"
  ) +
  scale_y_continuous(
    breaks=gap_plot$y_pos,
    labels=gap_plot$row_lab,
    limits=c(.6,6.65),
    expand=c(0,0)
  ) +
  scale_x_continuous(
    expand=expansion(mult=c(.04,.16))
  ) +
  labs(
    title="d  Adjustment for pre-entry scholarly records",
    x="Women − men difference in\nadjusted self-hiring probability (pp)",
    y=NULL
  ) +
  theme_ed() +
  theme(
    plot.title.position="plot",
    legend.position="none",
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid=element_blank()
  )

row1 <- (p_a | p_b) + plot_layout(widths=c(1,1))
row2 <- wrap_elements(full=patchworkGrob(p_c),clip=FALSE)
row3 <- (p_f | plot_spacer()) + plot_layout(widths=c(0.8,1))
ED_FIG3_2 <- (
  wrap_elements(full=patchworkGrob(row1),clip=FALSE) /
    row2 /
    row3
) +
  plot_layout(heights=c(1,1.1,0.8))
ed_fig4 <- ED_FIG3_2

list(plot = ED_FIG3_2, width = 9, height = 9, units = "in", issues = unique(issues))
}










#####################
# Extended Data Figure 5

make_edfig5 <- function(source_dir) {
  issues <- character()
  issue <- function(x) { issues <<- c(issues, x); invisible(NULL) }
  read_sd <- function(file, sheet = 1, columns = NULL) read_source(source_dir, file, sheet, columns)

GENDERS <- c("Female","Male")
COL <- c("Female"="#D62728","Male"="#1F77B4")
PERIODS <- c("0–2 years","3–5 years","6–8 years")
PATHWAYS <- c("Direct self-hire","Internal retention","External return")






# a. Read the original CEM balance diagnostics; no matching object is needed.
if(file.exists(file.path(source_dir,"ED5a.xlsx"))) {
 balance_ed <- read_sd("ED5a.xlsx",columns=c("label","unmatched","matched")) %>%
  arrange(unmatched) %>% mutate(label=factor(label,levels=label))
 stopifnot(all(is.finite(balance_ed$unmatched)),all(is.finite(balance_ed$matched)),
           all(balance_ed$unmatched>=0),all(balance_ed$matched>=0))
 balance_long <- balance_ed %>% pivot_longer(c(unmatched,matched),names_to="sample",values_to="smd")
 p_a1 <- ggplot(balance_long,aes(smd,label,colour=sample,shape=sample)) +
  geom_vline(xintercept=.1,linetype="dashed",colour="grey55") + geom_point(size=2.6) +
  scale_colour_manual(values=c(unmatched="grey50",matched="black"),labels=c("Matched","Unmatched")) +
  scale_shape_manual(values=c(unmatched=17,matched=16),labels=c("Matched","Unmatched")) +
  labs(title="a  CEM covariate balance",x="Absolute standardized mean difference",y=NULL,colour="Sample",shape="Sample") +
  theme_classic(base_size=10) + theme(legend.position="bottom",plot.title=element_text(face="bold",hjust=0))
} else {
 issue("EDFig5a: ED5a.xlsx missing; export label, unmatched and matched absolute SMDs.")
 p_a1 <- missing_panel("a  CEM covariate balance","Matching balance Source Data
ED5a.xlsx is missing")
}







# b and c. Read overall period-averaged DID estimates.
results_overall <- read_sd("ED5b&5c.xlsx")
assert_ci(results_overall,"estimate","conf.low","conf.high")






# d and e. Read pathway-specific period-averaged DID estimates.
results_pathway <- read_sd("ED5d&5e.xlsx")
assert_ci(results_pathway,"estimate","conf.low","conf.high")



theme_ed <- theme_classic(base_size=11) +
  theme(plot.tag=element_text(face="bold",size=13),
        plot.title=element_text(face="bold",size=12,hjust=.5),
        axis.text=element_text(colour="black",size=9.2),
        axis.title=element_text(size=10),legend.position="bottom",
        panel.spacing=grid::unit(1.3,"lines"))


plot_ed_periods <- function(data,outcome,tag,pathways=FALSE){
  d <- data %>%
    filter(.data$outcome==.env$outcome) %>%
    mutate(
      period=factor(period,levels=PERIODS),
      gender=factor(gender,levels=GENDERS),
      x=as.integer(period)+if_else(gender=="Female",-.09,.09)
    )
  p <- ggplot(d,aes(x=x,y=estimate,colour=gender,group=gender)) +
    geom_hline(yintercept=0,linetype="dashed",colour="grey65",linewidth=.45) +
    geom_errorbar(aes(ymin=conf.low,ymax=conf.high),width=.075,linewidth=.5,alpha=.8) +
    geom_line(linewidth=.7,alpha=.65) +
    geom_point(size=2.6) +
    scale_colour_manual(
      values=COL,
      labels=c("Female"="Women","Male"="Men")
    ) +
    scale_x_continuous(
      breaks=1:3,
      labels=PERIODS,
      limits=c(.7,3.3)
    ) +
    labs(
      title=paste0(
        tag,"  ",
        if(pathways) "Pathway-specific DID · " else "Overall DID · ",
        outcome
      ),
      x="Years since first faculty entry",
      y="Self-hire − non-self-hire DID (log1p units)",
      colour=NULL
    ) +
    theme_ed +
    theme(
      plot.title=element_text(size=13,face="bold",hjust=0),
      plot.title.position="plot",
      plot.margin=margin(8,10,8,8)
    )
  if(pathways){
    p <- p + facet_wrap(
      ~factor(pathway,levels=PATHWAYS),
      nrow=1
    )
  }
  p
}







P_ED_B <- plot_ed_periods(
  results_overall,"Publications","b"
) +
  theme(
    legend.position="none",
    axis.title.x=element_blank()
  )
P_ED_C <- plot_ed_periods(
  results_overall,"JIF","c"
) +
  theme(
    legend.position="none",
    axis.title.x=element_blank()
  )
P_ED_D <- plot_ed_periods(
  results_pathway,"Publications","d",pathways=TRUE
) +
  theme(
    legend.position="none",
    axis.title.x=element_blank()
  )
P_ED_E <- plot_ed_periods(
  results_pathway,"JIF","e",pathways=TRUE
)



P_ED_B <- P_ED_B +
  scale_y_continuous(
    breaks=seq(-0.20,0.15,0.05),
    labels=function(x) sprintf("%.2f",x)
  ) +
  coord_cartesian(ylim=c(-0.20,0.15))
P_ED_C <- P_ED_C +
  scale_y_continuous(
    breaks=seq(-0.20,0.15,0.05),
    labels=function(x) sprintf("%.2f",x)
  ) +
  coord_cartesian(ylim=c(-0.20,0.15))
P_ED_D <- P_ED_D +
  scale_y_continuous(
    breaks=seq(-0.20,0.15,0.05),
    labels=function(x) sprintf("%.2f",x)
  ) +
  coord_cartesian(ylim=c(-0.20,0.15))
P_ED_E <- P_ED_E +
  scale_y_continuous(
    breaks=seq(-0.20,0.15,0.05),
    labels=function(x) sprintf("%.2f",x)
  ) +
  coord_cartesian(ylim=c(-0.20,0.15))






