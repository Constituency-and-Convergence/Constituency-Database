# overviews and applications for discussion section

if (!require("pacman")) install.packages("pacman")
p_load(ggmap,
       ggrepel,
       ggsci,
       ggthemes,
       here,
       janitor,
       tidyverse,
       viridis)

# read in database
domains <- read_tsv(here("domains.tsv")) %>%
  modify_if(is.character, as.factor)

# list all abstract types
levels(domains$Abstract_Type)


# create map of sample languages of volume
# read in metadata file
metadata <- read_tsv(here("input/metadata.tsv"))
# subset for languages of volume 1 and Chacobo and Siksika
metadata_sub <- metadata %>%
  filter(Contribution=="Vol1"|Language_ID=="siks1238"|Language_ID=="chac1251")

# find min/max values for map
summary(metadata_sub)
# register API key for Stadia
register_stadiamaps("3a811dbc-3a12-4a65-a604-94e474a482af", write = FALSE)

# get basemap
americas <- get_stadiamap(bbox = c(left=-167, bottom=-36, right=-40, top=64), maptype = "stamen_terrain", zoom = 5)

# add points for languages
sample_map <- ggmap(americas) +
  geom_point(aes(x = Longitude, y = Latitude), data = metadata_sub, alpha = 1, size = 3) +
  geom_label_repel(aes(x = Longitude, y = Latitude, label = Short_Name), data = metadata_sub, box.padding = unit(.3, "lines"), label.padding = unit(.2, "lines"), max.overlaps = 30, size = 3) +
  theme_map()
sample_map

# color points for max number of convergences
# merge with domains file
metadata_sub_conv <- domains %>%
  select(Language_ID, Relative_Convergence) %>%
  group_by(Language_ID) %>%
  slice(which.max(Relative_Convergence)) %>%
  left_join(., metadata_sub)

# update map
sample_map_relconv <- ggmap(americas) +
  geom_point(aes(x = Longitude, y = Latitude, fill = Relative_Convergence), data = metadata_sub_conv, size = 4, pch = 21) +
  scale_fill_viridis(option = "inferno", direction = -1, end = 0.9,
                     name = "Max. Relative Convergence",
                     breaks = seq(0, 0.45, by = 0.05)) +
  geom_label_repel(aes(x = Longitude, y = Latitude, label = Short_Name), data = metadata_sub, box.padding = unit(.3, "lines"), label.padding = unit(.2, "lines"), max.overlaps = 30, size = 5) +
  theme_map() +
  theme(legend.position = c(0.05,0.05),
        legend.direction = "vertical",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))
sample_map_relconv

# export
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/map_languagesCCA_conv.png", sample_map_relconv, dpi = 600, height = 20, units = "cm")


# dot plot of convergences in the verbal domains
# exclude all that are not verbal; add short name for plotting
domains_verbal <- domains %>%
  filter(str_detect(Planar_ID, "verbal")) %>%
  filter(!is.na(Convergence)) %>%
  left_join(., select(metadata_sub, Language_ID, Short_Name)) %>%
  arrange(Relative_Size, Relative_Convergence)


rel_convergences_facet <- ggplot(aes(x = Relative_Size, y = Relative_Convergence, group = Language_Name), data = domains_verbal) +
  geom_point(aes(color = Relative_Convergence), size = 3) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~Short_Name, nrow = 6, ncol = 3, scales = "free_x") +
  ylab("Relative convergences") +
  xlab("Relative layer size") +
  scale_color_viridis(option = "mako", end = 0.7, guide = "none") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.2, "lines"))
rel_convergences_facet

ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_convergences_relative.png", rel_convergences_facet, height = 25, width = 17, device = "png", units = "cm", dpi = 600)


# tests and sizes in the verbal domain
# remove types with under 10 data points
include_at <- domains_verbal %>%
  count(Abstract_Type) %>%
  arrange(desc(n)) %>%
  filter(n>10) %>%
  droplevels() %>%
  pull(Abstract_Type)
domains_verbal_sub <- domains_verbal %>%
  filter(Abstract_Type %in% include_at) %>%
  droplevels()

test_sizes <- ggplot(aes(x = Relative_Size, group = Abstract_Type), data = domains_verbal_sub) +
  geom_density(aes(fill = Abstract_Type, color = Abstract_Type), alpha = 0, linewidth = 1) +
  scale_color_d3(palette = "category10", guide = "none") +
  scale_fill_d3(palette = "category10", name = "Abstract Type") +
  guides(fill = guide_legend(override.aes = list(alpha = 1, linewidth = 0))) +
  labs(x = "Relative size", y = "Density", ) +
  scale_x_continuous(expand = c(0.005, 0), breaks = seq(0, 1, 0.1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13))
test_sizes

# export plot
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_size_per_test.png", test_sizes, height = 12, width = 20, device = "png", units = "cm", dpi = 600)


# minimal and maximal domains per language
domains_mm <- domains_verbal_sub %>%
  filter(!is.na(MinMax_Fracture))

# plot density
minmax_sizes <- ggplot(aes(x = Relative_Size, group = MinMax_Fracture), data = domains_mm) +
  geom_density(aes(fill = MinMax_Fracture), alpha = 0.7) +
  facet_wrap(~Short_Name, scales = "free_x", nrow = 6, ncol = 3) +
  scale_fill_startrek(name = "Fracture") +
  #guides(fill = guide_legend(override.aes = list(alpha = 1, linewidth = 0))) +
  labs(x = "Relative size", y = "Density", ) +
  scale_x_continuous(expand = c(0.005, 0.005), breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  theme(legend.position = c(0.85,0.05),
        #legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.2, "lines"))

minmax_sizes
# export plot
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_minmax_per_lang.png", minmax_sizes, height = 27, width = 20, device = "png", units = "cm", dpi = 600)

