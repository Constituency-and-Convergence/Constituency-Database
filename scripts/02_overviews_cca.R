# overviews and applications for discussion chapter of 'Constituency and Convergence in the Americas

if (!require("pacman")) install.packages("pacman")
p_load(ggcorrplot,
       ggmap,
       ggrepel,
       ggpubr,
       ggsci,
       ggthemes,
       here,
       janitor,
       kableExtra,
       randomForest,
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
  geom_label_repel(aes(x = Longitude, y = Latitude, label = Short_Name), data = metadata_sub, box.padding = .6, point.padding = 1, max.overlaps = 30, min.segment.length = 0.1, size = 5) +
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


plot_rel_conv_facet <- ggplot(aes(x = Relative_Size, y = Relative_Convergence, group = Language_Name), data = domains_verbal) +
  geom_point(aes(), size = 2.5) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~Short_Name, nrow = 6, ncol = 3, scales = "free_x") +
  ylab("Relative convergences") +
  xlab("Relative span size") +
  #scale_color_viridis(option = "mako", end = 0.7, guide = "none") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.2, "lines"))
plot_rel_conv_facet

ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_convergences_relative.png", plot_rel_conv_facet, height = 25, width = 17, device = "png", units = "cm", dpi = 600)


# tests and sizes in the verbal domain
# remove types with under 10 data points
include_at <- domains_verbal %>%
  count(Abstract_Type) %>%
  arrange(desc(n)) %>%
  filter(n>5) %>%
  droplevels() %>%
  pull(Abstract_Type)
domains_verbal_sub <- domains_verbal %>%
  filter(Abstract_Type %in% include_at) %>%
  droplevels()

plot_test_sizes <- ggplot(aes(x = Relative_Size, group = Abstract_Type), data = domains_verbal_sub) +
  geom_density(aes(fill = Abstract_Type, color = Abstract_Type), alpha = 0, linewidth = 1) +
  scale_color_d3(palette = "category10", guide = "none") +
  scale_fill_d3(palette = "category10", name = "Abstract Type") +
  guides(fill = guide_legend(override.aes = list(alpha = 1, linewidth = 0))) +
  labs(x = "Relative span size", y = "Density", ) +
  scale_x_continuous(expand = c(0.005, 0), breaks = seq(0, 1, 0.1)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13))
plot_test_sizes

# export plot
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_size_per_test.png", plot_test_sizes, height = 12, width = 20, device = "png", units = "cm", dpi = 600)


# correlation plots
# recode the variables to numeric; sum overlaps by language and abstract type
abstract_type <- domains_verbal_sub %>%
  pivot_wider(., names_from = Abstract_Type, values_from = Abstract_Type) %>%
  mutate(across(nonpermutability:noninterruption, ~if_else(is.na(.x), 0, 1)))
# for the whole span
abstract_type_span <- abstract_type %>%
  group_by(Language_ID, Left_Edge, Right_Edge) %>%
  summarise(Nonpermut.=sum(nonpermutability),
            Noninterrupt.=sum(noninterruption),
            Selection.=sum(ciscategorial.selection),
            Segmental.=sum(segmental),
            Free_occur.=sum(free.occurrence),
            Recursion.=sum(recursion.based),
            Supraseg.=sum(suprasegmental),
            Deviations.=sum(deviations)) %>%
  ungroup() %>%
  select(-c(Language_ID, Left_Edge,Right_Edge))
glimpse(abstract_type_span)

# for the right edge
abstract_type_right <- abstract_type %>%
  group_by(Language_ID, Right_Edge) %>%
  summarise(Nonpermut.=sum(nonpermutability),
            Noninterrupt.=sum(noninterruption),
            Selection.=sum(ciscategorial.selection),
            Segmental.=sum(segmental),
            Free_occur.=sum(free.occurrence),
            Recursion.=sum(recursion.based),
            Supraseg.=sum(suprasegmental),
            Deviations.=sum(deviations)) %>%
  ungroup() %>%
  select(-c(Language_ID,Right_Edge))
glimpse(abstract_type_right)

# for the left edge
abstract_type_left <- abstract_type %>%
  group_by(Language_ID, Left_Edge) %>%
  summarise(Nonpermut.=sum(nonpermutability),
            Noninterrupt.=sum(noninterruption),
            Selection.=sum(ciscategorial.selection),
            Segmental.=sum(segmental),
            Free_occur.=sum(free.occurrence),
            Recursion.=sum(recursion.based),
            Supraseg.=sum(suprasegmental),
            Deviations.=sum(deviations)) %>%
  ungroup() %>%
  select(-c(Language_ID, Left_Edge))
glimpse(abstract_type_left)

# make correlation matrices
corr_span <- cor(abstract_type_span, method = "kendall")
corr_left <- cor(abstract_type_left, method = "kendall")
corr_right <- cor(abstract_type_right, method = "kendall")

# plot correlations
plot_corr_span <- ggcorrplot(corr_span,
           type = "lower",
           lab = TRUE,
           lab_size = 4)
plot_corr_span

plot_corr_left <- ggcorrplot(corr_left,
                             type = "lower",
                             lab = TRUE,
                             lab_size = 5,
                             tl.cex = 12)

plot_corr_right <- ggcorrplot(corr_right,
                             type = "lower",
                             lab = TRUE,
                             lab_size = 5,
                             tl.cex = 12)

# arrange plots for display
plots_corr_leftright <- ggarrange(plot_corr_left, plot_corr_right,
                                  #align = "v",
                                  legend = "top",
                                  common.legend = TRUE,
                                  labels = "auto")
plots_corr_leftright
# export plots
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_corr_span.png", plot_corr_span, height = 14, width = 16, device = "png", units = "cm", dpi = 600)
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_corr_lr.png", plots_corr_leftright, height = 14, width = 30, device = "png", units = "cm", dpi = 600)


# data set for comparing cross-language fractures and min-max domains
cross_l <- domains_verbal_sub %>%
  mutate(MinMax_Fracture = case_match(MinMax_Fracture,
                                      "minimal" ~ "min",
                                      "maximal" ~ "max")) %>%
  mutate(abstract_cross_l = case_when(Abstract_Type=="nonpermutability" ~ paste(Abstract_Type, CrossL_Fracture, sep = "_"),
                                      Abstract_Type=="noninterruption" ~ paste(Abstract_Type, CrossL_Fracture, sep = "_"),
                                      Abstract_Type=="ciscategorial.selection" ~ paste(Abstract_Type, MinMax_Fracture, sep = "_"),
                                      Abstract_Type=="free.occurrence" ~ paste(Abstract_Type, MinMax_Fracture, sep = "_"),
                                      Abstract_Type=="recursion.based" ~ paste(Abstract_Type, MinMax_Fracture, sep = "_"),
                                      Abstract_Type=="segmental" ~ paste(Abstract_Type, MinMax_Fracture, sep = "_"),
                                      Abstract_Type=="suprasegmental" ~ paste(Abstract_Type, MinMax_Fracture, sep = "_"))) %>%
  mutate(abstract_cross_l = str_remove_all(abstract_cross_l, "_NA"),
         abstract_cross_l = if_else(Abstract_Type=="deviations", Abstract_Type, abstract_cross_l)) %>%
  arrange(abstract_cross_l) %>%
  pivot_wider(., names_from = abstract_cross_l, values_from = abstract_cross_l) %>%
  mutate(across(ciscategorial.selection:suprasegmental_min, ~if_else(is.na(.x), 0, 1)))
# make data set for correlations using whole span
cross_l_span <- cross_l %>%
  group_by(Language_ID, Left_Edge, Right_Edge)%>%
  summarise(Noninterrupt_simpl = sum(noninterruption_simplex),
            Noninterrupt_compl = sum(noninterruption_complex),
            Noninterrupt_multipos = sum(noninterruption_multipositional),
            Nonpermut_rigid = sum(nonpermutability_rigid),
            Nonpermut_scopal = sum(nonpermutability_scopal),
            Selection_min = sum(ciscategorial.selection_min),
            Selection_max = sum(ciscategorial.selection_max),
            Recursion_min = sum(recursion.based_min),
            Recursion_max = sum(recursion.based_max),
            FreeOccur_min = sum(free.occurrence_min),
            FreeOccur_max = sum(free.occurrence_max),
            Deviations = sum(deviations),
            Segmental_min = sum(segmental_min),
            Segmental_max = sum(segmental_max),
            Supraseg_min = sum(suprasegmental_min),
            Supraseg_max = sum(suprasegmental_min)) %>%
  ungroup()
glimpse(cross_l_span)

# minimal domains and cross-language fractures
cross_l_min <- select(cross_l_span, -ends_with("_max"), -c(Language_ID:Right_Edge))
# maximal domains and cross-language fractures
cross_l_max <- select(cross_l_span, -ends_with("_min"), -c(Language_ID:Right_Edge))
# correlation matrices
corr_cross_l_min <- cor(cross_l_min, method = "kendall")
corr_cross_l_max <- cor(cross_l_max, method = "kendall")



# same but with edge convergences instead of whole span
# left edge:
cross_l_left <- cross_l %>%
  group_by(Language_ID, Left_Edge) %>%
  summarise(Noninterrupt_simpl = sum(noninterruption_simplex),
            Noninterrupt_compl = sum(noninterruption_complex),
            Noninterrupt_multipos = sum(noninterruption_multipositional),
            Nonpermut_rigid = sum(nonpermutability_rigid),
            Nonpermut_scopal = sum(nonpermutability_scopal),
            Selection_min = sum(ciscategorial.selection_min),
            Selection_max = sum(ciscategorial.selection_max),
            Recursion_min = sum(recursion.based_min),
            Recursion_max = sum(recursion.based_max),
            FreeOccur_min = sum(free.occurrence_min),
            FreeOccur_max = sum(free.occurrence_max),
            Deviations = sum(deviations),
            Segmental_min = sum(segmental_min),
            Segmental_max = sum(segmental_max),
            Supraseg_min = sum(suprasegmental_min),
            Supraseg_max = sum(suprasegmental_min)) %>%
  ungroup()

# right edge:
cross_l_right <- cross_l %>%
  group_by(Language_ID, Right_Edge) %>%
  summarise(Noninterrupt_simpl = sum(noninterruption_simplex),
            Noninterrupt_compl = sum(noninterruption_complex),
            Noninterrupt_multipos = sum(noninterruption_multipositional),
            Nonpermut_rigid = sum(nonpermutability_rigid),
            Nonpermut_scopal = sum(nonpermutability_scopal),
            Selection_min = sum(ciscategorial.selection_min),
            Selection_max = sum(ciscategorial.selection_max),
            Recursion_min = sum(recursion.based_min),
            Recursion_max = sum(recursion.based_max),
            FreeOccur_min = sum(free.occurrence_min),
            FreeOccur_max = sum(free.occurrence_max),
            Deviations = sum(deviations),
            Segmental_min = sum(segmental_min),
            Segmental_max = sum(segmental_max),
            Supraseg_min = sum(suprasegmental_min),
            Supraseg_max = sum(suprasegmental_min)) %>%
  ungroup()

# minimal domains and cross-language fractures - left edges
cross_l_min_left <- select(cross_l_left, -ends_with("_max"), -c(Language_ID:Left_Edge))
# maximal domains and cross-language fractures
cross_l_max_left <- select(cross_l_left, -ends_with("_min"), -c(Language_ID:Left_Edge))

# minimal domains and cross-language fractures - right edges
cross_l_min_right <- select(cross_l_right, -ends_with("_max"), -c(Language_ID:Right_Edge))
# maximal domains and cross-language fractures
cross_l_max_right <- select(cross_l_right, -ends_with("_min"), -c(Language_ID:Right_Edge))


# correlation matrices (left and right minimal, left and right maximal), combined for table export
# minimal
corr_cross_l_min_left <- cor(cross_l_min_left, method = "kendall") %>%
  as.data.frame() %>%
  rownames_to_column(., var = "Test1") %>%
  pivot_longer(-Test1, names_to = "Test2", values_to = "Min.Left")
corr_cross_l_min_right <- cor(cross_l_min_right, method = "kendall") %>%
  as.data.frame() %>%
  rownames_to_column(., var = "Test1") %>%
  pivot_longer(-Test1, names_to = "Test2", values_to = "Min.Right") %>%
  select(Min.Right)
# bind together, prepare for export; highlight cells with higher correlations;
corr_cross_l_min_lr <- bind_cols(corr_cross_l_min_left, corr_cross_l_min_right) %>%
  distinct(across(starts_with("M")), .keep_all = TRUE) %>%
  filter(Test1!=Test2) %>%
  mutate(across(starts_with("M"), ~round(.x, 2))) %>%
  mutate(across(starts_with("M"), ~ case_when(
    .x > 0.3 ~ paste0("\\cellcolor{red!45}", .x),
    between(.x, 0.2, 0.3) ~ paste0("\\cellcolor{red!25}", .x),
    .x < -0.3 ~ paste0("\\cellcolor{blue!45}", .x),
    between(.x, 0.2, 0.3) ~ paste0("\\cellcolor{blue!25}", .x),
    .default = paste(.x)))) %>%
  mutate(across(starts_with("Test"), ~ str_replace_all(.x, "_", ".")))

# maximal
corr_cross_l_max_left <- cor(cross_l_max_left, method = "kendall") %>%
  as.data.frame() %>%
  rownames_to_column(., var = "Test1") %>%
  pivot_longer(-Test1, names_to = "Test2", values_to = "Max.Left")
corr_cross_l_max_right <- cor(cross_l_max_right, method = "kendall") %>%
  as.data.frame() %>%
  rownames_to_column(., var = "Test1") %>%
  pivot_longer(-Test1, names_to = "Test2", values_to = "Max.Right") %>%
  select(Max.Right)
# bind together, prepare for export; highlight cells with higher correlations
corr_cross_l_max_lr <- bind_cols(corr_cross_l_max_left, corr_cross_l_max_right) %>%
  distinct(across(starts_with("M")), .keep_all = TRUE) %>%
  filter(Test1!=Test2) %>%
  mutate(across(starts_with("M"), ~round(.x, 2))) %>%
  mutate(across(starts_with("M"), ~ case_when(
    .x > 0.3 ~ paste0("\\cellcolor{red!45}", .x),
    between(.x, 0.2, 0.3) ~ paste0("\\cellcolor{red!25}", .x),
    .x < -0.3 ~ paste0("\\cellcolor{blue!45}", .x),
    between(.x, 0.2, 0.3) ~ paste0("\\cellcolor{blue!25}", .x),
    .default = paste(.x)))) %>%
  mutate(across(starts_with("Test"), ~ str_replace_all(.x, "_", ".")))

# make into latex tables for exporting;  exclude the rest for adding in an appendix
higher_corr_min <- corr_cross_l_min_lr %>%
  filter(if_any(starts_with("M"), ~str_detect(., "cellcolor"))) %>%
  kable(booktabs = TRUE, format = "latex", escape = FALSE, linesep = "",
        align = "llrr",
        caption = "Pairwise correlations between test domains over cross-language and minimal-maximal fractures on the left edges. Rows with weak or no correlations (> -0.2 < 0.2) were excluded.",
        label = "tab:corrtablemin") %>%
  kable_styling() %>%
  save_kable("correlation_table_min.tex", float = FALSE)

higher_corr_max <- corr_cross_l_max_lr %>%
  filter(if_any(starts_with("M"), ~str_detect(., "cellcolor"))) %>%
  kable(booktabs = TRUE, format = "latex", escape = FALSE, linesep = "",
        align = "llrr",
        caption = "Pairwise correlations between test domains over cross-language and minimal-maximal fractures on the left edges. Rows with weak or no correlations (> -0.2 < 0.2) were excluded.",
        label = "tab:corrtablemax") %>%
  kable_styling() %>%
  save_kable("correlation_table_max.tex", float = FALSE)


# Random Forests
# function for calculating baseline and accuracy
rf_acc <- function(x){
  cm <- as.matrix(x$confusion[ , -3])
  bl <- round(max(rowSums(cm))/sum(cm), 4)
  acc <- round(sum(diag(cm))/sum(cm), 4)
  diff <- round(acc-bl, 4)
  returnlist <- c("baseline" = bl, "accuracy" = acc, "difference"=diff)
}
# set seed for making reproducible
set.seed(200)
# subset for relevant variables, turn into factors
pw_patterns <- domains_verbal_sub %>%
  pivot_wider(., names_from = PW_Pattern, values_from = PW_Pattern) %>%
  mutate(across(morphosyntactic:`other process`, ~if_else(is.na(.x), 0, 1))) %>%
  select(Serial_Order, Language_ID, morphosyntactic:`other process`)


domains_verbal_rf <- domains_verbal_sub %>%
  select(Language_ID, Relative_Size, Convergence, Relative_Convergence, Noninterrupt.:PW_tone)

domains_vars$Language <- as.factor(domains_vars$Language)
domains_vars$Convergence <- as.factor(domains_vars$Convergence)


convergence_randomforest =randomForest(as.factor(Convergence)~.,data=domains_vars,ntree=1000, mtry=2, importance=TRUE)

varImpPlot(convergence_randomforest, main = "Diagnostics predicting convergence")
confusion_matrix=convergence_randomforest$confusion
confusion_matrix
convergence_randomforest

domains_vars_01 <- domains_vars %>%
  select(-Language, -Relative_Size, -Relative_Convergence)
convergence_randomforest_01 =randomForest(as.factor(Convergence)~.,data=domains_vars_01,ntree=1000, mtry=2, importance=TRUE)
varImpPlot(convergence_randomforest_01, main = "Predicting convergence")
confusion_matrix_01<-convergence_randomforest_01$confusion
confusion_matrix_01
rf_acc(convergence_randomforest_01)

convergence_randomforest_01

nrow(domains_vars)
188/463




# minimal and maximal domains per language
domains_mm <- domains_verbal_sub %>%
  filter(!is.na(MinMax_Fracture))

# plot density
plot_minmax_sizes <- ggplot(aes(x = Relative_Size, group = MinMax_Fracture), data = domains_mm) +
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

plot_minmax_sizes
# export plot
#ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_minmax_per_lang.png", minmax_sizes, height = 27, width = 20, device = "png", units = "cm", dpi = 600)


# density plot of edge and span convergences
# calculate right and left edge convergences
domains_verbal <- domains_verbal %>%
  group_by(Language_ID, Left_Edge) %>%
  mutate(Convergence_Left = n()) %>%
  group_by(Language_ID, Right_Edge) %>%
  mutate(Convergence_Right = n()) %>%
  ungroup() %>%
  mutate(Relative_Convergence_Left = round(Convergence_Left/Tests_Total, 2), Relative_Convergence_Right = round(Convergence_Right/Tests_Total, 2))
glimpse(domains_verbal)
# data set for plotting density
domains_verbal_density <- domains_verbal %>%
  rename(Span = Relative_Convergence,
         Left = Relative_Convergence_Left,
         Right = Relative_Convergence_Right) %>%
  distinct(Language_ID, Span, Right, Left) %>%
  pivot_longer(-Language_ID, names_to = "Convergence_Type", values_to = "Relative_Convergence") %>%
  mutate(Convergence_Type = factor(Convergence_Type, levels = c("Span", "Left", "Right")))
glimpse(domains_verbal_density)

# density plot all
plot_density_all <- ggplot(data = domains_verbal_density, aes(x = Relative_Convergence, group = Convergence_Type, fill = Convergence_Type)) +
  geom_density(alpha = 0.6) +
  scale_fill_d3(name = "Type of Convergence") +
  #guides(fill = guide_legend(override.aes = list(alpha = 1, linewidth = 0))) +
  labs(x = "Relative convergence", y = "Density", ) +
  scale_x_continuous(expand = c(0.005, 0.005), breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  theme(legend.position = c(0.85,0.85),
        #legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 11))
plot_density_all
# export plot
#ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_density_verbal_all.png", density_all, height = 13, width = 20, device = "png", units = "cm", dpi = 600)


# facet density plot of relative convergence per abstract type
plot_dens_conv_atype <- ggplot(aes(x = Relative_Convergence, group = Abstract_Type), data = domains_verbal_sub) +
  geom_density(aes(fill = Abstract_Type)) +
  scale_fill_d3(guide = "none") +
  facet_wrap(~Abstract_Type, scales = "free_x") +
  labs(x = "Relative convergence", y = "Density", ) +
  scale_x_continuous(expand = c(0.008, 0), breaks = seq(0, 5, 0.1), limits = c(0, 0.5)) +
  scale_y_continuous(breaks = seq(0, 14, 2), limits = c(0, 14)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.3, "lines"),
        strip.clip = "off",
        plot.margin = margin(5, 10, 5, 10))
plot_dens_conv_atype
# export plot
#ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_density_abstract.png", plot_dens_conv_atype, height = 17, width = 20, device = "png", units = "cm", dpi = 600)


# facet density plot of relative convergence per prosodic word pattern
# exclude categories with few data points
keep_pw <- domains_verbal_sub %>%
  count(PW_Pattern) %>%
  filter(n>5) %>%
  droplevels() %>%
  pull(PW_Pattern)

domains_verbal_sub_pw <- domains_verbal_sub %>%
  filter(PW_Pattern %in% keep_pw) %>%
  droplevels()

# make plot
plot_dens_conv_pw <- ggplot(aes(x = Relative_Convergence, group = PW_Pattern), data = domains_verbal_sub_pw) +
  geom_density(aes(fill = PW_Pattern)) +
  scale_fill_d3(guide = "none") +
  facet_wrap(~PW_Pattern, scales = "free_x", ncol = 3) +
  labs(x = "Relative convergence", y = "Density", ) +
  scale_x_continuous(expand = c(0.008, 0), breaks = seq(0, 5, 0.1), limits = c(0, 0.5)) +
  scale_y_continuous(breaks = seq(0, 11, 2), limits = c(0, 11)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.3, "lines"),
        strip.clip = "off",
        plot.margin = margin(5, 10, 5, 10))
plot_dens_conv_pw
# export plot
#ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_density_pw.png", plot_dens_conv_pw, height = 20, width = 20, device = "png", units = "cm", dpi = 600)


# density plots of cross-linguistic fractures with more than 10 token
# take subset with fractures that have more than 10 tokens; exclude NA
keep_clf <- domains_verbal %>%
  filter(!is.na(CrossL_Fracture)) %>%
  count(CrossL_Fracture) %>%
  filter(n>10) %>%
  droplevels() %>%
  pull(CrossL_Fracture)
domains_verbal_clf <- domains_verbal %>%
  filter(CrossL_Fracture %in% keep_clf) %>%
  droplevels()

plot_dens_conv_clf <- ggplot(aes(x = Relative_Convergence, group = CrossL_Fracture), data = domains_verbal_clf) +
  geom_density(aes(fill = CrossL_Fracture)) +
  scale_fill_d3(guide = "none") +
  facet_wrap(~CrossL_Fracture, scales = "free_x", ncol = 3) +
  labs(x = "Relative convergence", y = "Density", ) +
  scale_x_continuous(expand = c(0.008, 0), breaks = seq(0, 5, 0.1), limits = c(0, 0.5)) +
  scale_y_continuous(breaks = seq(0, 11, 2), limits = c(0, 11)) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.3, "lines"),
        strip.clip = "off",
        plot.margin = margin(5, 10, 5, 10))
plot_dens_conv_clf
# export plot
#ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_density_clf.png", plot_dens_conv_clf, height = 17, width = 20, device = "png", units = "cm", dpi = 600)


# plot convergences between phonological and morphosyntactic domains
# count convergences between those domains per language
conv_bisection <- domains %>%
  unite("Spans", Left_Edge:Right_Edge, sep = "-") %>%
  group_by(Planar_ID, Domain_Type) %>%
  mutate(Total_Tests_D = n()) %>%
  group_by(Planar_ID, Domain_Type, Spans) %>%
  mutate(DConvergence = n()) %>%
  ungroup()
# with indeterminate = morphosyntactic
conv_bisection_lumped <- domains %>%
  unite("Spans", Left_Edge:Right_Edge, sep = "-") %>%
  mutate(Domain_Type_Lumped = if_else(Domain_Type=="indeterminate", "morphosyntactic", Domain_Type)) %>%
  group_by(Planar_ID, Domain_Type) %>%
  mutate(Total_Tests_DL = n()) %>%
  group_by(Planar_ID, Domain_Type, Spans) %>%
  mutate(DLConvergence = n()) %>%
  ungroup()
# add back to df for plotting
domains_bisection <- conv_bisection %>%
  left_join(., conv_bisection_lumped) %>%
  mutate(Relative_DConvergence = round(DConvergence/Total_Tests_D, 2), Relative_DLConvergence = round(DLConvergence/Total_Tests_DL, 2))

summary(domains_bisection$Relative_DLConvergence)

# plot lumped, phonological domains
plot_points_phon <- ggplot(data = filter(domains_bisection, Domain_Type_Lumped=="phonological"), aes(x = Relative_Size, y = DLConvergence)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 3, color = "darkorange2") +
  facet_wrap(~Planar_ID, scales = "free_x", ncol = 3) +
  labs(x = "Relative span size", y = "Number of convergences") +
  scale_x_continuous(expand = c(0, 0.03), breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0.1, 0.1)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.3, "lines"),
        strip.clip = "off",
        plot.margin = margin(5, 10, 5, 10))
plot_points_phon
# export
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_points_bisection_phon.png", plot_points_phon, height = 26, width = 19, device = "png", units = "cm", dpi = 600)

# plot lumbed, morphosyntactic domains
plot_points_ms <- ggplot(data = filter(domains_bisection, Domain_Type_Lumped=="morphosyntactic"), aes(x = Relative_Size, y = DLConvergence)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 3, color = "dodgerblue3") +
  facet_wrap(~Planar_ID, scales = "free_x", ncol = 3) +
  labs(x = "Relative span size", y = "Number of convergences") +
  scale_x_continuous(expand = c(0, 0.04), breaks = seq(0, 1, 0.25), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0.3)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.3, "lines"),
        strip.clip = "off",
        plot.margin = margin(5, 10, 5, 10))
plot_points_ms
# export
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_points_bisection_ms.png", plot_points_ms, height = 26, width = 19, device = "png", units = "cm", dpi = 600)




# plot with indeterminate
domains_bisection <- domains_bisection %>%
  mutate(Domain_Type = factor(Domain_Type, levels = c("morphosyntactic", "phonological", "indeterminate")))

plot_dens_msphonind <- ggplot(data = domains_bisection, aes(x = Relative_DConvergence, group = Domain_Type, fill = Domain_Type)) +
  geom_density(alpha = 0.6) +
  scale_fill_d3(name = "Domain") +
  facet_wrap(~Planar_ID, scales = "free_x", ncol = 3) +
  labs(x = "Relative convergence", y = "Density", ) +
  scale_x_continuous(expand = c(0.008, 0), breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 25, 10), limits = c(0, 25)) +
  theme_bw() +
  theme(legend.position = "top",
        legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.3, "lines"),
        strip.clip = "off",
        plot.margin = margin(5, 10, 5, 10))
plot_dens_msphonind

# export
#ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_density_bisection_ind.png", plot_dens_msphonind, height = 25, width = 17, device = "png", units = "cm", dpi = 600)


# density plot left/right
density_lr <- ggplot(data = domains_verbal_d, aes(x = Relative_Convergence, group = MinMax_Fracture, col = MinMax_Fracture)) +
  geom_density() +
  scale_fill_startrek(name = "Fracture") +
  #guides(fill = guide_legend(override.aes = list(alpha = 1, linewidth = 0))) +
  labs(x = "Relative convergence", y = "Density", ) +
  scale_x_continuous(expand = c(0.005, 0.005), breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  theme(legend.position = c(0.85,0.05),
        #legend.key.size = unit(1, "lines"),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        strip.text.x = element_text(size = 11),
        panel.spacing = unit(1.2, "lines"))
density_min_max



# correlation plots
plot_corr_min <- ggcorrplot(corr_cross_l_min,
                            type = "lower",
                            lab = TRUE,
                            lab_size = 4.5,
                            tl.cex = 11)
plot_corr_min
plot_corr_max <- ggcorrplot(corr_cross_l_max,
                            type = "lower",
                            lab = TRUE,
                            lab_size = 4.5,
                            tl.cex = 11)
plot_corr_max
# export plots
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_corr_max.png", plot_corr_max,  height = 15, width = 22, device = "png", units = "cm", dpi = 600)
ggsave("/Users/auderset/Documents/GitHub/CCAmericas/07_figures/plot_corr_min.png", plot_corr_min, height = 15, width = 22, device = "png", units = "cm", dpi = 600)


