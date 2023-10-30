# create full output databases, various sanity checks

if (!require("pacman")) install.packages("pacman")
p_load(here,
       janitor,
       tidyverse)


# read in input files and metadata, convert characters to factors for checking
metadata <- read_tsv(here("input/metadata.tsv")) %>%
  mutate(across(where(is.character), as_factor))
overlaps <- read_tsv(here("input/overlaps.tsv")) %>%
  mutate(across(where(is.character), as_factor))
domains_input <- read_tsv(here("input/cc_domains_input.tsv")) %>%
  mutate(across(where(is.character),as_factor))
planar_input <- read_tsv(here("input/cc_planar_input.tsv")) %>%
  mutate(across(where(is.character), as_factor))

# check if planar input file is clean; check Planar_Type and Position_Type
summary(planar_input)
# check if domains input file is clean; check Planar_Type, Analysis_Type, Domain_Type
summary(domains_input)
# check on Type and Fracture labels specifically
levels(domains_input$Abstract_Type)
sort(levels(domains_input$CrossL_Fracture))
sort(levels(domains_input$Lspecific_Fracture))


# generate full planar structure file; clean up upper/lower case and mix of space/punctuation
planar_output <- planar_input %>%
  left_join(., select(metadata, Language_ID, Language_Name)) %>%
  unite("Planar_ID", Language_ID:Planar_Type, sep = "_", remove = FALSE) %>%
  mutate(Position_ID = paste0(Language_ID, "_", str_extract(Planar_Type, "[a-z]{1}"), "_", Position)) %>%
  mutate(Position_Type = str_to_lower(Position_Type)) %>%
  mutate(Elements = str_to_title(Elements)) %>%
  select(Language_ID, Language_Name, Planar_ID, Planar_Type, Position_ID, Position, Position_Type, Elements)
glimpse(planar_output)

# write output file
write_tsv(planar_output, here("planar.tsv"))


# generate full domains file

# calculate total positions per language
planar_w_total <- planar_output %>%
  group_by(Planar_ID) %>%
  mutate(Position_Total = max(Position))

domains_out <- domains_input %>%
  mutate(across(Analysis_Type:Lspecific_Fracture, ~str_to_lower(.x))) %>%
  mutate(across(Analysis_Type:Lspecific_Fracture, ~str_replace_all(., " ", "."))) %>%
  left_join(., overlaps) %>%
  left_join(., select(metadata, Language_ID, Language_Name)) %>%
  left_join(., select(planar_w_total, Language_ID, Planar_Type, Position_Total), relationship =
              "many-to-many") %>%
  distinct() %>%
  arrange(Language_ID) %>%
  group_by(Planar_ID) %>%
  mutate(Size = Right_Edge-Left_Edge+1) %>%
  mutate(Largest = max(Size)) %>%
  ungroup() %>%
  mutate(Relative_Size = round(Size/Largest, 2)) %>%
  mutate(Domain_ID = paste0(str_extract(Planar_ID, ".*\\_[a-z]{1}"), "_",
                              Abstract_Type, "_",
                              CrossL_Fracture, "_",
                              MinMax_Fracture, "_",
                              Lspecific_Fracture)) %>%
  mutate(Domain_ID = str_remove_all(Domain_ID, "_NA")) %>%
  arrange(Planar_ID, Domain_ID) %>%
  mutate(Serial_Order = seq(1:nrow(.))) %>%
  filter(!Analysis_Type=="lexphon") %>%
  select(Serial_Order, Language_Name, Language_ID, Domain_ID, Domain_Type, Abstract_Type, CrossL_Fracture, MinMax_Fracture, Lspecific_Fracture, Left_Edge, Right_Edge, Size, Relative_Size, Largest, Position_Total, Planar_ID, PW_Patterns, Test_Labels)
glimpse(domains_out)

# there should not be any duplicates in the Domain_ID! please fix in the input file if there are any
id_dupl <- domains_out %>%
  get_dupes(Domain_ID)


# calculate convergences
count_convergences <- domains_out %>%
  unite("Edges", Left_Edge:Right_Edge, sep = "_", remove = FALSE) %>%
  group_by(Planar_ID, Left_Edge, Right_Edge) %>%
  summarize(Convergence = n())
glimpse(count_convergences)

# calculate total tests per language/domain
count_tests <- domains_out %>%
  filter(!Analysis_Type=="lexphon") %>%
  group_by(Planar_ID) %>%
 summarize(Tests_Total = n())


# merge back to df
domains_out_conv <- domains_out %>%
  left_join(., count_convergences) %>%
  left_join(., count_tests) %>%
  mutate(Relative_Convergence = round(Convergence/Tests_Total, 2)) %>%
  relocate(Convergence, .after = Right_Edge) %>%
  relocate(Relative_Convergence, .after = Convergence) %>%
  relocate(Tests_Total, .after = Position_Total)
glimpse(domains_out_conv)

write_tsv(domains_out_conv, here("domains.tsv"))
