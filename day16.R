library(tidyverse)

#Read in Raw File
raw <- read_lines('data/day16.txt')

rules <- keep(raw, ~str_detect(.x, '.+: \\d+')) %>% 
  tibble(s = .) %>% 
  extract(s, into = c('name', 'r1s', 'r1e', 'r2s', 'r2e'),
          regex = '(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)', 
          convert = T)

#Parse My Ticket
my_ticket <- raw[which(raw=='your ticket:')+1] %>% str_split(',') %>% unlist()

#Parse Other Tickets
nearby_tickets <-tibble(
  x = raw[which(raw=='nearby tickets:')+1:length(raw)]
) %>% 
  mutate(ticket_id = row_number()) %>% 
  separate_rows(x, sep = ',', convert = T) %>% 
  filter(!is.na(x))


###Part 1
for(i in 1:nrow(rules)){
  candidates <- union(seq(as.numeric(rules[i, "r1s"]), as.numeric(rules[i, "r1e"])),
                      seq(as.numeric(rules[i, "r2s"]), as.numeric(rules[i, "r2e"]))
  )
  if(i == 1) {valid_nums <- candidates}
  else {valid_nums <- union(valid_nums, candidates)}
}

nearby_tickets %>% 
  filter(!x %in% valid_nums) %>% 
  summarize(ans = sum(x)) %>% 
  pull(ans)

###Part 2
valid_tickets <- nearby_tickets %>% 
  mutate(is_valid = x %in% valid_nums) %>% 
  group_by(ticket_id) %>% 
  filter(all(is_valid == T)) %>% 
  mutate(rid = 1:n()) %>% 
  ungroup() %>% 
  bind_rows(
    tibble(
      ticket_id = 0,
      x = as.numeric(my_ticket),
      is_valid = T,
      rid = 1:20
  )) 

rules_with_num <- rules %>% 
  mutate(valids = pmap(list(r1s=r1s, r1e=r1e, r2s=r2s, r2e=r2e),
                        function(r1s, r1e, r2s, r2e){
                          union(seq(r1s, r1e), seq(r2s, r2e))
                        })
  ) %>% 
  unnest(valids) %>% 
  select(name, valids)
  
#Get Starting Points for All Combination of Rules
assignments <- valid_tickets %>% 
  inner_join(rules_with_num, by = c('x' = 'valids')) %>% 
  count(name, rid) %>% 
  filter(n == n_distinct(valid_tickets$ticket_id)) %>% 
  add_count(name, name = 'instances', wt = n())

final_assignments = rep(NA, length(rules$name))

for(i in seq_along(final_assignments)){
  # Find Rule to Be Inserted
  tbi <- assignments %>% filter(instances == i 
                                & !name %in% final_assignments
                                & is.na(final_assignments[rid]))
  # Update Array
  final_assignments[tbi$rid] <- tbi$name
}

as.numeric(my_ticket)[which(str_detect(final_assignments, 'departure'))] %>% 
  reduce(`*`)
