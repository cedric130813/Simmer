# Full tutorial at https://r-simmer.org/articles/simmer-04-bank-1 -------------

library(simmer)

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  timeout(10) %>%
  log_("I must leave")

bank <-
  simmer("bank") %>%
  add_generator("Customer", customer, at(5))

bank %>% run(until = 100)
#> 5: Customer0: Here I am
#> 15: Customer0: I must leave
#> simmer environment: bank | now: 15 | next: 
#> { Monitor: in memory }
#> { Source: Customer | monitored: 1 | n_generated: 1 }
bank %>% get_mon_arrivals()
#>        name start_time end_time activity_time finished replication
#> 1 Customer0          5       15            10     TRUE           1

# Service Counter at https://r-simmer.org/articles/simmer-04-bank-1#a-service-counter

# One Service Counter -----------------------
library(simmer)

set.seed(1234)

bank <- simmer()

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  set_attribute("start_time", function() {now(bank)}) %>%
  seize("counter") %>%
  log_(function() {paste("Waited: ", now(bank) - get_attribute(bank, "start_time"))}) %>%
  timeout(12) %>%
  release("counter") %>%
  log_(function() {paste("Finished: ", now(bank))})

bank <-
  simmer("bank") %>%
  add_resource("counter") %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)})

bank %>% run(until = 400)
bank %>%
  get_mon_arrivals() %>%
  transform(waiting_time = end_time - start_time - activity_time)

# Server with random service time ----------------

library(simmer)

set.seed(1269)

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  set_attribute("start_time", function() {now(bank)}) %>%
  seize("counter") %>%
  log_(function() {paste("Waited: ", now(bank) - get_attribute(bank, "start_time"))}) %>%
  # timeout(rexp(1, 1/12)) would generate a single random time and use it for
  # every arrival, whereas the following line generates a random time for each
  # arrival
  timeout(function() {rexp(1, 1/12)}) %>%
  release("counter") %>%
  log_(function() {paste("Finished: ", now(bank))})

bank <-
  simmer("bank") %>%
  add_resource("counter") %>%
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)})

bank %>% run(until = 400)
bank %>%
  get_mon_arrivals() %>%
  transform(waiting_time = end_time - start_time - activity_time)

# Several Service Counters -----------------------

library(simmer)

set.seed(1269)

customer <-
  trajectory("Customer's path") %>%
  log_("Here I am") %>%
  set_attribute("start_time", function() {now(bank)}) %>%
  seize("counter") %>%
  log_(function() {paste("Waited: ", now(bank) - get_attribute(bank, "start_time"))}) %>%
  timeout(function() {rexp(1, 1/12)}) %>%
  release("counter") %>%
  log_(function() {paste("Finished: ", now(bank))})

bank <-
  simmer("bank") %>%
  add_resource("counter", 2) %>% # Here is the change
  add_generator("Customer", customer, function() {c(0, rexp(4, 1/10), -1)})

bank %>% run(until = 400)
bank %>%
  get_mon_arrivals() %>%
  transform(waiting_time = end_time - start_time - activity_time)

# Multiple runs ----------------

library(simmer)
library(parallel)

customer <-
  trajectory("Customer's path") %>%
  seize("counter") %>%
  timeout(function() {rexp(1, 1/12)}) %>%
  release("counter")

mclapply(c(393943, 100005, 777999555, 319999772), function(the_seed) {
  set.seed(the_seed)
  
  bank <-
    simmer("bank") %>%
    add_resource("counter", 2) %>%
    add_generator("Customer", customer, function() {c(0, rexp(49, 1/10), -1)})
  
  bank %>% run(until = 400)
  result <-
    bank %>%
    get_mon_arrivals() %>%
    transform(waiting_time = end_time - start_time - activity_time)
  paste("Average wait for ", sum(result$finished), " completions was ",
        mean(result$waiting_time), "minutes.")
}) %>% unlist()

#-------------------------------
