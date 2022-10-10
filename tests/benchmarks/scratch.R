x <- data.table(X = sample.int(1000, 10000, replace = TRUE),
                Y = sample.int(1000, 10000, replace = TRUE)) %>%
  mutate(across(.cols = "X",
                .fns = ~ ifelse(row_number(.x) %in% sample(1:n(), size = ((0.25 * 100) * n() / 100)), NA, .x)))

# na.omit is fastest
bench::mark(
  na.omit = na.omit(x, cols = "X"),
  stats_na.omit = stats::na.omit(x),
  is.na = x[!is.na(x$X),],
  which = x[-which(is.na(x$X)),],
  iterations = 1000,
  check = TRUE
)

# A tibble: 4 x 13
#expression         min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result                   memory              time               gc
#<bch:expr>    <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>                   <list>              <list>             <list>
#1 na.omit          163us    274us     3249.     182KB    19.6    994     6      306ms <data.table [7,500 x 2]> <Rprofmem [7 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#2 stats_na.omit    192us    307us     3076.     182KB    18.6    994     6      323ms <data.table [7,500 x 2]> <Rprofmem [7 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#3 is.na            362us    589us     1601.     222KB    11.3    993     7      620ms <data.table [7,500 x 2]> <Rprofmem [9 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#4 which            758us    988us      992.     202KB     4.98   995     5         1s <data.table [7,500 x 2]> <Rprofmem [10 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>

x <- data.table(X = sample.int(1000, 10000, replace = TRUE),
                Y = sample.int(10, 10000, replace = TRUE))
q <- quantile(x$X, probs = c(0.25, 0.5, 0.75))

# fcase is much faster
bench::mark(
  if_else = x[, C := if_else(X <= q[1], 1L, if_else(X <= q[2], 2L, if_else(X <= q[3], 3L, 4L))), by = "Y"],
  ifelse = x[, C := ifelse(X <= q[1], 1L, ifelse(X <= q[2], 2L, ifelse(X <= q[3], 3L, 4L))), by = "Y"],
  fcase = x[, C := fcase(X <= q[1], 1L, X <= q[2], 2L, X <= q[3], 3L, default = 4L)],
  case_when = x[, C := case_when(X <= q[1] ~ 1L, X <= q[2] ~ 2L, X <= q[3] ~ 4L, TRUE ~ 4L)],
  subset = x[, C := 0L][X <= q[1], C := 1L, by = "Y"][X <= q[2], C := 2L, by = "Y"][X <= q[3], C := 3L, by = "Y"],
  iterations = 1000,
  check = TRUE
)

# A tibble: 5 x 13
#expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result                    memory               time               gc
#<bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>                    <list>               <list>             <list>
#1 if_else      3.72ms   4.74ms      208.     1.8MB     9.56   956    44      4.61s <data.table [10,000 x 3]> <Rprofmem [502 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#2 ifelse       2.88ms   3.75ms      266.    1.25MB     8.80   968    32      3.63s <data.table [10,000 x 3]> <Rprofmem [373 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#3 fcase       523.6us 606.85us     1536.  228.07KB     9.27   994     6   647.33ms <data.table [10,000 x 3]> <Rprofmem [10 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#4 case_when    1.64ms    2.1ms      455.    1.79MB    21.4    955    45       2.1s <data.table [10,000 x 3]> <Rprofmem [84 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#5 subset       4.77ms   5.27ms      181.  669.88KB     4.06   978    22      5.42s <data.table [10,000 x 3]> <Rprofmem [62 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>


x <- data.table(X = sample.int(10000, 10000, replace = TRUE))
y <- copy(x)
z <- copy(x)
w <- copy(x)

# setorderv_subset is fastest
bench::mark(
  setorderv_subset = {setorderv(y, cols = "X", order = -1L)
                      y[1:5]},
  setorderv_head = {setorderv(z, cols = "X", order = -1L)
                    z[, head(.SD, 5)]},
  setorderv_SD = {setorderv(w, cols = "X", order = -1L)
                  w[, .SD[1:5]]},
  order_head = x[order(-X), head(.SD, 5)],
  order_SD = x[order(-X), .SD[1:5]],
  order_I = {setorderv(y, cols = "X", order = -1L)
             y[y[order(-X), .I[1:5]]]},
  iterations = 1000,
  check = TRUE
)

# A tibble: 6 x 13
#expression            min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
#<bch:expr>       <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>
#1 setorderv_subset  400.7us  488.9us     1645.     104KB     1.65   999     1
#2 setorderv_head    929.2us   1.02ms      895.     153KB     2.69   997     3
#3 setorderv_SD      858.4us  956.4us      961.     153KB     2.89   997     3
#4 order_head         1.26ms   1.57ms      614.     127KB     1.85   997     3
#5 order_SD           1.19ms   1.46ms      668.     127KB     2.01   997     3
#6 order_I             1.2ms   1.31ms      713.     150KB     2.86   996     4


x <- data.table(X = sample.int(20000, 10000, replace = FALSE),
                Y = sample.int(10000, 10000, replace = TRUE))
y <- data.table(X = sample.int(20000, 10000, replace = FALSE),
                Y = sample.int(10000, 10000, replace = TRUE))

# left_join: keys is fastest
bench::mark(
  on = y[x, on = "X"],
  merge = merge(x, y, all.x = TRUE, by = "X"),
  left_join = left_join(x, y, by = "X"),
  keys = {setkeyv(x, cols = "X")
          setkeyv(y, cols = "X")
          y[x]},
  iterations = 1000,
  check = FALSE
)

# A tibble: 4 x 13
#expression      min median itr/s~1 mem_al~2 gc/se~3 n_itr  n_gc total~4 result
#<bch:expr> <bch:tm> <bch:>   <dbl> <bch:by>   <dbl> <int> <dbl> <bch:t> <list>
#1 on           1.92ms 3.05ms    325. 463.98KB    2.29   993     7   3.05s <NULL>
#2 merge        2.31ms 2.99ms    315. 552.02KB    2.54   992     8   3.15s <NULL>
#3 left_join    3.43ms 3.87ms    244.   1.14MB    6.01   976    24      4s <NULL>
#4 keys          1.3ms 1.78ms    523. 514.65KB    4.22   992     8    1.9s <NULL>


x <- data.table(X = sample.int(100, 10000, replace = TRUE),
                Y = sample.int(10000, 10000, replace = TRUE))
y <- copy(x)
z <- copy(x)
v <- copy(x)
w <- copy(x)

# rowidv is fastest
bench::mark(
  rowid = x[, id := rowid(X)],
  rowidv = v[, id := rowidv(v, cols = "X")],
  seq_len = y[, id := seq_len(.N), by = X],
  rleid = z[, id := rleid(Y), by = X],
  row_number = x %>% group_by(X) %>% mutate(id = row_number()),
  iterations = 1000,
  check = FALSE
)

# A tibble: 5 x 13
#expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory               time               gc
#<bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>               <list>             <list>
#1 rowid       536.7us  753.9us     1218.     112KB     3.66   997     3   818.75ms <NULL> <Rprofmem [9 x 3]>   <bench_tm [1,000]> <tibble [1,000 x 3]>
#2 rowidv      521.2us  731.9us     1285.     112KB     5.16   996     4   774.94ms <NULL> <Rprofmem [9 x 3]>   <bench_tm [1,000]> <tibble [1,000 x 3]>
#3 seq_len     696.6us  946.2us     1019.     141KB     3.07   997     3   978.18ms <NULL> <Rprofmem [110 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#4 rleid        1.09ms   1.48ms      642.     158KB     2.58   996     4      1.55s <NULL> <Rprofmem [114 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#5 row_number   7.77ms   8.69ms      111.     296KB     2.49   978    22      8.83s <NULL> <Rprofmem [262 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>


x <- data.table(X = sample.int(10, 10000, replace = TRUE),
                Y = sample.int(10000, 10000, replace = TRUE))
y <- copy(x)
z <- copy(x)
v <- copy(x)
w <- copy(x)

# fifelse is (as expected) fastest
bench::mark(
  ifelse = x[, Z := ifelse(X == 5, 1, 0)],
  if_else = y[, Z := if_else(X == 5, 1, 0)],
  fifelse = z[, Z := fifelse(X == 5, 1, 0)],
  filter = {w[X == 5, Z := 1]
            w[X != 5, Z := 0]},
  iterations = 1000,
  check = FALSE
)

# A tibble: 4 x 13
#expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory              time               gc
#<bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>              <list>             <list>
#1 ifelse      614.8us   1.14ms      834.     580KB    1.67    998     2       1.2s <NULL> <Rprofmem [17 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#2 if_else     651.4us 897.65us      998.     658KB    2.00    998     2   999.62ms <NULL> <Rprofmem [19 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#3 fifelse       471us  661.8us     1389.     150KB    0      1000     0   720.16ms <NULL> <Rprofmem [7 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#4 filter       2.37ms   2.61ms      334.     454KB    0.669   998     2      2.99s <NULL> <Rprofmem [26 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>

x <- data.table(X = stri_rand_strings(10000, 2, "[A-Z]"),
                Y = sample.int(10000, 10000, replace = TRUE))

# which_chin is fastest (and much faster than %in%!)
bench::mark(
  filter = x %>% filter(X %in% c("AA", "BB")),
  .in = x[X %in% c("AA", "BB"),],
  chin = x[X %chin% c("AA", "BB"),],
  or = x[X == "AA" | X == "BB",],
  which_in = x[which(X %in% c("AA", "BB")),],
  which_chin = x[which(X %chin% c("AA", "BB")),],
  which_or = x[which(X == "AA" | X == "BB"),],
  iterations = 1000,
  check = FALSE
)
# A tibble: 7 x 13
#expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory              time               gc
#<bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>              <list>             <list>
#1 filter       6.86ms    7.6ms      125.   277.9KB     3.86   970    30      7.77s <NULL> <Rprofmem [21 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#2 .in          1.36ms   1.54ms      618.    89.9KB     3.10   995     5      1.61s <NULL> <Rprofmem [18 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#3 chin         1.33ms   1.54ms      623.    89.9KB     3.76   994     6       1.6s <NULL> <Rprofmem [18 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#4 or            226us 278.15us     3259.   173.5KB    19.7    994     6      305ms <NULL> <Rprofmem [10 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#5 which_in      298us  360.4us     2565.   212.6KB    20.7    992     8    386.8ms <NULL> <Rprofmem [10 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>
#6 which_chin  212.1us  263.6us     3455.    95.3KB    10.4    997     3   288.59ms <NULL> <Rprofmem [8 x 3]>  <bench_tm [1,000]> <tibble [1,000 x 3]>
#7 which_or    236.1us  297.1us     3172.   173.5KB    19.1    994     6   313.39ms <NULL> <Rprofmem [10 x 3]> <bench_tm [1,000]> <tibble [1,000 x 3]>