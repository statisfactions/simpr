## simpr methods for tidyverse verbs, auto-generated
 #' @export
add_count.simpr_sims = function(x, ..., wt = NULL, sort = FALSE, name = NULL, 
    .drop = deprecated()) {
mc = match.call()
mc[[1]] = quote(add_count)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
add_count.simpr_spec = function(x, ..., wt = NULL, sort = FALSE, name = NULL, 
    .drop = deprecated()) {
mc = match.call()

add_call(x, mc, 'add_count', replace_arg = 2)
}
#' @export
anti_join.simpr_sims = function(x, y, by = NULL, copy = FALSE, ...) {
mc = match.call()
mc[[1]] = quote(anti_join)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
anti_join.simpr_spec = function(x, y, by = NULL, copy = FALSE, ...) {
mc = match.call()

add_call(x, mc, 'anti_join', replace_arg = 2)
}
#' @export
arrange_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(arrange_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
arrange_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'arrange_', replace_arg = 2)
}
#' @export
arrange.simpr_sims = function(.data, ..., .by_group = FALSE) {
mc = match.call()
mc[[1]] = quote(arrange)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
arrange.simpr_spec = function(.data, ..., .by_group = FALSE) {
mc = match.call()

add_call(.data, mc, 'arrange', replace_arg = 2)
}
#' @export
as.tbl.simpr_sims = function(x, ...) {
mc = match.call()
mc[[1]] = quote(as.tbl)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
as.tbl.simpr_spec = function(x, ...) {
mc = match.call()

add_call(x, mc, 'as.tbl', replace_arg = 2)
}
#' @export
auto_copy.simpr_sims = function(x, y, copy = FALSE, ...) {
mc = match.call()
mc[[1]] = quote(auto_copy)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
auto_copy.simpr_spec = function(x, y, copy = FALSE, ...) {
mc = match.call()

add_call(x, mc, 'auto_copy', replace_arg = 2)
}
#' @export
collapse.simpr_sims = function(x, sep = "", width = Inf, last = "") {
mc = match.call()
mc[[1]] = quote(collapse)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
collapse.simpr_spec = function(x, sep = "", width = Inf, last = "") {
mc = match.call()

add_call(x, mc, 'collapse', replace_arg = 2)
}
#' @export
collect.simpr_sims = function(x, ...) {
mc = match.call()
mc[[1]] = quote(collect)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
collect.simpr_spec = function(x, ...) {
mc = match.call()

add_call(x, mc, 'collect', replace_arg = 2)
}
#' @export
compute.simpr_sims = function(x, ...) {
mc = match.call()
mc[[1]] = quote(compute)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
compute.simpr_spec = function(x, ...) {
mc = match.call()

add_call(x, mc, 'compute', replace_arg = 2)
}
#' @export
count.simpr_sims = function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
mc = match.call()
mc[[1]] = quote(count)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
count.simpr_spec = function(x, ..., wt = NULL, sort = FALSE, name = NULL) {
mc = match.call()

add_call(x, mc, 'count', replace_arg = 2)
}
#' @export
distinct_.simpr_sims = function(.data, ..., .dots, .keep_all = FALSE) {
mc = match.call()
mc[[1]] = quote(distinct_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
distinct_.simpr_spec = function(.data, ..., .dots, .keep_all = FALSE) {
mc = match.call()

add_call(.data, mc, 'distinct_', replace_arg = 2)
}
#' @export
distinct.simpr_sims = function(.data, ..., .keep_all = FALSE) {
mc = match.call()
mc[[1]] = quote(distinct)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
distinct.simpr_spec = function(.data, ..., .keep_all = FALSE) {
mc = match.call()

add_call(.data, mc, 'distinct', replace_arg = 2)
}
#' @export
do_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(do_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
do_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'do_', replace_arg = 2)
}
#' @export
do.simpr_sims = function(.data, ...) {
mc = match.call()
mc[[1]] = quote(do)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
do.simpr_spec = function(.data, ...) {
mc = match.call()

add_call(.data, mc, 'do', replace_arg = 2)
}
#' @export
dplyr_col_modify.simpr_sims = function(data, cols) {
mc = match.call()
mc[[1]] = quote(dplyr_col_modify)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
dplyr_col_modify.simpr_spec = function(data, cols) {
mc = match.call()

add_call(data, mc, 'dplyr_col_modify', replace_arg = 2)
}
#' @export
dplyr_reconstruct.simpr_sims = function(data, template) {
mc = match.call()
mc[[1]] = quote(dplyr_reconstruct)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
dplyr_reconstruct.simpr_spec = function(data, template) {
mc = match.call()

add_call(data, mc, 'dplyr_reconstruct', replace_arg = 2)
}
#' @export
dplyr_row_slice.simpr_sims = function(data, i, ...) {
mc = match.call()
mc[[1]] = quote(dplyr_row_slice)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
dplyr_row_slice.simpr_spec = function(data, i, ...) {
mc = match.call()

add_call(data, mc, 'dplyr_row_slice', replace_arg = 2)
}
#' @export
filter_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(filter_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
filter_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'filter_', replace_arg = 2)
}
#' @export
filter.simpr_sims = function(.data, ..., .preserve = FALSE) {
mc = match.call()
mc[[1]] = quote(filter)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
filter.simpr_spec = function(.data, ..., .preserve = FALSE) {
mc = match.call()

add_call(.data, mc, 'filter', replace_arg = 2)
}
#' @export
full_join.simpr_sims = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", 
    ".y"), ..., keep = FALSE) {
mc = match.call()
mc[[1]] = quote(full_join)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
full_join.simpr_spec = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", 
    ".y"), ..., keep = FALSE) {
mc = match.call()

add_call(x, mc, 'full_join', replace_arg = 2)
}
#' @export
group_by_.simpr_sims = function(.data, ..., .dots = list(), add = FALSE) {
mc = match.call()
mc[[1]] = quote(group_by_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
group_by_.simpr_spec = function(.data, ..., .dots = list(), add = FALSE) {
mc = match.call()

add_call(.data, mc, 'group_by_', replace_arg = 2)
}
#' @export
group_by.simpr_sims = function(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)) {
mc = match.call()
mc[[1]] = quote(group_by)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
group_by.simpr_spec = function(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)) {
mc = match.call()

add_call(.data, mc, 'group_by', replace_arg = 2)
}
#' @export
group_data.simpr_sims = function(.data) {
mc = match.call()
mc[[1]] = quote(group_data)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
group_data.simpr_spec = function(.data) {
mc = match.call()

add_call(.data, mc, 'group_data', replace_arg = 2)
}
#' @export
group_indices_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(group_indices_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
group_indices_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'group_indices_', replace_arg = 2)
}
#' @export
group_indices.simpr_sims = function(.data, ...) {
mc = match.call()
mc[[1]] = quote(group_indices)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
group_indices.simpr_spec = function(.data, ...) {
mc = match.call()

add_call(.data, mc, 'group_indices', replace_arg = 2)
}
#' @export
group_keys.simpr_sims = function(.tbl, ...) {
mc = match.call()
mc[[1]] = quote(group_keys)

.tbl[[get_sim_name(.tbl)]] =  purrr::map(.tbl[[get_sim_name(.tbl)]],
                                           ~ eval(mc))
.tbl
}

#' @export
group_keys.simpr_spec = function(.tbl, ...) {
mc = match.call()

add_call(.tbl, mc, 'group_keys', replace_arg = 2)
}
#' @export
group_map.simpr_sims = function(.data, .f, ..., .keep = FALSE) {
mc = match.call()
mc[[1]] = quote(group_map)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
group_map.simpr_spec = function(.data, .f, ..., .keep = FALSE) {
mc = match.call()

add_call(.data, mc, 'group_map', replace_arg = 2)
}
#' @export
group_modify.simpr_sims = function(.data, .f, ..., .keep = FALSE) {
mc = match.call()
mc[[1]] = quote(group_modify)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
group_modify.simpr_spec = function(.data, .f, ..., .keep = FALSE) {
mc = match.call()

add_call(.data, mc, 'group_modify', replace_arg = 2)
}
#' @export
group_nest.simpr_sims = function(.tbl, ..., .key = "data", keep = FALSE) {
mc = match.call()
mc[[1]] = quote(group_nest)

.tbl[[get_sim_name(.tbl)]] =  purrr::map(.tbl[[get_sim_name(.tbl)]],
                                           ~ eval(mc))
.tbl
}

#' @export
group_nest.simpr_spec = function(.tbl, ..., .key = "data", keep = FALSE) {
mc = match.call()

add_call(.tbl, mc, 'group_nest', replace_arg = 2)
}
#' @export
group_size.simpr_sims = function(x) {
mc = match.call()
mc[[1]] = quote(group_size)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
group_size.simpr_spec = function(x) {
mc = match.call()

add_call(x, mc, 'group_size', replace_arg = 2)
}
#' @export
group_split.simpr_sims = function(.tbl, ..., .keep = TRUE) {
mc = match.call()
mc[[1]] = quote(group_split)

.tbl[[get_sim_name(.tbl)]] =  purrr::map(.tbl[[get_sim_name(.tbl)]],
                                           ~ eval(mc))
.tbl
}

#' @export
group_split.simpr_spec = function(.tbl, ..., .keep = TRUE) {
mc = match.call()

add_call(.tbl, mc, 'group_split', replace_arg = 2)
}
#' @export
group_trim.simpr_sims = function(.tbl, .drop = group_by_drop_default(.tbl)) {
mc = match.call()
mc[[1]] = quote(group_trim)

.tbl[[get_sim_name(.tbl)]] =  purrr::map(.tbl[[get_sim_name(.tbl)]],
                                           ~ eval(mc))
.tbl
}

#' @export
group_trim.simpr_spec = function(.tbl, .drop = group_by_drop_default(.tbl)) {
mc = match.call()

add_call(.tbl, mc, 'group_trim', replace_arg = 2)
}
#' @export
group_vars.simpr_sims = function(x) {
mc = match.call()
mc[[1]] = quote(group_vars)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
group_vars.simpr_spec = function(x) {
mc = match.call()

add_call(x, mc, 'group_vars', replace_arg = 2)
}
#' @export
groups.simpr_sims = function(x) {
mc = match.call()
mc[[1]] = quote(groups)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
groups.simpr_spec = function(x) {
mc = match.call()

add_call(x, mc, 'groups', replace_arg = 2)
}
#' @export
inner_join.simpr_sims = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", 
    ".y"), ..., keep = FALSE) {
mc = match.call()
mc[[1]] = quote(inner_join)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
inner_join.simpr_spec = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", 
    ".y"), ..., keep = FALSE) {
mc = match.call()

add_call(x, mc, 'inner_join', replace_arg = 2)
}
#' @export
intersect.simpr_sims = function(x, y, ...) {
mc = match.call()
mc[[1]] = quote(intersect)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
intersect.simpr_spec = function(x, y, ...) {
mc = match.call()

add_call(x, mc, 'intersect', replace_arg = 2)
}
#' @export
left_join.simpr_sims = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", 
    ".y"), ..., keep = FALSE) {
mc = match.call()
mc[[1]] = quote(left_join)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
left_join.simpr_spec = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", 
    ".y"), ..., keep = FALSE) {
mc = match.call()

add_call(x, mc, 'left_join', replace_arg = 2)
}
#' @export
mutate_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(mutate_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
mutate_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'mutate_', replace_arg = 2)
}
#' @export
mutate.simpr_sims = function(.data, ...) {
mc = match.call()
mc[[1]] = quote(mutate)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
mutate.simpr_spec = function(.data, ...) {
mc = match.call()

add_call(.data, mc, 'mutate', replace_arg = 2)
}
#' @export
n_groups.simpr_sims = function(x) {
mc = match.call()
mc[[1]] = quote(n_groups)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
n_groups.simpr_spec = function(x) {
mc = match.call()

add_call(x, mc, 'n_groups', replace_arg = 2)
}
#' @export
nest_by.simpr_sims = function(.data, ..., .key = "data", .keep = FALSE) {
mc = match.call()
mc[[1]] = quote(nest_by)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
nest_by.simpr_spec = function(.data, ..., .key = "data", .keep = FALSE) {
mc = match.call()

add_call(.data, mc, 'nest_by', replace_arg = 2)
}
#' @export
nest_join.simpr_sims = function(x, y, by = NULL, copy = FALSE, keep = FALSE, 
    name = NULL, ...) {
mc = match.call()
mc[[1]] = quote(nest_join)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
nest_join.simpr_spec = function(x, y, by = NULL, copy = FALSE, keep = FALSE, 
    name = NULL, ...) {
mc = match.call()

add_call(x, mc, 'nest_join', replace_arg = 2)
}
#' @export
pull.simpr_sims = function(.data, var = -1, name = NULL, ...) {
mc = match.call()
mc[[1]] = quote(pull)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
pull.simpr_spec = function(.data, var = -1, name = NULL, ...) {
mc = match.call()

add_call(.data, mc, 'pull', replace_arg = 2)
}
#' @export
relocate.simpr_sims = function(.data, ..., .before = NULL, .after = NULL) {
mc = match.call()
mc[[1]] = quote(relocate)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
relocate.simpr_spec = function(.data, ..., .before = NULL, .after = NULL) {
mc = match.call()

add_call(.data, mc, 'relocate', replace_arg = 2)
}
#' @export
rename_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(rename_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
rename_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'rename_', replace_arg = 2)
}
#' @export
rename_with.simpr_sims = function(.data, .fn, .cols = everything(), ...) {
mc = match.call()
mc[[1]] = quote(rename_with)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
rename_with.simpr_spec = function(.data, .fn, .cols = everything(), ...) {
mc = match.call()

add_call(.data, mc, 'rename_with', replace_arg = 2)
}
#' @export
rename.simpr_sims = function(.data, ...) {
mc = match.call()
mc[[1]] = quote(rename)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
rename.simpr_spec = function(.data, ...) {
mc = match.call()

add_call(.data, mc, 'rename', replace_arg = 2)
}
#' @export
right_join.simpr_sims = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", 
    ".y"), ..., keep = FALSE) {
mc = match.call()
mc[[1]] = quote(right_join)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
right_join.simpr_spec = function(x, y, by = NULL, copy = FALSE, suffix = c(".x", 
    ".y"), ..., keep = FALSE) {
mc = match.call()

add_call(x, mc, 'right_join', replace_arg = 2)
}
#' @export
rows_delete.simpr_sims = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()
mc[[1]] = quote(rows_delete)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
rows_delete.simpr_spec = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()

add_call(x, mc, 'rows_delete', replace_arg = 2)
}
#' @export
rows_insert.simpr_sims = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()
mc[[1]] = quote(rows_insert)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
rows_insert.simpr_spec = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()

add_call(x, mc, 'rows_insert', replace_arg = 2)
}
#' @export
rows_patch.simpr_sims = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()
mc[[1]] = quote(rows_patch)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
rows_patch.simpr_spec = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()

add_call(x, mc, 'rows_patch', replace_arg = 2)
}
#' @export
rows_update.simpr_sims = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()
mc[[1]] = quote(rows_update)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
rows_update.simpr_spec = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()

add_call(x, mc, 'rows_update', replace_arg = 2)
}
#' @export
rows_upsert.simpr_sims = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()
mc[[1]] = quote(rows_upsert)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
rows_upsert.simpr_spec = function(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE) {
mc = match.call()

add_call(x, mc, 'rows_upsert', replace_arg = 2)
}
#' @export
rowwise.simpr_sims = function(data, ...) {
mc = match.call()
mc[[1]] = quote(rowwise)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
rowwise.simpr_spec = function(data, ...) {
mc = match.call()

add_call(data, mc, 'rowwise', replace_arg = 2)
}
#' @export
same_src.simpr_sims = function(x, y) {
mc = match.call()
mc[[1]] = quote(same_src)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
same_src.simpr_spec = function(x, y) {
mc = match.call()

add_call(x, mc, 'same_src', replace_arg = 2)
}
#' @export
sample_frac.simpr_sims = function(tbl, size = 1, replace = FALSE, weight = NULL, 
    .env = NULL, ...) {
mc = match.call()
mc[[1]] = quote(sample_frac)

tbl[[get_sim_name(tbl)]] =  purrr::map(tbl[[get_sim_name(tbl)]],
                                           ~ eval(mc))
tbl
}

#' @export
sample_frac.simpr_spec = function(tbl, size = 1, replace = FALSE, weight = NULL, 
    .env = NULL, ...) {
mc = match.call()

add_call(tbl, mc, 'sample_frac', replace_arg = 2)
}
#' @export
sample_n.simpr_sims = function(tbl, size, replace = FALSE, weight = NULL, .env = NULL, 
    ...) {
mc = match.call()
mc[[1]] = quote(sample_n)

tbl[[get_sim_name(tbl)]] =  purrr::map(tbl[[get_sim_name(tbl)]],
                                           ~ eval(mc))
tbl
}

#' @export
sample_n.simpr_spec = function(tbl, size, replace = FALSE, weight = NULL, .env = NULL, 
    ...) {
mc = match.call()

add_call(tbl, mc, 'sample_n', replace_arg = 2)
}
#' @export
select_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(select_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
select_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'select_', replace_arg = 2)
}
#' @export
select.simpr_sims = function(.data, ...) {
mc = match.call()
mc[[1]] = quote(select)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
select.simpr_spec = function(.data, ...) {
mc = match.call()

add_call(.data, mc, 'select', replace_arg = 2)
}
#' @export
semi_join.simpr_sims = function(x, y, by = NULL, copy = FALSE, ...) {
mc = match.call()
mc[[1]] = quote(semi_join)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
semi_join.simpr_spec = function(x, y, by = NULL, copy = FALSE, ...) {
mc = match.call()

add_call(x, mc, 'semi_join', replace_arg = 2)
}
#' @export
setdiff.simpr_sims = function(x, y, ...) {
mc = match.call()
mc[[1]] = quote(setdiff)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
setdiff.simpr_spec = function(x, y, ...) {
mc = match.call()

add_call(x, mc, 'setdiff', replace_arg = 2)
}
#' @export
setequal.simpr_sims = function(x, y, ...) {
mc = match.call()
mc[[1]] = quote(setequal)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
setequal.simpr_spec = function(x, y, ...) {
mc = match.call()

add_call(x, mc, 'setequal', replace_arg = 2)
}
#' @export
slice_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(slice_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
slice_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'slice_', replace_arg = 2)
}
#' @export
slice_head.simpr_sims = function(.data, ..., n, prop) {
mc = match.call()
mc[[1]] = quote(slice_head)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
slice_head.simpr_spec = function(.data, ..., n, prop) {
mc = match.call()

add_call(.data, mc, 'slice_head', replace_arg = 2)
}
#' @export
slice_max.simpr_sims = function(.data, order_by, ..., n, prop, with_ties = TRUE) {
mc = match.call()
mc[[1]] = quote(slice_max)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
slice_max.simpr_spec = function(.data, order_by, ..., n, prop, with_ties = TRUE) {
mc = match.call()

add_call(.data, mc, 'slice_max', replace_arg = 2)
}
#' @export
slice_min.simpr_sims = function(.data, order_by, ..., n, prop, with_ties = TRUE) {
mc = match.call()
mc[[1]] = quote(slice_min)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
slice_min.simpr_spec = function(.data, order_by, ..., n, prop, with_ties = TRUE) {
mc = match.call()

add_call(.data, mc, 'slice_min', replace_arg = 2)
}
#' @export
slice_sample.simpr_sims = function(.data, ..., n, prop, weight_by = NULL, 
    replace = FALSE) {
mc = match.call()
mc[[1]] = quote(slice_sample)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
slice_sample.simpr_spec = function(.data, ..., n, prop, weight_by = NULL, 
    replace = FALSE) {
mc = match.call()

add_call(.data, mc, 'slice_sample', replace_arg = 2)
}
#' @export
slice_tail.simpr_sims = function(.data, ..., n, prop) {
mc = match.call()
mc[[1]] = quote(slice_tail)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
slice_tail.simpr_spec = function(.data, ..., n, prop) {
mc = match.call()

add_call(.data, mc, 'slice_tail', replace_arg = 2)
}
#' @export
slice.simpr_sims = function(.data, ..., .preserve = FALSE) {
mc = match.call()
mc[[1]] = quote(slice)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
slice.simpr_spec = function(.data, ..., .preserve = FALSE) {
mc = match.call()

add_call(.data, mc, 'slice', replace_arg = 2)
}
#' @export
summarise_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(summarise_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
summarise_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'summarise_', replace_arg = 2)
}
#' @export
summarise.simpr_sims = function(.data, ..., .groups = NULL) {
mc = match.call()
mc[[1]] = quote(summarise)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
summarise.simpr_spec = function(.data, ..., .groups = NULL) {
mc = match.call()

add_call(.data, mc, 'summarise', replace_arg = 2)
}
#' @export
tally.simpr_sims = function(x, wt = NULL, sort = FALSE, name = NULL) {
mc = match.call()
mc[[1]] = quote(tally)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
tally.simpr_spec = function(x, wt = NULL, sort = FALSE, name = NULL) {
mc = match.call()

add_call(x, mc, 'tally', replace_arg = 2)
}
#' @export
tbl_vars.simpr_sims = function(x) {
mc = match.call()
mc[[1]] = quote(tbl_vars)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
tbl_vars.simpr_spec = function(x) {
mc = match.call()

add_call(x, mc, 'tbl_vars', replace_arg = 2)
}
#' @export
transmute_.simpr_sims = function(.data, ..., .dots = list()) {
mc = match.call()
mc[[1]] = quote(transmute_)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
transmute_.simpr_spec = function(.data, ..., .dots = list()) {
mc = match.call()

add_call(.data, mc, 'transmute_', replace_arg = 2)
}
#' @export
transmute.simpr_sims = function(.data, ...) {
mc = match.call()
mc[[1]] = quote(transmute)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
transmute.simpr_spec = function(.data, ...) {
mc = match.call()

add_call(.data, mc, 'transmute', replace_arg = 2)
}
#' @export
ungroup.simpr_sims = function(x, ...) {
mc = match.call()
mc[[1]] = quote(ungroup)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
ungroup.simpr_spec = function(x, ...) {
mc = match.call()

add_call(x, mc, 'ungroup', replace_arg = 2)
}
#' @export
union_all.simpr_sims = function(x, y, ...) {
mc = match.call()
mc[[1]] = quote(union_all)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
union_all.simpr_spec = function(x, y, ...) {
mc = match.call()

add_call(x, mc, 'union_all', replace_arg = 2)
}
#' @export
union.simpr_sims = function(x, y, ...) {
mc = match.call()
mc[[1]] = quote(union)

x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                           ~ eval(mc))
x
}

#' @export
union.simpr_spec = function(x, y, ...) {
mc = match.call()

add_call(x, mc, 'union', replace_arg = 2)
}
#' @export
complete_.simpr_sims = function(data, cols, fill = list(), ...) {
mc = match.call()
mc[[1]] = quote(complete_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
complete_.simpr_spec = function(data, cols, fill = list(), ...) {
mc = match.call()

add_call(data, mc, 'complete_', replace_arg = 2)
}
#' @export
complete.simpr_sims = function(data, ..., fill = list()) {
mc = match.call()
mc[[1]] = quote(complete)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
complete.simpr_spec = function(data, ..., fill = list()) {
mc = match.call()

add_call(data, mc, 'complete', replace_arg = 2)
}
#' @export
drop_na_.simpr_sims = function(data, vars) {
mc = match.call()
mc[[1]] = quote(drop_na_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
drop_na_.simpr_spec = function(data, vars) {
mc = match.call()

add_call(data, mc, 'drop_na_', replace_arg = 2)
}
#' @export
drop_na.simpr_sims = function(data, ...) {
mc = match.call()
mc[[1]] = quote(drop_na)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
drop_na.simpr_spec = function(data, ...) {
mc = match.call()

add_call(data, mc, 'drop_na', replace_arg = 2)
}
#' @export
expand_.simpr_sims = function(data, dots, ...) {
mc = match.call()
mc[[1]] = quote(expand_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
expand_.simpr_spec = function(data, dots, ...) {
mc = match.call()

add_call(data, mc, 'expand_', replace_arg = 2)
}
#' @export
expand.simpr_sims = function(data, ..., .name_repair = "check_unique") {
mc = match.call()
mc[[1]] = quote(expand)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
expand.simpr_spec = function(data, ..., .name_repair = "check_unique") {
mc = match.call()

add_call(data, mc, 'expand', replace_arg = 2)
}
#' @export
extract_.simpr_sims = function(data, col, into, regex = "([[:alnum:]]+)", 
    remove = TRUE, convert = FALSE, ...) {
mc = match.call()
mc[[1]] = quote(extract_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
extract_.simpr_spec = function(data, col, into, regex = "([[:alnum:]]+)", 
    remove = TRUE, convert = FALSE, ...) {
mc = match.call()

add_call(data, mc, 'extract_', replace_arg = 2)
}
#' @export
extract.simpr_sims = function(data, col, into, regex = "([[:alnum:]]+)", remove = TRUE, 
    convert = FALSE, ...) {
mc = match.call()
mc[[1]] = quote(extract)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
extract.simpr_spec = function(data, col, into, regex = "([[:alnum:]]+)", remove = TRUE, 
    convert = FALSE, ...) {
mc = match.call()

add_call(data, mc, 'extract', replace_arg = 2)
}
#' @export
fill_.simpr_sims = function(data, fill_cols, .direction = c("down", "up")) {
mc = match.call()
mc[[1]] = quote(fill_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
fill_.simpr_spec = function(data, fill_cols, .direction = c("down", "up")) {
mc = match.call()

add_call(data, mc, 'fill_', replace_arg = 2)
}
#' @export
fill.simpr_sims = function(data, ..., .direction = c("down", "up", "downup", 
    "updown")) {
mc = match.call()
mc[[1]] = quote(fill)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
fill.simpr_spec = function(data, ..., .direction = c("down", "up", "downup", 
    "updown")) {
mc = match.call()

add_call(data, mc, 'fill', replace_arg = 2)
}
#' @export
gather_.simpr_sims = function(data, key_col, value_col, gather_cols, na.rm = FALSE, 
    convert = FALSE, factor_key = FALSE) {
mc = match.call()
mc[[1]] = quote(gather_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
gather_.simpr_spec = function(data, key_col, value_col, gather_cols, na.rm = FALSE, 
    convert = FALSE, factor_key = FALSE) {
mc = match.call()

add_call(data, mc, 'gather_', replace_arg = 2)
}
#' @export
gather.simpr_sims = function(data, key = "key", value = "value", ..., na.rm = FALSE, 
    convert = FALSE, factor_key = FALSE) {
mc = match.call()
mc[[1]] = quote(gather)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
gather.simpr_spec = function(data, key = "key", value = "value", ..., na.rm = FALSE, 
    convert = FALSE, factor_key = FALSE) {
mc = match.call()

add_call(data, mc, 'gather', replace_arg = 2)
}
#' @export
nest_legacy.simpr_sims = function(data, ..., .key = "data") {
mc = match.call()
mc[[1]] = quote(nest_legacy)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
nest_legacy.simpr_spec = function(data, ..., .key = "data") {
mc = match.call()

add_call(data, mc, 'nest_legacy', replace_arg = 2)
}
#' @export
nest.simpr_sims = function(.data, ..., .names_sep = NULL, .key = deprecated()) {
mc = match.call()
mc[[1]] = quote(nest)

.data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                           ~ eval(mc))
.data
}

#' @export
nest.simpr_spec = function(.data, ..., .names_sep = NULL, .key = deprecated()) {
mc = match.call()

add_call(.data, mc, 'nest', replace_arg = 2)
}
#' @export
pivot_longer.simpr_sims = function(data, cols, names_to = "name", names_prefix = NULL, 
    names_sep = NULL, names_pattern = NULL, names_ptypes = list(), 
    names_transform = list(), names_repair = "check_unique", 
    values_to = "value", values_drop_na = FALSE, values_ptypes = list(), 
    values_transform = list(), ...) {
mc = match.call()
mc[[1]] = quote(pivot_longer)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
pivot_longer.simpr_spec = function(data, cols, names_to = "name", names_prefix = NULL, 
    names_sep = NULL, names_pattern = NULL, names_ptypes = list(), 
    names_transform = list(), names_repair = "check_unique", 
    values_to = "value", values_drop_na = FALSE, values_ptypes = list(), 
    values_transform = list(), ...) {
mc = match.call()

add_call(data, mc, 'pivot_longer', replace_arg = 2)
}
#' @export
pivot_wider.simpr_sims = function(data, id_cols = NULL, names_from = name, names_prefix = "", 
    names_sep = "_", names_glue = NULL, names_sort = FALSE, names_repair = "check_unique", 
    values_from = value, values_fill = NULL, values_fn = NULL, 
    ...) {
mc = match.call()
mc[[1]] = quote(pivot_wider)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
pivot_wider.simpr_spec = function(data, id_cols = NULL, names_from = name, names_prefix = "", 
    names_sep = "_", names_glue = NULL, names_sort = FALSE, names_repair = "check_unique", 
    values_from = value, values_fill = NULL, values_fn = NULL, 
    ...) {
mc = match.call()

add_call(data, mc, 'pivot_wider', replace_arg = 2)
}
#' @export
replace_na.simpr_sims = function(data, replace, ...) {
mc = match.call()
mc[[1]] = quote(replace_na)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
replace_na.simpr_spec = function(data, replace, ...) {
mc = match.call()

add_call(data, mc, 'replace_na', replace_arg = 2)
}
#' @export
separate_.simpr_sims = function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE, 
    convert = FALSE, extra = "warn", fill = "warn", ...) {
mc = match.call()
mc[[1]] = quote(separate_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
separate_.simpr_spec = function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE, 
    convert = FALSE, extra = "warn", fill = "warn", ...) {
mc = match.call()

add_call(data, mc, 'separate_', replace_arg = 2)
}
#' @export
separate_rows_.simpr_sims = function(data, cols, sep = "[^[:alnum:].]+", convert = FALSE) {
mc = match.call()
mc[[1]] = quote(separate_rows_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
separate_rows_.simpr_spec = function(data, cols, sep = "[^[:alnum:].]+", convert = FALSE) {
mc = match.call()

add_call(data, mc, 'separate_rows_', replace_arg = 2)
}
#' @export
separate_rows.simpr_sims = function(data, ..., sep = "[^[:alnum:].]+", convert = FALSE) {
mc = match.call()
mc[[1]] = quote(separate_rows)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
separate_rows.simpr_spec = function(data, ..., sep = "[^[:alnum:].]+", convert = FALSE) {
mc = match.call()

add_call(data, mc, 'separate_rows', replace_arg = 2)
}
#' @export
separate.simpr_sims = function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE, 
    convert = FALSE, extra = "warn", fill = "warn", ...) {
mc = match.call()
mc[[1]] = quote(separate)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
separate.simpr_spec = function(data, col, into, sep = "[^[:alnum:]]+", remove = TRUE, 
    convert = FALSE, extra = "warn", fill = "warn", ...) {
mc = match.call()

add_call(data, mc, 'separate', replace_arg = 2)
}
#' @export
spread_.simpr_sims = function(data, key_col, value_col, fill = NA, convert = FALSE, 
    drop = TRUE, sep = NULL) {
mc = match.call()
mc[[1]] = quote(spread_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
spread_.simpr_spec = function(data, key_col, value_col, fill = NA, convert = FALSE, 
    drop = TRUE, sep = NULL) {
mc = match.call()

add_call(data, mc, 'spread_', replace_arg = 2)
}
#' @export
spread.simpr_sims = function(data, key, value, fill = NA, convert = FALSE, 
    drop = TRUE, sep = NULL) {
mc = match.call()
mc[[1]] = quote(spread)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
spread.simpr_spec = function(data, key, value, fill = NA, convert = FALSE, 
    drop = TRUE, sep = NULL) {
mc = match.call()

add_call(data, mc, 'spread', replace_arg = 2)
}
#' @export
unite_.simpr_sims = function(data, col, from, sep = "_", remove = TRUE) {
mc = match.call()
mc[[1]] = quote(unite_)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
unite_.simpr_spec = function(data, col, from, sep = "_", remove = TRUE) {
mc = match.call()

add_call(data, mc, 'unite_', replace_arg = 2)
}
#' @export
unite.simpr_sims = function(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE) {
mc = match.call()
mc[[1]] = quote(unite)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
unite.simpr_spec = function(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE) {
mc = match.call()

add_call(data, mc, 'unite', replace_arg = 2)
}
#' @export
unnest_legacy.simpr_sims = function(data, ..., .drop = NA, .id = NULL, .sep = NULL, 
    .preserve = NULL) {
mc = match.call()
mc[[1]] = quote(unnest_legacy)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
unnest_legacy.simpr_spec = function(data, ..., .drop = NA, .id = NULL, .sep = NULL, 
    .preserve = NULL) {
mc = match.call()

add_call(data, mc, 'unnest_legacy', replace_arg = 2)
}
#' @export
unnest.simpr_sims = function(data, cols, ..., keep_empty = FALSE, ptype = NULL, 
    names_sep = NULL, names_repair = "check_unique", .drop = deprecated(), 
    .id = deprecated(), .sep = deprecated(), .preserve = deprecated()) {
mc = match.call()
mc[[1]] = quote(unnest)

data[[get_sim_name(data)]] =  purrr::map(data[[get_sim_name(data)]],
                                           ~ eval(mc))
data
}

#' @export
unnest.simpr_spec = function(data, cols, ..., keep_empty = FALSE, ptype = NULL, 
    names_sep = NULL, names_repair = "check_unique", .drop = deprecated(), 
    .id = deprecated(), .sep = deprecated(), .preserve = deprecated()) {
mc = match.call()

add_call(data, mc, 'unnest', replace_arg = 2)
}