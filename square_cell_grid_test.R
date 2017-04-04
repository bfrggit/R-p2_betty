cat("Cleaning up the environment...", "\n")
rm(list = ls())
cat("\n")

source("lib/square_cell_grid.R")

n_grids = 40L               # number of test grids
n_cases = 100L              # number of cases per test grid
lockBinding("n_grids", globalenv())
lockBinding("n_cases", globalenv())

num_cells_1_max = 100       # maximum number of cells in each col
num_cells_2_max = 100       # maximum number of cells in each row
num_offset_1_max = 200      # maximum number of dim 1 offset
num_offset_2_max = 200      # maximum number of dim 2 offset
lockBinding("num_cells_1_max", globalenv())
lockBinding("num_cells_2_max", globalenv())
lockBinding("num_offset_1_max", globalenv())
lockBinding("num_offset_2_max", globalenv())

map_len_x_min = 1000        # minimum WE length of map
map_len_x_max = 10000       # maximum WE length of map
map_len_y_min = map_len_x_min
map_len_y_max = map_len_x_max
lockBinding("map_len_x_min", globalenv())
lockBinding("map_len_x_max", globalenv())
lockBinding("map_len_y_min", globalenv())
lockBinding("map_len_y_max", globalenv())

# GENERATE test grid parameters
num_cells_1 = as.integer(runif(n_grids, min = 1, max = num_cells_1_max + 1))
num_cells_2 = as.integer(runif(n_grids, min = 1, max = num_cells_2_max + 1))
num_offset_1 = as.integer(runif(
    n_grids,
    min = -num_offset_1_max,
    max = num_offset_1_max + 1
))
num_offset_2 = as.integer(runif(
    n_grids,
    min = -num_offset_2_max,
    max = num_offset_2_max + 1
))
map_len_x = round(runif(n_grids, min = map_len_x_min, max = map_len_x_max), 0)
map_len_y = round(runif(n_grids, min = map_len_y_min, max = map_len_y_max), 0)
cell_len_x = map_len_x / num_cells_2
cell_len_y = map_len_y / num_cells_1

for(jnd in 1L:n_grids) {
    cat("Grid", jnd, "of", n_grids, "\n")

    # CONSTRUCT test grid using parameters
    g_t = SquareCellGrid(
        num_cells_1[jnd],
        num_cells_2[jnd],
        cell_len_x[jnd],
        cell_len_y[jnd],
        num_offset_1[jnd],
        num_offset_2[jnd]
    )

    # GENERATE test cases
    cord_x = runif(
        n_cases,
        min = 0,
        max = map_len_x[jnd]
    ) + num_offset_2[jnd] * cell_len_x[jnd]
    cord_y = runif(
        n_cases,
        min = 0,
        max = map_len_y[jnd]
    ) + num_offset_1[jnd] * cell_len_y[jnd]

    # RUN and COMPARE test cases
    for(knd in 1L:n_cases) {
        cat("Case", knd, "of", n_cases, "-", "")
        c_n = x_y_to_cell_num(g_t, cord_x[knd], cord_y[knd])
        comp_x_y = cell_num_to_x_y(g_t, c_n)
        stopifnot(cord_x[knd] >= comp_x_y[1] && cord_x[knd] < comp_x_y[2])
        stopifnot(cord_y[knd] >= comp_x_y[3] && cord_y[knd] < comp_x_y[4])
        cat("PASS", "\n")
    }

    cat("\n")
}
