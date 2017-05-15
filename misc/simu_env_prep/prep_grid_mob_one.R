# prep_grid_mob_one.R
#
# Created: 2017-5-14
#  Author: Charles Zhu
#
rm(list = ls())

require(methods)

# simulation CONSTANTS
num_cells_1 = 7L
num_cells_2 = 9L
cell_len_x = 500
cell_len_y = 500
num_offset_1 = -5 / cell_len_y
num_offset_2 = -5 / cell_len_x

# CONSTRUCT test grid using parameters
source("lib/square_cell_grid.R")

grid = SquareCellGrid(
    num_cells_1,
    num_cells_2,
    cell_len_x,
    cell_len_y,
    num_offset_1,
    num_offset_2
)

save(
    num_cells_1, num_cells_2,
    cell_len_x, cell_len_y,
    num_offset_1, num_offset_2,
    grid,
    file = sprintf("prep_RData/grid_mob_one.RData")
)
