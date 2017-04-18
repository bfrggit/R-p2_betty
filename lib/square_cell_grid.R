# square_cell_grid.R
#
# Created: As part of the initial version of the project
#  Author: Charles Zhu
#
if(!exists("EX_SQUARE_CELL_GRID_R")) {
    EX_SQUARE_CELL_GRID_R <<- TRUE

# CLASS DEFINITION of SquareCellGrid
setClass(
    "SquareCellGrid",

    representation(
        num_cells_1 = "integer",        # number of rows
        num_cells_2 = "integer",        # number of cols
        num_offset_1 = "numeric",
        num_offset_2 = "numeric",
        cell_len_x = "numeric",         # WE direction length
        cell_len_y = "numeric",         # NS direction length
        numbering = "matrix"
    ),

    prototype = list(
        numbering = matrix(NA, 0L, 0L)
    ),

    validity = function(object) {
        if(object@num_cells_1 <= 0L)     return(FALSE)
        if(object@num_cells_2 <= 0L)     return(FALSE)
        if(object@cell_len_x <= 0)      return(FALSE)
        if(object@cell_len_y <= 0)      return(FALSE)
        if(nrow(object@numbering) != object@num_cells_1)    return(FALSE)
        if(ncol(object@numbering) != object@num_cells_2)    return(FALSE)
        TRUE
    }
)

# CONSTRUCTOR of SquareCellGrid
setGeneric(
    "SquareCellGrid",

    valueClass = "SquareCellGrid",

    function(
        num_cells_1,
        num_cells_2,
        cell_len_x,
        cell_len_y,
        num_offset_1,
        num_offset_2
    ) {
        standardGeneric("SquareCellGrid")
    }
)

setMethod(
    "SquareCellGrid",

    signature(
        num_cells_1 = "integer",
        num_cells_2 = "integer",
        cell_len_x = "numeric",
        cell_len_y = "numeric",
        num_offset_1 = "numeric",
        num_offset_2 = "numeric"
    ),

    function(
        num_cells_1,
        num_cells_2,
        cell_len_x,
        cell_len_y,
        num_offset_1,
        num_offset_2
    ) {
        stopifnot(length(num_cells_1) == 1L)
        stopifnot(length(num_cells_2) == 1L)
        stopifnot(num_cells_1 > 0L)
        stopifnot(num_cells_2 > 0L)

        stopifnot(length(cell_len_x) == 1L)
        stopifnot(length(cell_len_y) == 1L)
        stopifnot(cell_len_x > 0)
        stopifnot(cell_len_y > 0)

        stopifnot(length(num_offset_1) == 1L)
        stopifnot(length(num_offset_2) == 1L)

        mat_numbering = matrix(
            1L:(num_cells_1 * num_cells_2),
            nrow = num_cells_1,
            ncol = num_cells_2,
            byrow = TRUE
        )
        rownames(mat_numbering) = paste(
            as.character((0L:(num_cells_1 - 1L) + num_offset_1) * cell_len_y),
            "",
            sep = "_"
        )
        colnames(mat_numbering) = paste(
            as.character((0L:(num_cells_2 - 1L) + num_offset_2) * cell_len_x),
            "",
            sep = "_"
        )

        new("SquareCellGrid",
            num_cells_1 = num_cells_1,
            num_cells_2 = num_cells_2,
            num_offset_1 = num_offset_1,
            num_offset_2 = num_offset_2,
            cell_len_x = cell_len_x,
            cell_len_y = cell_len_y,
            numbering = mat_numbering
        ) # RETURN
    }
)

# METHODS of SquareCellGrid
setGeneric(
    "x_y_to_cell_1_2",

    valueClass = "integer",

    function(object, x, y) {
        standardGeneric("x_y_to_cell_1_2")
    }
)

setMethod(
    "x_y_to_cell_1_2",

    signature(
        object = "SquareCellGrid",
        x = "numeric",
        y = "numeric"
    ),

    function(object, x, y) {
        bnd_lt = object@num_offset_2 * object@cell_len_x
        bnd_tp = object@num_offset_1 * object@cell_len_y
        stopifnot(x > bnd_lt)
        stopifnot(y > bnd_tp)
        stopifnot(x < bnd_lt + ncol(object@numbering) * object@cell_len_x)
        stopifnot(y < bnd_tp + nrow(object@numbering) * object@cell_len_y)
        c_2 = as.integer((x - bnd_lt) / object@cell_len_x) + 1L
        c_1 = as.integer((y - bnd_tp) / object@cell_len_y) + 1L

        c(c_1, c_2) # RETURN
    }
)

setGeneric(
    "cell_1_2_to_num",

    valueClass = "integer",

    function(object, c_1, c_2) {
        standardGeneric("cell_1_2_to_num")
    }
)

setMethod(
    "cell_1_2_to_num",

    signature(
        c_1 = "integer",
        c_2 = "integer"
    ),

    function(object, c_1, c_2) {
        object@numbering[c_1, c_2] # RETURN
    }
)

setGeneric(
    "x_y_to_cell_num",

    valueClass = "integer",

    function(object, x, y) {
        standardGeneric("x_y_to_cell_num")
    }
)

setMethod(
    "x_y_to_cell_num",

    signature(
        object = "SquareCellGrid",
        x = "numeric",
        y = "numeric"
    ),

    function(object, x, y) {
        c_c = x_y_to_cell_1_2(object, x, y)

        cell_1_2_to_num(object, c_c[1], c_c[2]) # RETURN
    }
)

setGeneric(
    "cell_num_to_1_2",

    valueClass = "integer",

    function(object, c_n) {
        standardGeneric("cell_num_to_1_2")
    }
)

setMethod(
    "cell_num_to_1_2",

    signature(
        object = "SquareCellGrid",
        c_n = "integer"
    ),

    function(object, c_n) {
        stopifnot(c_n > 0)
        stopifnot(length(c_n) == 1)
        stopifnot(c_n <= object@num_cells_1 * object@num_cells_2)

        c_1 = (c_n - 1L) %/% object@num_cells_2 + 1L
        c_2 = (c_n - 1L)  %% object@num_cells_2 + 1L

        c(c_1, c_2) # RETURN
    }
)

setGeneric(
    "cell_1_2_to_x_y",

    valueClass = "numeric",

    function(object, c_1, c_2) {
        standardGeneric("cell_1_2_to_x_y")
    }
)

setMethod(
    "cell_1_2_to_x_y",

    signature(
        object = "SquareCellGrid",
        c_1 = "integer",
        c_2 = "integer"
    ),

    function(object, c_1, c_2) {
        x_min = (c_2 - 1 + object@num_offset_2) * 1 * object@cell_len_x
        x_max = x_min + object@cell_len_x
        y_min = (c_1 - 1 + object@num_offset_1) * 1 * object@cell_len_y
        y_max = y_min + object@cell_len_y

        c(x_min, x_max, y_min, y_max) # RETURN
    }
)

setGeneric(
    "cell_num_to_x_y",

    valueClass = "numeric",

    function(object, c_n) {
        standardGeneric("cell_num_to_x_y")
    }
)

setMethod(
    "cell_num_to_x_y",

    signature(
        object = "SquareCellGrid",
        c_n = "integer"
    ),

    function(object, c_n) {
        c_c = cell_num_to_1_2(object, c_n)

        cell_1_2_to_x_y(object, c_c[1], c_c[2]) # RETURN
    }
)

setGeneric(
    "cell_dist",

    valueClass = "numeric",

    function(object, c_n_1, c_n_2) {
        standardGeneric("cell_dist")
    }
)

# METHOD to compute distance for a pair of cells using cell numbers
setMethod(
    "cell_dist",

    signature(
        object = "SquareCellGrid",
        c_n_1 = "integer",
        c_n_2 = "integer"
    ),

    function(object, c_n_1, c_n_2) {
        if(c_n_1 == c_n_2) return(0)

        c_c_1 = cell_num_to_1_2(object, c_n_1)
        c_c_2 = cell_num_to_1_2(object, c_n_2)
        dif_1 = (c_c_1[1] - c_c_2[1]) * object@cell_len_y
        dif_2 = (c_c_1[2] - c_c_2[2]) * object@cell_len_x

        sqrt(dif_1 * dif_1 + dif_2 * dif_2) # RETURN
    }
)

setGeneric(
    "cell_dist_mat",

    valueClass = "matrix",

    function(object) {
        standardGeneric("cell_dist_mat")
    }
)

# METHOD to compute distance matrix for pairs of cell numbers
setMethod(
    "cell_dist_mat",

    signature(
        object = "SquareCellGrid"
    ),

    function(object) {
        val_m = object@num_cells_1 * object@num_cells_2
        grid_dist_mat = matrix(0, nrow = val_m, ncol = val_m)
        rownames(grid_dist_mat) = z_nd_str("c", val_m)
        colnames(grid_dist_mat) = z_nd_str("c", val_m)
        for(ind in 1:val_m) {
            for(lnd in 1:val_m) {
                grid_dist_mat[ind, lnd] = cell_dist(object, ind, lnd)
            }
        }

        grid_dist_mat # RETURN
    }
)

} # ENDIF
