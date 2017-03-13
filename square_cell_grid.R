setClass(
    "SquareCellGrid",
    
    slots = c(
        cell_len_x = "numeric",
        cell_len_y = "numeric",
        numbering = "matrix",
        num_offset_x = "integer",
        num_offset_y = "integer"
    ),
    
    prototype = list(
        numbering = matrix(NA, 0, 0)
    )
)

# constructor
SquareCellGrid = function(
    num_cells_x, num_cells_y,
    cell_len_x, cell_len_y,
    num_offset_x = 0, num_offset_y = 0
) {
    stopifnot(num_cells_x == as.integer(num_cells_x))
    stopifnot(num_cells_y == as.integer(num_cells_y))
    stopifnot(num_offset_x == as.integer(num_offset_x))
    stopifnot(num_offset_y == as.integer(num_offset_y))
    stopifnot(num_cells_x > 0)
    stopifnot(num_cells_y > 0)
    # stopifnot(num_offset_x > 0)
    # stopifnot(num_offset_y > 0)
    
    mat_numbering = matrix(
        1:(num_cells_x * num_cells_y),
        nrow = num_cells_y,
        ncol = num_cells_x,
        byrow = TRUE
    )
    new("SquareCellGrid",
        cell_len_x = cell_len_x,
        cell_len_y = cell_len_y,
        num_offset_x = as.integer(num_offset_x),
        num_offset_y = as.integer(num_offset_y),
        numbering = mat_numbering
    )
}

setGeneric(
    name = "cord_to_num",
    def = function(object, cord_x, cord_y) {
        standardGeneric("cord_to_num")
    }
)

setMethod(
    f = "cord_to_num",
    signature = "SquareCellGrid",
    definition = function(object, cord_x, cord_y) {
        # object
        # cord_x    numeric
        # cord_y    numeric
        bnd_lt = -object@num_offset_x * object@cell_len_x
        bnd_tp = -object@num_offset_y * object@cell_len_y
        stopifnot(cord_x > bnd_lt)
        stopifnot(cord_y > bnd_tp)
        stopifnot(cord_x < bnd_lt + ncol(object@numbering) * object@cell_len_x)
        stopifnot(cord_y < bnd_tp + nrow(object@numbering) * object@cell_len_y)
        
        c_x = (cord_x - bnd_lt) / object@cell_len_x + 1
        c_y = (cord_y - bnd_tp) / object@cell_len_y + 1
        return(object@numbering[c_y, c_x])
    }
)