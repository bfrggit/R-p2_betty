# CLASS DEFINITION OF SquareCellGrid
setClass(
    "SquareCellGrid",
    
    representation(
        num_cells_1 = "integer",
        num_cells_2 = "integer",
        num_offset_1 = "integer",
        num_offset_2 = "integer",
        cell_len_x = "numeric",         # WE direction length
        cell_len_y = "numeric",         # NS direction length
        numbering = "matrix"
    ),
    
    prototype = list(
        numbering = matrix(NA, 0, 0)
    ),
    
    validity = function(object) {
        if(object@num_cells_1 <= 0)     return(FALSE)
        if(object@num_cells_2 <= 0)     return(FALSE)
        if(object@cell_len_x <= 0)      return(FALSE)
        if(object@cell_len_y <= 0)      return(FALSE)
        if(nrow(object@numbering) != object@num_cells_1)    return(FALSE)
        if(ncol(object@numbering) != object@num_cells_2)    return(FALSE)
        TRUE
    }
)

# CONSTRUCTOR OF SquareCellGrid
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
        num_offset_1 = "integer",
        num_offset_2 = "integer"
    ),

    function(
        num_cells_1,
        num_cells_2,
        cell_len_x,
        cell_len_y,
        num_offset_1,
        num_offset_2
    ) {
        stopifnot(num_cells_1 > 0)
        stopifnot(num_cells_2 > 0)
        
        mat_numbering = matrix(
            1:(num_cells_1 * num_cells_2),
            nrow = num_cells_1,
            ncol = num_cells_2,
            byrow = TRUE
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

# METHODS OF SquareCellGrid
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
        bnd_lt = -object@num_offset_1 * object@cell_len_x
        bnd_tp = -object@num_offset_2 * object@cell_len_y
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
        c = x_y_to_cell_1_2(object, x, y)
        cell_1_2_to_num(object, c[1], c[2])
    }
)