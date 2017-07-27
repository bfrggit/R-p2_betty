# ga_1.R
#
# Created: 2017-7-22
# Updated: 2017-7-26
#  Coding: Charles Zhu
#
if(!exists("EX_GA_1_R")) {
    EX_GA_1_R <<- TRUE

    source("lib/basic.R")
    source("lib/objective_multi.R")

    suppressPackageStartupMessages(library(GA))

get_fitness_f_ga_1 <<- function(
    mat_w_zero,             # as template for mat_w
    data_type_specs,
    placement_frame,
    this_quota,
    gamma_x,
    gamma_u,
    gamma_y,
    x_mat_prev
) {
    function(chromosome) {
        val_m = ncol(placement_frame)
        val_n = nrow(mat_w_zero)
        val_k = ncol(mat_w_zero)

        vec_w = numeric(val_n * val_k)
        vec_w[ga_capa_avail] = chromosome
        mat_w = matrix(
            data = vec_w,
            nrow = val_n,
            ncol = val_k,
            byrow = FALSE, dimnames = dimnames(mat_w_zero)
        )

        # compute omega mat
        omega_frame_mat = omg_omega_mat(
            val_m = val_m,
            val_n = val_n,
            val_k = val_k,
            placement_frame = placement_frame,
            work_mat_frame = mat_w
        )

        # EVALUATION of coverage
        x_0_frame_mat = omg_x_0_mat(omega_frame_mat)
        tmp_x_cur = omg_x_mat(
            simu_n              = 0L,
            x_0_frame_mat       = x_0_frame_mat,
            arg_x_0_mat_history = array(0, dim = c(val_m, val_k, 1L)),
            arg_s_impact_mat    = s_impact_mat,             # global
            arg_t_impact_mat    = t_impact_mat              # global
        )
        x_frame_mat = 1 - (1 - x_mat_prev) * (1 - tmp_x_cur)
        x_objective_frame_bt = omg_xu_obj_type(x_frame_mat) # bt = by type
        x_objective_frame = omg_xu_obj(data_type_specs, x_objective_frame_bt)

        # EVALUATION of utility
        u_frame_mat = omg_u_mat(omega_frame_mat)
        u_objective_frame_bt = omg_xu_obj_type(u_frame_mat)
        u_objective_frame = omg_xu_obj(data_type_specs, u_objective_frame_bt)

        # EVALUATION of num of active nodes
        y_frame_vec = omg_y_vec(mat_w)
        y_objective_frame = sum(y_frame_vec) / val_m / val_k

        # EVALUATION of traffic
        d_frame_vec = omg_d_vec(data_type_specs, mat_w)
        d_objective_frame = sum(d_frame_vec)

        # handle quota constraint
        if(d_objective_frame > this_quota) return(0)

        # compute overall performance
        obj_overall = max(0, gamma_x * x_objective_frame +
                             gamma_u * u_objective_frame +
                             gamma_y * y_objective_frame) # RETURN
    } # RETURN
}

ga_reduce_chromosome = function(chromosome, this_quota) {
    while(any(chromosome > 0) &&
        sum(chromosome * ga_data_rates) > this_quota) { # global
        chromosome[sample(which(chromosome > 0), 1L)] = 0
    }
    chromosome # RETURN
}

get_population_f_ga_1 <<- function(base_f, this_quota) {
    function(...) {
        population = base_f(...)

        for(xnd in 1:nrow(population)) {
            population[xnd, ] = ga_reduce_chromosome(
                population[xnd, ],
                this_quota
            )
        }
        population # RETURN
    } # RETURN
}

get_crossover_f_ga_1 <<- function(base_f, this_quota) {
    function(...) {
        children_list = base_f(...)

        for(xnd in 1:nrow(children_list$children)) {
            children_list$children[xnd, ] = ga_reduce_chromosome(
                children_list$children[xnd, ],
                this_quota
            )
        }
        children_list # RETURN
    } # RETURN
}

get_mutation_f_ga_1 <<- function(base_f, this_quota) {
    function(...) {
        mutate = ga_reduce_chromosome(base_f(...), this_quota)
    } # RETURN
}

calc_work_mat_ga_1 <<- function(
    t_frame,                # time frame length, sec
    simu_n,                 # time frame number, on which simulation is running
    val_n,
    val_m,
    val_k,
    grid,                   # unused in this example implementation
    data_type_specs,
    capacity_mat,
    get_placement_f,
    local_util_f,           # unused in this example implementation
    data_quota,
    verbose = FALSE,
    gamma_x,
    gamma_u,
    gamma_y,
    ga_parallel = FALSE,
    ga_seed = NULL
) {
    # this function, as well as its alternatives, is supposed to be called
    # very often. no type check occurs here. data types should be checked in
    # the simulation script

    mat_w_zero = matrix(
        0,
        nrow = val_n,
        ncol = val_k
    )
    rownames(mat_w_zero) = z_nd_str("n", val_n)
    colnames(mat_w_zero) = z_nd_str("d", val_k)

    # override verbose msg
    # verbose = FALSE

    # prepare data for quick access
    placement_roll = get_placement_f()
    placement_frame = placement_roll[, , simu_n + 1L]
    duration_frames = dim(placement_roll)[3] - 1L
    xu_mat_zero = matrix(0, nrow = val_m, ncol = val_k)
    rownames(xu_mat_zero) = z_nd_str("c", val_m)
    colnames(xu_mat_zero) = z_nd_str("d", val_k)

    # generate global vectors for constraint check
    if(simu_n <= 0L) {
        # accquire available sensors from capacity_mat
        ga_capaci_vec <<- as.vector(capacity_mat)
        ga_capa_avail <<- which(ga_capaci_vec > 0)
        # ga_capa_zeros <<- which(ga_capaci_vec == 0)

        # GA will only consider available sensors and ignore the zeros
        ga_data_rates <<- as.vector(
            matrix(
                rep(data_type_specs$rate, val_n),
                ncol = val_k,
                byrow = TRUE
            )
        )[ga_capa_avail]
    }

    # adaptive data quota
    if(simu_n <= 0L) {
        ga_1_data_history               <<- rep(0, duration_frames + 1L)
        names(ga_1_data_history)        <<- dimnames(placement_roll)[[3]]
        ga_1_quota_history              <<- ga_1_data_history
        ga_1_quota_history[simu_n + 1L] <<- data_quota
    } else {
        ga_1_quota_history[simu_n + 1L] <<- data_quota +
            ga_1_quota_history[simu_n] -
            ga_1_data_history[simu_n]
    }
    this_quota = ga_1_quota_history[simu_n + 1L]

    # initial mat x at current time frame, assuming zero mat x_0
    x_mat_prev = xu_mat_zero
    if(simu_n > 0L) {
        x_mat_prev[] = omg_x_mat(
            simu_n              = simu_n,
            x_0_frame_mat       = xu_mat_zero,
            arg_x_0_mat_history = x_0_mat_history,  # global
            arg_s_impact_mat    = s_impact_mat,     # global
            arg_t_impact_mat    = t_impact_mat      # global
        )
    }

    #-----------------------------------------------------------------------
    # all lines above are preparation for context

    # num_chosen = 0L
    if(verbose) cat("...", "\n")

    # prepare parameters for GA call
    ga_pop_size = 50L
    ga_max_iter = 200L

    ga_it_num <<- 0L
    ga_proc_t_0 <<- proc.time()[3]
    ga_proc_t <<- ga_proc_t_0
    ga_proc_t_diff = 0
    ga_update_proc_t_f = function() {
        ga_it_num <<- ga_it_num + 1L
        ga_proc_t_diff <<- proc.time()[3] - ga_proc_t
        ga_proc_t <<- proc.time()[3]
    }

    # ga_monitor_f = FALSE
    if(verbose) {
        ga_monitor_f = function(obj) {
            ga_update_proc_t_f()
            cat(
                sprintf("    Iteration num = %d,", ga_it_num),
                sprintf("proc_t = %.0f msec,", 1000 * ga_proc_t_diff),
                sprintf("fitness = %.4f", max(obj@fitness)),
                "\n"
            )
        }
    } else ga_monitor_f = function(obj) {
        ga_update_proc_t_f()
    }

    # post fitness evaluation function to stop GA once time frame is up
    ga_post_fit_f = function(obj) {
        if(proc.time()[3] - ga_proc_t_0 + ga_proc_t_diff > t_frame) {
            obj@iter = ga_max_iter
        }
        obj # RETURN
    }

    # run GA algorithm
    ga_obj = ga(
        type = "binary",
        nBits = length(ga_capa_avail), # nBits = val_n * val_k,
        fitness = get_fitness_f_ga_1(
            mat_w_zero      = mat_w_zero,
            data_type_specs = data_type_specs,
            placement_frame = placement_frame,
            this_quota      = this_quota,
            gamma_x         = gamma_x,
            gamma_u         = gamma_u,
            gamma_y         = gamma_y,
            x_mat_prev      = x_mat_prev
        ),
        population = get_population_f_ga_1(gabin_Population, this_quota),
        crossover = get_crossover_f_ga_1(gabin_spCrossover, this_quota),
        mutation = get_mutation_f_ga_1(gabin_raMutation, this_quota),
        # min = as.vector(mat_w_zero),    # these settings do not work for bin
        # max = as.vector(capacity_mat),  # these settings do not work for bin
        popSize = ga_pop_size,  # default value = 50
        pcrossover = 0.8,       # default value = 0.8
        pmutation = 0.2,        # default value = 0.1
        elitism = round(0.05 * ga_pop_size),
        updatePop = FALSE,      # do not use this experimental feature for now
        postFitness = ga_post_fit_f,
        maxiter = ga_max_iter,  # default value = 100
        run = 20L,              # default value = maxiter
        maxFitness = Inf,
        names = NULL,
        suggestions = NULL,
        parallel = ga_parallel,
        monitor = ga_monitor_f,        # do not plot during execution
        seed = ga_seed
    )

    # extract solution from GA object
    vec_w = numeric(val_n * val_k)
    if(is.matrix(ga_obj@solution)) {
        vec_w[ga_capa_avail] = ga_obj@solution[1, ]
    } else if(is.vector(ga_obj@solution)) {
        vec_w[ga_capa_avail] = ga_obj@solution
    }
    mat_w = matrix(
        vec_w,
        nrow = val_n,
        ncol = val_k,
        byrow = FALSE,
        dimnames = dimnames(mat_w_zero)
    )

    # print(score)
    if(verbose) {
        cat(
            sprintf("    Iteration end = %d", ga_it_num),
            "...", ""
        )
    }
    ga_1_data_history[simu_n + 1L] <<-
        sum(colSums(mat_w) * data_type_specs$rate)

    mat_w # RETURN
}

get_calc_work_mat_ga_1_f <<- function(
    gamma_x,
    gamma_u,
    gamma_y,
    parallel = FALSE,
    seed = NULL
){
    function(...) {
        calc_work_mat_ga_1(
            ...,
            gamma_x = gamma_x,
            gamma_u = gamma_u,
            gamma_y = gamma_y,
            ga_parallel = parallel,
            ga_seed = seed
        )
    } # RETURN
}

} # ENDIF
