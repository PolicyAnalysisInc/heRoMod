context("Matrix objects")


test_that(
  "Matrix definition", {
    mat1 <- define_transition(
      state_names = c("X1", "X2"),
      .3, .7,
      .6, .4
    )
    expect_output(
      str(mat1),
      'List of 4
 $ cell_1_1:List of 2
  ..$ expr: num 0.3',
      fixed = TRUE
    )
    expect_error(
      define_transition(
        state_names = c("X1", "X1"),
        .3, .7,
        .6, .4
      )
    )
    expect_error(
      define_transition(
        state_names = c("X1", "X2", "X3"),
        .3, .7,
        .6, .4
      )
    )
    expect_error(
      define_transition(
        state_names = c("X1", "X2"),
        .3, .7,
        .6, .4, .4
      )
    )
    expect_error(
      modify(
        mat1,
        marcel = .4,
        cell_1_2 = .6
      )
    )
    expect_output(
      str(
        modify(
          mat1,
          cell_1_1 = .4,
          cell_1_2 = .6
        )
      ),
      'List of 4
 $ cell_1_1:List of 2
  ..$ expr: num 0.4',
      fixed = TRUE
    )
    expect_output(
      print(mat1),
      'A transition matrix, 2 states.

   X1  X2 
X1 0.3 0.7
X2 0.6 0.4',
      fixed = TRUE
    )
  }
)

test_that(
  "Functions on matrix objects", {
    mat1 <- define_transition(
      state_names = c("X1", "X2"),
      .3, .7,
      .6, .4
    )
    plot(mat1)
    expect_equal(
      heRomod:::get_matrix_order(mat1),
      2
    )
    expect_equal(
      get_state_names(mat1),
      c("X1", "X2")
    )
    # 
    # test_array <- array(0, dim = c(2, 2, 2))
    # test_array[1,,] <- c(1, -1, 0, 2)
    # test_array[2,,] <- c(1, 0, 1, 1)
    # attr(test_array, "state_names") <- c("A", "B")
    
    test_array <- tibble(
      model_time = c(1,1,1,1,2,2,2,2),
      .from = c("A", "A", "B", "B", "A", "A", "B", "B"),
      .from_e = c("A", "A", "B", "B", "A", "A", "B", "B"),
      .to = c("A", "B", "A", "B", "A", "B", "A", "B"),
      .to_e = c("A", "B", "A", "B", "A", "B", "A", "B"),
      .value = c(1,1,-1,0,0,1,2,1)
    )
    
    expect_error(
      check_matrix(test_array),
      "rows sum to 1"
    )
    
    test_array$.value <- c(2, -1, -1, 2, -30, 31, 0, 1)
    expect_error(
      check_matrix(test_array),
      "outside the interval [0 - 1]",
      fixed = TRUE
    )
    
    ## test that we get expected error with expanded states
    par1 <- define_parameters(a = ifelse(state_time == 3, 1.1, 0.5))
    mat1 <- define_transition(a, C, 0.2, 0.8, state_names = c("A","B"))
    
    A1 <- define_state(cost = 1, utility = 1)
    B1 <- define_state(cost = 2, utility = 2)
    st1 <- define_strategy(A = A1, B = B1, transition = mat1)
    
    expect_error(run_model(
      st1, init = c(100, 0), cycles = 5, parameters = par1
    ),
    "outside the interval [0 - 1]",
    fixed = TRUE)
    ## and that it works without the error
    par1 <- define_parameters(a = ifelse(state_time == 3, 0.4, 0.5))
    
    expect_identical(
      class(run_model(st1, init = c(100, 0), cycles = 5, parameters = par1,
              cost = cost, effect = utility))[1],
      "run_model")
  }
)

test_that(
  "Matrix evaluation", {
    par1 <- define_parameters(
      a = .1,
      b = 1 / (markov_cycle + 1)
    )
    mat1 <- define_transition(
      state_names = c("X1", "X2"),
      1-a, a,
      1-b, b
    )
    matC <- define_transition(
      state_names = c("X1", "X2"),
      C, a,
      C, b
    )
    e_par1 <- heRomod:::eval_parameters(
      par1, 10
    )
    e_mat <- heRomod:::eval_transition.uneval_matrix(
      mat1, e_par1
    )
    e_matC <- heRomod:::eval_transition.uneval_matrix(
      matC, e_par1
    )
    
    # Evaluated matrices should have dim names
    expect_output(
      str(e_mat),
      'List of 10
 $ 1 :Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.5 0.1 0.5
  .. ..@ factors : list()
 $ 2 :Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.667 0.1 0.333
  .. ..@ factors : list()
 $ 3 :Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.75 0.1 0.25
  .. ..@ factors : list()
 $ 4 :Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.8 0.1 0.2
  .. ..@ factors : list()
 $ 5 :Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.833 0.1 0.167
  .. ..@ factors : list()
 $ 6 :Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.857 0.1 0.143
  .. ..@ factors : list()
 $ 7 :Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.875 0.1 0.125
  .. ..@ factors : list()
 $ 8 :Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.889 0.1 0.111
  .. ..@ factors : list()
 $ 9 :Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.9 0.1 0.1
  .. ..@ factors : list()
 $ 10:Formal class \'dgCMatrix\' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:4] 0 1 0 1
  .. ..@ p       : int [1:3] 0 2 4
  .. ..@ Dim     : int [1:2] 2 2
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. .. ..$ : chr [1:2] "X1" "X2"
  .. ..@ x       : num [1:4] 0.9 0.9091 0.1 0.0909
  .. ..@ factors : list()
 - attr(*, "class")= chr [1:2] "eval_matrix" "list"
 - attr(*, "state_names")= chr [1:2] "X1" "X2"
 - attr(*, "entry")= logi [1:2] TRUE TRUE',
      fixed = TRUE
    )
    expect_output(
      print(e_mat),
      'An evaluated transition matrix, 2 states, 10 markov cycles.

State names:

X1
X2

$`1`
    X1  X2
X1 0.9 0.1
X2 0.5 0.5',
      fixed = TRUE
    )
    expect_equal(
      get_state_names(e_mat),
      c("X1", "X2")
    )
    expect_equal(
      get_matrix_order(e_mat), 2
    )
    expect_equal(e_mat, e_matC)
  }
)


test_that(
  "C bug #82 doesnt come back", {
    sampleTM <- define_transition(0.1, 0.1, C, C, 0.3, 0.25, C, 0, 0.5)
    A <- define_state(cost = 1, utility = 2)
    B <- define_state(cost = 5, utility = 7)
    C <- define_state(cost = 4, utility = 4)
    sample_mod <- define_strategy(transition = sampleTM, A = A,
                                  B = B, C = C)
    res <- run_model(sample_mod, cost = cost, effect = utility,
                     method = "beginning")
    
    
    expect_equal(
      res$run_model$cost, 3800
    )
  }
)
