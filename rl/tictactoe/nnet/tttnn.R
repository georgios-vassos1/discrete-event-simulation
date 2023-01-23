
TicTacToeNNet <- nn_module(
  "TicTacToeNNet",

  initialize = function(game, args) {
    board_size       <- getBoardSize(game)
    self$board_x     <- board_size[1]
    self$board_y     <- board_size[2]
    self$action_size <- getActionSize(game)
    self$args        <- args

    self$conv1 <- nn_conv2d( # batch_size  x board_x x board_y x num_channels
      in_channels = 1L, out_channels = args[["num_channels"]], kernel_size = 3L, stride = 1L, padding = 1L)
    self$conv2 <- nn_conv2d( # batch_size  x board_x x board_y x num_channels
      in_channels = args[["num_channels"]], out_channels = args[["num_channels"]], kernel_size = 3L, stride = 1L, padding = 1L)
    self$conv3 <- nn_conv2d( # batch_size  x board_x x board_y x num_channels
      in_channels = args[["num_channels"]], out_channels = args[["num_channels"]], kernel_size = 3L, stride = 1L, padding = 1L)
    self$conv4 <- nn_conv2d( # batch_size  x (board_x-2) x (board_y-2) x num_channels
      in_channels = args[["num_channels"]], out_channels = args[["num_channels"]], kernel_size = 3L, stride = 1L)

    self$bn1 <- nn_batch_norm2d(args[["num_channels"]])
    self$bn2 <- nn_batch_norm2d(args[["num_channels"]])
    self$bn3 <- nn_batch_norm2d(args[["num_channels"]])
    self$bn4 <- nn_batch_norm2d(args[["num_channels"]])

    self$fc1    <- nn_linear(args[["num_channels"]]*(self$board_x-2L)*(self$board_y-2L), 1024L)
    self$fc_bn1 <- nn_batch_norm1d(1024L)
    self$fc2    <- nn_linear(1024L, 512L)
    self$fc_bn2 <- nn_batch_norm1d(512L)
    self$fc3    <- nn_linear(512L, self$action_size)
    self$fc4    <- nn_linear(512L, 1L)

  },

  forward = function(x) {
    x %>%                                  # N * 1 * 28 * 28
      torch_flatten(start_dim = 2) %>%     # N * 784

      self$conv1() %>%
      self$bn1() %>%
      nnf_relu() %>%

      self$conv2() %>%
      self$bn2() %>%
      nnf_relu() %>%

      self$conv3() %>%
      self$bn3() %>%
      nnf_relu() %>%

      self$conv4() %>%
      self$bn4() %>%
      nnf_relu() %>%

      self$fc1() %>%
      self$fc_bn1() %>%
      nnf_relu() %>% 
      nn_dropout2d(p = self$åargs[["dropout"]], self$training) %>%

      self$fc2() %>%
      self$fc_bn2() %>%
      nnf_relu() %>% 
      nn_dropout2d(p = self$åargs[["dropout"]], self$training) %>%

      self$fc2() -> x

    pi <- self$fc3(x)
    v  <- self$fc4(x)

    c(nn_log_softmax(pi, dim = 1L), torch_tanh(v))
  }
)
