# Dataset

n_timesteps <- 7 * 24 * 2 # days * hours * half-hours
n_forecast  <- 7 * 24 * 2 # days * hours * half-hours
batch_size  <- 32

vic_elec_get_year <- function(year, month = NULL) {
  tsibbledata::vic_elec |>
    dplyr::filter(lubridate::year(Date) == year, lubridate::month(Date) == if (is.null(month)) lubridate::month(Date) else month) |>
    tibble::as_tibble() |>
    dplyr::select(Demand)
}

elec_train <- vic_elec_get_year(2012) |> as.matrix()
elec_valid <- vic_elec_get_year(2013) |> as.matrix()
elec_test  <- vic_elec_get_year(2014, 1) |> as.matrix()

train_mean <- mean(elec_train)
train_sd <- sd(elec_train)

# Data input

elec_dataset <- torch::dataset(
  name = "elec_dataset",

  initialize = function(x, n_timesteps, n_forecast, sample_frac = 1) {

    self$n_timesteps <- n_timesteps
    self$n_forecast  <- n_forecast
    self$x <- torch::torch_tensor((x - train_mean) / train_sd)

    n <- length(self$x) - self$n_timesteps - self$n_forecast + 1

    self$starts <- sort(sample.int(
      n = n,
      size = n * sample_frac
    ))

  },

  .getitem = function(i) {

    start <- self$starts[i]
    end   <- start + self$n_timesteps - 1
    pred_length <- self$n_forecast

    list(
      x = self$x[start:end],
      y = self$x[(end + 1):(end + pred_length)]$squeeze(2)
    )

  },

  .length = function() {
    length(self$starts) 
  }
)

train_ds <- elec_dataset(elec_train, n_timesteps, n_forecast, sample_frac = 0.5)
train_dl <- train_ds |> torch::dataloader(batch_size = batch_size, shuffle = TRUE)

valid_ds <- elec_dataset(elec_valid, n_timesteps, n_forecast, sample_frac = 0.5)
valid_dl <- valid_ds |> torch::dataloader(batch_size = batch_size)

test_ds <- elec_dataset(elec_test, n_timesteps, n_forecast)
test_dl <- test_ds |> torch::dataloader(batch_size = 1)

# Model

model <- torch::nn_module(

  initialize = function(type, input_size, hidden_size, linear_size, output_size,
                        num_layers = 1L, dropout = 0.0, linear_dropout = 0.0) {
    self$type           <- type
    self$num_layers     <- num_layers
    self$linear_dropout <- linear_dropout

    self$rnn <- if (self$type == "gru") {
      torch::nn_gru(
        input_size  = input_size,
        hidden_size = hidden_size,
        num_layers  = num_layers,
        dropout     = dropout,
        batch_first = TRUE
      )
    } else {
      nn_lstm(
        input_size  = input_size,
        hidden_size = hidden_size,
        num_layers  = num_layers,
        dropout     = dropout,
        batch_first = TRUE
      )
    }

    self$nlp <- torch::nn_sequential(
      torch::nn_linear(hidden_size, linear_size),
      torch::nn_relu(),
      torch::nn_dropout(linear_dropout),
      torch::nn_linear(linear_size, output_size)
    )
  },

  forward = function(x) {
    x <- self$rnn(x)
    x[[1L]][,-1L,..] |>
      self$mlp()
  }
)

net <- model("gru", input_size = 1L, hidden_size = 32L, linear_size = 512L, output_size = n_forecast, linear_dropout = .0)
net <- net$to(device = "cpu")

# Training

optimizer <- torch::optim_adam(net$parameters, lr = .001)

num_epochs <- 30

train_batch <- function(b) {

  optimizer$zero_grad()
  output <- net(b$x$to(device = device))
  target <- b$y$to(device = device)

  loss <- torch::nnf_mse_loss(output, target)
  loss$backward()
  optimizer$step()

  loss$item()
}

valid_batch <- function(b) {

  output <- net(b$x$to(device = device))
  target <- b$y$to(device = device)

  loss <- torch::nnf_mse_loss(output, target)
  loss$item()
  
}

for (epoch in 1:num_epochs) {

  net$train()
  train_loss <- c()

  coro::loop(for (b in train_dl) {
    loss <- train_batch(b)
    train_loss <- c(train_loss, loss)
  })

  cat(sprintf("\nEpoch %d, training: loss: %3.5f \n", epoch, mean(train_loss)))

  net$eval()
  valid_loss <- c()

  coro::loop(for (b in valid_dl) {
    loss <- valid_batch(b)
    valid_loss <- c(valid_loss, loss)
  })

  cat(sprintf("\nEpoch %d, validation: loss: %3.5f \n", epoch, mean(valid_loss)))
}

# Evaluation

net$eval()

test_preds <- vector(mode = "list", length = length(test_dl))

i <- 1

coro::loop(for (b in test_dl) {
  
  input  <- b$x
  output <- net(input$to(device = device))
  preds  <- as.numeric(output)
  
  test_preds[[i]] <- preds
  i <<- i + 1
  
})

