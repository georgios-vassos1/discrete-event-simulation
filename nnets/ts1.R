
tsibbledata::vic_elec |>
  dplyr::glimpse()

tsibbledata::vic_elec |>
  dplyr::filter(lubridate::year(Date) == 2014L) |>
  dplyr::select(-c(Date, Holiday)) |>
  dplyr::mutate(Demand = scale(Demand), Temperature = scale(Temperature)) |>
  tidyr::pivot_longer(-Time, names_to = "variable") |>
  tsibble::update_tsibble(key = variable)

elec_dataset <- torch::dataset(
  name = "elec_dataset",

  initialize = function(x, n_timesteps, sample_frac = 1L) {

    self$n_timesteps <- n_timesteps
    self$x <- torch::torch_tensor((x - train_mean) / train_sd)

    n <- length(self$x) - self$n_timesteps 
    
    self$starts <- sort(sample.int(
      n = n,
      size = n * sample_frac
    ))

  },
  
  .getitem = function(i) {

    start <- self$starts[i]
    end <- start + self$n_timesteps - 1L

    list(
      x = self$x[start:end],
      y = self$x[end + 1L]
    )

  },

  .length = function() {
    length(self$starts) 
  }
)

vic_elec_get_year <- function(year, month = NULL) {
  tsibbledata::vic_elec |>
    dplyr::filter(lubridate::year(Date) == year, lubridate::month(Date) == if (is.null(month)) lubridate::month(Date) else month) |>
    dplyr::as_tibble() |>
    dplyr::select(Demand)
}

elec_train <- vic_elec_get_year(2012L)    |> as.matrix()
elec_valid <- vic_elec_get_year(2013L)    |> as.matrix()
elec_test  <- vic_elec_get_year(2014L, 1L) |> as.matrix() # or 2014, 7, alternatively

train_mean <- mean(elec_train)
train_sd <- sd(elec_train)

n_timesteps <- 7L * 24L * 2L # days * hours * half-hours
train_ds <- elec_dataset(elec_train, n_timesteps, sample_frac = 0.5)
length(train_ds)

batch_size <- 32L
train_dl <- train_ds |> torch::dataloader(batch_size = batch_size, shuffle = TRUE)
length(train_dl)

b <- train_dl |> torch::dataloader_make_iter() |> torch::dataloader_next()
b

valid_ds <- elec_dataset(elec_valid, n_timesteps, sample_frac = 0.5)
valid_dl <- valid_ds |> torch::dataloader(batch_size = batch_size)

test_ds <- elec_dataset(elec_test, n_timesteps)
test_dl <- test_ds |> torch::dataloader(batch_size = 1)

library(torch)

model <- nn_module(

  initialize = function(type, input_size, hidden_size, num_layers = 1L, dropout = 0.0) {

    self$type       <- type
    self$num_layers <- num_layers

    self$rnn <- if (self$type == "gru") {
      nn_gru(
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

    self$output <- nn_linear(hidden_size, 1)

  },

  forward = function(x) {

    # list of [output, hidden]
    # we use the output, which is of size (batch_size, n_timesteps, hidden_size)
    x <- self$rnn(x)[[1]]

    # from the output, we only want the final timestep
    # shape now is (batch_size, hidden_size)
    x <- x[ , dim(x)[2], ]

    # feed this to a single output neuron
    # final shape then is (batch_size, 1)
    x %>% self$output() 
  }

)

device <- torch_device(if (cuda_is_available()) "cuda" else "cpu")

net <- model("gru", 1, 32)
net <- net$to(device = device)

optimizer <- optim_adam(net$parameters, lr = 0.001)

num_epochs <- 30

train_batch <- function(b) {
  
  optimizer$zero_grad()
  output <- net(b$x$to(device = device))
  target <- b$y$to(device = device)
  
  loss <- nnf_mse_loss(output, target)
  loss$backward()
  optimizer$step()
  
  loss$item()
}

valid_batch <- function(b) {
  
  output <- net(b$x$to(device = device))
  target <- b$y$to(device = device)
  
  loss <- nnf_mse_loss(output, target)
  loss$item()
  
}

for (epoch in 1:num_epochs) {

  net$train()
  train_loss <- c()

  coro::loop(for (b in train_dl) {
    loss <-train_batch(b)
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

net$eval()

preds <- rep(NA, n_timesteps)

coro::loop(for (b in test_dl) {
  output <- net(b$x$to(device = device))
  preds <- c(preds, output %>% as.numeric())
})

vic_elec_jan_2014 <-  vic_elec %>%
  filter(year(Date) == 2014, month(Date) == 1) %>%
  select(Demand)

preds_ts <- vic_elec_jan_2014 %>%
  add_column(forecast = preds * train_sd + train_mean) %>%
  pivot_longer(-Time) %>%
  update_tsibble(key = name)

preds_ts %>%
  autoplot() +
  scale_colour_manual(values = c("#08c5d1", "#00353f")) +
  theme_minimal()
