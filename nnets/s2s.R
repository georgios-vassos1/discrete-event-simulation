library(torch)

# Data input

vic_elec_get_year <- function(year, month = NULL) {
  tsibbledata::vic_elec |>
    dplyr::filter(lubridate::year(Date) == year, lubridate::month(Date) == if (is.null(month)) lubridate::month(Date) else month) |>
    tsibble::as_tibble() |>
    dplyr::select(Demand)
}

elec_dataset <- torch::dataset(
  name = "elec_dataset",

  initialize = function(x, n_timesteps, sample_frac = 1) {

    self$n_timesteps <- n_timesteps
    self$x <- torch::torch_tensor((x - train_mean) / train_sd)

    n <- length(self$x) - self$n_timesteps - 1

    self$starts <- sort(sample.int(
      n = n,
      size = n * sample_frac
    ))

  },

  .getitem = function(i) {

    start <- self$starts[i]
    end <- start + self$n_timesteps - 1
    lag <- 1

    list(
      x = self$x[start:end],
      y = self$x[(start+lag):(end+lag)]$squeeze(2)
    )

  },

  .length = function() {
    length(self$starts) 
  }
)

# Encoder

# Decoder

# seq2seq

# Case study

n_timesteps <- 7 * 24 * 2
n_forecast <- n_timesteps


elec_train <- vic_elec_get_year(2012) |> as.matrix()
elec_valid <- vic_elec_get_year(2013) |> as.matrix()
elec_test  <- vic_elec_get_year(2014, 1) |> as.matrix()

train_mean <- mean(elec_train)
train_sd   <- sd(elec_train)


batch_size <- 32

train_ds <- elec_dataset(elec_train, n_timesteps, sample_frac = 0.5)
train_dl <- train_ds |> dataloader(batch_size = batch_size, shuffle = TRUE)

valid_ds <- elec_dataset(elec_valid, n_timesteps, sample_frac = 0.5)
valid_dl <- valid_ds |> dataloader(batch_size = batch_size)

test_ds <- elec_dataset(elec_test, n_timesteps)
test_dl <- test_ds |> dataloader(batch_size = 1)
