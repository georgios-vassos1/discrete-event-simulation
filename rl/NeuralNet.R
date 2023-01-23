library(torch)
library(torchvision)
library(luz)

dir <- "~/solutions/rl/misc/data"

visualize <- function(ds, idx) {
  ds$data[idx,1:28,1:28] |>
    reshape2::melt() |>
    ggplot(aes(x=Var2, y=Var1, fill=value))+
    geom_tile(show.legend = FALSE) + 
    xlab("") + ylab("") +
    scale_fill_gradient(low="white", high="black")
  
}

transform <- function(img, maxval=255.0) {
  img |>
    (\(x) x/maxval)() |>
    transform_to_tensor()
}

train_ds <- mnist_dataset(
  dir,
  download  = FALSE,
  transform = transform
)

test_ds <- mnist_dataset(
  dir,
  train     = FALSE,
  transform = transform
)

# visualize(train_ds, 2L)

batch_size <- 128 # Number of images to be seen during one training iteration

train_dl <- dataloader(train_ds, batch_size = batch_size, shuffle = TRUE)
test_dl  <- dataloader(test_ds,  batch_size = batch_size)

net <- nn_module(
  "Net",

  initialize = function() {
    self$fc1 <- nn_linear(28L * 28L, 128L)
    self$fc2 <- nn_linear(128L, 10L)

    self$dropout <- nn_dropout2d(0.5)
  },

  forward = function(x) {
    x %>%                                  # N * 1 * 28 * 28
      torch_flatten(start_dim = 2) %>%     # N * 784
      self$fc1() %>%                       # N * 128
      nnf_relu() %>% 
      self$dropout() %>% 
      self$fc2()                           # N * 10
  }
)

fitted <- net %>%
  setup(
    loss      = nn_cross_entropy_loss(),
    optimizer = optim_adam,
    metrics   = list(
      luz_metric_accuracy()
    )
  ) %>%
  fit(train_dl, epochs = 10, valid_data = test_dl)
# luz_save(obj = fitted, path = "~/solutions/rl/misc/data/mnist_fc_1.pt")


## Example use of torch with R

library(torch)
library(luz)
library(torchvision)
library(torchdatasets)

train_indices <-     1:10000
val_indices   <- 10001:15000
test_indices  <- 15001:20000

add_channel_dim <- function(img) img$unsqueeze(1)
crop_axes       <- function(img) torchvision::transform_crop(img, top = 0, left = 21, height = 131, width = 130)
binarize        <- function(tensor) torch_round(torch_abs(tensor)) # Classification

root <- file.path("~/Desktop/correlation")

train_ds <- guess_the_correlation_dataset(
  # where to unpack
  root = root,
  # additional preprocessing 
  # transform = function(img) crop_axes(img) %>% torch_flatten(), # For linear layer input
  transform = function(img) crop_axes(img) %>% add_channel_dim(),
  # binarize target data for classification
  # target_transform = binarize,
  # don't take all data, but just the indices we pass in
  indexes = train_indices,
  download = FALSE
)

valid_ds <- guess_the_correlation_dataset(
  root = root,
  transform = function(img) crop_axes(img) %>% add_channel_dim(),
  indexes = val_indices,
  download = FALSE
)

test_ds <- guess_the_correlation_dataset(
  root = root,
  transform = function(img) crop_axes(img) %>% add_channel_dim(),
  indexes = test_indices,
  download = FALSE
)

length(train_ds)
length(valid_ds)
length(test_ds)

# DataLoader object
train_dl <- dataloader(train_ds, batch_size = 64, shuffle = TRUE)
length(train_dl)

valid_dl <- dataloader(valid_ds, batch_size = 64)
length(valid_dl)

test_dl <- dataloader(test_ds, batch_size = 64)
length(test_dl)

#############################################
# Load one batch
batch <- dataloader_make_iter(train_dl) %>% dataloader_next()

# Vizualise
par(mfrow = c(8,8), mar = rep(0, 4))

images <- as.array(batch$x$squeeze(2))

images %>%
  purrr::array_tree(1) %>%
  purrr::map(as.raster) %>%
  purrr::iwalk(~{plot(.x)})

# Verify correlation vizually
batch$y %>% as.numeric() %>% round(digits = 2)
#############################################

net <- nn_module(

  # ...
  "corr-cnn",

  # Instantiate five submodules: two convolutional layers and two linear ones
  initialize = function() {

    self$conv1 <- nn_conv2d(in_channels =  1, out_channels =  32, kernel_size = 3)
    self$conv2 <- nn_conv2d(in_channels = 32, out_channels =  64, kernel_size = 3)
    self$conv3 <- nn_conv2d(in_channels = 64, out_channels = 128, kernel_size = 3)

    self$fc1 <- nn_linear(in_features = 14 * 14 * 128, out_features = 128)
    self$fc2 <- nn_linear(in_features = 128, out_features = 1)

  },

  # ...
  forward = function(x) {

    x %>% 
      self$conv1() %>%
      nnf_relu() %>%
      nnf_avg_pool2d(2) %>%

      self$conv2() %>%
      nnf_relu() %>%
      nnf_avg_pool2d(2) %>%

      self$conv3() %>%
      nnf_relu() %>%
      nnf_avg_pool2d(2) %>%

      torch_flatten(start_dim = 2) %>%
      self$fc1() %>%
      nnf_relu() %>%

      self$fc2()
  }
)

model <- net()
# model(batch$x)

## Train the network
# Run the model on the input, to obtain its current predictions:
# output <- model(b$x)
# Calculate the loss, a measure of divergence between model estimate and ground truth:
# loss <- nnf_mse_loss(output, b$y$unsqueeze(2))
# Have that loss propagate back through the network, causing gradients to be computed for all parameters:
# loss$backward()
# Ask the optimizer to update the parameters accordingly:
# optimizer$step()

fitted <- luz_load(path = "~/Desktop/correlation/model.pt")
# fitted <- net %>%
#   setup(
#     loss = function(y_hat, y_true) nnf_mse_loss(y_hat, y_true$unsqueeze(2)),
#     # Loss function for binary output (classification)
#     # loss = function(y_hat, y_true) nnf_binary_cross_entropy_with_logits(y_hat, y_true$unsqueeze(2)),
#     optimizer = optim_adam
#   ) %>%
#   fit(train_dl, epochs = 10, valid_data = test_dl)
# luz_save(obj = fitted, path = "~/Desktop/correlation/model.pt")

## Evaluate performance
preds   <- predict(fitted, test_dl)
preds   <- preds$to(device = "cpu")$squeeze() %>% as.numeric()
test_dl <- dataloader(test_ds, batch_size = 5000)
targets <- (test_dl %>% dataloader_make_iter() %>% dataloader_next())$y %>% as.numeric()

df <- data.frame(preds = preds, targets = targets)

library(ggplot2)

ggplot(df, aes(x = targets, y = preds)) +
  geom_point(size = 0.1) +
  theme_classic() +
  xlab("true correlations") +
  ylab("model predictions")
