library(testthat)
library(potgrowth)

# Créer des données factices pour les tests
factanal_object <- list(loadings = matrix(c(0.5, 0.2, -0.7, 2, -4, 1), nrow = 3, byrow = FALSE,
                                          dimnames = list(c("A", "B", "C"), NULL)))
survey_data <- data.frame(A = c(1, 2, 3), B = c(3, 0, 1), C = c(2, 3, 1))
scale_order <- 1

# Test 1: Vérifier que la fonction retourne des valeurs correctes pour des données d'entrée valides
test_that("compute_scale_scores returns correct values", {
  expected_scores <- c(0.5 * 1 + 0.2 * 3 - 0.7 * 2,
                       0.5 * 2 + 0.2 * 0 - 0.7 * 3,
                       0.5 * 3 + 0.2 * 1 - 0.7 * 1)
  computed_scores <- compute_scale_scores(factanal_object, survey_data, scale_order)
  expect_equal(computed_scores, expected_scores)
})

# Test 2: Vérifier que la fonction s'arrête avec un message d'erreur approprié si des variables manquent
test_that("compute_scale_scores stops with an error if variables are missing", {
  incomplete_data <- survey_data[, -1, drop = FALSE] # Enlever la colonne 'A'
  expect_error(compute_scale_scores(factanal_object, incomplete_data, scale_order),
               "Survey data is missing the following variables required for the scale: A.")
})
