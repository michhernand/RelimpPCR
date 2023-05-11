plot_results <- function(raw_ranked, r2_values_out, validation_split) {
    p1_data <- cbind(
        r2_values_out[["original_r2_train"]],
        r2_values_out[["relimp_r2_train"]],
        r2_values_out[["pca_r2_train"]],
        r2_values_out[["pca_relimp_r2_train"]],
        1:length(r2_values_out[["pca_relimp_r2_train"]])
    )

    p1_data <- as.data.frame(p1_data)
    if (raw_ranked$ok == TRUE) {
      colnames(p1_data) <- c(
        "Original_R2",
        "Relimp_R2",
        "PCA_R2",
        "PCA_Relimp_R2",
        "Num_Predictors")
    } else {
      colnames(p1_data) <- c(
        "Original_R2",
        "PCA_R2",
        "PCA_Relimp_R2",
        "Num_Predictors")
    }
    p1_data <- reshape2::melt(
        data = p1_data,
        id = "Num_Predictors")

    p1 <- ggplot2::ggplot(
        data = p1_data,
        ggplot2::aes(
            x = Num_Predictors,
            y = value,
            group = variable,
            color = variable
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::ggtitle(
            "Improvement of Fit W/ # of Predictors (Train)") +
        ggplot2::labs(
            x = "Number of Predictors",
            y = "Determination Coefficient")

    Num_Predictors <- NULL
    value <- NULL
    variable <- NULL

    if (validation_split != 1) {
      p2_data = cbind(
        r2_values_out[["original_r2_test"]],
        r2_values_out[["relimp_r2_test"]],
        r2_values_out[["pca_r2_test"]],
        r2_values_out[["pca_relimp_r2_test"]],
        1:length(r2_values_out[["pca_relimp_r2_test"]]))

      p2_data = as.data.frame(p2_data)

      if (raw_ranked$ok == TRUE) {
        colnames(p2_data) <- c(
            "Original_R2",
            "Relimp_R2",
            "PCA_R2",
            "PCA_Relimp_R2",
            "Num_Predictors")
      } else {
        colnames(p2_data) <- c(
            "Original_R2",
            "PCA_R2",
            "PCA_Relimp_R2",
            "Num_Predictors")
      }
      p2_data <- reshape2::melt(data = p2_data, id = "Num_Predictors")

      p2 <- ggplot2::ggplot(
        data = p2_data,
        ggplot2::aes(
            x = Num_Predictors,
            y = value,
            group = variable,
            color = variable)) +
        ggplot2::geom_line() +
        ggplot2::ggtitle(
            "Improvement of Fit W/ # of Predictors (Test)") +
        ggplot2::labs(
            x = "Number of Predictors",
            y = "Determination Coefficient")

      Rmisc::multiplot(p1, p2, cols = 2)
    }
}