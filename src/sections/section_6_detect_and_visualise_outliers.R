cleaning.log.outliers <- data.frame()

cols.integer_main <- filter(tool.survey, type == "integer")
if (length(cols.integer_raw.main) == 0) cols.integer_raw.main <- cols.integer_main[cols.integer_main$name %in% colnames(raw.main),] %>% pull(name)
if (length(cols.integer_raw.loop1) == 0 & exists('raw.loop1')) cols.integer_raw.loop1 <- cols.integer_main[cols.integer_main$name %in% colnames(raw.loop1),] %>% pull(name)
if (length(cols.integer_raw.loop2) == 0 & exists('raw.loop2')) cols.integer_raw.loop2 <- cols.integer_main[cols.integer_main$name %in% colnames(raw.loop2),] %>% pull(name)
if (length(cols.integer_raw.loop3) == 0 & exists('raw.loop3')) cols.integer_raw.loop3 <- cols.integer_main[cols.integer_main$name %in% colnames(raw.loop3),] %>% pull(name)

outliers.list <- c()
raw.data_frames.list <- c()
columns.list <- c()

if (length(cols.integer_raw.main) != 0) {
  raw.main.outliers <- utilityR::detect.outliers(
    df = raw.main,
    id = 'uuid',
    colnames = cols.integer_raw.main,
    is.loop = F,
    n.sd = n.sd,
    method = method,
    ignore_0=ignore_0)
  outliers.list <- c(outliers.list, list(raw.main.outliers))
  raw.data_frames.list <- c(raw.data_frames.list, list(raw.main))
  columns.list <- c(columns.list, list(cols.integer_raw.main))
} else raw.main.outliers <- data.frame()

if (length(cols.integer_raw.loop1) != 0) {
  raw.loop1.outliers <- utilityR::detect.outliers(
    df = raw.loop1,
    id = 'loop_index',
    colnames = cols.integer_raw.loop1,
    is.loop = T,
    n.sd = n.sd,
    method = method,
    ignore_0=ignore_0)
  outliers.list <- c(outliers.list, list(raw.loop1.outliers))
  raw.data_frames.list <- c(raw.data_frames.list, list(raw.loop1))
  columns.list <- c(columns.list, list(cols.integer_raw.loop1))
} else raw.loop1.outliers <- data.frame()

if (length(cols.integer_raw.loop2) != 0) {
  raw.loop2.outliers <- utilityR::detect.outliers(
    df = raw.loop2,
    id = 'loop_index',
    colnames = cols.integer_raw.loop2,
    is.loop = T,
    n.sd = n.sd,
    method = method,
    ignore_0=ignore_0)
  outliers.list <- c(outliers.list, list(raw.loop2.outliers))
  raw.data_frames.list <- c(raw.data_frames.list, list(raw.loop2))
  columns.list <- c(columns.list, list(cols.integer_raw.loop2))
} else raw.loop2.outliers <- data.frame()

if (length(cols.integer_raw.loop3) != 0) {
  raw.loop3.outliers <- utilityR::detect.outliers(
    df = raw.loop3,
    id = 'loop_index',
    colnames = cols.integer_raw.loop3,
    is.loop = T,
    n.sd = n.sd,
    method = method,
    ignore_0=ignore_0)
  outliers.list <- c(outliers.list, list(raw.loop3.outliers))
  raw.data_frames.list <- c(raw.data_frames.list, list(raw.loop3))
  columns.list <- c(columns.list, list(cols.integer_raw.loop3))
} else raw.loop3.outliers <- data.frame()

utilityR::generate.boxplot(outliers.list=outliers.list,
                raw.data_frames.list=raw.data_frames.list,
                columns.list=columns.list,
                n.sd=n.sd, boxplot.path = "output/checking/outliers/outlier_analysis_")

cleaning.log.outliers <- rbind(raw.main.outliers,raw.loop1.outliers,raw.loop2.outliers,raw.loop3.outliers)

