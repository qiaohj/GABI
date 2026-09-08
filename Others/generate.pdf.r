# ==============================================================================
# 批量合并指定文件夹中的 .R 文件为单个 PDF
# ==============================================================================

merge_r_scripts_to_pdf <- function(
    target_dirs,                          # 文件夹路径向量，如 c("./R", "./scripts")
    output_pdf = "merged_r_scripts.pdf",  # 导出的 PDF 文件路径
    recursive = TRUE                      # 是否递归查找子文件夹
) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("请先安装 rmarkdown 包: install.packages('rmarkdown')")
  }
  
  # 1. 扫描所有目录下的 .R 和 .r 文件
  r_files <- unlist(lapply(target_dirs, function(d) {
    if (!dir.exists(d)) {
      warning(paste("目录不存在，已跳过:", d))
      return(character(0))
    }
    list.files(d, pattern = "\\.[Rr]$", full.names = TRUE, recursive = recursive)
  }))
  print(r_files)
  r_files <- unique(normalizePath(r_files, mustWork = FALSE))
  r_files <- r_files[file.exists(r_files)]
  
  if (length(r_files) == 0) {
    stop("未在指定文件夹中找到任何 .R 文件！")
  }
  
  message(sprintf("共找到 %d 个 R 文件，开始处理...", length(r_files)))
  
  # 2. 构建 R Markdown 文档结构
  # 配置 XeLaTeX + fvextra 确保长代码自动换行且支持中文注释
  yaml_header <- c(
    "---",
    "title: 'R Codebase for Documentation'",
    paste0("date: 'Compiled on: ", Sys.Date(), "'"),
    "geometry: margin=1.8cm",
    "output:",
    "  pdf_document:",
    "    latex_engine: xelatex",
    "    highlight: tango",
    "header-includes:",
    "  - \\usepackage{fvextra}",
    "  - \\DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines,commandchars=\\\\\\{\\}}",
    "---",
    ""
  )
  
  body <- list()
  for (i in seq_along(r_files)) {
    file_path <- r_files[i]
    file_name <- basename(file_path)
    code_lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
    
    # 格式化单个文件的内容
    section <- c(
      sprintf("# [%d] %s", i, file_name),
      sprintf("**Full Path:** `%s`", gsub("\\\\", "/", file_path)),
      "",
      "```r",
      code_lines,
      "```",
      "",
      "\\newpage",
      ""
    )
    body[[i]] <- section
  }
  
  full_rmd <- c(yaml_header, unlist(body))
  
  # 3. 写入临时文件并渲染
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(full_rmd, temp_rmd, useBytes = TRUE)
  
  message("正在渲染 PDF 文档，请稍候...")
  out_dir <- dirname(normalizePath(output_pdf, mustWork = FALSE))
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  rmarkdown::render(
    input = temp_rmd,
    output_file = basename(output_pdf),
    output_dir = out_dir,
    quiet = TRUE
  )
  
  unlink(temp_rmd)
  message(sprintf("生成完成！文件已输出至: %s", output_pdf))
}

# ------------------------------------------------------------------------------
# 使用示例：
# ------------------------------------------------------------------------------
target_folders <- c("Y:/GABI/GABI/Analysis", 
                    "Y:/GABI/GABI/Configuration", 
                    "Y:/GABI/GABI/Figures")
merge_r_scripts_to_pdf(target_folders, output_pdf = "../Data/all_r_code.pdf", 
                       recursive=F)


