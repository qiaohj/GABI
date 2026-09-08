comment_clip <- function() {
  # 1. 检查 Linux 下剪贴板读取工具
  if (Sys.which("xclip") == "") {
    stop("系统缺少 xclip，请在 Ubuntu 终端运行: sudo apt install -y xclip")
  }
  
  # 从剪贴板读取内容
  code_text <- tryCatch(
    readLines(pipe("xclip -selection clipboard -o"), warn = FALSE),
    error = function(e) character(0)
  )
  code_str <- paste(code_text, collapse = "\n")
  
  if (nchar(trimws(code_str)) == 0) {
    message("⚠️ 剪贴板为空！请先在编辑器中选中代码并按 Ctrl + C。")
    return(invisible(NULL))
  }
  
  api_key <- Sys.getenv("DEEPSEEK_API_KEY")
  if (api_key == "") {
    stop("未找到 DEEPSEEK_API_KEY，请检查 .Renviron。")
  }
  
  message("正在请求 DeepSeek 生成注释...")
  
  sys_prompt <- paste(
    "你是一位资深 R 语言专家。",
    "为用户提供的 R 代码添加准确、精炼的中文行内注释。",
    "严格只输出带注释的纯 R 代码，严禁包含任何 Markdown 语法标记（如 ```r 或 ```），绝对不要输出任何前后解释或客套话。"
  )
  
  # 2. 构造显式请求，避免管道符导致的 URL 丢失
  api_url <- "https://api.deepseek.com/chat/completions"
  
  req <- httr2::request(api_url)
  req <- httr2::req_headers(
    req,
    "Authorization" = paste("Bearer", trimws(api_key)),
    "Content-Type"  = "application/json"
  )
  req <- httr2::req_body_json(
    req,
    list(
      model = "deepseek-chat",
      messages = list(
        list(role = "system", content = sys_prompt),
        list(role = "user", content = code_str)
      ),
      temperature = 0.2
    )
  )
  req <- httr2::req_timeout(req, 60)
  
  # 3. 发送请求
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    message("HTTP 请求失败: ", e$message)
    return(NULL)
  })
  
  if (is.null(resp)) return(invisible(NULL))
  
  res_json <- httr2::resp_body_json(resp)
  print(res_json)
  result_code <- res_json$choices[[1]]$message$content
  
  # 清洗可能带有的 markdown 标记
  result_code <- gsub("^```r?\\s*|```\\s*$", "", result_code)
  
  # 4. 写入剪贴板
  con <- pipe("xclip -selection clipboard", "w")
  writeLines(result_code, con)
  close(con)
  
  message("✅ 注释完成！直接在编辑器按 Ctrl + V 粘贴覆盖。")
}