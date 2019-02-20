build_feedback_message <- function(details) {
  if (is.character(details)) {
    return(capitalize(trim(details)))
  }
  
  total_msg <- ""
  for (det in details) {
    if (!is.null(det$message)) {
      msg <- det$message
    } else {
      class(det) <- det$type
      msg <- build_message(det)
    }
    
    # only do something if msg is actually a message
    if (!is.null(msg)) {
      if (isTRUE(det$append)) {
        total_msg <- paste(total_msg, msg)
      } else {
        total_msg <- msg
      }
    }
  }
  return(capitalize(trim(total_msg)))
}

build_message <- function(det) {
  UseMethod("build_message", det)
}

build_message.default <- function(det) {
  return(NULL)
}

build_message.object <- function(det) {
  switch(det$case,
         defined = sprintf("您是否正确的定义了变量 `%s`？", det$name),
         correct = sprintf("变量`%s`的值不正确。", det$name),
         equal = build_diff(sol = det$solution, stud = det$student,
                            eq_condition = det$eq_condition,
                            id = "it"),
         NULL)
}

# build_message.object <- function(det) {
#   switch(det$case,
#          defined = sprintf("您是否正确的定义了变量`%s`？", det$name),
#          correct = sprintf("变量`%s`的值不正确。", det$name),
#          equal = build_diff(sol = det$solution, stud = det$student,
#                             eq_condition = det$eq_condition,
#                             id = "it"),
#          NULL)
# }

build_message.column <- function(det) {
  switch(det$case,
         defined = sprintf("是否包含列`%s`？", det$name),
         correct = sprintf("列`%s`似乎不正确。", det$name),
         equal = NULL,
         NULL)
}

build_message.element <- function(det) {
  switch(det$case,
         defined = sprintf("是否包含元素`%s`？", det$name),
         correct = sprintf("列`%s`似乎不正确。", det$name),
         equal = NULL,
         NULL)
}


build_message.function <- function(det) {
  switch(det$case,
         called = sprintf("您是否调用了`%s()`%s?", det$name, get_times(det$index)),
         correct = sprintf("检查函数`%s()`的调用.", det$name),
         result_runs = "再次运行会引发错误。",
         result_correct = "再次运行不会给出正确的结果。",
         result_equal = build_diff(sol = det$solution, stud = det$student,
                                   eq_condition = det$eq_condition,
                                   id = "the result"),
         NULL)
}

# build_message.function <- function(det) {
#   switch(det$case,
#          called = sprintf("您有访问`%s()`%s吗？", det$name, get_times(det$index)),
#          correct = sprintf("检查一下是否有访问`%s()`.", det$name),
#          result_runs = "再次运行会引发错误。",
#          result_correct = "再次运行不会给出正确的结果。",
#          result_equal = build_diff(sol = det$solution, stud = det$student,
#                                    eq_condition = det$eq_condition,
#                                    id = "the result"),
#          NULL)
# }


build_message.operator <- function(det) {
  switch(det$case,
         called = sprintf("你是否使用`%s`操作%s？", det$name, get_times(det$index)),
         correct = sprintf("你有没有正确使用`%s`运算符？", det$name),
         result_runs = "再次运行该操作引发了错误。",
         result_correct = "再次运行该操作不会给出正确的结果。",
         result_equal = build_diff(sol = det$solution, stud = det$student,
                                   eq_condition = det$eq_condition,
                                   id = "the result"),
         NULL)
}

build_message.argument <- function(det) {
  msg <- NULL
  if (det$case == "specified") {
    if (det$name == "...") {
      msg <- sprintf("你有没有指定任何与`...`匹配的参数？?", det$name)
    } else {
      msg <- sprintf("你有没有指定参数`%s`？", det$name)
    }
  }
  if (det$case == "correct") {
    if (det$name == "...") {
      msg <- "你是否正确指定了与`...`匹配的参数？"
    } else {
      msg <- sprintf("你是否正确指定了参数`%s`？", det$name)
    }
  }
  if (det$case == "equal") {
    if (!det$is_dots) {
      msg <- build_diff(sol = det$solution, stud = det$student,
                        eq_condition = det$eq_condition,
                        id = "it")  
    }
  }
  return(msg)
}

build_message.if <- function(det) {
  build_message_control(det, "if")
}

build_message.for <- function(det) {
  build_message_control(det, "for")
}

build_message.while <- function(det) {
  build_message_control(det, "while")
}

build_message_control <- function(det, type) {
  switch(det$case,
         defined = sprintf("你确定编码%s %s 语句%s？", get_num(det$index), type, ifelse(det$index > 1, "s", "")),
         correct = sprintf("检查%s %s 语句。", get_ord(det$index), type),
         NULL)
}

build_message.condition <- function(det) {
  "Check the condition."
}

build_message.body <- function(det) {
  "Check the body."
}

build_message.ifexpression <- function(det) {
  "Check the if part."
}

build_message.elseexpression <- function(det) {
  switch(det$case,
         defined = "其他部分缺失了。",
         correct = "检查其他部分。",
         NULL)
}

build_message.typed <- function(det) {
  if (det$type == "typed") {
    if (det$fixed) {
      msg <- sprintf("你是否有键入%s%s?", collapse_args(det$regex, conn = " or "), get_times(det$times))
    } else {
      msg <- sprintf("系统想要找到模式%s%s，但没有。", collapse_args(det$regex, conn = " or "), get_times(det$times))  
    }
  }
  return(msg)
}

build_message.fundef <- function(det) {
  switch(det$case,
         defined = sprintf("您是否定义了函数 `%s()`?", det$name),
         correcttype = sprintf("您是否确定`%s`是一个函数?", det$name),
         correct = sprintf("您是否正确地定义了函数 `%s()`?", det$name),
         arguments = "您是否正确指定了参数的个数?",
         coded = sprintf("系统不能在你的代码中找到函数 `%s()` 的定义。", det$name),
         NULL)
}

# build_message.fundef <- function(det) {
#   switch(det$case,
#          defined = sprintf("您定义了函数`%s()`吗？", det$name),
#          correcttype = sprintf("您确定`%s`是一个函数吗？", det$name),
#          correct = sprintf("您是否正确定义了函数`%s()`？", det$name),
#          arguments = "您是否正确指定了参数的个数？",
#          coded = sprintf("系统不能在你的代码中找到函数 `%s()` 的定义。", det$name),
#          NULL)
# }

build_message.expr <- function(det) {
  switch(det$case, 
         result_runs = sprintf("运行`%s`会出错。", det$expr_str),
         result_correct = sprintf("运行`%s`没有给出正确的结果。", det$expr_str),
         result_equal = build_diff(sol = det$solution, stud = det$student,
                                   eq_condition = det$eq_condition,
                                   id = "the result"),
         output_runs = sprintf("运行`%s`会出错。", det$expr_str),
         output_correct = sprintf("运行`%s`没有给出正确的结果。", det$expr_str),
         output_equal = sprintf("应该是%s，不是%s",
                                ifelse(length(det$solution) == 0, "no output", sprintf("`%s`", det$solution)),
                                ifelse(length(det$student) == 0, "no output", sprintf("`%s`", det$student))),
         error_fails = sprintf("运行`%s`没有生成错误，但应该要出错的。", det$expr_str),
         error_correct = sprintf("运行`%s`没有生成正确的错误。", det$expr_str),
         error_equal = sprintf("预期错误是`%s`，但得到错误`%s`",
                               det$solution, det$student),
         NULL)
}

build_message.file <- function(det) {
  msg <- NULL
  if (det$case == "available") {
    if (det$folder == ".") {
      msg <- sprintf("文件`%s`似乎不在您的工作目录中。", det$file)
    } else {
      msg <- sprintf("文件`%s`似乎不在您工作目录的文件夹`%s`中。", det$file, det$folder)
    }
  }
  return(msg)
}

build_message.output <- function(det) {
  switch(det$case, 
         regex = "代码生成的输出不是我们正在寻找的模式。",
         expr = sprintf("您的代码是否与`%s'产生相同的输出？", det$expr),
         NULL)
}

# Markdown Messaging ----------------------------------------------------------

build_message.markdown_header <- function(det) {
  switch(det$case,
         defined = sprintf("你的代码中包含%s级别%i标题%s吗？", get_num(det$index), det$level, if (det$index > 1) "s" else ""),
         correct = sprintf("检查%s级别%i标题。", get_ord(det$index), det$level)
  )
}

build_message.markdown_title <- function(det) {
  switch(det$case,
         defined = sprintf("系统找不到标题。"),
         correct = sprintf("检查标题。")
  )
}

build_message.markdown_chunk <- function(det) {
  switch(det$case,
         defined = sprintf("你有%s代码块%s吗？", get_num(det$index), if (det$index > 1) "s" else ""),
         correct = sprintf("查看%s代码块。", get_ord(det$index))
  )
}

build_message.markdown_chunk_option <- function(det) {
  switch(det$case,
         defined = sprintf("你有没有指定块选项`%s`？", det$name),
         correct = sprintf("块选项`%s`不正确。", det$name),
         equal = build_diff(sol = det$solution, stud = det$student,
                            eq_condition = "equal", id = "it")
  )
}

build_message.markdown_yaml <- function(det) {
  switch(det$case,
         parsing_error = sprintf("解析YAML标头时出错了。你确定你都缩进了吗？"),
         correct = "检查您的YAML标题"
  )
}

build_message.markdown_yaml_option <- function(det) {
  switch(det$case,
         defined = sprintf("您是否指定了YAML标题选项%s？", yaml_option_desc(det$name)),
         correct = sprintf("选项%s不正确。", yaml_option_desc(det$name)),
         equal = build_diff(sol = det$solution, stud = det$student, eq_condition = "equal", id = "it")
  )
}
