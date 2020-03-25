library(Ryacas)
library(iomath)
library(shiny)

choices_x_coef <- c("a", "2*a", "3*a")
choices_x_pow <- 1:3

compare_grid <- expand.grid(
    x = seq(-10, 10, len = 6),
    a = seq(-10, 10, len = 6)
)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    generate_f <- function() {
        x_coef <- sample(choices_x_coef, 1)
        x_pow <- sample(choices_x_pow, 1)
        
        x_part <- paste0(x_coef, "*x^", x_pow)
        
        eq <- ysym(x_part)
        eq
    }
    
    problem_f_eq <- generate_f()
    true_ans <- list(
        x = deriv(problem_f_eq, "x")
    )
    
    output$problem <- renderUI({
        problem <- paste0("Let $$f(x) = ", tex(problem_f_eq), ".$$",
                          "Calculate the derivative with respect to \\(x\\) and enter the result below.")
        
        res <- withMathJax(
                helpText(problem)
            )

        return(res)
    })
    
    feedback <- reactiveValues(x = FALSE)
    
    feedback_var_numeric <- function(input, var, true_ans) {
        reply <- input[[paste0("answer_", var)]]
        reply <- gsub(" ", "", reply, fixed = TRUE)
        
        # Empty string
        if (nchar(reply) == 0L) {
            reply <- "0"
        }
        
        parsed_input <- iomath::prepare_input(reply, 
                                              replace_comma = TRUE, 
                                              insert_products = TRUE)

        if (inherits(parsed_input, "error")) {
            return(list(correct = FALSE, 
                        feedback = "Could not prepare the input (remember that I'm simple-minded!)."))
        }
        
        reply_sym <- tryCatch(Ryacas::ysym(parsed_input), 
                              error = function(e) e)
        
        if (inherits(reply_sym, "error")) {
            return(list(correct = FALSE, 
                        feedback = "Could not understand the input (remember that I'm simple-minded!)."))
        }
        
        is_correct <- tryCatch(iomath::compare_reply_answer(reply = reply, 
                                                            answer = true_ans[[var]], 
                                                            compare_grid = compare_grid), 
                     error = function(e) e)

        if (inherits(is_correct, "error")) {
            return(list(correct = FALSE, 
                        feedback = paste0("Error: ", is_correct$message)))
        }
        
        prefix <- if (is_correct) {
            "CORRECT"
        } else {
            "WRONG"
        }
        
        details <- paste0("Expected $$", tex(true_ans[[var]]), "$$ and got $$", 
                          tex(reply_sym), "$$")
        feedback <- paste0(prefix, ": ", details)
        
        return(list(correct = is_correct, 
                    feedback = feedback))
    }
    
    observeEvent(input$go, {
        feedback$x <- feedback_var_numeric(input = input, 
                                           var = "x", 
                                           true_ans = true_ans)
    }) 
    
    output$feedback <- renderUI({
        req(feedback$x)
        
        col <- if (feedback$x$correct) {
            "green"
        } else {
            "red"
        }
        
        withMathJax(
            h3("Feedback"),
            h4("df / dx"),
            div(
                style = paste0("color: ", col),
                feedback$x$feedback
            )
        )
    })
    
})
