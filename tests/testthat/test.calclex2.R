#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Lineno and position tracking with error tokens")

Lexer <- R6Class("Lexer",
    public = list(
        tokens = c('NAME','NUMBER', 
            'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 
            'LPAREN','RPAREN'),
        t_PLUS   = '\\+',
        t_MINUS  = '-',
        t_TIMES  = '\\*',
        t_DIVIDE = '/',
        t_EQUALS = '=',
        t_LPAREN = '\\(',
        t_RPAREN = '\\)',
        t_NAME = '[a-zA-Z_][a-zA-Z0-9_]*',
        t_NUMBER = function(re='\\d+', t) {
          cat('t_NUMBER\n')
          t$value <- strtoi(t$value)
          return(t)
        },
        t_ignore = " \t",
        t_newline = function(re='\\n+', t) {
          t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
          return(NULL)
        },
        t_error = function(t) {
          cat(sprintf("Illegal character '%s'\n", t$value[[1]]))
          t$lexer$skip(1)
          return(t)
        }
    )
)

test_that("calclex: error", {
  lexer <- rly::lex(Lexer)
data <- "a = 3 +
(4*5) +
(a b c) +
+ 6 + 7"
  
  lexer$input(data)
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
  cat(lexer$token()$value)
  cat('\n')
})