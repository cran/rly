#! /usr/bin/env Rscript

library(testthat)
library(rly)

context("Panic mode recovery test")

Lexer <- R6Class("Lexer",
  public = list(
    tokens = c('NAME','NUMBER', 'PLUS','MINUS','TIMES','DIVIDE','EQUALS', 'LPAREN','RPAREN'),
    t_PLUS = '\\+',
    t_MINUS = '-',
    t_TIMES = '\\*',
    t_DIVIDE = '/',
    t_EQUALS = '=',
    t_LPAREN = '\\(',
    t_RPAREN = '\\)',
    t_NAME = '[a-zA-Z_][a-zA-Z0-9_]*',
    t_NUMBER = function(re='\\d+', t) {
      t$value <- strtoi(t$value)
      return(t)
    },
    t_ignore = " \t",
    t_newline = function(re='\\n+', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    t_error = function(t) {
      cat(sprintf("Illegal character '%s'\n", t$value[1]))
      t$lexer$skip(1)
      return(t)
    }
  )
)

Parser <- R6Class("Parser",
  public = list(
    tokens = Lexer$public_fields$tokens,
    # Parsing rules
    precedence = list(c('left','PLUS','MINUS'),
                      c('left','TIMES','DIVIDE','MINUS'),
                      c('right','UMINUS')),
    # dictionary of names
    names = new.env(hash=TRUE),
    p_statement_assign = function(doc='statement : LPAREN NAME EQUALS expression RPAREN', p) {
      cat(sprintf("%s=%s", p[2], p[4]))
    },
    p_statement_expr = function(doc='statement : LPAREN expression RPAREN', t) {
      cat(t[1])
    },
    p_expression_binop = function(doc='expression : expression PLUS expression
                                     | expression MINUS expression
                                     | expression TIMES expression
                                     | expression DIVIDE expression', t) {
      if(t[2] == '+')      t[0] <- t[1] + t[3]
      else if(t[2] == '-') t[0] <- t[1] - t[3]
      else if(t[2] == '*') t[0] <- t[1] * t[3]
      else if(t[2] == '/') t[0] <- t[1] / t[3]
    },
    p_expression_uminus = function(doc='expression : MINUS expression %prec UMINUS', t) {
      t[0] <- -t[2]
    },
    p_expression_group = function(doc='expression : LPAREN expression RPAREN', t) {
      t[0] <- t[2]
    },
    p_expression_number = function(doc='expression : NUMBER', t) {
      t[0] <- t[1]
    },
    p_expression_name = function(doc='expression : NAME', t) {
      t[0] <- names[t[1]]
    },
    p_error = function(p) {
      if(!is.null(p)) cat(sprintf("Line %d: Syntax error at '%s'", p$lineno, p$value))
      
      # Scan ahead looking for a name token
      while(TRUE) {
        tok <- parser$token()
        if(is.null(tok) || tok$type == 'RPAREN') break
      }
      if(!is.null(tok)) parser$restart()
      return(NULL)
    }
  )
)

test_that("error", {
  lexer <- rly::lex(Lexer)
  parser <- rly::yacc(Parser)
  data <- "(a = 3 + 4)
(b = 4 + * 5 - 6 + *)
(c = 10 + 11)"
#  parser$parse(data, lexer, debug=TRUE)
})
