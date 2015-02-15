" Vim syntax file
" Language: parsel
" Maintainer: Robert Ying
" Latest Revision: 15 Feb 2015

if exists("b:current_syntax")
	finish
endif

" disable spellcheck
set nospell

" Functions
syntax keyword pslTodo contained TODO FIXME XXX NOTE
syntax match pslComment "\v#.*$" contains=pslTodo

syntax keyword pslKeyword map merge chop ft applyFilterF applyFilterT
syntax keyword pslKeyword with in let
syntax keyword pslKeyword if then else

syntax keyword pslConstant sysin sysout

syntax region pslString start='"' end='"'

syntax match pslNumber "\d\+"
syntax match pslNumber "[-+]\d\+"
syntax match pslNumber "\d\+\.\d*"
syntax match pslNumber "[-+]\d\+\d*"
syntax match pslNumber "\d\+[MKmu]\?s\W"
syntax match pslNumber "\d\+[MKmu]\?Hz\W"

syntax keyword pslType interval signal fsignal
syntax keyword pslType hertz seconds
syntax keyword pslType float

syntax match pslFunction /\w\+\s*(/me=e-1,he=e-1

syntax match pslOperator "\v-\>"
syntax match pslOperator "\v\<"
syntax match pslOperator "\v\>"
syntax match pslOperator "\v\+"
syntax match pslOperator "\v-"
syntax match pslOperator "\v\\"
syntax match pslOperator "\v/"
syntax match pslOperator "\v\*"
syntax match pslOperator "\v\="

highlight def link pslComment Comment
highlight def link pslTodo Todo
highlight def link pslOperator Operator
highlight def link pslFunction Function
highlight def link pslKeyword Keyword
highlight def link pslNumber Number
highlight def link pslString String
highlight def link pslConstant Constant
highlight def link pslType Type

let b:current_syntax = "parsel"
