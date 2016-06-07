" Vim syntax file
" Language:    ReactiveIS
" Maintainer:  Jaime Arias <jaime.arias@labri.fr>
" Last Change: Sept 22 2014
" Version:     1


if exists("b:current_syntax")
  finish
endif

" No case sensitive
syn case ignore


" Comments
syn keyword irsTodo     contained TODO FIXME XXX NOTE
syn region  irsComment    start="/\*\*" end="\*\*/" contains=ntccTodo
syn match  irsCommentLine "//.*$" contains=ntccTodo


syn keyword irsProcessKeyword Texture Structure Scenario
syn keyword irsConditionKeyword Wait Event
syn keyword irsEventKeyword Start End
syn keyword irsInfKeyword Inf



syn region irsString start='"' end='"'
syn region irsParameter start='_' end='_'
syn match irsName "[a-z][0-9a-z]*"
syn match irsNumber '\d\+'
syn match irsOps '&'



hi def link irsComment Comment
hi def link irsCommentLine Comment

hi def link irsProcessKeyword Identifier
hi def link irsNumber Constant
hi def link irsInfKeyword Constant


hi def link irsName Special
hi def link irsConditionKeyword  Todo
hi def link irsEventKeyword  Operator
hi def link irsOps  Todo
hi def link irsParameter Type

hi def link irsString Statement


let b:current_syntax = "ris"

