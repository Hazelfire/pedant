" Vim syntax file
" Language: Dimensional
" Maintainer: Sam Nolan
" Latest Revision: 31 Oct 2021

if exists("b:current_syntax")
  finish
endif

" Keywords

syn match celNumber '\d\+.\?\d*' nextgroup=celType skipwhite
syn match celType '\^\?\([a-z]\+-\?\d*\s*\)\+' contained
syn keyword celFunc ln 
syn keyword celUnit unit nextgroup=unitDecl
syn match unitDecl '[a-z ]\+' contained
syn match celOperator '[*+-/]'
syn match celVariable '^[a-zA-Z_]\+'
syn match celComment '//\.\+'

let b:current_syntax = "ped"

hi def link celTodo        Todo
hi def link celComment     Comment
hi def link celBlockCmd    Statement
hi def link celFunc        Function
hi def link celString      Constant
hi def link celDesc        PreProc
hi def link celNumber      Number
hi def link celType        Type
hi def link celVariable    Identifier
hi def link celOperator    Label
hi def link celUnit        Keyword
hi def link unitDecl       Type
