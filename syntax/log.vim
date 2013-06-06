" Vim syntax file
" Language: Log (Combination log file)
" Maintainer: Jonas Collberg
" Latest Revision: 2011-10-01 

if exists("b:current_syntax")
  finish
endif

syn match logEntryId /\v^[0-9]+/
syn match logEntryTime /\v[0-9]{2}\:[0-9]{2}\:[0-9]{2}/
syn match logBalance /\v[$€][0-9]+\.[0-9]+/

hi link logEntryId Structure
hi link logEntryTime Macro
hi link logBalance Special

