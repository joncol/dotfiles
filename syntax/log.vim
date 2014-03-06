" Vim syntax file
" Language: Log (Combination log file)
" Maintainer: Jonas Collberg
" First Revision: 2011-10-01
" Latest Revision: 2014-03-06

if exists("b:current_syntax")
  finish
endif

syn match logEntryId /\v^[0-9]+/ nextgroup=logEntryTime
syn region logEntryTime matchgroup=Comment start=/\v \| / end=/\v \|/ contained nextgroup=logBalance
syn match logBalance /\v (\$[0-9]+\.[0-9]+)?/ contained nextgroup=logEntryStatus
syn match logEntryStatus /\v \|\s+[0-9]+/ contained nextgroup=logEntryType
syn match logEntryType /\v \|\s+[0-9]+/ contained nextgroup=logMsg
syn region logMsg matchgroup=Comment start=/\v \|/ end=/\v\|/ contained nextgroup=logDetails
syn region logDetails matchgroup=Comment start=/ / end=/\v\|/ contained nextgroup=logEntryCrc
syn match logEntryCrc /\v\s+[0-9]+$/ contained

hi link logEntryId Comment
hi link logBalance Keyword
hi link logEntryTime Structure
hi link logEntryStatus Comment
hi link logEntryType Comment
hi link logMsg Constant
hi link logDetails String
hi link logEntryCrc Comment

