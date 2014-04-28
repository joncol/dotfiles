" Vim syntax file
" Language: Log (Combination log file)
" Maintainer: Jonas Collberg
" First Revision: 2011-10-01
" Latest Revision: 2014-03-07

if exists("b:current_syntax")
  finish
endif

" EGM log
syn match logEntryId /\v^\d+/ nextgroup=logEntryTime
syn region logEntryTime matchgroup=Comment start=/\v \| / end=/\v \|/ contained nextgroup=logBalance
syn match logBalance /\v ((\$|THB)\d+(\,\d+)?(\.\d+)?)?/ contained nextgroup=logEntryStatus
syn match logEntryStatus /\v \|\s+\d+/ contained nextgroup=logEntryType
syn match logEntryType /\v \|\s+\d+/ contained nextgroup=logMsg
syn region logMsg matchgroup=Comment start=/\v \|/ end=/\v\|/ contained nextgroup=logDetails
syn region logDetails matchgroup=Comment start=/ / end=/\v\|/ contained nextgroup=logEntryCrc
syn match logEntryCrc /\v\s+\d+$/ contained

hi link logEntryId Comment
hi link logBalance Operator
hi link logEntryTime Structure
hi link logEntryStatus Comment
hi link logEntryType Comment
hi link logMsg Identifier
hi link logDetails String
hi link logEntryCrc Comment

