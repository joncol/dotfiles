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
syn match logBalance /\v (\$\d+(\,\d+)?(\.\d+)?)?/ contained nextgroup=logEntryStatus
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

" ApplicationRuntime log
syn region appRuntimeLog_timestamp matchgroup=Comment start=/\v^\[/ end=/\v\]/ contains=appRuntimeLog_date,appRuntimeLog_time nextgroup=appRuntimeLog_type
syn match appRuntimeLog_date :\v(\d{2}/){2}([0-9]{4}) : contained nextgroup=appRunTimeLog_time
syn match appRuntimeLog_time /\v\d{2}(:\d{2}){2} (a|p)\.m\./ contained
syn match appRuntimeLog_type /\v\s*(Trace|Debug|Info|Warning|Error)\s+/ contained nextgroup=appRuntimeLog_separator1
syn match appRuntimeLog_separator1 /- / contained nextgroup=appRuntimeLog_class
syn match appRuntimeLog_class /\v[^:]*/ contained nextgroup=appRuntimeLog_separator2
syn match appRuntimeLog_separator2 /: / contained nextgroup=appRuntimeLog_msg
syn match appRuntimeLog_msg /.*$/ contained

syn match appRuntimeLog_exception /\v^Exception/ nextgroup=appRuntimeLog_exceptionSeparator1
syn match appRuntimeLog_exceptionSeparator1 /: / contained nextgroup=appRuntimeLog_exceptionType
syn match appRuntimeLog_exceptionType /\v[^:]*/ contained nextgroup=appRuntimeLog_exceptionSeparator2
syn match appRuntimeLog_exceptionSeparator2 /: / contained nextgroup=appRuntimeLog_exceptionText
syn match appRuntimeLog_exceptionText /.*$/ contained

syn match appRuntimeLog_stackTrace /\v^((   (at |--- End of inner))|StackTrace:).*$/

hi link appRuntimeLog_date Macro
hi link appRuntimeLog_time Structure
hi link appRuntimeLog_type Identifier
hi link appRuntimeLog_separator1 Comment
hi link appRuntimeLog_class Number
hi link appRuntimeLog_separator2 Comment
hi link appRuntimeLog_msg String
hi link appRuntimeLog_exception Operator
hi link appRuntimeLog_exceptionSeparator1 Comment
hi link appRuntimeLog_exceptionType Type
hi link appRuntimeLog_exceptionSeparator2 Comment
hi link appRuntimeLog_exceptionText PreProc
hi link appRuntimeLog_stackTrace Comment

