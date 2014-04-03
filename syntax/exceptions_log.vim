" Vim syntax file
" Language: exceptions_log (XgsOS exceptions log file)
" Maintainer: Jonas Collberg
" First Revision: 2014-03-25
" Latest Revision: 2014-03-25

if exists("b:current_syntax")
  finish
endif

syn region logRow start=/^/ end=/$/ contains=CONTAINED
syn match logDate ;\v(\d{2}/){2}\d{4}; contained nextgroup=logTime
syn match logTime /\v\d{2}(:\d{2}){2}/ contained nextgroup=logType
syn match logType /\v] (Trace|Debug|Info(rmation)?|Warning|Error)/hs=s+2 contained nextgroup=logClass
syn match logClass /\v - [^:]+/hs=s+3 contained nextgroup=logMsg
syn match logMsg /\v: .*$/hs=s+2 contained nextgroup=logDetails

hi link logRow Comment
hi link logDate Operator
hi link logTime Structure
hi link logType Identifier
hi link logClass Number
hi link logMsg String

