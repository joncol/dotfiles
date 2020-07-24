" Vim syntax file
" Language: xgsos_log (XgsOS log file)
" Maintainer: Jonas Collberg
" First Revision: 2014-03-19
" Latest Revision: 2014-03-19

if exists("b:current_syntax")
  finish
endif

syn region logRow start=/^/ end=/$/ contains=CONTAINED
syn match logTime /\v\d{2}(:\d{2}){2}/ contained nextgroup=logType
syn match logType /\v \| (Trace|Debug|Info(rmation)?|Warning|Error)/hs=s+2 contained nextgroup=logClass
syn match logClass /\v \| [^|]+/hs=s+3 contained nextgroup=logMsg
syn match logMsg /\v\| [^|]+/hs=s+2 contained nextgroup=logDetails
syn match logDetails /\v\| .*$/hs=s+2 contained

" syn match logException /\v^Exception/ nextgroup=logExceptionSeparator1
" syn match logExceptionSeparator1 /: / nextgroup=logExceptionType
" syn match logExceptionType /\v[^:]*/ nextgroup=logExceptionSeparator2
" syn match logExceptionSeparator2 /: / nextgroup=logExceptionText
" syn match logExceptionText /.*$/
"
" syn match logStackTrace /\v^((   (at |--- End of inner))|StackTrace:).*$/

hi link logRow Comment
hi link logTime Structure
hi link logType Identifier
hi link logClass Number
hi link logMsg String
hi link logDetails PreProc
" hi link logException Operator
" hi link logExceptionSeparator1 Comment
" hi link logExceptionType Type
" hi link logExceptionSeparator2 Comment
" hi link logExceptionText PreProc
" hi link logStackTrace Comment

