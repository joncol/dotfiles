" Vim syntax file
" Language: Log (Orzone log file)
" Maintainer: Jonas Collberg
" First Revision: 2015-03-25
" Latest Revision: 2015-03-25

if exists("b:current_syntax")
  finish
endif

syn match logMessageCont /\v^ \s+.*$/
syn match logEntryTime /\v\d{1,2}:\d{2}:\d{2}:\d{3} / nextgroup=logLevelTrace,logLevelDebug,logLevelInfo,logLevelWarning,logLevelError
syn match logLevelTrace /\v\[trace\]/ contained nextgroup=logProcessId
syn match logLevelDebug /\v\[debug\]/ contained nextgroup=logProcessId
syn match logLevelInfo /\v\[info \]/ contained nextgroup=logProcessId
syn match logLevelWarning /\v\[warn \]/ contained nextgroup=logProcessId
syn match logLevelError /\v\[error\]/ contained nextgroup=logProcessId
syn match logProcessId /\v\s+\d+/ contained nextgroup=logThreadId
syn match logThreadId /\v\s+\d+/ contained nextgroup=logLocation
syn match logLocation /\v\s+[^:]*/ contained nextgroup=logLocationSeparator
syn match logLocationSeparator /\v\s*:\s*/ contained nextgroup=logMessage
syn match logMessage /\v.*$/ contained

hi logEntryTime guifg=#606060
hi logLevelInfo guifg=#32cd32
hi logLevelDebug guifg=#b0c4de
hi logLevelWarning guifg=#ff8c00
hi logLevelError guifg=White guibg=Red
hi logProcessId guifg=#606060
hi logThreadId guifg=#606060
hi logLocation guifg=#80b0f0
hi logLocationSeparator guifg=#808080
hi logMessage guifg=#ff69b4
hi logMessageCont guifg=#ff69b4
