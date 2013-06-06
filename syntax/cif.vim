" Vim syntax file
" Language: CIF (Combination format)
" Maintainer: Jonas Collberg
" Latest Revision: 2011-09-30 

if exists("b:current_syntax")
  finish
endif

syn match keyName /\v<[a-zA-Z_]+[0-9a-zA-Z_]*/

syn match cifSection /\v^\[.*\]/ contains=cifSectionName
syn match cifString /\v\"[^"]*\"/
syn match cifSectionName /\v[a-zA-Z_]+[0-9a-zA-Z_-]*/ contained

syn match cifComment /\v;.*/
syn match cifHeader /\v\;\={13}\[ COMBINATION INI FILE\]\={13}/

syn match cifNumber /\v<[0-9]+/

":syn region sectionBlock start="[" end="]" fold transparent contains=sectionName
hi link cifSectionName Structure
hi link cifComment Comment
hi link cifHeader Macro
hi link keyName Identifier
"hi def cifNumber guifg=darkgreen
hi link cifNumber Special
hi def cifString guifg=darkgreen
