set hidden
set nocompatible
set modelines=0

set tabstop=2
set shiftwidth=2
set softtabstop=2
set autoindent smartindent
set expandtab
set smarttab

"set encoding=utf-8
set encoding=latin1
"set scrolloff=3
set showmode
set showcmd
set wildmenu

set titlestring=%f title

set rulerformat=%l:%c ruler

"set wildmode=list:longest
"set visualbell
set cursorline
set ttyfast
"set ruler
"set backspace=indent, eol, start
"set laststatus=2
set number
"set relativenumber
"set undofile

nnoremap / /\v
vnoremap / /\v

set ignorecase " Make searches case-insensitive, unless they contain upper-case letters
set smartcase
set gdefault
set incsearch
set showmatch
set hlsearch

set nowrap
"set textwidth=79
"set formatoptions=qrn1
"set colorcolumn=85
set history=1000
set scrolloff=3
"set backupdir=~/.vimtmp,~/tmp,~/tmp,/var/tmp,/tmp
"set directory=~/.vmptmp,~/tmp,~/tmp,/var/tmp,/tmp

let mapleader=","

syntax enable
set hlsearch
nnoremap <esc> :noh<CR><esc>
set background=dark
let g:solarized_bold=0
colorscheme solarized
filetype on
filetype plugin on
filetype indent on

let g:showmarks_include="abcdefzxABJio"

if &term =~ '^xterm'
  " solid underscore
  let &t_SI .= "\<Esc>[4 q"
  " solid block
  let &t_EI .= "\<Esc>[2 q"
  " 1 or 0 -> blinking block
  " 3 -> blinking underscore
endif

" --------------------------------------------------
" Keyboard mappings
" --------------------------------------------------

"nnoremap <Space> @q

nnoremap <C-space> i
inoremap <C-space> <Esc>

nnoremap <S-Enter> O<Esc>
nnoremap <CR> o<Esc>

inoremap <S-Tab> <C-d>

" Simplify navigation of the results of quickfix commands such as :helpgrep
nnoremap <S-F1>  :cc<CR>
nnoremap <F2>    :cnext<CR>
nnoremap <S-F2>  :cprev<CR>
nnoremap <F3>    :cnfile<CR>
nnoremap <S-F3>  :cpfile<CR>
nnoremap <F4>    :cfirst<CR>
nnoremap <S-F4>  :clast<CR>

nnoremap <F8> <Esc>:1,$!xmllint --noout --format -<CR>
nnoremap <S-F8> <Esc>:1,$!xmllint --noout --valid -<CR>

nnoremap <A-o> :A<CR> 
inoremap <A-o> <Esc>:A<CR> 

nnoremap <Leader>f :FufFile<CR>
nnoremap <Leader>m :MRU<CR>
nnoremap <Leader>o :only<CR>
nnoremap <Leader>n :noh<CR>
nnoremap <Leader>d :DiffSaved<CR>

" --------------------------------------------------
" File-specific stuff
" --------------------------------------------------

autocmd FileType cpp :setlocal tabstop=4 shiftwidth=4 softtabstop=4 noexpandtab background=dark
autocmd FileType cs :setlocal tabstop=4 shiftwidth=4 softtabstop=4 background=dark

" --------------------------------------------------
" Spelling
" --------------------------------------------------

if v:version >= 700
  setlocal spell spelllang=en
  nmap <Leader>ll :set spell!<cr>
  nmap <Leader>le :set spelllang=en<cr>
  nmap <Leader>ls :set spelllang=sv<cr>
endif

ia teh      the
ia htis     this
ia tihs     this
ia funciton function
ia fucntion function
ia funtion  function
ia retunr   return
ia reutrn   return
ia sefl     self
ia eslf     self

set nospell

let g:showmarks_include="abcdefzxABJio"

" ex command for toggling hex mode - define mapping if desired
command -bar Hexmode call ToggleHex()

" Helper function to toggle hex mode
function ToggleHex()
  " hex mode should be considered a read-only operation
  " save values for modified and read-only for restoration later,
  " and clear the read-only flag for now
  let l:modified=&mod
  let l:oldreadonly=&readonly
  let &readonly=0
  let l:oldmodifiable=&modifiable
  let &modifiable=1
  if !exists("b:editHex") || !b:editHex
    " save old options
    let b:oldft=&ft
    let b:oldbin=&bin
    " set new options
    setlocal binary " make sure it overrides any textwidth, etc.
    let &ft="xxd"
    " set status
    let b:editHex=1
    " switch to hex editor
    %!xxd
  else
    " restore old options
    let &ft=b:oldft
    if !b:oldbin
      setlocal nobinary
    endif
    " set status
    let b:editHex=0
    " return to normal editing
    %!xxd -r
  endif
  " restore values for modified and read only state
  let &mod=l:modified
  let &readonly=l:oldreadonly
  let &modifiable=l:oldmodifiable
endfunction
 
nnoremap <C-H> :Hexmode<CR>
inoremap <C-H> <Esc>:Hexmode<CR>
vnoremap <C-H> :<C-U>Hexmode<CR>

autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o " Disable automatic comment insertion


" Function to diff current buffer with saved file

function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()


" Windows-specific stuff

if has("gui_running")             " 'guifont' doesn't work in the console
  if has("gui_gtk2")              " GTK+2 but not GTK+1
    set guifont=Droid\ Sans\ Mono\ 10
  elseif has("gui_kde")           " the obsolete kvim (6.2 or earlier)
    set guifont=Droid\ Sans\ Mono\ 10-1/5/50/0/0/1/0
  elseif has("gui_photon")        " QNX Photon
    set guifont=Droid\ Sans\ Mono:s10
  elseif has("x11")               " other X11 GUIs, including GTK+1
    set guifont=*-courier-medium-r-normal-*-*-100-*-*-m-*-*
  else                            " non-X11 GUIs (Windows, Carbon, ...)
    set guifont=Droid_Sans_Mono:h10
  endif
endif

set guioptions-=m " No menu
set guioptions-=T " No toolbar

set lines=50
set columns=140


