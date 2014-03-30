" Vundle

set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#rc()

" let Vundle manage Vundle
" required!
Bundle 'gmarik/Vundle.vim'

" My bundles here:
"
" original repos on GitHub
Bundle '29decibel/codeschool-vim-theme'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'Lokaltog/vim-powerline'
Bundle 'SirVer/ultisnips'
Bundle 'altercation/vim-colors-solarized'
Bundle 'chrisbra/NrrwRgn'
Bundle 'elzr/vim-json'
Bundle 'ervandew/supertab'
Bundle 'jeetsukumaran/vim-buffergator'
Bundle 'junegunn/vim-easy-align'
Bundle 'kien/ctrlp.vim'
Bundle 'nanotech/jellybeans.vim'
Bundle 'othree/html5.vim'
Bundle 'plasticboy/vim-markdown'
Bundle 'scrooloose/nerdtree'
Bundle 'scrooloose/syntastic'
Bundle 'tomasr/molokai'
Bundle 'tpope/vim-eunuch'
Bundle 'tpope/vim-haml'
Bundle 'tpope/vim-repeat'
Bundle 'tpope/vim-surround'
Bundle 'tsaleh/vim-matchit'
Bundle 'w0ng/vim-hybrid'

" vim-scripts repos
Bundle 'CSApprox'
Bundle 'ZoomWin'
Bundle 'a.vim'
Bundle 'ruby-matchit'
Bundle 'tComment'
Bundle 'visualrepeat'

if has("unix")
  let s:uname=system("uname -s")
else
  let s:uname="Windows"
endif

"" --------------------------------------------------
"" Keyboard mappings
"" --------------------------------------------------

let mapleader=","

nnoremap <leader>ev :vsplit ~/.vim/vimrc<cr>
nnoremap <leader>sv :source ~/.vim/vimrc<cr>

nnoremap <leader>es :UltiSnipsEdit<cr>

nnoremap <space> @q

nnoremap <c-space> i
inoremap <c-space> <esc>

" nnoremap <s-enter> O<esc>
" nnoremap <cr> o<esc>

inoremap <s-tab> <c-d>

" Simplify navigation of the results of quickfix commands such as :helpgrep
nnoremap <s-f1> :cc<cr>
"nnoremap <f3> :cnfile<cr>
"nnoremap <s-f3> :cpfile<cr>
nnoremap <f4> :cnext<cr>
nnoremap <s-f4> :cprev<cr>

let $XMLLINT_INDENT="    "
nnoremap <leader>lf <esc>:1,$!xmllint --format -<cr>
nnoremap <leader>lv <esc>:%w !xmllint --noout --valid -<cr>

nnoremap <leader>n :noh<cr>
nnoremap <leader>d :DiffSaved<cr>
noremap <leader>N :NarrowRegion<cr>

nnoremap <leader>c :SyntasticCheck<cr>
let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [],'passive_filetypes': [] }
let g:syntastic_always_populate_loc_list=1
let g:syntastic_java_javac_custom_classpath_command=
      \ "ant -q path | grep echo | cut -f2- -d] | tr -d ' ' | tr -d '\033' | sed -e s/[[]m$//"
let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [],'passive_filetypes': [] }

nnoremap <leader>ln :lnext<cr>
nnoremap <leader>lp :lprevious<cr>
nnoremap <leader>lN :lprevious<cr>

inoremap <expr> j ((pumvisible())?("\<c-n>"):("j"))
inoremap <expr> k ((pumvisible())?("\<c-p>"):("k"))

" Start interactive EasyAlign in visual mode
vmap <Leader>a <Plug>(EasyAlign)

" Start interactive EasyAlign with a Vim movement
nmap <Leader>a <Plug>(EasyAlign)

" Make tab work as indent in the beginning of lines, autocomplete otherwise
function! InsertTabWrapper()
  let col=col('.') - 1
  if !col || getline('.')[col - 1] !~ '\k'
    return "\<tab>"
  else
    return "\<c-p>"
  endif
endfunction

inoremap <tab> <c-r>=InsertTabWrapper()<cr>

set hidden
set modelines=0

set tabstop=4
set shiftwidth=4
set softtabstop=4
set shiftround
set autoindent
set smartindent
set expandtab
set smarttab
set cino=:0,g0,(0,N-s
set foldmethod=syntax
set foldlevelstart=20

scriptencoding utf-8
set encoding=utf-8
"set encoding=latin1
"set fileformats=dos
"set scrolloff=3
set showmode
set showcmd
set wildmenu
set t_vb=
set list
set listchars=trail:·,precedes:«,extends:»,tab:»·

if s:uname != "Darwin"
  nnoremap <a-s-l> :NERDTree<cr>
  nnoremap <a-o> :A<cr> 
  inoremap <a-o> <esc>:A<cr> 
endif

set titlestring=%f title

set rulerformat=%l:%c ruler
"set formatprg=par

"set wildmode=list:longest
" set visualbell
" set cursorline
set ttyfast
"set ruler
"set backspace=indent, eol, start
"set laststatus=2
set number
"set relativenumber
"set undofile
set colorcolumn=81

nnoremap / /\v
vnoremap / /\v

set ignorecase
set smartcase
"set gdefault
set incsearch
set showmatch
set hlsearch

set nowrap
"set textwidth=79
"set formatoptions=qrn1
"set colorcolumn=85
set history=1000
set scrolloff=3

let g:buffergator_autoexpand_on_split=0
let g:buffergator_viewport_split_policy="R"
" let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"

"set t_Co=256
set hlsearch
"let g:solarized_bold=0
colorscheme molokai
"set background=light
"jellybeans

set guioptions-=m " no menu
set guioptions-=T " no toolbar
set guioptions-=r " no right scrollbar
set guioptions-=L " no left scrollbar

"set synmaxcol=140

"let g:Powerline_symbols='fancy'
let g:Powerline_symbols='unicode'
set laststatus=2 " always show status line

"let g:showmarks_include="abcdefzxABJio"

"if &term=~'^xterm'
"" solid underscore
"let &t_SI .= "\<Esc>[4 q"
"" solid block
"let &t_EI .= "\<Esc>[2 q"
"" 1 or 0 -> blinking block
"" 3 -> blinking underscore
"endif

call tcomment#DefineType('ant', g:tcommentInlineXML)
call tcomment#DefineType('ant_block', g:tcommentBlockXML)
call tcomment#DefineType('ant_inline', g:tcommentInlineXML)

if has("win32") || has("win16")
  set grepprg=grep\ -n
endif

" --------------------------------------------------
" File-specific stuff
" --------------------------------------------------

au FileType c :setlocal tabstop=4 shiftwidth=4 softtabstop=4 noexpandtab
au FileType cpp :setlocal tabstop=4 shiftwidth=4 softtabstop=4 noexpandtab
au FileType cs :setlocal tabstop=4 shiftwidth=4 softtabstop=4
au FileType log :setlocal nonumber
au FileType markdown :setlocal textwidth=79 formatoptions+=t nonumber
au FileType objc :setlocal tabstop=4 shiftwidth=4 softtabstop=4
au FileType python :setlocal tabstop=4 shiftwidth=4 softtabstop=4
au FileType xml :setlocal tabstop=4 shiftwidth=4 softtabstop=4
au FileType html :setlocal tabstop=2 shiftwidth=2 softtabstop=2
au FileType vim :setlocal tabstop=2 shiftwidth=2 softtabstop=2
au FileType ruby :setlocal tabstop=2 shiftwidth=2 softtabstop=2

au FileType ruby :nnoremap <leader>r :Ruby<cr>
au FileType ruby :nnoremap <leader>s :RSpecTest<cr>
au FileType ruby :nnoremap <leader>S :RSpecFile<cr>

autocmd FileType java :nnoremap <leader>T :!ant test<cr>

autocmd BufRead,BufNewFile *.md set filetype=markdown

if s:uname=="Windows"
  autocmd BufWritePost *.java silent! !start /B ctags -R .
elseif has("unix")
  autocmd BufWritePost *.java silent! ctags -R . &
endif

autocmd Syntax c,cpp,vim,xml,xsd,html,xhtml,ruby,python,lua,objc setlocal foldmethod=syntax
" au Syntax cs setlocal foldmethod=indent
autocmd Syntax cs setlocal foldmethod=syntax
autocmd Syntax c,cpp,vim,xml,xsd,html,xhtml,ruby,python,lua,objc,cs normal zR

autocmd BufRead,BufNewFile *.cif,*.cif.txt setfiletype cif
autocmd BufRead,BufNewFile managed_*.log setfiletype managed_log
autocmd BufRead,BufNewFile *-xgsos.*.log setfiletype xgsos_log
autocmd BufRead,BufNewFile *.log setfiletype log
autocmd BufRead,BufNewFile *.xaml,*.msbuild setfiletype xml

augroup BgHighlight
  autocmd!
  autocmd WinEnter * set number
  autocmd WinLeave * set nonumber
augroup END

" --------------------------------------------------
" Spelling
" --------------------------------------------------

if v:version >= 700
  setlocal spell spelllang=en
  nnoremap <leader>ll :set spell!<cr>
  nnoremap <leader>le :set spelllang=en<cr>
  nnoremap <leader>ls :set spelllang=sv<cr>
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
if !exists(":Hexmode")
  command -bar Hexmode call ToggleHex()
endif

if !exists("*ToggleHex")
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
end

nnoremap <C-H> :Hexmode<cr>
" inoremap <C-H> <Esc>:Hexmode<cr>
" vnoremap <C-H> :<C-U>Hexmode<cr>

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


" TortoiseHg functions

if !exists(":TortoiseHgLog")
  command TortoiseHgLog call ShowTortoiseHgLog()
endif

if !exists("*ShowTortoiseHgLog")
  fun ShowTortoiseHgLog()
    execute '!start thg log %'
  endfun
endif

nnoremap <leader>tl :TortoiseHgLog<cr>

if !exists(":TortoiseHgVDiff")
  command TortoiseHgVDiff call ShowTortoiseHgVDiff()
endif

if !exists("*ShowTortoiseHgVDiff")
  fun ShowTortoiseHgVDiff()
    execute '!start thg vdiff %'
  endfun
endif

nnoremap <leader>td :TortoiseHgVDiff<cr>

if !exists(":MakeCheck")
  command MakeCheck call RunMakeCheck()
endif

if !exists(":Ruby")
  command Ruby call RunRuby()
endif

if !exists(":RSpecTest")
  command RSpecTest call RunRSpecTest()
endif

if !exists(":RSpecFile")
  command RSpecFile call RunRSpecFile()
endif


if !exists("*RunMakeCheck")
  fun RunMakeCheck()
    execute '!make check'
  endfun
endif

if !exists("*RunRuby")
  fun RunRuby()
    execute '!ruby ' . expand('%')
  endfun
endif

if !exists("*RunRSpecTest")
  fun RunRSpecTest()
    execute '!spec ' . expand('%') . ':' . line('.')
  endfun
endif

if !exists("*RunRSpecFile")
  fun RunRSpecFile()
    execute '!spec ' . expand('%')
  endfun
endif

" GUI stuff

if has("gui_running")             " 'guifont' doesn't work in the console
  if has("gui_macvim")
    set transparency=5
    set guifont=Inconsolata:h14

    let b:screen_height = system("osascript -e 'tell application \"Finder\" to get bounds of window of desktop' | cut -d ' ' -f 4")
    if b:screen_height > 900
      set columns=210
      set lines=78
    else
      set columns=115
      set lines=50
    endif
  else
    set columns=115
    set lines=80

    if has("gui_gtk2")              " GTK+2 but not GTK+1
      set guifont=Inconsolata\ 12
    else                            " non-X11 GUIs (Windows, Carbon, ...)
      set guifont=Inconsolata:h12
    endif

    autocmd GUIEnter * simalt ~X
  endif

  if has('title')
    set title titlestring=%F%y%m%r
  endif
endif

nnoremap <a-g> <c-]>

filetype off
filetype plugin indent on
syntax on

autocmd FileType java set cino=j1,(0

