" Vundle

set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#rc()

" let Vundle manage Vundle
" required!
Plugin 'gmarik/Vundle.vim'

" My bundles here:
"
" original repos on GitHub
Plugin '29decibel/codeschool-vim-theme'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'Lokaltog/vim-powerline'
Plugin 'OrangeT/vim-csharp'
Plugin 'Raimondi/delimitMate'
Plugin 'Shougo/vimproc.vim'
Plugin 'SirVer/ultisnips'
Plugin 'altercation/vim-colors-solarized'
Plugin 'chilicuil/vim-sml-coursera'
Plugin 'chrisbra/NrrwRgn'
Plugin 'ciaranm/inkpot'
Plugin 'croaker/mustang-vim'
Plugin 'dag/vim2hs'
Plugin 'digitaltoad/vim-jade'
Plugin 'elzr/vim-json'
Plugin 'ervandew/supertab'
Plugin 'honza/vim-snippets'
Plugin 'jeetsukumaran/vim-buffergator'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'jonathanfilip/vim-lucius'
Plugin 'jpalardy/vim-slime'
Plugin 'junegunn/vim-easy-align'
Plugin 'kchmck/vim-coffee-script'
Plugin 'kien/ctrlp.vim'
Plugin 'ludovicchabant/vim-lawrencium'
Plugin 'majutsushi/tagbar'
Plugin 'mileszs/ack.vim'
Plugin 'morhetz/gruvbox'
Plugin 'nanotech/jellybeans.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'othree/html5.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'pbrisbin/vim-syntax-shakespeare'
Plugin 'plasticboy/vim-markdown'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'sickill/vim-monokai'
Plugin 'sjl/badwolf'
Plugin 'tomasr/molokai'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-endwise'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-haml'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'vim-ruby/vim-ruby'
Plugin 'vim-scripts/BusyBee'
Plugin 'vim-scripts/YankRing.vim'
Plugin 'vim-scripts/darktango.vim'
Plugin 'vim-scripts/phd'
Plugin 'vim-scripts/summerfruit256.vim'
Plugin 'w0ng/vim-hybrid'
Plugin 'wavded/vim-stylus'
Plugin 'wlangstroth/vim-racket'

" vim-scripts repos
Plugin 'CSApprox'
Plugin 'ZoomWin'
Plugin 'a.vim'
Plugin 'actionscript.vim'
Plugin 'glsl.vim'
Plugin 'matchit.zip'
" Bundle 'ruby-matchit'
Plugin 'tComment'
Plugin 'visualrepeat'

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

inoremap <s-space> <space>

inoremap kj <c-c>

" nnoremap <s-enter> O<esc>
" nnoremap <cr> o<esc>

inoremap <s-tab> <c-d>

" Simplify navigation of the results of quickfix commands such as :helpgrep
nnoremap <s-f1> :cc<cr>
"nnoremap <f3> :cnfile<cr>
"nnoremap <s-f3> :cpfile<cr>
nnoremap <f4> :cnext<cr>
nnoremap <s-f4> :cprev<cr>

" Ack
nnoremap <f12> "zyiw :exe "Ack --type-set=this=." . fnamemodify(@%, ":e") . " --this \"\\b" . @z . "\\b\"" <cr> :cope <cr>
vnoremap <f12> "zy :exe "Ack --type-set=this=." . fnamemodify(@%, ":e") . " --this \"" . @z . "\"" <cr> :cope <cr>

" Ag
" nnoremap <f12> "zyiw :exe "Ack \"\\b" . @z . "\\b\"" <cr> :cope <cr>
" xnoremap <f12> "zy :exe "Ack \"" . @z . "\"" <cr> :cope <cr>

nnoremap <f5> "=strftime("%Y-%m-%d")<cr>P
inoremap <f5> <c-r>=strftime("%Y-%m-%d")<cr>

nnoremap <s-f5> "=strftime("%H:%M:%S")<cr>P
inoremap <s-f5> <c-r>=strftime("%H:%M:%S")<cr>

nnoremap <f6> :let @+=fnamemodify(@%, ":p")<cr>

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

" let g:ackprg = 'ag --nogroup --nocolor --column'
let g:ack_default_options = " -H --nocolor --nogroup --column --ignore-dir={node_modules,bower_components,dist}"

let g:yankring_replace_n_pkey = "<c-k>"
let g:yankring_replace_n_nkey = "<c-j>"
nnoremap <silent> <leader>y :YRShow<cr>

nnoremap <leader>ln :lnext<cr>
nnoremap <leader>lp :lprevious<cr>
nnoremap <leader>lN :lprevious<cr>

inoremap <expr> j ((pumvisible())?("\<c-n>"):("j"))
inoremap <expr> k ((pumvisible())?("\<c-p>"):("k"))

imap <c-space> <Plug>delimitMateS-Tab

" Start interactive EasyAlign in visual mode
vmap <Leader>a <Plug>(EasyAlign)

" Start interactive EasyAlign with a Vim movement
nmap <Leader>a <Plug>(EasyAlign)

" if !exists(":bb")
"   command -bar Hexmode call ToggleHex()
" endif

nnoremap <leader>. :CtrlPTag<cr>
nmap <f8> :TagbarToggle<cr>

" if has("gui_running")
"   autocmd VimEnter * NERDTree
" endif

autocmd VimEnter * wincmd p

if !exists("*AltBufferAndDeleteCurrent")
  function AltBufferAndDeleteCurrent()
    b#
    bd#
  endfunction
end

let g:ctrlp_custom_ignore = {
  \ 'dir': '\v(release|debug|data|assets|node_modules|bower_components|dist)$'
  \ }

if !exists("*InsertTabWrapper")
  " Make tab work as indent in the beginning of lines, autocomplete otherwise
  function InsertTabWrapper()
    let col=col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
      return "\<tab>"
    else
      return "\<c-p>"
    endif
  endfunction

  inoremap <tab> <c-r>=InsertTabWrapper()<cr>
end

set hidden
set modelines=0

set tabstop=4
set shiftwidth=4
set softtabstop=4
set shiftround
set autoindent
set smartindent
if !exists("g:vim_initialized")
  set expandtab
  set smarttab
end
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

set splitright
set splitbelow

set wildignore+=node_modules/**

if s:uname != "Darwin"
  " Navigate help
  noremap <c-¨> <c-]>

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

set tags=./tags;

" nnoremap / /\v
" vnoremap / /\v

" nmap s <Plug>(easymotion-s2)
" nmap t <Plug>(easymotion-t2)
" map / <Plug>(easymotion-sn)
" omap / <Plug>(easymotion-tn)
" map n <Plug>(easymotion-next)
" map N <Plug>(easymotion-prev)
let g:EasyMotion_smartcase=1
map <Leader>h <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>l <Plug>(easymotion-linebackward)
let g:EasyMotion_startofline=0 " keep cursor column when JK motion

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
let g:UltiSnipsSnippetsDir="~/.vim/UltiSnips"
let g:UltiSnipsExpandTrigger="<tab>"

" syn match ExtraWhitespace /\s\+$/
if has("gui_running")
  autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
  autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
end

" match trailing whitespace, except when typing at the end of a line
" autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
" autocmd InsertLeave * match ExtraWhitespace /\s\+$/

if s:uname != "Windows"
  set term=screen-256color
endif

" set t_Co=256
set hlsearch
"let g:solarized_bold=0
if has("gui_running")
  colorscheme gruvbox
else
  colorscheme summerfruit256
endif

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

set nojoinspaces

"let g:showmarks_include="abcdefzxABJio"

" if &term=~'^xterm'
" " solid underscore
"   let &t_SI .= "\<Esc>[4 q"
" " solid block
"   let &t_EI .= "\<Esc>[2 q"
" " 1 or 0 -> blinking block
" " 3 -> blinking underscore
" endif

if !has("gui_running") && has("unix")
  if $TMUX != ''
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  endif
endif

call tcomment#DefineType('ant', g:tcommentInlineXML)
call tcomment#DefineType('ant_block', g:tcommentBlockXML)
call tcomment#DefineType('ant_inline', g:tcommentInlineXML)
call tcomment#DefineType('java', '// %s')
call tcomment#DefineType('glsl', '// %s')
call tcomment#DefineType('coffeescript_block', '###%s###')

if has("win32") || has("win16")
  set grepprg=grep\ -n
endif

if has("win32") || has("win16")
  let g:haddock_browser = "C:/Program Files/Opera/Opera.exe"
elseif has("unix")
  let g:haddock_browser = "links"
end

if !exists("*SmlModeStuff")
  function SmlModeStuff()
    let g:slime_no_mappings = 1
    let g:slime_target = "tmux"
    let g:slime_paste_file = tempname()
    nmap <c-c><c-c> "zyip \| :call SmlSlimeSend(@z)<cr>
    xmap <c-c><c-c> "zy \| :call SmlSlimeSend(@z)<cr>
    nmap <c-c>v :SlimeConfig<cr>

    nnoremap <leader>r :call SmlFile()<cr>
  endfunction
end

if !exists("*SmlSlimeSend")
  function SmlSlimeSend(data)
    let l:d = substitute(a:data, "\\n\\+$", "", "") " remove trailing newlines
    exe "SlimeSend1 " . l:d . ";"
  endfunction
end

" --------------------------------------------------
" File-specific stuff
" --------------------------------------------------

filetype on

augroup filetypes
  autocmd!
  autocmd FileType c setlocal tabstop=4 shiftwidth=4 noexpandtab
  autocmd FileType cpp setlocal tabstop=4 shiftwidth=4 noexpandtab
  autocmd FileType cs setlocal tabstop=4 shiftwidth=4
  autocmd FileType log setlocal nonumber
  autocmd FileType markdown setlocal textwidth=79 formatoptions+=t
  autocmd FileType objc setlocal tabstop=4 shiftwidth=4
  autocmd FileType python setlocal tabstop=4 shiftwidth=4
  autocmd FileType xml setlocal tabstop=4 shiftwidth=4
  autocmd FileType html setlocal tabstop=2 shiftwidth=2
  autocmd FileType vim setlocal tabstop=2 shiftwidth=2
  autocmd FileType jade setlocal tabstop=2 shiftwidth=2
  autocmd FileType stylus setlocal tabstop=2 shiftwidth=2
  autocmd FileType scss setlocal tabstop=2 shiftwidth=2
  autocmd FileType haskell setlocal tabstop=8 shiftwidth=2

  autocmd FileType ruby setlocal tabstop=2 shiftwidth=2
  autocmd FileType ruby nnoremap <leader>r :call RunRuby()<cr>
  autocmd FileType ruby nnoremap <leader>s :RSpecTest<cr>
  autocmd FileType ruby nnoremap <leader>S :RSpecFile<cr>

  autocmd FileType python nnoremap <leader>r :call RunPython()<cr>

  autocmd FileType lua nnoremap <leader>s :BustedFile<cr>

  autocmd FileType javascript nnoremap <leader>l :!jslint %<cr>

  autocmd FileType coffee setlocal tabstop=2 shiftwidth=2 softtabstop=2

  autocmd FileType sml :call SmlModeStuff()

  autocmd FileType haskell nnoremap <leader>r :call RunGhc()<cr>
  autocmd FileType haskell nnoremap <leader>g :call RunGhci()<cr>
  autocmd FileType haskell map <silent> tu :call GHC_BrowseAll()<CR>
  autocmd FileType haskell map <silent> tw :call GHC_ShowType(1)<CR>
augroup END

augroup autocommands
  autocmd!
  if s:uname=="Windows"
    autocmd BufWritePost *.java silent! !start /B ctags -R .
    autocmd BufWritePost *.rb silent! !start /B ctags -a %
  elseif has("unix")
    autocmd BufWritePost *.java silent! ctags -R . &
    autocmd BufWritePost *.rb silent! ctags -a % &
  endif

  autocmd Syntax c,cpp,vim,xml,xsd,html,xhtml,ruby,python,lua,objc setlocal foldmethod=syntax
  " au Syntax cs setlocal foldmethod=indent
  autocmd Syntax cs setlocal foldmethod=syntax
  autocmd Syntax c,cpp,vim,xml,xsd,html,xhtml,ruby,python,lua,objc,cs normal zR

  autocmd BufRead,BufNewFile *.md set filetype=markdown
  autocmd BufRead,BufNewFile *.cif,*.cif.txt setfiletype cif
  autocmd BufRead,BufNewFile managed_*.log,global_*.log setfiletype managed_log
  autocmd BufRead,BufNewFile *-xgsos.*.log,horizon_*.log setfiletype xgsos_log
  autocmd BufRead,BufNewFile exceptions*.log setfiletype exceptions_log
  autocmd BufRead,BufNewFile *.log setfiletype log
  autocmd BufRead,BufNewFile *.xaml,*.msbuild,*.targets,*.plist setfiletype xml
  autocmd BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setfiletype glsl
  autocmd BufRead,BufNewFile *.as set filetype=actionscript

  autocmd WinEnter * set number
  autocmd WinLeave * set nonumber
  autocmd BufWinEnter * set number
  autocmd BufWinLeave * set nonumber
  autocmd BufWinEnter * set foldlevel=999
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

" nnoremap <C-H> :Hexmode<cr>
" inoremap <C-H> <Esc>:Hexmode<cr>
" vnoremap <C-H> :<C-U>Hexmode<cr>


" Function to diff current buffer with saved file

function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()


if !exists(":TortoiseHgLog")
  command TortoiseHgLog call ShowTortoiseHgLog()
endif

if !exists("*ShowTortoiseHgLog")
  fun ShowTortoiseHgLog()
    if has("unix")
      execute '!thg log %'
    else
      execute '!start thg log %'
    endif
  endfun
endif

nnoremap <leader>tl :TortoiseHgLog<cr>

if !exists(":HgOpenDiffCommand")
  command HgOpenDiffCommand call HgOpenDiff()
endif

if !exists("*HgOpenDiff")
  fun HgOpenDiff()
    if has("unix")
      execute '!hg opendiff %'
    else
      execute '!start thg vdiff %'
    endif
  endfun
endif

nnoremap <leader>td :HgOpenDiffCommand<cr>

if !exists(":TortoiseHgAnnotate")
  command TortoiseHgAnnotate call ShowTortoiseHgAnnotate()
endif

if !exists("*ShowTortoiseHgAnnotate")
  fun ShowTortoiseHgAnnotate()
    if has("unix")
      execute '!thg annotate %'
    else
      execute '!start thg annotate %'
    endif
  endfun
endif

nnoremap <leader>ta :TortoiseHgAnnotate<cr>

if !exists(":MakeCheck")
  command MakeCheck call RunMakeCheck()
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

if !exists(":RSpecTest")
  command RSpecTest call RunRSpecTest()
endif

if !exists("*RunRSpecTest")
  fun RunRSpecTest()
    execute '!spec ' . expand('%') . ':' . line('.')
  endfun
endif

if !exists(":RSpecFile")
  command RSpecFile call RunRSpecFile()
endif

if !exists("*RunRSpecFile")
  fun RunRSpecFile()
    execute '!spec ' . expand('%')
  endfun
endif

if !exists("*RunPython")
  fun RunPython()
    execute '!python ' . expand('%')
  endfun
endif

if !exists(":BustedFile")
  command BustedFile call RunBustedFile()
endif

if !exists("*RunBustedFile")
  fun RunBustedFile()
    execute '!busted ' . expand('%')
  endfun
endif

if !exists('*SmlFile')
  fun SmlFile()
    execute '!sml ' . expand('%')
  endfun
endif

if !exists("*RunGhc")
  fun RunGhc()
    execute '!runghc ' . expand('%')
  endfun
endif

if !exists("*RunGhci")
  fun RunGhci()
    execute '!ghci ' . expand('%')
  endfun
endif

" GUI stuff

if has("gui_running")             " 'guifont' doesn't work in the console
  if has("gui_macvim")
    set transparency=0
    set guifont=Inconsolata:h14

    let b:screen_height = system("osascript -e 'tell application \"Finder\" to get bounds of window of desktop' | cut -d ' ' -f 4")
    if b:screen_height > 900
      set columns=210
      set lines=78
    else
      set columns=140
      set lines=56
    endif
  else
    set columns=140
    set lines=80

    if has("gui_gtk2")              " GTK+2 but not GTK+1
      set guifont=Inconsolata\ 12
    else                            " non-X11 GUIs (Windows, Carbon, ...)
      set guifont=Inconsolata:h12
    endif

    augroup gui_au
      autocmd!
      autocmd GUIEnter * simalt ~X
    augroup END
  endif

  if has('title')
    set title titlestring=%F%y%m%r
  endif
endif

nnoremap <a-g> <c-]>

filetype off
filetype plugin indent on
syntax on

" For some reason, this needs to be put here to have effect
augroup more_au
  autocmd!

  autocmd FileType java set cino=j1,(0
  autocmd FileType java nnoremap <leader>T :!ant test<cr>

  autocmd FileType cmake set indentexpr=

  " Disable automatic comment insertion
  autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
augroup END

let g:vim_initialized = 1
