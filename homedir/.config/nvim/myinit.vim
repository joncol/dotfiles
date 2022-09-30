set nocompatible

if empty(glob('~/.local/share/nvim/site/autoload/plug.vim'))
  silent !curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

let g:go_version_warning = 0

call plug#begin('~/.vim/plugged')
Plug '29decibel/codeschool-vim-theme'
Plug 'LnL7/vim-nix'
Plug 'Lokaltog/vim-easymotion'
Plug 'MaxSt/FlatColor'
Plug 'OrangeT/vim-csharp'
Plug 'Raimondi/delimitMate'
Plug 'Shougo/vimproc.vim'
" Plug 'SirVer/ultisnips'
Plug 'altercation/vim-colors-solarized'
Plug 'cespare/vim-toml'
Plug 'chilicuil/vim-sml-coursera'
Plug 'chrisbra/NrrwRgn'
Plug 'ciaranm/inkpot'
Plug 'croaker/mustang-vim'
Plug 'dag/vim2hs'
Plug 'dag/vim-fish'
Plug 'digitaltoad/vim-jade'
Plug 'ekalinin/Dockerfile.vim'
Plug 'elzr/vim-json'
Plug 'ervandew/supertab'
Plug 'fatih/vim-go'
Plug 'guns/vim-clojure-static'
Plug 'guns/vim-sexp'
Plug 'honza/vim-snippets'
Plug 'hynek/vim-python-pep8-indent'
Plug 'itchyny/vim-haskell-indent'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'jelera/vim-javascript-syntax'
Plug 'jonathanfilip/vim-lucius'
" Plug 'jpalardy/vim-slime'
Plug 'jremmen/vim-ripgrep'
Plug 'junegunn/vim-easy-align'
Plug 'kchmck/vim-coffee-script'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'ludovicchabant/vim-lawrencium'
Plug 'luochen1990/rainbow'
Plug 'majutsushi/tagbar'
Plug 'mileszs/ack.vim'
Plug 'morhetz/gruvbox'
Plug 'nanotech/jellybeans.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'nelstrom/vim-markdown-folding'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'pbrisbin/vim-syntax-shakespeare'
Plug 'peterhoeg/vim-qml'
Plug 'plasticboy/vim-markdown'
Plug 'purescript-contrib/purescript-vim'
Plug 'rust-lang/rust.vim'
Plug 'scrooloose/nerdtree'
Plug 'scrooloose/syntastic'
Plug 'sickill/vim-monokai'
Plug 'sjl/badwolf'
Plug 'tomasr/molokai'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-haml'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-salve'
Plug 'tpope/vim-sexp-mappings-for-regular-people'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-ctrlspace/vim-ctrlspace'
Plug 'vim-jp/vim-cpp'
Plug 'vim-ruby/vim-ruby'
Plug 'vim-scripts/BusyBee'
" Plug 'vim-scripts/YankRing.vim'
Plug 'vim-scripts/darktango.vim'
Plug 'vim-scripts/phd'
Plug 'vim-scripts/summerfruit256.vim'
Plug 'w0ng/vim-hybrid'
Plug 'wavded/vim-stylus'
Plug 'wlangstroth/vim-racket'

" vim-scripts repos
" Plug 'CSApprox'
Plug 'vim-scripts/Toggle'
Plug 'vim-scripts/a.vim'
Plug 'vim-scripts/actionscript.vim'
Plug 'vim-scripts/glsl.vim'
Plug 'vim-scripts/matchit.zip'
Plug 'vim-scripts/python.vim'
" Plug 'vim-scripts/ruby-matchit'
Plug 'vim-scripts/tComment'
Plug 'vim-scripts/visualrepeat'

call plug#end()

if has("unix")
  let s:uname=system("uname -s")
else
  let s:uname="Windows"
endif

let g:go_fmt_autosave = 0

"" --------------------------------------------------
"" Keyboard mappings
"" --------------------------------------------------

let mapleader=","

nnoremap <leader>ev :e ~/.config/nvim/myinit.vim<cr>
nnoremap <leader>sv :source ~/.config/nvim/myinit.vim<cr>

unmap Y

" nnoremap <leader>es :UltiSnipsEdit<cr>

inoremap <s-space> <space>

" inoremap <expr> kj (pumvisible())?("kj") : ("\<c-c>")
inoremap kj <c-c>
inoremap lh <c-c>
inoremap <c-g> <esc>

" nnoremap <s-enter> O<esc>
" nnoremap <cr> o<esc>

inoremap <s-tab> <c-d>

vnoremap <leader>p "0p
nnoremap <leader>P viw"0p

" Simplify navigation of the results of quickfix commands such as :helpgrep
nnoremap <s-f1> :cc<cr>
"nnoremap <f3> :cnfile<cr>
"nnoremap <s-f3> :cpfile<cr>
nnoremap <f4> :cnext<cr>
nnoremap <s-f4> :cprev<cr>

" Copy paste to global register.
vnoremap <C-insert> "+y
nnoremap <S-insert> "+p<esc>
inoremap <S-insert> <C-r>+

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

" underline
nnoremap <f7> yyp<c-v>$r-
nnoremap <s-f7> yyp<c-v>$r=
nnoremap <f8> yyp<c-v>$r=

inoremap <f7> <esc>yyp<c-v>$r-A
inoremap <f7> <esc>yyp<c-v>$r-A
inoremap <s-f7> <esc>yyp<c-v>$r=A
inoremap <f8> <esc>yyp<c-v>$r=A

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
nmap <f9> :TagbarToggle<cr>

" if has("gui_running")
"   autocmd VimEnter * NERDTree
" endif

if has("nvim")
  set inccommand=split
endif

" emacs compatibility

nnoremap <C-a> <Nop>
nnoremap <C-x> <Nop>
nnoremap <C-x><C-s> :w<cr>
inoremap <C-x><C-s> <esc>:w<cr>
nnoremap <C-x><C-c> :qa<cr>

autocmd VimEnter * wincmd p

" easy window navigation
nmap <silent> <S-Up> :wincmd k<CR>
nmap <silent> <S-Down> :wincmd j<CR>
nmap <silent> <S-Left> :wincmd h<CR>
nmap <silent> <S-Right> :wincmd l<CR>

let g:vim_json_syntax_conceal = 0
let g:haskell_conceal = 0
let g:haskell_conceal_enumerations = 0

let g:rainbow_active = 0

if !exists("*AltBufferAndDeleteCurrent")
  function AltBufferAndDeleteCurrent()
    b#
    bd#
  endfunction
end

let g:ctrlp_custom_ignore = {
  \ 'dir': '\v(release|debug|data|assets|node_modules|bower_components|dist)$'
  \ }
let g:ctrlp_show_hidden = 1

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
set modelines=5
set relativenumber
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

set showtabline=0

" Turn off beep.
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=
set belloff=all

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

nnoremap <leader>a :A<cr>

set titlestring=%f title

set rulerformat=%l:%c ruler

set ttyfast
set number
set colorcolumn=81

set tags=./TAGS

let g:EasyMotion_smartcase=1
map <Leader>h <Plug>(easymotion-lineforward)
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)
map <Leader>l <Plug>(easymotion-linebackward)
let g:EasyMotion_startofline=0 " keep cursor column when JK motion

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch

set nowrap
set history=1000
set scrolloff=3

let g:buffergator_autoexpand_on_split=0
let g:buffergator_viewport_split_policy="R"
" let g:UltiSnipsSnippetsDir="~/.vim/MySnippets"
" let g:UltiSnipsSnippetDirectories=[$HOME.'/.vim/MySnippets']
" let g:UltiSnipsExpandTrigger="<tab>"

if has("gui_running")
  autocmd BufWinEnter * match TrailingWhitespace /\s\+$/
  autocmd ColorScheme * highlight TrailingWhitespace ctermbg=red guibg=red
end

if s:uname != "Windows" && !has("gui_running") && !has("nvim")
  set term=screen-256color
endif

set hlsearch

set guioptions-=m " no menu
set guioptions-=T " no toolbar
set guioptions-=r " no right scrollbar
set guioptions-=L " no left scrollbar

set laststatus=2 " always show status line

set nojoinspaces

let g:airline_powerline_fonts = 1

if !has("gui_running") && has("unix")
  if $TMUX != ''
    let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
    let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
  else
    let &t_SI = "\<Esc>]50;CursorShape=1\x7"
    let &t_EI = "\<Esc>]50;CursorShape=0\x7"
  endif
endif

nnoremap <A-;> :TComment<cr>
vnoremap <A-;> :TComment<cr>

if has("win32") || has("win16")
  set grepprg=grep\ -n
endif

if has("win32") || has("win16")
  let g:haddock_browser = "C:/Program Files/Opera/Opera.exe"
elseif has("unix")
  let g:haddock_browser = "links"
end

" if !exists("*SmlModeStuff")
"   function SmlModeStuff()
"     let g:slime_no_mappings = 1
"     let g:slime_target = "tmux"
"     let g:slime_paste_file = tempname()
"     nmap <c-c><c-c> "zyip \| :call SmlSlimeSend(@z)<cr>
"     xmap <c-c><c-c> "zy \| :call SmlSlimeSend(@z)<cr>
"     nmap <c-c>v :SlimeConfig<cr>
"
"     nnoremap <leader>r :call SmlFile()<cr>
"   endfunction
" end
"
" if !exists("*SmlSlimeSend")
"   function SmlSlimeSend(data)
"     let l:d = substitute(a:data, "\\n\\+$", "", "") " remove trailing newlines
"     exe "SlimeSend1 " . l:d . ";"
"   endfunction
" end

" --------------------------------------------------
" File-specific stuff
" --------------------------------------------------

filetype on

augroup filetypes
  autocmd!
  autocmd FileType c setlocal tabstop=4 shiftwidth=4 "noexpandtab
  autocmd FileType cpp setlocal tabstop=4 shiftwidth=4 "noexpandtab
  autocmd FileType cs setlocal tabstop=4 shiftwidth=4
  autocmd FileType log setlocal nonumber
  autocmd FileType markdown setlocal textwidth=79 formatoptions+=t
  autocmd FileType objc setlocal tabstop=4 shiftwidth=4
  autocmd FileType python setlocal tabstop=4 shiftwidth=4
  autocmd FileType xml setlocal tabstop=2 shiftwidth=2
  autocmd FileType yaml setlocal tabstop=2 shiftwidth=2
  autocmd FileType html setlocal tabstop=2 shiftwidth=2
  autocmd FileType vim setlocal tabstop=2 shiftwidth=2
  autocmd FileType jade setlocal tabstop=2 shiftwidth=2
  autocmd FileType stylus setlocal tabstop=2 shiftwidth=2
  autocmd FileType scss setlocal tabstop=2 shiftwidth=2
  autocmd FileType spdlog setlocal nonumber
  autocmd FileType haskell setlocal tabstop=8 shiftwidth=2
  autocmd FileType tex setlocal tabstop=2 shiftwidth=2
  autocmd FileType plaintex setlocal tabstop=2 shiftwidth=2

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
  autocmd FileType haskell map <silent> tu :call GHC_BrowseAll()<cr>
  autocmd FileType haskell map <silent> tw :call GHC_ShowType(1)<cr>
augroup END

augroup autocommands
  autocmd!
  if s:uname=="Windows"
    autocmd BufWritePost *.java silent! !start /B ctags -f TAGS -R .
    autocmd BufWritePost *.rb silent! !start /B ctags -f TAGS -a %
  elseif has("unix")
    autocmd BufWritePost *.java silent! ctags -f TAGS -R . &
    autocmd BufWritePost *.rb silent! ctags -f TAGS -a % &
  endif

  autocmd Syntax c,cpp,vim,xml,xsd,html,xhtml,ruby,python,lua,objc setlocal foldmethod=syntax
  " au Syntax cs setlocal foldmethod=indent
  autocmd Syntax cs setlocal foldmethod=syntax
  autocmd Syntax c,cpp,vim,xml,xsd,html,xhtml,ruby,python,lua,objc,cs normal zR

  autocmd BufRead,BufNewFile *.md setfiletype markdown
  autocmd BufRead,BufNewFile *.cif,*.cif.txt setfiletype cmb_cif
  autocmd BufRead,BufNewFile managed_*.log,global_*.log setfiletype cmb_managed_log
  autocmd BufRead,BufNewFile *-xgsos.*.log,horizon_*.log setfiletype cmb_xgsos_log
  autocmd BufRead,BufNewFile exceptions*.log setfiletype cmb_exceptions_log
  autocmd BufRead,BufNewFile *.log setfiletype cmb_log
  autocmd BufRead,BufNewFile *.xaml,*.msbuild,*.targets,*.plist setfiletype xml
  autocmd BufNewFile,BufRead *.frag,*.vert,*.fp,*.vp,*.glsl setfiletype glsl
  autocmd BufRead,BufNewFile *.as setfiletype actionscript
  autocmd BufRead,BufNewFile *_log.txt setfiletype spdlog
  autocmd BufRead,BufNewFile *.asd setfiletype lisp

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

fun! s:json_lint()
  :%!python -m json.tool
endfun

command! JsonLint call s:json_lint()

fun! s:remove_trailing_whitespace()
  call inputsave()
  echohl ModeMsg
  let ans = input('Do you want to remove all trailing whitespace (y/n)? ')
  echohl None
  call inputrestore()
  if ans == 'y' || ans == 'Y'
    %s/\v\s+$//
  end
endfun

command! RemoveTrailingWhiteSpace call s:remove_trailing_whitespace()

if s:uname != "Darwin"
  nnoremap <a-bs> :RemoveTrailingWhiteSpace<cr>
else
  nnoremap <c-bs> :RemoveTrailingWhiteSpace<cr>
endif

" QVariant findPackageMarker() const;

fun! s:cpp_declaration_to_definition()
  call inputsave()
  echohl ModeMsg
  let class_name = input('Class name: ')
  echohl None
  call inputrestore()
  s/\v^\s*//
  if class_name != ""
    execute "normal! W\"=class_name\<c-m>Pa::\<esc>f;s\<c-m>{\<c-m>}\<c-m>"
  else
    execute "normal! f;s\<c-m>{\<c-m>}\<c-m>"
  endif
endfun

command! CppDeclToDef call s:cpp_declaration_to_definition()
nnoremap <leader>ci :CppDeclToDef<cr>

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
      execute '!thg vdiff %'
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
    execute '!rspec ' . expand('%') . ':' . line('.')
  endfun
endif

if !exists(":RSpecFile")
  command RSpecFile call RunRSpecFile()
endif

if !exists("*RunRSpecFile")
  fun RunRSpecFile()
    execute '!rspec ' . expand('%')
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
    set guifont=Hack:h14

    let b:screen_height = system("osascript -e 'tell application \"Finder\" to get bounds of window of desktop' | cut -d ' ' -f 4")
    if b:screen_height > 900
      set columns=210
      set lines=78
    else
      set columns=140
      set lines=56
    endif
  else
    set columns=100
    set lines=60

    if has("gui_gtk") || has("gui_gtk2")
      let b:screen_height = system("xrandr | grep 'Screen 0' | cut -d ',' -f2 | cut -d 'x' -f2 | sed 's/^ //'")
      if b:screen_height > 2000
        set guifont=Hack\ 14
      else
        set guifont=FiraCodeMedium\ 10
      endif
    else                            " non-X11 GUIs (Windows, Carbon, ...)
      set guifont=FiraCodeMedium\ 10
    endif

    if s:uname == "Windows"
      " fullscreen mode
      augroup gui_au
        autocmd!
        autocmd GUIEnter * simalt ~X
      augroup END
    endif
  endif

  if has('title')
    set title titlestring=%F%y%m%r
  endif
endif

nnoremap <a-g> <c-]>

filetype off
"filetype plugin indent on
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

"let g:solarized_bold=0
if has("gui_running")
  colorscheme molokai
  set background=dark
  set cursorline
elseif s:uname != "Windows" || !has("nvim")
  colorscheme gruvbox
  " autocmd InsertEnter * set cul
  " autocmd InsertLeave * set nocul
endif

let g:clojure_align_subforms = 1
let g:clojure_fuzzy_indent = 1
let g:clojure_fuzzy_indent_patterns = ['^with', '^def', '^let', '^comment']
let g:clojure_fuzzy_indent_blacklist = ['-fn$', '\v^with-%(meta|out-str|loading-context)$']

let g:CtrlSpaceDefaultMappingKey = "<C-space> "
if executable("ag")
  let g:CtrlSpaceGlobCommand = 'ag -l --nocolor -g ""'
endif

" Telescope
nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

let g:vim_initialized = 1
