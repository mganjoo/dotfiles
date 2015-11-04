" Vim configuration.

" == Prologue == {{{1
scriptencoding utf-8      " Enable UTF-8 chars in listchars
set nocompatible          " Disable compatibility mode

let mapleader = ' '       " Change leader key
let maplocalleader = ','  " Change local leader key

" == Plugins == {{{1
call plug#begin("~/.vim/external")

" Motions and editing enhancements
Plug 'Raimondi/delimitMate'
Plug 'bkad/CamelCaseMotion'
Plug 'godlygeek/tabular'
Plug 'qpkorr/vim-bufkill'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

" Appearance
Plug 'chriskempson/base16-vim'
Plug 'bling/vim-airline'
Plug 'scrooloose/syntastic'
Plug 'airblade/vim-gitgutter'

" Shell interaction
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-dispatch'
Plug 'epeli/slimux'

" Search and replace
Plug 'kien/ctrlp.vim' | Plug 'FelikZ/ctrlp-py-matcher'
Plug 'rking/ag.vim'
Plug 'benjifisher/matchit.zip'
Plug 'nelstrom/vim-visual-star-search'
Plug 'tpope/vim-abolish'

" Snippets
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" Tags
Plug 'chazy/cscope_maps'

" Languages
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
Plug 'edma2/vim-pants', { 'for': 'pants' }
Plug 'klen/python-mode', { 'branch': 'develop', 'for': 'python' }
Plug 'lervag/vimtex', { 'for': 'tex' }
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
Plug 'solarnz/thrift.vim', { 'for': 'thrift' }
Plug 'tpope/vim-rails', { 'for': 'ruby' }
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
Plug 'pantsbuild/vim-pants'

" Autocompletion
Plug 'Shougo/neocomplete.vim'
Plug 'wellle/tmux-complete.vim'

" Browsing
Plug 'scrooloose/nerdtree' | Plug 'jistr/vim-nerdtree-tabs'
Plug 'majutsushi/tagbar'
Plug 'milkypostman/vim-togglelist'
Plug 'rizzatti/dash.vim'
Plug 'regedarek/ZoomWin'

" Source control
Plug 'tpope/vim-fugitive'

" Other dependencies
Plug 'tpope/vim-repeat'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }

" Scripting
Plug 'tpope/vim-scriptease'

call plug#end()

" == Syntax & Indentation == {{{1
syntax on                  " Syntax highlighting
filetype plugin indent on  " Enable file-specific indentation and other settings
set sw=2 sts=2 ts=2        " Tab and shift sizes
set expandtab              " Expand tab to spaces
set shiftround             " Round indent to multiple of shiftwidth
set smarttab               " Expand tabs to spaces in front of line
set copyindent             " Copy indent structure from previous line
set autoindent             " Copy actual indent from previous line

" == General Settings == {{{1
set autoread             " Reload files changed outside vim
set autowrite            " Save before :next, :make etc
set bs=indent,eol,start  " Allow backspace in insert mode
set cursorline           " Show line cursor is on
set encoding=utf-8       " Ensure correct encoding
set hidden               " For correct behavior of buffers
set history=5000         " Store lots of :cmdline history
set number               " Always show line numbers
set relativenumber       " Use relative numbers by default
set showcmd              " Show incomplete cmds down the bottom
set showmode             " Show current mode down the bottom
set visualbell           " No sounds
set shortmess+=I         " Disable startup message
set mouse=a              " Enable mouse mode
set timeoutlen=600       " Shorten the timeout for multi-key commands
set clipboard=unnamed    " Sync clipboard

" == Search Settings {{{1
set incsearch   " Find the next match as we type the search
set hlsearch    " Highlight searches by default
set ignorecase  " Case insensitive searches
set smartcase   " Ignore sensitivity setting with uppercase patterns

" == Backup == {{{1
set backup                     " Create backups
set writebackup                " Create backup before writing
set backupdir=~/.vim-backup//  " Change backup dir
set directory=~/.vim-swap//    " Change swap directory

" == Folds == {{{1
set foldmethod=indent  " Fold based on indent
set foldnestmax=4      " Set conservative fold limit
set nofoldenable       " Don't fold by default
set foldcolumn=1       " Show a single fold column

" == Completion == {{{1
set omnifunc=syntaxcomplete#Complete " Function for omni-completion
set wildmode=list:longest            " List all matches; complete till longest string
set wildmenu                         " Show a menu of completion options

" == Appearance == {{{1
set nowrap      " Don't wrap lines
set linebreak   " Break lines at useful points
set list        " Show auxiliary characters
" Note: 'scriptencoding utf-8' must be called for this to work (see top)
set listchars=tab:▸\ ,trail:·
set colorcolumn=80 " Show column at 80 characters
set laststatus=2   " Always show the status line
set background=dark
set t_Co=256
let base16colorspace=256 " Access colors present in 256 colorspace
silent! colorscheme base16-tomorrow

" == Enhancements == {{{1
" Use ag instead of grep, if available
if executable("ag")
  set grepprg=ag\ --nogroup\ --nocolor
endif

" == Filetype-specific settings == {{{1
au BufRead,BufNewFile *.mesos set filetype=python
au BufRead,BufNewFile *.aurora set filetype=python

" == Plugin Settings == {{{1

" == vim-airline == {{{2
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#formatter = "unique_tail"
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#branch#displayed_head_limit = 20
let g:airline#extensions#branch#format = 1

" == ctrlp.vim == {{{2
let g:ctrlp_max_files = 0                     " No file limit
let g:ctrlp_use_caching = 1                   " Use caching
let g:ctrlp_clear_cache_on_exit = 0           " Don't clear cache on exit
let g:ctrlp_cache_dir = $HOME.'/.cache/ctrlp' " Cache directory
let g:ctrlp_by_filename = 1                   " Default to filename search
let g:ctrlp_root_markers = ['.ci', '.git', '.svn', '.hg'] " .ci is useful
let g:ctrlp_match_func = { "match": "pymatcher#PyMatch" } " Use faster matcher
let g:ctrlp_lazy_update = 1                   " Allow more responsive typing

" Use ag for searching, if available; otherwise fall back to find
if executable("ag")
  let s:search_command = 'ag %s -i --nocolor --nogroup --hidden
      \ --ignore .git
      \ --ignore .svn
      \ --ignore .hg
      \ --ignore .pants.d
      \ -g ""'
else
  let s:search_command = 'find %s -type f'
endif

" Faster ctrlp population if in git repo
let g:ctrlp_user_command = {
  \ 'types': {
    \ 1: ['.git', 'git --git-dir=%s/.git ls-files --exclude-standard -co'],
    \ },
  \ 'fallback': s:search_command
  \ }

" == delimitMate == {{{2
let g:delimitMate_expand_cr = 1     " Create line break when pressing enter
let g:delimitMate_expand_space = 1  " Expand spaces inside delimiters
let g:delimitMate_autoclose = 1     " Auto-close brackets

" == NERDTree == {{{2
let g:NERDTreeChDirMode = 3                  " Change on root change
let g:nerdtree_tabs_open_on_gui_startup = 0  " Don't open NERDTree tabs

" == vim-signify == {{{2
let g:signify_vcs_list = [ 'git', 'hg' ]
let g:signify_sign_overwrite = 1

" == vim-slime == {{{2
let g:slime_target = "tmux"

" == UltiSnips == {{{2
let g:UltiSnipsSnippetDirectories=["UltiSnips", "MySnips"]

" == neocomplete == {{{2
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1

" == syntastic == {{{2
let g:syntastic_python_checkers = [ 'pyflakes' ]
let g:syntastic_haskell_checkers = [ 'ghc_mod', 'hlint' ]
let g:syntastic_cpp_compiler_options = '-std=c++0x'


" == pymode == {{{2
let g:pymode_rope = 0
let g:pymode_rope_completion = 0

" == markdown == {{{2
let g:vim_markdown_folding_disabled = 1

" == tmux-complete.vim == {{{2
let g:tmuxcomplete#trigger = ''

" == vimtex == {{{2
let g:vimtex_view_general_viewer = 'displayline'
let g:vimtex_view_general_options = '@line @pdf @tex'

" }}}1
" == Keymaps == {{{1

" == Tab manipulation == {{{2
nnoremap <silent> [W :tabfirst<CR>
nnoremap <silent> ]W :tabnext<CR>
nnoremap <silent> [w :tabprevious<CR>
nnoremap <silent> ]w :tabnext<CR>
nnoremap <silent> <Leader>wx :tabclose<CR>

" == Search shortcuts == {{{2
" Mute highlighting temporarily
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>
" Show number of matches made by a recent search
nnoremap <silent> <Leader>s :%s///gn<CR>

" == Paste toggle == {{{2
set pastetoggle=<F6>

" == Bindings to source vim files == {{{2
nnoremap <silent> <Leader>vv :source ~/.vimrc<CR>

" == Bindings to show panels == {{{2
nnoremap <silent> <Leader>n :NERDTreeTabsToggle<CR>
nnoremap <silent> <Leader>t :TagbarToggle<CR>

" == Bindings to enable/disable plugins == {{{2
nnoremap co. :DelimitMateSwitch<CR>

" == vim-signify bindings == {{{2
let g:signify_mapping_next_hunk = '<leader>cj'
let g:signify_mapping_prev_hunk = '<leader>ck'
let g:signify_mapping_toggle = '<leader>ct'
let g:signify_mapping_toggle_highlight = '<leader>ch'

" == fugitive == {{{2
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gr :Gread<CR>
nnoremap <leader>gm :Gmove<Space>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :Gdiff<CR>

" == UltiSnips == {{{2
let g:UltiSnipsExpandTrigger="<c-e>"
let g:UltiSnipsJumpForwardTrigger="<c-e>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" == Dash == {{{2
nmap <silent> <leader>d <Plug>DashSearch

" == Slimux == {{{2
nmap <C-c><C-c> :SlimuxREPLSendLine<CR>
vmap <C-c><C-c> :SlimuxREPLSendSelection<CR>

" }}}1
" == Modeline == {{{1
" vim: foldmethod=marker:fen
