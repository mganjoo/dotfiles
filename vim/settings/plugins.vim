" Settings for various plugins.

" == vim-airline == {{{1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#formatter = "unique_tail"
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#branch#displayed_head_limit = 20
let g:airline#extensions#branch#format = 1

" == ctrlp.vim == {{{1
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

" == delimitMate == {{{1
let g:delimitMate_expand_cr = 1     " Create line break when pressing enter
let g:delimitMate_expand_space = 1  " Expand spaces inside delimiters
let g:delimitMate_autoclose = 1     " Auto-close brackets

" == NERDTree == {{{1
let g:NERDTreeChDirMode = 3                  " Change on root change
let g:nerdtree_tabs_open_on_gui_startup = 0  " Don't open NERDTree tabs

" == vim-signify == {{{1
let g:signify_vcs_list = [ 'git', 'hg' ]
let g:signify_sign_overwrite = 1

" == vim-slime == {{{1
let g:slime_target = "tmux"

" == UltiSnips == {{{1
let g:UltiSnipsSnippetDirectories=["UltiSnips", "MySnips"]

" == neocomplete == {{{1
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1

" == syntastic == {{{1
let g:syntastic_python_checkers = [ 'pyflakes' ]
let g:syntastic_haskell_checkers = [ 'ghc_mod', 'hlint' ]
let g:syntastic_cpp_compiler_options = '-std=c++0x'


" == pymode == {{{1
let g:pymode_rope_completion = 0

" == markdown == {{{1
let g:vim_markdown_folding_disabled = 1

" == tmux-complete.vim == {{{1
let g:tmuxcomplete#trigger = ''

" == vimtex == {{{1
let g:vimtex_view_general_viewer = 'displayline'
let g:vimtex_view_general_options = '@line @pdf @tex'
"
" == Modeline == {{{1
" vim: fdm=marker:fen
