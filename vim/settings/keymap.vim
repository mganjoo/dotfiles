" Keymaps.

" == Tab manipulation == {{{1
nnoremap <silent> [W :tabfirst<CR>
nnoremap <silent> ]W :tabnext<CR>
nnoremap <silent> [w :tabprevious<CR>
nnoremap <silent> ]w :tabnext<CR>
nnoremap <silent> <Leader>wx :tabclose<CR>

" == Search shortcuts == {{{1
" Mute highlighting temporarily
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>
" Show number of matches made by a recent search
nnoremap <silent> <Leader>s :%s///gn<CR>

" == Paste toggle == {{{1
set pastetoggle=<F6>

" == Bindings to source vim files == {{{1
nnoremap <silent> <Leader>vv :source ~/.vimrc<CR>
nnoremap <silent> <Leader>va :source ~/.vim/settings/theme.vim<CR>

" == Bindings to show panels == {{{1
nnoremap <silent> <Leader>u :GundoToggle<CR>
nnoremap <silent> <Leader>n :NERDTreeTabsToggle<CR>
nnoremap <silent> <Leader>t :TagbarToggle<CR>

" == Bindings to enable/disable plugins == {{{1
nnoremap co. :DelimitMateSwitch<CR>

" == vim-signify bindings == {{{1
let g:signify_mapping_next_hunk = '<leader>cj'
let g:signify_mapping_prev_hunk = '<leader>ck'
let g:signify_mapping_toggle = '<leader>ct'
let g:signify_mapping_toggle_highlight = '<leader>ch'

" == fugitive == {{{1
nnoremap <leader>gw :Gwrite<CR>
nnoremap <leader>gr :Gread<CR>
nnoremap <leader>gm :Gmove<Space>
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gd :Gdiff<CR>

" == UltiSnips == {{{1
let g:UltiSnipsExpandTrigger="<c-e>"
let g:UltiSnipsJumpForwardTrigger="<c-e>"
let g:UltiSnipsJumpBackwardTrigger="<c-k>"

" == Modeline == {{{1
" vim: foldmethod=marker
