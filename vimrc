" Code Search
" ,cw " Code Search for word under cursor
" ,cf " Open current file in Code Search

" You Complete Me
" ,yg " Go To
" ,yd " Go To Declaration
" ,yf " Go To Definition
" ,yt " Get Type
" ,yp " Get Parent

"Grok
" ,gr " Go To Reference
" ,gd " Go To Definition

" Side Windows
" ,tt " taglist outline
" ,tr " NerdTree Current File
" ,te " NerdTree Toggle

" Commenting
" ,cc " Comment
" ,cu " Uncomment

" Ctrl-P Fuzzy Finder
" <C-p> " open fuzzy file finder
" ,p paste from register

set nocompatible              " be iMproved, required
let mapleader = ','
filetype plugin indent on

func! VimrcCallback()
endfunc

if filereadable(expand('~/.at_work'))
  source ~/.vimrc_work
else
  source ~/.vimrc_home
endif

"colorscheme elflord

set clipboard=unnamedplus
set nu  " show line numbers
set so=5  " scroll offset
set ignorecase  " make search case insensitive
filetype off                  " required

call plug#begin('~/vim.plugged')

Plug 'gmarik/Vundle.vim'

Plug 'scrooloose/nerdtree'

Plug 'scrooloose/nerdcommenter'

Plug 'scrooloose/syntastic'

Plug 'kien/ctrlp.vim'

Plug 'bling/vim-airline'

Plug 'gcmt/taboo.vim'

Plug 'SirVer/ultisnips'

Plug 'honza/vim-snippets'

Plug 'tpope/vim-dispatch'

Plug 'majutsushi/tagbar'

Plug 'altercation/vim-colors-solarized'

Plug 'morhetz/gruvbox'

Plug 'tpope/vim-obsession'

Plug 'tpope/vim-fugitive'

Plug 'lrvick/Conque-Shell'

Plug 'christoomey/vim-tmux-runner'

Plug 'christoomey/vim-tmux-navigator'

Plug 'mileszs/ack.vim'

Plug 'starcraftman/vim-eclim'

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'vhakulinen/neovim-intellij-complete-deoplete'
endif


call plug#end()            " required
filetype plugin indent on    " required


syntax on
set noruler
set laststatus=2

if has('nvim')
  let g:deoplete#enable_at_startup = 1
  let g:deoplete#auto_complete_start_length = 1
  inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"
  "inoremap <silent><expr> <Tab>
  "\ pumvisible() ? "\<C-n>" :
  "\ deoplete#mappings#manual_complete()
else
  " YouCompleteMe remappings
  nnoremap <leader>yg :YcmCompleter GoTo<CR>
  nnoremap <leader>yd :YcmCompleter GoToDeclaration<CR>
  nnoremap <leader>yf :YcmCompleter GoToDefinition<CR>
  nnoremap <leader>yt :YcmCompleter GetType<CR>
  nnoremap <leader>yp :YcmCompleter GetParent<CR>
  let g:EclimCompletionMethod = 'omnifunc'
  let g:ycm_auto_trigger = 1
  let g:ycm_add_preview_to_completeopt = 2
  "let g:ycm_filetype_whitelist = {'cpp' :1, 'cc' : 1}
endif

" Taglist remappings

" Tagbar settings
autocmd VimEnter * nested :call tagbar#autoopen(1)
let g:tagbar_autoclose = 0
let g:tagbar_left = 1
let g:tagbar_autofocus = 1
nnoremap <leader>tt :TagbarOpen j<CR>
nnoremap <leader>tb :TagbarToggle<CR>


let g:tagbar_type_sh = {
    \ 'kinds' : [
        \ 'f:functions',
        \ 'a:aliases',
        \ 'v:variables:1',
    \ ],
\ }

let g:tagbar_type_gcl = {
    \ 'ctagstype': 'gcl',
    \ 'kinds' : [
        \ 'r:rollouts',
        \ 'p:params',
        \ 'c:conditions',
        \ 'l:local condition',
    \ ],
\ }

" NERDTree remappings
nnoremap <leader>tr :NERDTreeFind<CR>
nnoremap <leader>te :NERDTreeToggle<CR>

" paste remapping
nnoremap <leader>p "*p

" nnoremap <leader>oo <leader>fh :vsp<CR> <leader>fc :vsp<CR> <leader>ft
" nnoremap <leader>ii :vsp<CR> :vsp<CR> <leader>ft


" Taboo commands
nnoremap <leader>rr :TabooRename
nnoremap <leader>rt :TabooOpen

noremap <leader>so :so $MYVIMRC <CR>

" filetype plugin indent o
hi StatusLine ctermbg=3 ctermfg=4
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 0

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

nnoremap <leader>en :lnext<CR>
nnoremap <leader>ep :lprevious<CR>

" Fugitive shortcuts
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gh :diffget //2<CR>
nnoremap <leader>gl :diffget //3<CR>
nnoremap <leader>gg :diffget <CR>
nnoremap <leader>gc :Gcommit <CR>

" eclim shortcuts
nnoremap <leader>ji :JavaImport<cr>
nnoremap <leader>jd :JavaDocSearch -x declarations<cr>
nnoremap <leader>js :JavaSearchContext<cr>
nnoremap <leader>jr :JavaRename<space>
nnoremap <leader>jt :JavaRename <c-r>=expand("<cword>")<cr>
nnoremap <leader>jc :JavaCorrect<cr>

" Bind ag to ack.vim
let g:ackprg = 'ag --nogroup --nocolor --column'

" CtrlP remaps
nnoremap <c-l> :CtrlPBuffer<cr>
let g:ctrlp_max_files = 20000
let g:ctrlp_max_depth = 10
"let g:ctrlp_user_command = 'ag %s -i --nocolor --nogroup --hidden
      "\ --ignore .git
      "\ --ignore .svn
      "\ --ignore .hg
      "\ --ignore .DS_Store
      "\ --ignore "**/*.pyc"
      "\ --ignore .git5_specs
      "\ --ignore review
      "\ -g ""'
"let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }

" ag remaps
nnoremap <leader>aa :Ag <c-r>=expand("<cword>")<cr><cr>

" remap ultisnips
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"

imap jk <Esc>

"Conque settings
let g:ConqueTerm_ReadUnfocused = 1

nnoremap <leader><leader>o :Files<cr>
nnoremap <leader><leader>t :BTags<cr>
nnoremap <leader><leader>l :Lines<cr>

syntax enable
if !has('nvim')
  set term=screen-256color
  set t_Co=256
endif
set background=dark
"let g:gruvbox_italic=1
let g:gruvbox_contrast_dark='hard'
colorscheme gruvbox
"colorscheme solarized

call VimrcCallback()
