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

if filereadable(expand('~/.at_work'))
  source ~/.vimrc_work
else
  source ~/.vimrc_home
endif

colorscheme elflord

set nu  " show line numbers
set so=5  " scroll offset
set ignorecase  " make search case insensitive
filetype off                  " required


set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'scrooloose/nerdtree'

Plugin 'scrooloose/nerdcommenter'

Plugin 'scrooloose/syntastic'

Plugin 'kien/ctrlp.vim'

Plugin 'bling/vim-airline'

Plugin 'gcmt/taboo.vim'

Plugin 'SirVer/ultisnips'

Plugin 'honza/vim-snippets'

Plugin 'tpope/vim-dispatch'

Plugin 'majutsushi/tagbar'

call vundle#end()            " required
filetype plugin indent on    " required

syntax on
set noruler
set laststatus=2

" YouCompleteMe remappings
nnoremap <leader>yg :YcmCompleter GoTo<CR>
nnoremap <leader>yd :YcmCompleter GoToDeclaration<CR>
nnoremap <leader>yf :YcmCompleter GoToDefinition<CR>
nnoremap <leader>yt :YcmCompleter GetType<CR>
nnoremap <leader>yp :YcmCompleter GetParent<CR>
let g:EclimCompletionMethod = 'omnifunc'
let g:ycm_auto_trigger = 1
let g:ycm_add_preview_to_completeopt = 2

" Taglist remappings

" Tagbar settings
autocmd VimEnter * nested :call tagbar#autoopen(1)
let g:tagbar_autoclose = 0
let g:tagbar_left = 1
let g:tagbar_autofocus = 1
nnoremap <leader>tt :TagbarOpen j<CR>
nnoremap <leader>tb :TagbarToggle<CR>

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

" eclim shortcuts
nnoremap <leader>ji :JavaImport<cr>
nnoremap <leader>jd :JavaDocSearch -x declarations<cr>
nnoremap <leader>js :JavaSearchContext<cr>
nnoremap <leader>jr :JavaRename<space>
nnoremap <leader>jt :JavaRename <c-r>=expand("<cword>")<cr>
nnoremap <leader>jc :JavaCorrect<cr>

" remap ultisnips
let g:UltiSnipsExpandTrigger = "<c-j>"
let g:UltiSnipsJumpForwardTrigger = "<c-j>"
let g:UltiSnipsJumpBackwardTrigger = "<c-k>"

imap jk <Esc>
