parse_git_branch() {
     git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}

setopt PROMPT_SUBST

PS1=$'%F{green}%n%F{13} %1~'' $(parse_git_branch)'" %F{green}->%f "


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

[ -f ~/.bash_files/bash_aliases ] &&   source ~/.bash_files/bash_aliases
[ -f ~/.bash_files/bash_functions ] && source ~/.bash_files/bash_functions


eval "$(direnv hook zsh)"

export LC_CTYPE=C
export LANG=C

# Created by `pipx` on 2024-12-23 22:21:08
export PATH="$PATH:/Users/matt.goldfarb/.local/bin"

#OktaAWSCLI
if [[ -f "$HOME/.okta/bash_functions" ]]; then
    . "$HOME/.okta/bash_functions"
fi
if [[ -d "$HOME/.okta/bin" && ":$PATH:" != *":$HOME/.okta/bin:"* ]]; then
    PATH="$HOME/.okta/bin:$PATH"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
