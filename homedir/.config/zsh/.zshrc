# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if [[ -f "$HOME/.config/zsh/functions" ]]; then
    source "$HOME/.config/zsh/functions"
fi

zstyle ':completion:*' menu select
zmodload zsh/complist

fpath+=~/.config/zsh/completions
# The styles for completion descriptions and messages, zsh will hide them by default.
zstyle ':completion:*:descriptions' format "%U%B%d%b%u"
zstyle ':completion:*:messages' format "%F{green}%d%f"
autoload -Uz compinit && compinit -u
autoload -U +X bashcompinit && bashcompinit

if [[ -f ~/work/arthur/arthur ]]; then
    alias arthur=~/work/arthur/arthur
    if command -v arthur &> /dev/null; then
        source <(arthur completion)
    fi
fi

# use the vi navigation keys in menu completion
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

if [[ -f /opt/shell-color-scripts/colorscript.sh ]]; then
    alias colorscript=/opt/shell-color-scripts/colorscript.sh
fi

if [[ -f /usr/share/zsh/share/antigen.zsh ]]; then
    source /usr/share/zsh/share/antigen.zsh
elif [[ -f /usr/share/zsh-antigen/antigen.zsh ]]; then
    source /usr/share/zsh-antigen/antigen.zsh
fi

antigen init ~/.antigenrc

alias sudo="sudo "
alias git=hub
alias a="gcalcli agenda now tomorrow --details=description --details=conference"
alias aw="gcalcli agenda --details=description"
alias cu="yay -Syy && yay -Qu"
alias mo=mimeopen
alias u="yay -Syu --noconfirm"
alias t=todoist
alias vim=nvim

if [ -d "$HOME/repos/mitmproxy" ]; then
    alias mitmproxy="cd $HOME/repos/mitmproxy/ && pipenv run mitmproxy && cd -"
    alias mitmweb="cd $HOME/repos/mitmproxy/ && pipenv run mitmweb && cd -"
    alias mitmdump="cd $HOME/repos/mitmproxy/ && pipenv run mitmdump && cd -"
fi

if command -v exa &> /dev/null; then
    tree_ignore="cache|log|logs|node_modules|vendor"
    alias ls='exa --group-directories-first --icons'
    alias la='ls -a'
    alias ll='ls --git -l'
    alias lt='ls --tree -D -L 2 -I ${tree_ignore}'
    alias ltt='ls --tree -D -L 3 -I ${tree_ignore}'
    alias lttt='ls --tree -D -L 4 -I ${tree_ignore}'
    alias ltttt='ls --tree -D -L 5 -I ${tree_ignore}'
fi

alias urlencode='python3 -c "import sys, urllib.parse as ul; \
    print (ul.quote_plus(sys.argv[1]))"'

alias urldecode='python3 -c "import sys, urllib.parse as ul; \
    print(ul.unquote_plus(sys.argv[1]))"'

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
# Vi style:
# zle -N edit-command-line
# bindkey -M vicmd v edit-command-line

eval "$(direnv hook $SHELL)"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"

if alias run-help > /dev/null 2>&1; then
    unalias run-help
    autoload run-help
fi
