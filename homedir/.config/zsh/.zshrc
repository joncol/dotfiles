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

# use the vi navigation keys in menu completion
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

alias git=hub
alias a="gcalcli agenda now tomorrow"
alias cu="yay -Syy && yay -Pu"
alias mo=mimeopen
alias u="yay -Syu --noconfirm"
alias t="todoist"

export PATH="$PATH:$HOME/.cargo/bin:$HOME/.local/bin"
export PATH="$PATH:$HOME/.rvm/bin"

# n/npm paths
export N_PREFIX=$HOME/.n
if [[ -d "$N_PREFIX/bin" ]]; then
    export PATH=$N_PREFIX/bin:$PATH
fi

npm_bin=$(npm bin 2>/dev/null)
if [[ -d ${npm_bin} ]]; then
    export PATH=${npm_bin}:$PATH
fi

npm_global_bin=$(npm bin --global 2>/dev/null)
if [[ -d ${npm_global_bin} ]]; then
    export PATH=${npm_global_bin}:$PATH
fi

if [[ -f /usr/share/zsh/share/antigen.zsh ]]; then
    source /usr/share/zsh/share/antigen.zsh
elif [[ -f /usr/share/zsh-antigen/antigen.zsh ]]; then
    source /usr/share/zsh-antigen/antigen.zsh
fi

antigen init ~/.antigenrc

export FZF_DEFAULT_OPTS='--bind ctrl-f:page-down,ctrl-b:page-up --color fg:124,hl:202,fg+:214,bg+:52,hl+:231 --color info:52,prompt:196,spinner:208,pointer:196,marker:208'
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview' --bind 'ctrl-y:execute-silent(echo -n {2..} | xsel -b)+abort' --header 'Press CTRL-Y to copy command into clipboard'"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export EDITOR=vim
# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line
# Vi style:
# zle -N edit-command-line
# bindkey -M vicmd v edit-command-line

export LC_ALL=C # To get rid of `xmessage` warning

eval "$(direnv hook $SHELL)"

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"
