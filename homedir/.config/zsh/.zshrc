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
alias cu="yay -Syy ;and yay -Pu"
alias mo=mimeopen
alias u="yay -Syu --noconfirm"
alias t="todoist"

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

source <(antibody init)
antibody bundle < ~/.config/zsh/zsh_plugins.txt

export FZF_DEFAULT_OPTS='--bind ctrl-f:page-down,ctrl-b:page-up --color fg:124,hl:202,fg+:214,bg+:52,hl+:231 --color info:52,prompt:196,spinner:208,pointer:196,marker:208'

export fish_pager_color_prefix='444'
export fish_color_search_match=--background='eee'

export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview' --bind 'ctrl-y:execute-silent(echo -n {2..} | xsel -b)+abort' --header 'Press CTRL-Y to copy command into clipboard'"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
