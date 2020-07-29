ZDOTDIR=~/.config/zsh

export EDITOR=nvim

export LC_ALL=C # To get rid of `xmessage` warning

export LESS="-FRX $LESS"
export PATH="$PATH:$HOME/.cargo/bin:$HOME/.local/bin"

export FZF_DEFAULT_OPTS='--bind ctrl-f:page-down,ctrl-b:page-up --color fg:124,hl:202,fg+:214,bg+:52,hl+:231 --color info:52,prompt:196,spinner:208,pointer:196,marker:208'
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview' --bind 'ctrl-y:execute-silent(echo -n {2..} | xsel -b)+abort' --header 'Press CTRL-Y to copy command into clipboard'"

export ANSIBLE_NOCOWS=1

if [[ -f /etc/profile.d/apps-bin-path.sh ]]; then
    source /etc/profile.d/apps-bin-path.sh
fi
