set -x PATH ~/.local/bin $PATH
set -x EDITOR vim
set -x LESS " -R"

eval (python -m virtualfish)

set -x PROXY_SSH_AUTH_SOCK $SSH_AUTH_SOCK

if status --is-interactive
  keychain --eval --quiet -Q id_rsa | source
end

eval (direnv hook fish)
thefuck --alias | source

set -x FZF_DEFAULT_OPTS '--bind ctrl-f:page-down,ctrl-b:page-up
--color fg:124,hl:202,fg+:214,bg+:52,hl+:231
--color info:52,prompt:196,spinner:208,pointer:196,marker:208
'

set -x fish_pager_color_prefix '444'
set -x fish_color_search_match --background='eee'
