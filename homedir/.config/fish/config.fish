set -x PATH ~/.local/bin $PATH

if test -d ~/Android
    set -x ANDROID_HOME ~/Android/Sdk
    set -x PATH $PATH $ANDROID_HOME/tools $ANDROID_HOME/platform-tools
end

if test -d /usr/share/logstash/bin
    set -x PATH $PATH /usr/share/logstash/bin
end

set -x EDITOR vim
set -x LESS "-NRi"

eval (python -m virtualfish)

set -x PROXY_SSH_AUTH_SOCK $SSH_AUTH_SOCK

if status --is-interactive
    keychain --eval --quiet -Q id_rsa | source
end

eval (direnv hook fish)
thefuck --alias | source

set -x FZF_DEFAULT_OPTS '--bind ctrl-f:page-down,ctrl-b:page-up
--color fg:124,hl:202,fg+:214,bg+:52,hl+:231
--color info:52,prompt:196,spinner:208,pointer:196,marker:208'

set -x fish_pager_color_prefix '444'
set -x fish_color_search_match --background='eee'

# bobthefish theme settings
set -g theme_display_date no
set -g theme_display_cmd_duration no

# if type -q rbenv
#     status --is-interactive; and source (rbenv init -|psub)
#     set PATH $HOME/.rbenv/bin $PATH
#     set PATH $HOME/.rbenv/shims $PATH
#     rbenv rehash >/dev/null ^&1
# end
