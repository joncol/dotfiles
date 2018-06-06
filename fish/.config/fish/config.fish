# set -x PATH ~/.local/bin ~/.stack/programs/x86_64-linux/ghc-8.0.1/bin $PATH
set -x PATH ~/.local/bin $PATH ~/.rbenv/shims ~/ledgers/bin
set -x PATH $PATH /usr/local/racket/bin
set -x PATH $PATH ~/.cargo/bin
set -x PATH $PATH /opt/visualvm_14/bin
set -x PATH $PATH /opt/gradle-4.6/bin
# set -x PATH $PATH /opt/arduino-1.8.5
set -x PATH $PATH /home/jco/eclipse/java-oxygen/eclipse
set -x EDITOR vim
set -x LESSOPEN "| /usr/bin/source-highlight-esc.sh %s 2>/dev/null"
set -x LESS " -R"
set -x QTDIR ~/Qt/5.10.0/gcc_64
set -x LEDGER_FILE ~/ledgers/2017.journal
set -x LUA_PATH ~/.luarocks/share/lua/5.1/\?.lua

eval (python -m virtualfish)

set -x PROXY_SSH_AUTH_SOCK $SSH_AUTH_SOCK

if status --is-interactive
  keychain --eval --quiet -Q id_rsa | source
end

eval (direnv hook fish)
thefuck --alias | source
