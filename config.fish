# OPAM configuration
#. /home/raichoo/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true

# setup paths
if test "$NVIM_LISTEN_ADDRESS" = ""
  set PATH /home/raichoo/Local/bin /home/raichoo/.local/bin $PATH
  set MANPATH /home/raichoo/Local/man /usr/share/man $MANPATH
  set PYTHONPATH /home/raichoo/.local/lib/python3.5/site-packages /home/raichoo/Local/python/lib/python2.7/site-packages /home/raichoo/Local/z3/lib/python2.7/dist-packages $PYTHONPATH
end

set -x __fish_bin_dir /home/raichoo/Local/fish/bin
set -x BROWSER /usr/bin/firefox
set -x PAGER "less -R"
set -x DARCS_DO_COLOR_LINES 1
set -x DARCS_ALWAYS_COLOR 1
set -x JAVA_HOME /usr
set -x TERM xterm-256color
set -x EDITOR /home/raichoo/Local/bin/nvim
set -x fish_key_bindings fish_vi_key_bindings
set -g fish_term24bit 1

alias vi=nvim
alias vim=nvim
alias vimdiff="nvim -d"

function fish_greeting
end

function pwd_prompt
  if test $PWD = $HOME
    echo "~"
  else
    echo (basename $PWD)
  end
end

function current_mode
  echo -n "⟦"
  switch $fish_bind_mode
    case default
      set_color --bold E6DB74
      echo -n "NORMAL"
    case insert
      set_color --bold 66D9EF
      echo -n "INSERT"
    case visual
      set_color --bold FD971F
      echo -n "VISUAL"
  end
  set_color normal
  echo -n "⟧"
end

function fish_prompt
  printf "%s%s%s@%s%s%s:%s%s%s%s» " (set_color --bold FD971F) (whoami) (set_color normal) (set_color --bold F92672) (hostname) (set_color normal) (set_color 66D9EF) (pwd_prompt) (set_color normal) (__terlar_git_prompt)
end

function fish_right_prompt
  echo -n "⟦"
  set_color --bold A6E22E
  date +"%T"
  set_color normal
  echo -n "⟧"
end

function fish_mode_prompt
  current_mode
  echo -n ":"
end

function fish_user_key_bindings
  fish_vi_key_bindings
end
