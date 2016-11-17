# OPAM configuration
#. /home/raichoo/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true

# setup paths

if test "$NVIM_LISTEN_ADDRESS" = ""
  set PATH /home/raichoo/Local/bin /home/raichoo/.local/bin $PATH
  set MANPATH /home/raichoo/Local/man /usr/share/man $MANPATH
  set PYTHONPATH /home/raichoo/.local/lib/python3.5/site-packages /home/raichoo/Local/python/lib/python2.7/site-packages /home/raichoo/Local/z3/lib/python2.7/dist-packages $PYTHONPATH
  set -x MANPAGER "nvim -c 'set ft=man' -"
end

set -x __fish_bin_dir /home/raichoo/Local/fish/bin
set -x BROWSER /usr/bin/firefox
set -x PAGER "less -R"
set -x DARCS_DO_COLOR_LINES 1
set -x DARCS_ALWAYS_COLOR 1
set -x JAVA_HOME /usr
set -x TERM gnome-256color
set -x EDITOR /home/raichoo/Local/bin/nvim
set -g fish_term24bit 1

function start_vi
  if test -e .stack-work
    stack exec /home/raichoo/Local/bin/nvim -- $argv
  else
    /home/raichoo/Local/bin/nvim $argv
  end
end

function rebuild_haskell_tags
  if test -e .stack-work
    hasktags --ignore-close-implementation --ctags .; sort tags
  end
end

alias vi=start_vi
alias vim=start_vi

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
  printf "%s%s%s@%s%s%s:%s%s%s%s» " (set_color --bold FD971F) (whoami) (set_color normal) (set_color --bold F92672) (hostname) (set_color normal) (set_color 66D9EF) (pwd_prompt) (set_color normal) (__fix_terlar_git_prompt)
end

function fish_right_prompt
  set -l _status $status
  echo -n "⟦"
  if test $_status = 0
    set_color --bold A6E22E
  else
    set_color --bold EF5939
  end
  date +"%T"
  set_color normal
  echo -n "⟧"
end

function fish_mode_prompt
  current_mode
  echo -n ":"
end

function fish_user_key_bindings
  bind -e -M insert \n
  bind -e -M insert \r
  bind -e -M default \n
  bind -e -M default \r
  bind -M insert \n force-repaint execute
  bind -M insert \r force-repaint execute
  bind -M default --sets-mode insert \n force-repaint execute
  bind -M default --sets-mode insert \r force-repaint execute
end
