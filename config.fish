# OPAM configuration
#. /home/raichoo/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true

# setup paths
if status --is-login
  set -x PATH /home/raichoo/Local/bin /home/raichoo/.local/bin $PATH
  set -x MANPATH /home/raichoo/Local/man /usr/share/man $MANPATH
  set -x PYTHONPATH /home/raichoo/.local/lib/python3.6/site-packages /home/raichoo/Local/python/lib/python2.7/site-packages /home/raichoo/Local/z3/lib/python2.7/dist-packages $PYTHONPATH
  set -x __fish_bin_dir /home/raichoo/Local/fish/bin
end

if test "$STACK_PROJECT_ROOT" != ""; and test "$STACK_PROJECT_ROOT" != $PWD; and test -e "stack.yaml"
 set -x GHC_PACKAGE_PATH (stack exec printenv GHC_PACKAGE_PATH)
 stack exec printenv PATH | tr ':' ' ' | read -x -a PATH
end

set -x MANPAGER "nvim -c 'set ft=man' -"
set -x BROWSER /usr/bin/firefox
set -x PAGER "less"
set -x LESS "-qR"
set -x DARCS_DO_COLOR_LINES 1
set -x DARCS_ALWAYS_COLOR 1
set -x JAVA_HOME /usr
set -x TERM gnome-256color
set -x EDITOR /home/raichoo/Local/bin/nvim
set -g fish_term24bit 1

set -g __fish_git_prompt_show_informative_status 1
set -g __fish_git_prompt_hide_untrackedfiles 1

set -g __fish_git_prompt_color_branch yellow
set -g __fish_git_prompt_showupstream "informative"
set -g __fish_git_prompt_char_upstream_ahead "↑"
set -g __fish_git_prompt_char_upstream_behind "↓"
set -g __fish_git_prompt_char_upstream_prefix ""

set -g __fish_git_prompt_char_stagedstate "●"
set -g __fish_git_prompt_char_dirtystate "✚"
set -g __fish_git_prompt_char_untrackedfiles "…"
set -g __fish_git_prompt_char_conflictedstate "✖"
set -g __fish_git_prompt_char_cleanstate "✔"

set -g __fish_git_prompt_color_dirtystate red
set -g __fish_git_prompt_color_stagedstate yellow
set -g __fish_git_prompt_color_invalidstate red
set -g __fish_git_prompt_color_untrackedfiles red
set -g __fish_git_prompt_color_cleanstate green

set -x LS_COLORS (cat ~/.dircolors)

alias vi=nvim
alias vim=nvim

function fish_greeting
end

function pwd_prompt
  set_color 66D9EF
  if test $PWD = $HOME
    echo -n "~"
  else
    echo -n (basename $PWD)
  end
  set_color normal
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
    case replace-one
      set_color --bold EF5939
      echo -n "REPLACE"
  end
  set_color normal
  echo -n "⟧"
end

function time_prompt
  set -l _status $status
  echo -n "⟦"
  if test $_status = 0
    set_color --bold B8E673
  else
    set_color --bold EF5939
  end
  echo -n (date +"%T")
  set_color normal
  echo -n "⟧:"
end

function user_prompt
  printf "%s%s%s" (set_color --bold FD971F) (whoami) (set_color normal)
end

function host_prompt
  printf "%s%s%s" (set_color --bold F92672) (hostname) (set_color normal)
end

function fish_prompt
  printf "%s@%s:%s» " (user_prompt) (host_prompt) (pwd_prompt)
end

function fish_right_prompt
  current_mode
end

function fish_mode_prompt
  time_prompt
end

fish_vi_key_bindings

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
