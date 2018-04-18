# setup paths
if status --is-login
  source /usr/home/raichoo/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true
  set -x PATH /usr/home/raichoo/Local/bin /usr/home/raichoo/.local/bin $PATH
  #  set -x MANPATH /usr/home/raichoo/Local/man $MANPATH
  set -x PYTHONPATH /usr/home/raichoo/.local/lib/python3.6/site-packages /usr/home/raichoo/Local/python/lib/python2.7/site-packages /usr/home/raichoo/Local/z3/lib/python2.7/dist-packages $PYTHONPATH
  set -x __fish_bin_dir /usr/home/raichoo/Local/fish/bin
end

set -x EDITOR /usr/home/raichoo/Local/bin/nvim

set -x DARCS_ALWAYS_COLOR 1
set -x DARCS_ALTERNATIVE_COLOR 1
set -x DARCS_DO_COLOR_LINES 1

set -x MANPAGER "nvim -c 'set ft=man' -"
set -x BROWSER (which firefox)
set -x PAGER "less"
set -x LESS "-qR"
set -g fish_term24bit 1

set -g __fish_git_prompt_show_informative_status 1
set -g __fish_git_prompt_hide_untrackedfiles 1

set -g __fish_git_prompt_color_branch yellow
set -g __fish_git_prompt_showupstream "informative"
set -g __fish_git_prompt_char_upstream_ahead "↑"
set -g __fish_git_prompt_char_upstream_behind "↓"
set -g __fish_git_prompt_char_stagedstate "●"
set -g __fish_git_prompt_char_dirtystate "✚"
set -g __fish_git_prompt_char_untrackedfiles "…"
set -g __fish_git_prompt_char_conflictedstate "✖"
set -g __fish_git_prompt_char_cleanstate "✔"

set -g __fish_git_prompt_color_dirtystate EF5939
set -g __fish_git_prompt_color_stagedstate E6DB74
set -g __fish_git_prompt_color_invalidstate EF5939
set -g __fish_git_prompt_color_untrackedfiles EF5939
set -g __fish_git_prompt_color_cleanstate B8E673
set -g __fish_git_prompt_color_branch E6DB74

set -x LSCOLORS "gxfxcxdxbxegedabagacad"

alias vi='nvim'
alias vim='nvim'

function fish_greeting
end

function pwd_prompt
  set_color 66D9EF
  if test $PWD = "/usr$HOME"
    echo -n "~"
  else
    echo -n (basename $PWD)
  end
  set_color normal
end

function mode_prompt
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
    case replace_one
      set_color --bold EF5939
      echo -n "REPLACE"
  end
  set_color normal
  echo -n "⟧"
end

function time_prompt
  if test $status = 0
    printf "%s" (set_color --bold B8E673)
  else
    printf "\a%s" (set_color --bold EF5939)
  end
  printf "%s%s" (date +"%T") (set_color normal)
end

function user_prompt
  printf "%s%s%s" (set_color --bold FD971F) (whoami) (set_color normal)
end

function host_prompt
  printf "%s%s%s" (set_color --bold F92672) (hostname) (set_color normal)
end

function fish_prompt
  printf ":%s@%s:%s%s» " (user_prompt) (host_prompt) (pwd_prompt) (__fish_git_prompt "(%s)")
end

function battery_prompt
  set -l power (sysctl -n hw.acpi.battery.life)

  if test $power -gt 50
    return
  end

  echo -n "("
  if test $power -le 10
    echo -n (set_color --bold EF5939)
  else if test $power -le 30
    echo -n (set_color --bold FD971F)
  else if test $power -le 50
    echo -n (set_color --bold E6DB74)
  end
  printf "%d%%%s)" $col $power (set_color normal)
end

function fish_right_prompt
  printf "⟦%s%s⟧" (time_prompt) (battery_prompt)
end

function fish_mode_prompt
  mode_prompt
end

function fish_user_key_bindings
  fish_vi_key_bindings
  bind -e -M insert \n
  bind -e -M insert \r
  bind -e -M default \n
  bind -e -M default \r
  bind -e -M default u
  bind -e -M default \cr
  bind -e -M insert \cV
  bind -M insert \n force-repaint execute
  bind -M insert \r force-repaint execute
  bind -M insert \ef forward-word
  bind -M insert \eb backward-word
  bind -M insert \cF end-of-line
  bind -M insert \cB beginning-of-line
  bind -M default w forward-word
  bind -M insert  \cN history-search-backward
  bind -M insert  \cP history-search-forward
  bind -M default --sets-mode insert \n force-repaint execute
  bind -M default --sets-mode insert \r force-repaint execute
end

set -g fish_color_redirection F92672 --bold
set -g fish_color_operator F92672 --bold
set -g fish_color_match FD971F
set -g fish_color_param B8E673
set -g fish_color_command FFFFFF
set -g fish_color_quote E6DB74
set -g fish_color_comment 7E8E91
set -g fish_color_autosuggestion 66D9EF --underline
set -g fish_color_error EF5939
set -g fish_color_end 8F8F8F
set -g fish_color_valid_path --bold
