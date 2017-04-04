# OPAM configuration
#. /home/raichoo/.opam/opam-init/init.fish > /dev/null 2> /dev/null or true

# setup paths
if status --is-login
  set -x PATH /home/raichoo/Local/bin /home/raichoo/.local/bin $PATH
  set -x MANPATH /home/raichoo/Local/man /usr/share/man $MANPATH
  set -x PYTHONPATH /home/raichoo/.local/lib/python3.6/site-packages /home/raichoo/Local/python/lib/python2.7/site-packages /home/raichoo/Local/z3/lib/python2.7/dist-packages $PYTHONPATH
  set -x __fish_bin_dir /home/raichoo/Local/fish/bin
end

if test "$STACK_PROJECT_ROOT" != $PWD; and test -e "stack.yaml"
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
function print_color
  printf "\x1b[38;2;%d;%d;%dm%d %d %d\x1b[0m\n" $argv[1] $argv[2] $argv[3] $argv[1] $argv[2] $argv[3]
end

set -x LS_COLORS "no=0:rs=0:di=01;38;2;114;159;207:ln=00;38;5;37:mh=00:pi=48;5;230;38;5;136;01:so=48;5;230;38;5;136;01:do=48;5;230;38;5;136;01:bd=48;5;230;38;5;244;01:cd=48;5;230;38;5;244;01:or=48;5;235;38;5;160:su=48;5;160;38;5;230:sg=48;5;136;38;5;230:ca=30;41:tw=48;5;64;38;5;230:ow=48;5;235;38;5;33:st=48;5;33;38;5;230:ex=00;38;5;64:*.tar=00;38;5;61:*.tgz=00;38;5;61:*.arj=00;38;5;61:*.taz=00;38;5;61:*.lzh=00;38;5;61:*.lzma=00;38;5;61:*.tlz=00;38;5;61:*.txz=00;38;5;61:*.zip=00;38;5;61:*.z=00;38;5;61:*.Z=00;38;5;61:*.dz=00;38;5;61:*.gz=00;38;5;61:*.lz=00;38;5;61:*.xz=00;38;5;61:*.bz2=00;38;5;61:*.bz=00;38;5;61:*.tbz=00;38;5;61:*.tbz2=00;38;5;61:*.tz=00;38;5;61:*.deb=00;38;5;61:*.rpm=00;38;5;61:*.jar=00;38;5;61:*.rar=00;38;5;61:*.ace=00;38;5;61:*.zoo=00;38;5;61:*.cpio=00;38;5;61:*.7z=00;38;5;61:*.rz=00;38;5;61:*.apk=00;38;5;61:*.gem=00;38;5;61:*.jpg=00;38;5;136:*.JPG=00;38;5;136:*.jpeg=00;38;5;136:*.gif=00;38;5;136:*.bmp=00;38;5;136:*.pbm=00;38;5;136:*.pgm=00;38;5;136:*.ppm=00;38;5;136:*.tga=00;38;5;136:*.xbm=00;38;5;136:*.xpm=00;38;5;136:*.tif=00;38;5;136:*.tiff=00;38;5;136:*.png=00;38;5;136:*.PNG=00;38;5;136:*.svg=00;38;5;136:*.svgz=00;38;5;136:*.mng=00;38;5;136:*.pcx=00;38;5;136:*.dl=00;38;5;136:*.xcf=00;38;5;136:*.xwd=00;38;5;136:*.yuv=00;38;5;136:*.cgm=00;38;5;136:*.emf=00;38;5;136:*.eps=00;38;5;136:*.CR2=00;38;5;136:*.ico=00;38;5;136:*.tex=00;38;5;245:*.rdf=00;38;5;245:*.owl=00;38;5;245:*.n3=00;38;5;245:*.ttl=00;38;5;245:*.nt=00;38;5;245:*.torrent=00;38;5;245:*.xml=00;38;5;245:*Makefile=00;38;5;245:*Rakefile=00;38;5;245:*Dockerfile=00;38;5;245:*build.xml=00;38;5;245:*rc=00;38;5;245:*1=00;38;5;245:*.nfo=00;38;5;245:*README=00;38;5;245:*README.txt=00;38;5;245:*readme.txt=00;38;5;245:*.md=00;38;5;245:*README.markdown=00;38;5;245:*.ini=00;38;5;245:*.yml=00;38;5;245:*.cfg=00;38;5;245:*.conf=00;38;5;245:*.h=00;38;5;245:*.hpp=00;38;5;245:*.c=00;38;5;245:*.cpp=00;38;5;245:*.cxx=00;38;5;245:*.cc=00;38;5;245:*.objc=00;38;5;245:*.sqlite=00;38;5;245:*.go=00;38;5;245:*.sql=00;38;5;245:*.csv=00;38;5;245:*.log=00;38;5;240:*.bak=00;38;5;240:*.aux=00;38;5;240:*.lof=00;38;5;240:*.lol=00;38;5;240:*.lot=00;38;5;240:*.out=00;38;5;240:*.toc=00;38;5;240:*.bbl=00;38;5;240:*.blg=00;38;5;240:*~=00;38;5;240:*#=00;38;5;240:*.part=00;38;5;240:*.incomplete=00;38;5;240:*.swp=00;38;5;240:*.tmp=00;38;5;240:*.temp=00;38;5;240:*.o=00;38;5;240:*.pyc=00;38;5;240:*.class=00;38;5;240:*.cache=00;38;5;240:*.aac=00;38;5;166:*.au=00;38;5;166:*.flac=00;38;5;166:*.mid=00;38;5;166:*.midi=00;38;5;166:*.mka=00;38;5;166:*.mp3=00;38;5;166:*.mpc=00;38;5;166:*.ogg=00;38;5;166:*.opus=00;38;5;166:*.ra=00;38;5;166:*.wav=00;38;5;166:*.m4a=00;38;5;166:*.axa=00;38;5;166:*.oga=00;38;5;166:*.spx=00;38;5;166:*.xspf=00;38;5;166:*.mov=00;38;5;166:*.MOV=00;38;5;166:*.mpg=00;38;5;166:*.mpeg=00;38;5;166:*.m2v=00;38;5;166:*.mkv=00;38;5;166:*.ogm=00;38;5;166:*.mp4=00;38;5;166:*. m4v=00;38;5;166:*.mp4v=00;38;5;166:*.vob=00;38;5;166:*.qt=00;38;5;166:*.nuv=00;38;5;166:*.wmv=00;38;5;166:*.asf=00;38;5;166:*.rm=00;38;5;166:*.rmvb=00;38;5;166:*.flc=00;38;5;166:*.avi=00;38;5;166:*.fli=00;38;5;166:*.flv=00;38;5;166:*.gl=00;38;5;166:*.m2ts=00;38;5;166:*.divx=00;38;5;166:*.webm=00;38;5;166:*.axv=00;38;5;166:*.anx=00;38;5;166:*.ogv=00;38;5;166:*.ogx=00;38;5;166:"

alias vi=nvim
alias vim=nvim

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
    case replace-one
      set_color --bold EF5939
      echo -n "REPLACE"
  end
  set_color normal
  echo -n "⟧"
end

function fish_prompt
  printf "%s%s%s@%s%s%s:%s%s%s%s» " (set_color --bold FD971F) (whoami) (set_color normal) (set_color --bold F92672) (hostname) (set_color normal) (set_color 66D9EF) (pwd_prompt) (set_color normal) (__fish_git_prompt)
end

function fish_right_prompt
  set -l _status $status
  echo -n "⟦"
  if test $_status = 0
    set_color --bold B8E673
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
