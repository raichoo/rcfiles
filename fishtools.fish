function print_color
  printf "\x1b[38;2;%d;%d;%dm%3d %3d %3d \x1b[0m #%02X%02X%02X\n" $argv[1] $argv[2] $argv[3] $argv[1] $argv[2] $argv[3] $argv[1] $argv[2] $argv[3]
end
