#!/usr/bin/env bash

__indent=0

str_mul() {
  res=$(printf "%$2s");
  echo ${res// /$1}
  echo $res
}

print_info() {
  echo $(str_mul " " $__indent) "[I]: " $1
}

indent() {
  __indent=$((__indent + 1))
}

unindent() {
  if [[ $__indent -eq 0 ]]; then
    return;
  fi
  __indent=$((__indent - 1))
}

begin_block() {
  echo $1
  indent
}

end_block() {
  unindent
}

# Setup network privacy
begin_block "Network privacy setup"
  indent
  print_info "Installing TOR and Polipo"
  brew install tor polipo
  print_info "Creating a new user for TOR"
  sudo dscl . -create /Users/tor
  sudo dscl . -passwd /Users/tor 545454
  print_info "Creating a new user for Polipo"
  sudo dscl . -create /Users/polipo
  sudo dscl . -passwd /Users/polipo 545454
end_block
