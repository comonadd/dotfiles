# CLI Tools Cheatsheet

## lazygit
Terminal GUI for git workflow management.

### Keybindings
- `w` - Save work in progress (WIP)
- `c` - Commit changes
- `P` - Push changes to origin
- `p` - Pull changes from remote
- `space` - Stage/unstage file or hunk
- `d` - View diff options
- `a` - Stage all changes
- `A` - Amend last commit
- `shift+s` - Stash changes
- `shift+p` - Pop stash
- `q` - Quit lazygit
- `?` - Show help menu

## fd
Fast and user-friendly alternative to `find` for searching file names.

### Usage
```bash
fd <pattern>                    # Search for files matching pattern in current directory
fd <pattern> <path>             # Search in specific directory
fd -e <ext> <pattern>           # Search for files with specific extension
fd -t f <pattern>               # Search for files only
fd -t d <pattern>               # Search for directories only
fd -H <pattern>                 # Include hidden files
fd -I <pattern>                 # Include ignored files (.gitignore)
fd -E <pattern> <search>        # Exclude pattern from search
```

## rg (ripgrep)
Fast text search tool for searching content recursively.

### Usage
```bash
rg <pattern>                    # Search for pattern in current directory
rg <pattern> <path>             # Search in specific directory
rg -i <pattern>                 # Case-insensitive search
rg -w <pattern>                 # Match whole words only
rg -t <type> <pattern>          # Search in specific file types (e.g., -t py)
rg -T <type> <pattern>          # Exclude specific file types
rg -l <pattern>                 # List only filenames containing matches
rg -c <pattern>                 # Count matches per file
rg -A <n> <pattern>             # Show n lines after match
rg -B <n> <pattern>             # Show n lines before match
rg -C <n> <pattern>             # Show n lines before and after match
rg --hidden <pattern>           # Search hidden files
rg --no-ignore <pattern>        # Don't respect .gitignore
```
