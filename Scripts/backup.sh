#!/bin/sh

# ~/Projects
echo "[!] Info: Backuping the ~/Projects directory"
tar -cjf ~/Backups/Projects.tbz2 ~/Projects

# ~/Documents directory
echo "[!] Info: Backuping the ~/Documents directory"
tar -cjf ~/Backups/Documents.tbz2 ~/Documents

# /etc/portage directory
echo "[!] Info: Backuping the /etc/portage directory"
tar -cjf ~/Backups/portage.tbz2 /etc/portage

# ~/Books directory
echo "[!] Info: Backuping the ~/Books directory"
tar -cjf ~/Backups/Books.tbz2 ~/Books

# ~/Podcasts directory
echo "[!] Info: Backuping the ~/Podcasts directory"
tar -cjf ~/Backups/Podcasts.tbz2 ~/Podcasts

# ~/Pictures directory
echo "[!] Info: Backuping the ~/Pictures directory"
tar -cjf ~/Backups/Pictures.tbz2 ~/Pictures

# ~/Music directory
echo "[!] Info: Backuping the ~/Music directory"
tar -cjf ~/Backups/Music.tbz2 ~/Music

# ~/Learning directory
echo "[!] Info: Backuping the ~/Learning directory"
tar -cjf ~/Backups/Learning.tbz2 ~/Learning

# Home directory tree
echo "[!] Info: Backuping the home directory tree"
tree ~/ > ~/Temp/home_dir_tree.txt

# Sync the stuff with the Yandex Disk
echo "[!] Info: Syncronizing files with the Yandex Disk"
ln -s ~/Backups ~/Yandex.Disk/Backups 2>/dev/null
yandex-disk start
yandex-disk sync
yandex-disk stop

