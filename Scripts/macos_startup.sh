# Start network privacy services
sudo killall -9 tor 1>/dev/null 2>/dev/null
sudo sh 1>/dev/null -c "tor -f /Users/wrongway4you/Documents/System/MacOS/torrc.conf"
sudo killall -9 polipo 1>/dev/null 2>/dev/null
sudo sh 1>/dev/null -c "polipo -c /Users/wrongway4you/Documents/System/MacOS/polipo.conf"
