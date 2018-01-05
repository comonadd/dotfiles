#!/bin/bash

restart_services()
{
  sudo systemctl stop ccpd.service
  sudo systemctl stop org.cups.cupsd.service
  sudo systemctl start org.cups.cupsd.service
  sudo systemctl start ccpd.service
}

# Prepare the file system
sudo mkdir /var/captmon
sudo mkdir /var/ccpd
sudo mkfifo /var/ccpd/fifo0
sudo chmod 777 /var/ccpd/fifo0
sudo chown root /var/ccpd/fifo0
sudo chmod -R a+rx /usr/share/cups/model

restart_services

# Delete the old configuration
sudo ccpdadmin -x LBP2900
sudo lpadmin -x LBP2900

# Restart the services for the configuration approval
restart_services

# Add the new spooler
sudo lpadmin -p LBP2900 -m CNCUPSLBP2900CAPTK.ppd -v ccp:/var/ccpd/fifo0 -E
sudo ccpdadmin -p LBP2900 -o /dev/usb/lp0

# Restart the services for the refresh
restart_services

# Enable the printer
sudo cupsenable LBP2900
sudo cupsaccept LBP2900

# Restart the services for the working printer
restart_services
