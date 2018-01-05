#!/bin/bash

sudo systemctl stop ccpd.service
sudo systemctl stop org.cups.cupsd.service
sudo systemctl start org.cups.cupsd.service
sudo systemctl start ccpd.service
