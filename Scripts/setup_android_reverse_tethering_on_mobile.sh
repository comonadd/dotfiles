IFACE=rndis0
FST_DNS=8.8.8.8
SND_DNS=8.8.4.4

ifconfig $IFACE 10.42.0.2 netmask 255.255.255.0
route add default gw 10.42.0.1 dev $IFACE

# Do some DNS magic
setprop net.dns1 $FST_DNS
ndc resolver flushif $IFACE
ndc resolver flushdefaultif
ndc resolver setifdns $IFACE localhost $FST_DNS $SND_DNS
ndc resolver setdefaultif $IFACE
