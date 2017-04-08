interface=enp0s19f2u4
ifconfig $interface 10.42.0.1 netmask 255.255.255.0
echo 1 > /proc/sys/net/ipv4/ip_forward
iptables -t nat -F
iptables -t nat -A POSTROUTING -j MASQUERADE
