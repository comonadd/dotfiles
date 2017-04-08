TYPE="${BLOCK_INSTANCE:-mem}"

awk -v type=$TYPE '
/^MemTotal:/ {
	mem_total=$2
}
/^MemFree:/ {
	mem_free=$2
}
/^Buffers:/ {
	mem_free+=$2
}
/^Cached:/ {
	mem_free+=$2
}
/^SwapTotal:/ {
	swap_total=$2
}
/^SwapFree:/ {
	swap_free=$2
}
END {
	if (type == "swap") {
		free=swap_free/1024/1024
		used=(swap_total-swap_free)/1024/1024
		total=swap_total/1024/1024
	} else {
		free=mem_free/1024/1024
		used=(mem_total-mem_free)/1024/1024
		total=mem_total/1024/1024
	}
	pct=used/total*100
	# full text
	printf("%.1fG/%.1fG (%.f%%)\n", used, total, pct)
	# short text
	printf("%.f%%\n", pct)
	# color
	if (pct > 90) {
		print("#FF0000\n")
	} else if (pct > 80) {
		print("#FFAE00\n")
	} else if (pct > 70) {
		print("#FFF600\n")
	}
}
' /proc/meminfo
