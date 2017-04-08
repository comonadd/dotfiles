URL='http://www.accuweather.com/en/ua/luhansk/324763/weather-forecast/324763'
wget -q -O- "$URL" | awk -F\' '/acm_RecentLocationsCarousel\.push/{print $12"C" }'| head -1
