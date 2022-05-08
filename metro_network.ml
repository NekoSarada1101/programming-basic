type station_info_t = {
  kanji : string;
  kana : string;
  roma : string;
  route_name : string;
} ;;
type between_stations_info_t = {
  start_station_name : string;
  end_station_name : string;
  go_through_route_name : string;
  distance : float;
  duration : int;
} ;;
