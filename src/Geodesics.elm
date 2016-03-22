module Geodesics(LatLon, latLon, distance, moveWithBearing) where

{-| This file contains the implementation of geographic coordinates Latitude/Longitude for elm

@docs LatLon, latLon, distance, moveWithBearing

The implementation of this library is based on the work of Chris Veness[1], who converted
the haversine elliptical formulae into javascript, and also on the work of Peter Haldbæk[2],
who implemented the javascript mt-latlon library.

[1] http://www.movable-type.co.uk/scripts/latlong.html
[2] https://github.com/peterhaldbaek/mt-latlon

-}

{-| Represents a Latitude/Longitude coordinate in a geodesic system -}
type alias LatLon = { latidude: Float, longitude: Float, radius: Int }

defaultRadius : Int
defaultRadius = 6371

toRad : Float -> Float
toRad deg = deg * pi / 180

toDeg : Float -> Float
toDeg rad = rad * 180 / pi

{-| Builds a LatLon record assuming earth radius of 6371 km -}
latLon : Float -> Float -> LatLon
latLon lat lon = { latidude = lat, longitude = lon, radius = defaultRadius }

{-| Calculates the geodesic distance in km between two coordinates -}
distance : LatLon -> LatLon -> Float
distance orig dest =
  let
     lat1 = toRad orig.latidude
     lat2 = toRad dest.latidude
     lon1 = toRad orig.longitude
     lon2 = toRad dest.longitude
     dLat = lat2 - lat1
     dLon = lon2 - lon1
     arc = (sin dLat/2) * (sin dLat/2) + (cos lat1) * (cos lat2) * (sin dLon/2) * (sin dLon/2)
     circle = 2 * atan2 (sqrt arc) (sqrt 1 - arc)
  in
     (toFloat orig.radius) * circle

fmod : Float -> Float -> Float
fmod all by =
  let
     fitted = toFloat (truncate (all / by))
  in
     all - fitted * by


normalize : Float -> Float
normalize deg = (fmod (deg + 540) 360) - 180

{-| Calculates a new coordinate given an initial coordinate, a bearing and a distance in km -}
moveWithBearing : LatLon -> Float -> Float -> LatLon
moveWithBearing orig bearing distance =
  let
     dist = distance / (toFloat orig.radius)
     bear = toRad bearing
     lat1 = toRad orig.latidude
     lon1 = toRad orig.longitude

     lat2 = asin ( (sin lat1) * (cos dist) + (cos lat1) * (sin dist) * (cos bear) )
     lon2 = lon1 + (atan2
       ((sin bear) * (sin dist) * (cos lat1))
       ((cos dist) - (sin lat1) * (sin lat2))
     )
  in
     { latidude = (toDeg lat2), longitude = normalize (toDeg lon2), radius = orig.radius }
