module Main where

import Utils
import Line4 (line4)
import Bar13 (bar13)
import Scatter3 (scatter3)
import K (k)
import Lasagna (lasagna)
import Radar3 (radar3)
import Chord2 (chord2)
import Force4 (force4)
import Map11 (map11)
import Gauge4 (gauge4)
import Funnel2 (funnel2) 
import EventRiver1 (eventRiver) 
import DynamicLineBar (dynamicLineBar)
import Loading (loading)
import Events (events) 
import Connect (connectM)
import Mix2Safe (mix2safe)

main = onLoad $ do
  line4 "line4"
  bar13 "bar13"
  scatter3 "scatter3"
  k "k"
  lasagna "lasagna"
  radar3 "radar3"
  chord2 "chord2"
  force4 "force4"

  map11 "map11"
  
  gauge4 "gauge4"
  funnel2 "funnel2"
  eventRiver "event-river"
  dynamicLineBar "dynamic-line-bar"
  loading "loading"
  events "events"
  connectM "connect1" "connect2"
  mix2safe "mix2safe"

