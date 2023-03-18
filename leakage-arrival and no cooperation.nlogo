;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;城市间无配合-近端查验-time option;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

turtles-own[
  departure ;;出发地
  destination ;;目的地
  state ;;携带病毒状态，0表示健康，1表示携带病毒
  p ;;传染力
  s ;;易感性
  transit_preference ;;避难偏好
  distance_preference ;;距离偏好
  transition ;;避难状态，0表示非避难者，正整数表示避难第几天
]

patches-own[
  core ;;是否为核心城市
  risk ;;是否涉疫，0表示无病例，1表示有病例
  capacity ;;病毒检测能力
  neighbors_risk ;;周边城市涉疫情况
]

globals[
  num_risk_city ;;涉疫城市数量
  all_traveler ;;所有出行者数量
  transit_traveler ;;正在避难中的出行者数量
  fail_transit_traveler ;;避难失败的出行者数量
  give_up_traveler ;;放弃出行的出行者数量
  mob_scale ;;核心城市流量
  tran_mob_scale ;;经过避难到达核心城市流量
  infect_traveler ;;病毒携带者数量
  infect_mob_scale ;;入境核心城市的病毒携带者数量
  threshold ;;方案阈值，选择避难的可能性
  threshold2 ;;方案阈值2，选择直接前往目的地的可能性
]

to setup
  clear-all
  reset-ticks

  ;;设置方案阈值
  ifelse time_option = 3 [
    set threshold 0.6637
    set threshold2 0.4268
  ][
    ifelse time_option = 7 [
      set threshold 0.6856
      set threshold2 0.3912
    ][
      ifelse time_option = 14 [
        set threshold 0.7407
        set threshold2 0.3318
      ][
        set threshold 0.7527
        set threshold2 0.2913
      ]
    ]
  ]

  ;;设定中心为核心城市
  ask patch 0 0[
    set core 1
    set pcolor yellow
    set capacity 0.999
  ]
  ask patches with [pcolor = black][
    set core 0
    set capacity 0.95 + random 0.05
  ]

  ;;产生一定规模的出行者
  create-travel-demand
  ;;初始有一部分病毒携带者
  ask n-of initial-outbreak-size turtles [
    become-infected
  ]

  ;;设定涉疫城市
  ask patches with [core = 0][
    let turtles-group turtles-here
    let case sum [state] of turtles-group
    if case > 0 [
      become-risk ;;初始有病例的城市列为涉疫
    ]
  ]
  ;;设定周边城市涉疫情况
  ask patches[
    set neighbors_risk count neighbors with [risk = 1]
  ]
end

to go
  ;;新出行者
  let new_turtles turtles with [transition = 0]
  ask new_turtles[
    ifelse [risk] of departure = 0 [ ;;非涉疫城市出发
      move-to destination
    ][ ;;涉疫城市出发
      ifelse destination = patch 0 0 [ ;;如果目的地是核心城市，则选择是否直接去，是否避难，去哪避难
        ifelse transit_preference > threshold [
          set transition 1
          find-sanctuary
        ][
          ifelse transit_preference > threshold2 [
            move-to destination
          ][
            set give_up_traveler give_up_traveler + 1
            die
          ]
        ]
      ][ ;;如果目的地是其他城市，则直接出行
        move-to destination
      ]
    ]
  ]

  ;;避难中的出行者
  let transit_turtles turtles with [transition > 0]
  ;;检查避难所是否仍安全
  ask transit_turtles[
    ifelse [risk] of patch-here = 0 [ ;;若避难所安全，则避难天数+1，并判断是否满足离开条件
      set transition transition + 1
      if transition > time_option [
        move-to destination
      ]
    ][ ;;若避难所沦陷，则避难失败，离开系统
      set fail_transit_traveler fail_transit_traveler + 1 ;;计入避难失败的出行者数量
      die
    ]
  ]

  ;;所有普通城市进行一轮检测
  ask patches with [core = 0][
    let case sum [state] of turtles-here
    if case > 0 [
      ask turtles-here with [state = 1] [
        if random-float 1 < capacity [ ;;每一个感染者被检出的概率为capacity，感染者越多，城市转变为涉疫的概率越大
          become-risk
        ]
      ]
    ]
  ]
  ;;检测后调整周边城市涉疫情况
  ask patches[
    set neighbors_risk count neighbors with [risk = 1]
  ]

  ;;统计量
  ;;系统中出行者数量
  set all_traveler count turtles
  ;;系统中正在避难中的出行者数量
  set transit_traveler count turtles with [transition > 0 and transition <= time_option]
  ;;涉疫城市数量
  set num_risk_city count patches with [risk = 1]
  ;;核心城市流量
  set mob_scale count turtles-on patches with [core = 1]
  ;;经过避难到达核心城市流量
  set tran_mob_scale count turtles with [transition > time_option]
  ;;系统中病毒携带者数量
  set infect_traveler count turtles with [state = 1]
  ;;入境核心城市的病毒携带者数量
  set infect_mob_scale count turtles with [state = 1 and xcor = 0 and ycor = 0]

  ;;产生新的一定规模的出行者
  create-travel-demand
  ;;发生传染
  ask turtles with [state = 0][
    let agset other turtles-here
    if sum [state] of agset > 0 [
      let d one-of agset with [state = 1]
      if random-float 1 < (1 - (1 - [p] of d) ^ s) [
        become-infected
      ]
    ]
  ]

  ;;已到达目的地的人员离开系统
  ask turtles[
    if destination = patch-here [
      die
    ]
  ]

  tick
end

to create-travel-demand
  ;;产生一定规模的出行人员
  set-default-shape turtles "circle"
  create-turtles num_agents[
    set size 0.5 ;;人身体的大小
    move-to one-of patches with [core = 0] ;;初始化位置随机，排除核心城市
    become-susceptible
    set transit_preference random-float 1
    set distance_preference distance patch 0 0 ;;到核心城市的直线距离
    set transition 0
  ]
  ;;设定人员出发地与目的地
  ask turtles[
    set departure patch-here
    set destination one-of other patches
    if random-float 1 < 0.5 [ ;;设定50%的概率目的地是核心城市
      set destination patch 0 0
    ]
  ]
end

to become-infected
  ;;转化为感染者
  set state 1
  set color red
  set p 0.5 + random 0.5
end

to become-susceptible
  ;;转化为易感者
  set state 0
  set color blue
  set s random-float 1
end

to become-risk
  ;;转化为涉疫地区
  set risk 1
  set pcolor green
end

to find-sanctuary
  ;;选择避难所
  let x xcor
  let y ycor
  let another-x 0
  let another-y 0
  ifelse xcor > 0 [
    set x x + 1
    set another-x -1
  ][
    set x x - 1
    set another-x 1
  ]
  ifelse ycor > 0 [
    set y y + 1
    set another-y -1
  ][
    set y y - 1
    set another-y 1
  ]
  let max-range-x max list x another-x
  let min-range-x min list x another-x
  let max-range-y max list y another-y
  let min-range-y min list y another-y
  let sanc-set patches with [neighbors_risk + risk = 0
    and pxcor <= max-range-x
    and pxcor >= min-range-x
    and pycor <= max-range-y
    and pycor >= min-range-y]
  let sanctuary one-of sanc-set ;;在距离半径内选择任意一个周围无涉疫的安全城市
  ifelse sanc-set = no-patches [ ;;如果可选集为空，人员出行失败，离开系统，若非空则到达避难所
    set give_up_traveler give_up_traveler + 1 ;;计入放弃出行的出行者数量
    die
  ][
    move-to sanctuary
  ]
end

to-report compute-R
  let R infect_mob_scale
  report R
end

to-report compute-M
  let M mob_scale
  report M
end

to-report compute-TM
  let TM tran_mob_scale
  report TM
end

to-report compute-at
  let at all_traveler
  report at
end

to-report compute-it
  let it infect_traveler
  report it
end

to-report compute-tt
  let tt transit_traveler
  report tt
end

to-report compute-ftt
  let ftt fail_transit_traveler
  report ftt
end

to-report compute-gut
  let gut give_up_traveler
  report gut
end

to-report compute-rc
  let rc num_risk_city
  report rc
end
@#$#@#$#@
GRAPHICS-WINDOW
337
38
671
373
-1
-1
7.95122
1
10
1
1
1
0
0
0
1
-20
20
-20
20
1
1
1
ticks
30.0

BUTTON
28
38
94
71
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
12
93
113
126
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
123
38
295
71
num_agents
num_agents
400
1000
1000.0
1
1
NIL
HORIZONTAL

SLIDER
123
84
318
117
initial-outbreak-size
initial-outbreak-size
1
100
100.0
1
1
NIL
HORIZONTAL

PLOT
691
38
921
218
city with case
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"num_risk_city" 1.0 0 -16777216 true "" "plot num_risk_city"

PLOT
691
233
923
412
mobility of core city
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"mob_scale" 1.0 0 -16777216 true "" "plot mob_scale"

PLOT
13
396
320
608
number of travelers
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"all_traveler" 1.0 0 -16777216 true "" "plot all_traveler"

PLOT
12
168
320
371
travelers in transition
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"transit_traveler" 1.0 0 -16777216 true "" "plot transit_traveler"

PLOT
337
396
672
608
mobility by transition of core city
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"tran_mob_scale" 1.0 0 -16777216 true "" "plot tran_mob_scale"

PLOT
691
432
924
608
number of infected travelers
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"infect_traveler" 1.0 0 -16777216 true "" "plot infect_traveler"

PLOT
13
628
320
807
infected mobility of core city
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"infect_mob_scale" 1.0 0 -16777216 true "" "plot infect_mob_scale"

PLOT
337
628
624
807
travelers who give up
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"give_up_traveler" 1.0 0 -16777216 true "" "plot give_up_traveler"

PLOT
645
627
925
807
travelers who fail in transition
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"fail_transit_traveler" 1.0 0 -16777216 true "" "plot fail_transit_traveler"

SLIDER
123
130
295
163
time_option
time_option
3
21
7.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment-arrival and no cooperation" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="70"/>
    <metric>compute-R</metric>
    <metric>compute-M</metric>
    <metric>compute-TM</metric>
    <metric>compute-at</metric>
    <metric>compute-it</metric>
    <metric>compute-tt</metric>
    <metric>compute-ftt</metric>
    <metric>compute-gut</metric>
    <metric>compute-rc</metric>
    <enumeratedValueSet variable="time_option">
      <value value="3"/>
      <value value="7"/>
      <value value="14"/>
      <value value="21"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="20"/>
      <value value="40"/>
      <value value="60"/>
      <value value="80"/>
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num_agents" first="400" step="100" last="1000"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
