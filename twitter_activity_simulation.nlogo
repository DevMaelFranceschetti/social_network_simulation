;;; Upgrades to come in the future ;;;
; About global values :
; - use stat values (don't compute every step)
; - avoid using total sums on all steps on distributed systems, prefer using max/min variation

;;; Notes ;;;
; Because of the propabilities, agents need a certain time to stabilize their activities.
; At start, a lot of agents quickly become influencers (in red), but it's just because the mean-IP is low.
; We have to wait to see the majority of agents become non-influencers again, and keep only the real influencers in red.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals and Agents definition ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

globals
[
  ; stats values
  total-likes
  total-posts
  total-comments
  total-retweets
  total-users
  max-W
  min-W
  mean-W
  mean-IP
  last-max-W-sum
  max-W-sum
  ; coefficients for actions in activity agregation
  coeff-like
  coeff-comment
  coeff-retweet
  ; proba for actions in twitter activity simulation
  like-proba
  comment-proba
  follow-proba
  retweet-proba
  post-proba
  follow-back-proba
  d ;dumping factor
]

turtles-own
[
  age ; age (not used yet)
  nb-content
  loc ; localisation (not used yet)
  IP ; influence power
  nb-followers ; number of agents following this one
  nb-followees ; number of agents followed by this one
  followees ; list of agents followed by this one
]

links-own
[ ; edges are non-oriented : we store the data in the two directions
   ; end1 to end2 direction (end1 necessarly follow end2, or the edge will not exist):
   nb-like-end1-to-end2 ; nb likes this step
   nb-comments-end1-to-end2 ; nb comments this step
   nb-retweets-end1-to-end2 ; nb retweets this step
   W-end1-over-end2; influence of agent end1 over agent end2
   ; end2 to end1 direction :
   nb-like-end2-to-end1
   nb-comments-end2-to-end1
   nb-retweets-end2-to-end1
   W-end2-over-end1
   ; if an edge existe, direction end1 to end2 is necessarly active.
   ; But we have to check if other direction is active or not :
   end2-follow-end1 ; is end2 following back end1 ? (boolean)
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to make-turtle
  create-turtles 1
  [
    setxy random-xcor random-ycor
    set color blue + 1 ; blue if not a big influencer
    set size 1.8
    set age 0
    set nb-content 0
    set IP 1 ; initialized to 1 because it stabilizes itself
             ;and if we set it to 0 at start, it's blocked
             ; (because IP is depending on neighbors IP -> stay 0)
    set nb-followers 0
    set nb-followees 0
    set followees []
  ]
  set total-users total-users + 1
end

to setup-globals
    ; coefficients for actions in activity agregation
    set coeff-like 0.2
    set coeff-comment 0.3
    set coeff-retweet 0.5
    ; proba for actions in twitter activity simulation
    set post-proba 0.01
    set follow-proba 0.001
    set like-proba 0.05
    set comment-proba 0.01
    set retweet-proba 0.001
    set follow-back-proba 0.01
    ; mean W
    set mean-W 0
    set max-W-sum 1
    set last-max-W-sum 1
    ; mean IP
    set mean-IP 0
    ;dumping factor
    set d 0.9
end

to setup
  clear-all
  setup-globals ; setup the constants
  set-default-shape turtles "circle" ; set the agents shape
  repeat agent-count [make-turtle] ; create agents
  compute-all-W ; compute all initial W values (if initial data not null)
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to go
  reset-actitity ; end the last step (current activity reset to 0)
  simulate-twitter-activity ; simulate social interactions
  update-all-W ; update W values
  compute-all-IP ; compute IP values
  set mean-W compute-mean-W ; update means
  set mean-IP compute-mean-IP
  update-color ; update color (big influencers are red)
  tick
end

to simulate-twitter-activity ; simulate the activity of every user
  ask turtles [simulate-user-activity self]
end

to simulate-user-activity [user] ; simulate the activity of one user (probabilistic)
  if random-float 1 < follow-proba [follow-user user one-of turtles ] ; sometimes follow another user
  if random-float 1 < post-proba [post user] ; sometimes post something
  if not empty? [followees] of user [
    if random-float 1 < like-proba [like-post user one-of [followees] of user] ; sometimes like a post
    if random-float 1 < comment-proba [comment-post user one-of [followees] of user] ; sometimes comment a post
    if random-float 1 < retweet-proba [retweet-post user one-of [followees] of user] ; sometimes retweet a post
    if random-float 1 < follow-back-proba [
      follow-user one-of [followees] of user user ; a followed user follow back its follower sometimes
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to create-link[ follower followee ] ; create an edge between two users
  ifelse (not link-exist follower followee) and (follower != followee) [
    ask follower
    [
      create-link-with followee
      [
        set nb-like-end1-to-end2 0 ; ephemeral values of activite at this time step
        set nb-comments-end1-to-end2 0
        set nb-retweets-end1-to-end2  0
        set W-end1-over-end2  0; influence of agent end1 over agent end2
        set nb-like-end2-to-end1 0
        set nb-comments-end2-to-end1 0
        set nb-retweets-end2-to-end1  0
        set W-end2-over-end1  0; influence of agent end2 over agent end1
        set end2-follow-end1 False ; initialy, only end1 follow end2 (implicitly true)
        set color blue
      ]
    ]
  ]
  [
    if link-exist follower followee [
      ;show "link exist"
      let l one-of get-link follower followee
        if [end1] of l = followee and [end2] of l = follower and [end2-follow-end1] of l = False [
          ;show "link get"
          ask l [
            set end2-follow-end1 True
            set color green ; if two users follow each other, display the edge in green
          ]
        ]
    ]
  ]
end

to-report link-exist [turtle1 turtle2] ; check if two agents are connected by an edge
  if count links with [end1 = turtle1 and end2 = turtle2] > 0 [report True]
  if count links with [end1 = turtle2 and end2 = turtle1] > 0 [report True]
  report False
end

to-report get-link [turtle1 turtle2] ; get the edge connecting two users
  let l links with [(end1 = turtle1 and end2 = turtle2) or (end1 = turtle2 and end2 = turtle1)]
  report l
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Influence Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report compute-mean-IP
  let sum-IP 0
  ask turtles [
    set sum-IP sum-IP + IP
  ]
  if count turtles > 0 [
    report sum-IP / count turtles
  ]
  report 0
end

to compute-all-W ; compute the W value for every edge
  set last-max-W-sum max-W-sum
  set max-W-sum 1
  set max-W 0
  set min-W 0
  ask links [
    let temp1 compute-W end1 end2
    let temp2 compute-W end2 end1
    ;show temp1
    ;show temp2
  ]
end

to update-all-W ; update the W value for every edge
  set last-max-W-sum max-W-sum
  set max-W-sum 1
  set max-W 0
  set min-W 0
  ask links [
    let temp1 update-W end1 end2
    let temp2 update-W end2 end1
    ;show temp1
    ;show temp2
  ]
end

to-report get-W-weighted-sum [user] ; compute the W weighted sum for an agent (used to compute IP)
  let user-W-weighted 0
  ask links with [end2 = user][ ; first case
    if ([nb-followees] of end1) > 0 [
      set user-W-weighted user-W-weighted + (W-end2-over-end1 * [IP] of end1) / ([nb-followees] of end1) + 0.0001 ; we add a small value to be sure it's not 0
    ]
  ]
  ask links with [end1 = user and end2-follow-end1][ ; first case
    if ([nb-followees] of end2) > 0 [
      set user-W-weighted user-W-weighted + (W-end2-over-end1 * [IP] of end2) / ([nb-followees] of end2) + 0.0001 ; we add a small value to be sure it's not 0
    ]
  ]
 if user-W-weighted > max-W-sum [set max-W-sum user-W-weighted ]
 report user-W-weighted / abs last-max-W-sum
end

to compute-all-IP ; compute IP value of all agents
  ask turtles [compute-IP self]
end

to compute-IP [user] ; compute IP on an agent
  let W-weighted get-W-weighted-sum user
  ;show "W-weighted"
  ;show W-weighted
  ask user [set IP d * W-weighted ]
end

to-report compute-mean-W ; compute the mean of the W values
  let totalW 0
  let totalActiveUsers 0
  ask links [
    set totalW totalW + W-end2-over-end1
    set totalActiveUsers totalActiveUsers + 1
    if end2-follow-end1 [
      set totalW totalW + W-end1-over-end2
      set totalActiveUsers totalActiveUsers + 1
    ]
  ]
  if totalActiveUsers > 0 [ report totalW / totalActiveUsers ]
  report 0
end

to-report update-W [ follower followee ] ; update W values based on precedent W value and current activity
  let link-fol get-link follower followee
  if any? link-fol
  [
    let fol one-of link-fol
    let like 0
    let com 0
    let retweet 0
    ifelse [end1] of fol = follower [
       set like [nb-like-end1-to-end2] of fol
       set com [nb-comments-end1-to-end2] of fol
       set retweet [nb-retweets-end1-to-end2] of fol
       if [nb-content] of followee > 0 [  ; update with the new activities :
         let W-new [W-end2-over-end1] of fol + ( coeff-retweet * retweet + coeff-comment * com + coeff-like * like ) / [nb-content] of followee - mean-W
         ask fol [set W-end2-over-end1 W-new]
         if W-new > max-W [set max-W W-new] ; update max W
         if W-new < min-W [set min-W W-new] ; update min W
         report W-new
      ]
    ]
    [
       set like [nb-like-end2-to-end1] of fol
       set com [nb-comments-end2-to-end1] of fol
       set retweet [nb-retweets-end2-to-end1] of fol
       if [nb-content] of followee > 0 [ ; update with the new activities :
         let W-new [W-end1-over-end2] of fol +( coeff-retweet * retweet + coeff-comment * com + coeff-like * like ) / [nb-content] of followee - mean-W
         ask fol [set W-end1-over-end2 W-new]
         if W-new > max-W [set max-W W-new] ; update max W
         if W-new < min-W [set min-W W-new] ; update min W
         report W-new
      ]
    ]
   ]
  report 0
end

to-report compute-W [ follower followee ] ; don't update online, juste compute all current values (use at init for example)
  let link-fol get-link follower followee
  if any? link-fol
  [
    let fol one-of link-fol
    let like 0
    let com 0
    let retweet 0
    ifelse [end1] of fol = follower [
       set like [nb-like-end1-to-end2] of fol
       set com [nb-comments-end1-to-end2] of fol
       set retweet [nb-retweets-end1-to-end2] of fol
       if [nb-content] of followee > 0 [
         let W ( coeff-retweet * retweet + coeff-comment * com + coeff-like * like ) / [nb-content] of followee - mean-W
         ask fol [set W-end2-over-end1 W]
         if W > max-W [set max-W W]
         if W < min-W [set min-W W]
         report W
      ]
    ]
    [
       set like [nb-like-end2-to-end1] of fol
       set com [nb-comments-end2-to-end1] of fol
       set retweet [nb-retweets-end2-to-end1] of fol
       if [nb-content] of followee > 0 [
         let W ( coeff-retweet * retweet + coeff-comment * com + coeff-like * like ) / [nb-content] of followee - mean-W
         ask fol [set W-end1-over-end2 W]
         if W > max-W [set max-W W]
         if W < min-W [set min-W W]
         report W
      ]
    ]
   ]
  report 0
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Twitter Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to follow-user [ follower followee ] ; follow another user
  if follower != followee [
    ifelse member? followee [followees] of follower [
      ;show "already followed"
    ]
    [
      ask follower
      [
        set nb-followees nb-followees + 1
        set followees lput followee followees
      ]
      ask followee
      [
        set nb-followers nb-followers + 1
      ]
      create-link follower followee
    ]
  ]
end

to like-post [ follower followee ] ; like a post of a followee
  if not link-exist follower followee [
    create-link follower followee
  ]
  let link-fol get-link follower followee
  ask link-fol [
    ifelse end1 = follower [
      set nb-like-end1-to-end2 nb-like-end1-to-end2 + 1
    ]
    [
      set nb-like-end2-to-end1 nb-like-end2-to-end1 + 1
    ]
  ]
  set total-likes total-likes + 1
end

to retweet-post [ follower followee ] ; retweet the post of a followee
  if not link-exist follower followee [
    create-link follower followee
  ]
  let link-fol get-link follower followee
  ask link-fol [
    ifelse end1 = follower [
      set nb-retweets-end1-to-end2 nb-retweets-end1-to-end2 + 1
    ]
    [
      set nb-retweets-end2-to-end1 nb-retweets-end2-to-end1 + 1
    ]
  ]
    set total-retweets total-retweets + 1
end

to comment-post [ follower followee ]  ; comment the post of a followee
  if not link-exist follower followee [
    create-link follower followee
  ]
  let link-fol get-link follower followee
  ask link-fol [
    ifelse end1 = follower [
      set nb-comments-end1-to-end2 nb-comments-end1-to-end2 + 1
    ]
    [
      set nb-comments-end2-to-end1 nb-comments-end2-to-end1 + 1
    ]
  ]
    set total-comments total-comments + 1
end

to post [ user ] ; post something
  ask user[ set nb-content nb-content + 1]
    set total-posts total-posts + 1
end

to reset-actitity ; reset all the current activity (retweets, likes, comments)
  ask links [
    set nb-like-end1-to-end2 0
    set nb-like-end2-to-end1 0
    set nb-comments-end1-to-end2 0
    set nb-comments-end2-to-end1 0
    set nb-retweets-end1-to-end2 0
    set nb-retweets-end2-to-end1 0
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-color
  ask turtles [
    ifelse IP > mean-IP [ ; if an influencer has an IP greater than mean IP
      set color red + 1 ; we display it in red
    ]
    [
      set color blue + 1 ; else, in blue
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Old Manual Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to connect-random [ nb act ] ; connect together two by two some random agents (no more used)
  repeat nb [
    let follower one-of turtles
    let followee one-of turtles
    follow-user follower followee
    if act and [nb-content] of followee > 0 [ ; simulate some actions if act is True
      if random-float 1 < like-proba [ like-post follower followee ]
      if random-float 1 < comment-proba [ comment-post follower followee ]
      if random-float 1 < retweet-proba [ retweet-post follower followee ]
    ]
  ]
end

to react-random [ nb ] ; simulate activity for some random agents (no more used)
  repeat nb [
    let l one-of links
    let follower [end1] of l
    let followee [end2] of l
    if [nb-content] of followee > 0 [
      if random-float 1 < like-proba [ like-post follower followee ]
      if random-float 1 < comment-proba [ comment-post follower followee ]
      if random-float 1 < retweet-proba [ retweet-post follower followee ]
    ]
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
347
10
759
423
-1
-1
4.0
1
10
1
1
1
0
1
1
1
-50
50
-50
50
1
1
1
ticks
30.0

BUTTON
9
20
114
53
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
119
20
224
53
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
0

BUTTON
15
131
138
164
create user
make-turtle
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
158
124
243
169
NIL
total-users
17
1
11

MONITOR
15
181
123
226
NIL
total-posts
17
1
11

MONITOR
15
232
123
277
NIL
total-likes
17
1
11

MONITOR
15
283
123
328
NIL
total-retweets
17
1
11

MONITOR
15
334
123
379
NIL
total-comments
17
1
11

MONITOR
143
310
227
355
NIL
max-W
17
1
11

MONITOR
143
212
229
257
NIL
mean-W
17
1
11

SLIDER
10
64
226
97
agent-count
agent-count
0
100
50.0
1
1
NIL
HORIZONTAL

MONITOR
143
261
228
306
NIL
min-W
17
1
11

MONITOR
262
210
335
255
NIL
mean-IP
17
1
11

TEXTBOX
171
190
321
208
Stats before rescale
12
0.0
1

@#$#@#$#@
## WHAT IS IT?

This model illustrates the activity of users on twitter and the influence power of every user on the others

## HOW IT WORKS

We followed a simplified method based on the published work "An Efficient Two-Phase Model for Computing Influential Nodes inSocial Networks Using Social Actions" from Mehdi Azaouzi and Lotfi Ben Romdhane. We first compute the influence of each user on its followers, and we next compute the général influence power of each agent.

## HOW TO USE IT

Click the SETUP button so create users, and click the GO button to simulate twitter activity. Users in red have a Influence Power greater than the mean Influence Power.
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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
set layout? false
setup repeat 175 [ go ]
repeat 35 [ layout ]
@#$#@#$#@
@#$#@#$#@
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
