
 #  DINO BOY

<img src="dinoBoyDemo.gif" width="400">

  This game is written Haskell language using reactive framework. The reactive framework
I developed was inspired by book Haskell School of Expression by Paul Hudak and Conal Eliott

For the graphics rendering I used SDL2 haskell bindings.

## Design

Module Hudak.hs has the Behavior features that user can use to build a game. Here is the sample usage.

1) Show an ellipse at position (0,0) with radious r1 = 0.05 , r2 = 0.05
```
r1 = 0.05
r2 = 0.05
pic1 = paintedPicture  red (translate (0, 0) (ell r1 r2) )
runReact "Reactive Game" pic1 resoures
```
This picture will be a static at one place in red color. Now how to make it dynamic?

2) Now to animate ellipse by shrink and grow, try this```
```
r1 = cos time
r1 = sin time
pic1 = paintedPicture  red (translate (0, 0) (ell r1 r2) )
runReact "Reactive Game" pic1 resoures
```
'time' is a behavior which represents current time in milli seocnds. when we use
cos time will have values betiween 0 and 1, which will be used in each iteration of the animation.

3) Now to make more interesting, let us make the color of ellipse to variying colors.
The color will be changing every 1.0 seonds.
```
twingling = red `switch` ( timer 1.0 `withElem_` cycle[red, yellow, blue ])
r1 = cos time
r1 = sin time
pic1 = paintedPicture  twingling (translate (0, 0) (ell r1 r2) )
runReact "Reactive Game" pic1 resoures
```

Hope you liked this framework!. For any questions, send to s.muttath.d@gmail.com

Thanks
Shajen Muttath
