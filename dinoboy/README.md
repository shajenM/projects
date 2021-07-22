
 #  DINO BOY

<img src="dinoBoyDemo.gif" width="400">

  This game is written in Haskell language using functional reactive design. The reactive framework
I developed is based on book "Haskell School of Expression" by Paul Hudak and Conal Elliott
(https://www.amazon.com/Haskell-School-Expression-Functional-Programming/dp/0521644089)

For the graphics I used library SDL2 for haskell bindings.

## Design

Entire game logic is in a single DinoBoyGame.hs. All other files belongs to the game framework.

The components in the game are defined at the end of DinoBoyGame.hs as
```
game  = backGrd `over` boySprite `over` boxes `over` sounds `over` messages
```

* backGrd - is a moving background picture
* boySprite - is the picture of boy who reacts by jumping when space key is pressed.
- boxes - are the objects moving from right to left at different speeds
- sounds - are sounds produced when boy hit a box
- mesages - are text messages on screen which are score and "you Died" message when hit.

Module Hudak.hs and Fal.hs have the features(Behaviors) that user can use to build a game.
Here is the sample exmaple of using the framework:

1) Draw an ellipse on screen at position (2,2) with radious r1 = 0.05 , r2 = 0.05
```
r1 = 0.05
r2 = 0.05
pic1 = paintedPicture  red (translate (2, 2) (ell r1 r2) )
runReact "Reactive Game" pic1 resoures
```
'ell' draws an ellipse on screen. 'translate' moves ellipse at position (0, 0) to (2,2)
This picture will be a static at one place in red color. Now how to make it dynamic?

2) To animate ellipse by shrink and grow, try this

```
r1 = cos time
r2 = sin time
pic1 = paintedPicture  red (translate (0, 0) (ell r1 r2) )
runReact "Reactive Game" pic1 resoures
```
'time' is a behavior which represents current time in milli seconds. When we use
cosine of time, we will have values betiween 0 and 1, which will be used in each iteration of the animation.

3) Now to make more interesting, let us make the color of ellipse varying.
The color will be changing in every 1.0 seond.
```
twinkling = red `switch` ( timer 1.0 `withElem_` cycle[red, yellow, blue ])
r1 = cos time
r2 = sin time
pic1 = paintedPicture  twinkling (translate (0, 0) (ell r1 r2) )
runReact "Reactive Game" pic1 resoures
```

4) The same way we can make the color dynamic, we can have dynamic x and y positions, which enables the movement of ellipse.
To move the ellipse in circle, try this

```
x = cos time
y = sin time
r = 0.05
pic1 = paintedPicture  twingling (translate (x, y) (ell r r) )
runReact "Reactive Game" pic1 resoures
```

Thanks for visiting this page!. Hope you liked this framework!. For any questions, send to s.muttath.d@gmail.com

Shajen Muttath
