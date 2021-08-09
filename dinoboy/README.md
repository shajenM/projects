
 #  Dino Boy

<img src="dinoBoyDemo2.gif" width="400">

  This game is written in Haskell language with functional reactive design. The reactive framework
for the game I developed is based on book "Haskell School of Expression" by Paul Hudak and Conal Elliott
(https://www.amazon.com/Haskell-School-Expression-Functional-Programming/dp/0521644089)

For the graphics I used library SDL2 for haskell bindings.

I use Ubuntu running on WSL2 Windows 10 for development. For code editing I use Visual Studio Code.
It was not easy to develop graphical applications on WSL2. I used Xming for XWindow Server and Pulse Audio 
for Sound Server.

## Design

Entire game logic is in file DinoBoyGame.hs. Folder Hudak contains the reactive game framework.

The components in the game are defined at the end of DinoBoyGame.hs as
```
game  = backGrd `over` boySprite `over` boxes `over` sounds `over` messages
```

* backGrd   - a moving background picture
* boySprite - the picture of boy who reacts by jumping when space key is pressed.
* boxes     - the objects moving from right to left at different speeds
* sounds    - sounds produced when boy hit a box
* mesages   - text messages on screen which are score and "you Died" message when hit.
* 
## Functional Reactive Framework for Graphics
Module GameEngine.hs and Fal.hs have the features(Behaviors) that user can use to build a game.
Here is the sample exmaple of using the framework:


1) Draw an ellipse on screen at position (2,2) with radious r1 = 0.05 , r2 = 0.05
<img src="demo_static_ellipse.gif" width="400">

```
r1 = 0.05
r2 = 0.05
pic1 = paintedPicture  red (translate (2, 2) (ell r1 r2) )
runReact "Reactive Game" pic1 resoures
```

'ell' draws an ellipse on screen. 'translate' moves ellipse at position (0, 0) to (2,2)
This picture will be a static at one place in red color. Now how to make it dynamic?

2) To animate ellipse by shrink and grow, try this
<img src="demo_growing_ellipse.gif" width="400">

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
<img src="demo_twinkling_ellipse.gif" width="400">

```
twinkling = red `switch` ( timer 1.0 `withElem_` cycle[red, yellow, blue ])
r1 = cos time
r2 = sin time
pic1 = paintedPicture  twinkling (translate (0, 0) (ell r1 r2) )
runReact "Reactive Game" pic1 resoures
```

4) The same way we can make the color dynamic, we can have dynamic x and y positions, which enables the movement of ellipse.
To move the ellipse in circle, try this

<img src="demo_circling_ellipse.gif" width="400">

```
x = cos time
y = sin time
r = sin time + 0.2
pic1 = paintedPicture  twingling (translate (x, y) (ell r r) )
runReact "Reactive Game" pic1 resoures
```

Thanks for visiting this page!. Hope you liked this framework!. For any questions, send to s.muttath.d@gmail.com

Shajen Muttath
