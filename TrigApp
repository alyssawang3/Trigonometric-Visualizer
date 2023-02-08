import Array
import Random

type SCQFormat = Sine | Cosine
type QFormat = Ratio | Side | Angle
type Trig = Sin | Cos | Tan

type AnimationStatus = Play | Stop


type alias Model =
    { time : Float
    , state : State
    -- NEEDED FOR SOHCAHTOA QUIZ
    , score : Int
    , sideX : Int
    , sideY : Int
    , answerPos : Int
    , limit : Int
    , qFormat : QFormat
    , trig : Trig
    , result : String
    , clr : Color 
    -- NEEDED FOR SINE COSINE QUIZ
    , scqScore : Int
    , scqSideX1 : Int
    , scqSideY1 : Int
    , scqSideX2 : Int
    , scqSideY2 : Int
    , scqAnswerPos : Int
    , scqLimit : Int
    , scqQFormat : SCQFormat
    , scqResult : String
    , scqClr : Color
    -- NEEDED FOR FUNCTION VISUALIZER
    , posRed: (Float, Float)
    , posS: (Float, Float)
    , posC: (Float, Float)
    , angle: Float
    , animationButton: AnimationStatus
    , debug: String
    -- NEEDED FOR SOHCAHTOA
    , rightSide : Float
    , leftSide : Float
    , bottom : Float
    , topAngle : Float
    , leftAngle : Float
    , rightAngle : Float
    , errorMsg : String
    -- NEEDED FOR BASICTRIG
    , btRightSide : Float
    , btLeftSide : Float
    , btBottom : Float
    , btTopAngle : Float
    , btLeftAngle : Float
    , btRightAngle : Float
    , btErrorMsg : String
    }

init : Model
init = { time = 0 
       , state = MainMenu 
       -- NEEDED FOR SOHCAHTOA QUIZ
       , score = 0
       , sideX = 90
       , sideY = 40
       , answerPos = 1
       , limit = 3
       , qFormat = Ratio
       , trig = Sin
       , result = ""
       , clr = lightGreen 
       -- NEEDED FOR SINE COSINE QUIZ
       , scqScore = 0
       , scqSideX1 = 90
       , scqSideY1 = 40
       , scqSideX2 = 0
       , scqSideY2 = 0
       , scqAnswerPos = 1
       , scqLimit = 3
       , scqQFormat = Sine
       , scqResult = ""
       , scqClr = lightPurple
       -- NEEDED FOR FUNCTION VISUALIZER
       , posRed = (-40, 0)
       , posS = (-50,0)
       , posC = (-50, 10)
       , angle = 0 
       , animationButton = Stop
       , debug = ""
       -- NEEDED FOR SOHCAHTOA
       , rightSide = 0
       , leftSide = 0
       , bottom = 0
       , topAngle = 0
       , rightAngle = 0
       , leftAngle = 0
       , errorMsg = " "
       -- NEEDED FOR BASICTRIG
       , btRightSide = 0
       , btLeftSide = 0
       , btBottom = 0
       , btTopAngle = 0
       , btRightAngle = 0
       , btLeftAngle = 90
       , btErrorMsg = " "
       }

myShapes model =
    case model.state of
        MainMenu  ->
            [ rect 192 128 |> filled orange |> makeTransparent 0.5
            , rect 192 128 |> outlined (solid 1) orange
            , text "Trigonometry Helper" |> size 20 |> centered |> filled black |> move (0, 30)
            , drawRButton 25 "SOHCAHTOA" lightYellow darkYellow |> move (-50, 0) |> notifyTap ToSOHCAHTOA
            , drawRButton 25 "SOHCAHTOA QUIZ" lightGreen darkGreen |> move (-50, -40) |> notifyTap ToSOHQuiz
            , drawRButton 25 "Sine Cosine visualizer" lightBlue blue |> move (0, -20) |> notifyTap ToSCViz
            , drawRButton 25 "SINECOSINE" (rgb 255 154 155) red |> move (50, 0) |> notifyTap ToSineCosine
            , drawRButton 25 "SINECOSINE QUIZ" (rgb 213 139 232) darkPurple |> move (50, -40) |> notifyTap ToSCQUIZ
            ]
        SOHCAHTOAQuiz  ->
            [ drawSCT model
            ]
        SineCosineQuiz  ->
            [ drawSCQ model
            ]
        SineCosine ->
            [ drawSC model
            ]
        SOHCAHTOA  ->
            [ drawBT model
            ]
        SineCosineVisualizer  ->
            [ drawFuncVis model 
            ]

type Msg = Tick Float GetKeyState
         | ToMenu 
         | ToSineCosine 
         | ToSCQUIZ 
         | ToSCViz 
         | ToSOHCAHTOA 
         | ToSOHQuiz 
         -- NEEDED FOR SOHCAHTOA QUIZ
         | RightAnswer
         | WrongAnswer
         | GenerateQuestion
         | UpdateScreen Int Int QFormat Trig Int
         -- NEEDED FOR SINE COSINE QUIZ
         | SCQRightAnswer
         | SCQWrongAnswer
         | SCQGenerateQuestion
         | SCQUpdateScreen Int Int Int Int Int
         -- NEEDED FOR FUNCTION VISUALIZER
         | StartAnimating
         | StopAnimating
         -- NEEDED FOR SOHCAHTOA
         | Increase Int
         | Decrease Int
         | Calculate
         | Reset
         -- NEEDED FOR BASICTRIG
         | BTIncrease Int
         | BTDecrease Int
         | BTCalculate
         | BTReset

type State = MainMenu 
           | SOHCAHTOAQuiz 
           | SineCosineQuiz 
           | SineCosine
           | SOHCAHTOA 
           | SineCosineVisualizer 

update msg model =
    case msg of
        Tick t _ ->
          case model.state of
            SineCosineVisualizer ->
              case model.animationButton of
                Play -> ( {model | time = t}, Cmd.none )
                Stop -> ( { model| time = t, angle = visGetAngle model.angle model.posS, posRed =   moveRedDot model.angle model.posRed, posS = moveSinDot model.angle  model.posS, posC = moveCosDot model.angle  model.posC}, Cmd.none )
            _ ->            
              ( { model | time = t }, Cmd.none )
        ToMenu  ->
            case model.state of
                SOHCAHTOAQuiz  ->
                   ( { model | state = MainMenu  }, Cmd.none )

                SineCosineQuiz  ->
                   ( { model | state = MainMenu  }, Cmd.none )

                SineCosine ->
                   ( { model | state = MainMenu  }, Cmd.none )

                SOHCAHTOA  ->
                   ( { model | state = MainMenu  }, Cmd.none )

                SineCosineVisualizer  ->
                   ( { model | state = MainMenu  }, Cmd.none )

                otherwise ->
                   ( model, Cmd.none )
        ToSineCosine  ->
            case model.state of
                MainMenu  ->
                   ( { model | state = SineCosine }, Cmd.none )

                otherwise ->
                   ( model, Cmd.none )
        ToSCQUIZ  ->
          ( { model | state = SineCosineQuiz  }, Cmd.none )
        ToSCViz  ->
            case model.state of
                MainMenu  ->
                   ( { model | state = SineCosineVisualizer  }, Cmd.none )

                otherwise ->
                   ( model, Cmd.none )
        ToSOHCAHTOA  ->
            case model.state of
                MainMenu  ->
                   ( { model | state = SOHCAHTOA  }, Cmd.none )

                otherwise ->
                   ( model, Cmd.none )
        ToSOHQuiz  ->
          ( { model | state = SOHCAHTOAQuiz  }, Cmd.none )
        --SOHCAHTOA QUIZ CMDS
        RightAnswer ->
          ( { model | score = model.score + 1, clr = lightGreen }, cmdScreen model)
      
        WrongAnswer ->
          ( { model | score = model.score - 1, clr = lightRed }, cmdScreen model)
      
        UpdateScreen newX newY nextFormat nextTrig newPos ->
          ( { model | sideX = newX, sideY = newY, qFormat = nextFormat, trig = nextTrig, answerPos = newPos}, Cmd.none )
    
        GenerateQuestion ->
          ( {model | limit = (if model.limit < 9 then model.limit + 1 else model.limit)}, cmdScreen model)
        --SINECOSINE QUIZ CMDS
        SCQRightAnswer ->
          ( { model | scqScore = model.scqScore + 1, scqClr = lightPurple, scqQFormat = (if model.scqQFormat == Sine then Cosine else Sine)}, scqCmdScreen model)
      
        SCQWrongAnswer ->
          ( { model | scqScore = model.scqScore - 1, scqClr = lightRed }, scqCmdScreen model)
      
        SCQUpdateScreen newX1 newY1 newX2 newY2 newPos ->
          ( { model | scqSideX1 = newX1, scqSideY1 = newY1, scqSideX2 = newX2, scqSideY2 = newY2, scqAnswerPos = newPos}, Cmd.none )
    
        SCQGenerateQuestion ->
          ( {model | scqLimit = (if model.scqLimit < 9 then model.scqLimit + 1 else model.scqLimit)}, scqCmdScreen model)
        --FUNCTION VISUALIZER CMDS
        StartAnimating ->
          ( { model | animationButton = Stop}, Cmd.none )
        StopAnimating ->
          ( { model | animationButton = Play}, Cmd.none )
        Increase idx -> 
          ( {model | rightSide = (if idx == 1 then model.rightSide + 1 else model.rightSide)
            , leftSide = (if idx == 2 then model.leftSide + 1 else model.leftSide)
            , bottom = (if idx == 3 then model.bottom + 1 else model.bottom)
            , topAngle = (if idx == 4 then (if model.topAngle <= 177 then model.topAngle + 1 else model.topAngle) else model.topAngle)
            , leftAngle = (if idx == 5 then (if model.leftAngle <= 177 then model.leftAngle + 1 else model.leftAngle) else model.leftAngle)
            , rightAngle = (if idx == 6 then (if model.rightAngle <= 177 then model.rightAngle + 1 else model.rightAngle) else model.rightAngle)
            }, Cmd.none )
        Decrease idx -> 
          ( {model | rightSide = (if idx == 1 then (if model.rightSide >= 1 then model.rightSide - 1 else model.rightSide) else model.rightSide)
            , leftSide = (if idx == 2 then (if model.leftSide >= 1 then model.leftSide - 1 else model.leftSide) else model.leftSide)
            , bottom = (if idx == 3 then (if model.bottom >= 1 then model.bottom - 1 else model.bottom) else model.bottom)
            , topAngle = (if idx == 4 then (if model.topAngle >= 1 then model.topAngle - 1 else model.topAngle) else model.topAngle)
            , leftAngle = (if idx == 5 then (if model.leftAngle >= 1 then model.leftAngle - 1 else model.leftAngle) else model.leftAngle)
            , rightAngle = (if idx == 6 then (if model.rightAngle >= 1 then model.rightAngle - 1 else model.rightAngle) else model.rightAngle)
            }, Cmd.none )
        Calculate -> 
          ( {model | errorMsg = (if (((model.topAngle + model.leftAngle + model.rightAngle) > 180) || ((model.topAngle + model.leftAngle) > 179) || ((model.topAngle + model.rightAngle) > 179) || ((model.leftAngle + model.rightAngle) > 179))
                                 then "Incorrect Angles, Sum Exceeds 180! Try Again!"
                                 else if ((model.topAngle == 0 && model.leftAngle == 0 && model.rightAngle == 0) && (model.leftSide == 0 || model.rightSide == 0 || model.bottom == 0))
                                 then "Not Enough Information, Provide At Least One Angle!"
                                 else if (model.leftSide == 0 && model.rightSide == 0 && model.bottom == 0)
                                 then "Not Enough Information, Provide At Least One Side!"
                                 else if (isNaN model.leftAngle || isNaN model.rightAngle || isNaN model.topAngle)
                                 then "Imaginary Triangle, Proportions Do Not Form A Triangle!"
                                 else " ")
                   , topAngle = (if(model.topAngle == 0 && model.leftAngle > 0 && model.rightAngle > 0 && model.errorMsg /= "Incorrect Angles, sum exceeds 180! Try Again!") 
                                 then (180 - model.leftAngle - model.rightAngle)
                                 else if (model.topAngle == 0 && model.leftSide > 0 && model.rightSide > 0 && model.bottom > 0) 
                                 then (toFloat (round ((18000/pi)*(acos ((1/(2*model.rightSide * model.leftSide))*(model.rightSide^2 + model.leftSide^2 - model.bottom^2))))))/100 
                                 else if (model.topAngle == 0 && model.bottom > 0 && ((model.leftSide > 0 && model.rightAngle > 0) || (model.rightSide > 0 && model.leftAngle > 0)))
                                 then (if (model.rightSide > 0 && model.leftAngle > 0) 
                                 then (toFloat(round((180/pi)*(asin((model.bottom*((sin(model.leftAngle*(pi/180)))/model.rightSide)))))))
                                 else (toFloat(round((180/pi)*(asin((model.bottom*((sin(model.rightAngle*(pi/180)))/model.leftSide))))))))
                                 else model.topAngle)
                   , leftAngle = (if(model.leftAngle == 0 && model.topAngle > 0 && model.rightAngle > 0 && model.errorMsg /= "Incorrect Angles, sum exceeds 180! Try Again!") 
                                  then (180 - model.topAngle - model.rightAngle)
                                  else if (model.leftAngle == 0 && model.leftSide > 0 && model.rightSide > 0 && model.bottom > 0) 
                                  then (toFloat (round ((18000/pi)*(acos ((1/(2*model.bottom * model.leftSide))*(model.bottom^2 + model.leftSide^2 - model.rightSide^2))))))/100 
                                  else if (model.leftAngle == 0 && model.rightSide > 0 && ((model.leftSide > 0 && model.rightAngle > 0) || (model.bottom > 0 && model.topAngle > 0)))
                                  then (if (model.bottom > 0 && model.topAngle > 0) 
                                  then (toFloat(round((180/pi)*(asin((model.rightSide*((sin(model.topAngle*(pi/180)))/model.bottom)))))))
                                  else (toFloat(round((180/pi)*(asin((model.rightSide*((sin(model.rightAngle*(pi/180)))/model.leftSide))))))))
                                  else model.leftAngle)
                   , rightAngle = (if(model.rightAngle == 0 && model.topAngle > 0 && model.rightAngle > 0 && model.errorMsg /= "Incorrect Angles, sum exceeds 180! Try Again!") 
                                  then (180 - model.topAngle - model.bottom)
                                  else if (model.rightAngle == 0 && model.leftSide > 0 && model.rightSide > 0 && model.bottom > 0) 
                                  then (toFloat (round ((18000/pi)*(acos ((1/(2*model.bottom * model.rightSide))*(model.bottom^2 + model.rightSide^2 - model.leftSide^2))))))/100 
                                  else if (model.rightAngle == 0 && model.leftSide > 0 && ((model.rightSide > 0 && model.leftAngle > 0) || (model.bottom > 0 && model.topAngle > 0)))
                                  then (if (model.bottom > 0 && model.topAngle > 0) 
                                  then (toFloat(round((180/pi)*(asin((model.leftSide*((sin(model.topAngle*(pi/180)))/model.bottom)))))))
                                  else (toFloat(round((180/pi)*(asin((model.leftSide*((sin(model.leftAngle*(pi/180)))/model.rightSide))))))))
                                  else model.rightAngle)
                   , leftSide = (if(model.leftSide == 0 && model.rightSide > 0 && model.leftAngle > 0 && model.rightAngle > 0)
                                 then (toFloat(round(((model.rightSide/(sin(model.leftAngle*(pi/180))))*sin(model.rightAngle*(pi/180))))))
                                 else (if (model.leftSide == 0 && model.bottom > 0 && model.topAngle > 0 && model.rightAngle > 0)
                                 then (toFloat(round(((model.bottom/sin(model.topAngle*(pi/180)))*sin(model.rightAngle*(pi/180))))))
                                 else model.leftSide))
                   , rightSide = (if(model.rightSide == 0 && model.topAngle > 0 && model.bottom > 0 && model.leftAngle > 0)
                                  then (toFloat(round(((model.bottom/sin(model.topAngle*(pi/180)))*sin(model.leftAngle*(pi/180))))))
                                  else (if (model.rightSide == 0 && model.leftSide > 0 && model.rightAngle > 0 && model.leftAngle > 0)
                                  then (toFloat(round(((model.leftSide/sin(model.rightAngle*(pi/180)))*sin(model.leftAngle*(pi/180))))))
                                  else model.rightSide))
                   , bottom = (if(model.bottom == 0 && model.leftSide > 0 && model.rightAngle > 0 && model.topAngle > 0)
                               then (toFloat(round(((model.leftSide/sin(model.rightAngle*(pi/180)))*sin(model.topAngle*(pi/180))))))
                               else (if (model.bottom == 0 && model.rightSide > 0 && model.leftAngle > 0 && model.topAngle > 0)
                               then (toFloat(round(((model.rightSide/sin(model.leftAngle*(pi/180)))*sin(model.topAngle*(pi/180))))))
                               else model.bottom))}, Cmd.none )
        Reset -> ( {model | topAngle = 0, leftAngle = 0, rightAngle = 0, leftSide = 0, rightSide = 0, bottom = 0}, Cmd.none )
        BTIncrease idx -> ( {model | btRightSide = (if idx == 1 then model.btRightSide + 1 else model.btRightSide)
                                   , btLeftSide = (if idx == 2 then model.btLeftSide + 1 else model.btLeftSide)
                                   , btBottom = (if idx == 3 then model.btBottom + 1 else model.btBottom)
                                   , btTopAngle = (if idx == 4 then (if model.btTopAngle <= 177 then model.btTopAngle + 1 else model.btTopAngle) else model.btTopAngle)
                                   , btLeftAngle = (if idx == 5 then (if model.btLeftAngle <= 177 then model.btLeftAngle + 1 else model.btLeftAngle) else model.btLeftAngle)
                                   , btRightAngle = (if idx == 6 then (if model.btRightAngle <= 177 then model.btRightAngle + 1 else model.btRightAngle) else model.btRightAngle)
                                   }, Cmd.none )
        BTDecrease idx -> ( {model | btRightSide = (if idx == 1 then (if model.btRightSide >= 1 then model.btRightSide - 1 else model.btRightSide) else model.btRightSide)
                                   , btLeftSide = (if idx == 2 then (if model.btLeftSide >= 1 then model.btLeftSide - 1 else model.btLeftSide) else model.btLeftSide)
                                   , btBottom = (if idx == 3 then (if model.btBottom >= 1 then model.btBottom - 1 else model.btBottom) else model.btBottom)
                                   , btTopAngle = (if idx == 4 then (if model.btTopAngle >= 1 then model.btTopAngle - 1 else model.btTopAngle) else model.btTopAngle)
                                   , btLeftAngle = (if idx == 5 then (if model.btLeftAngle >= 1 then model.btLeftAngle - 1 else model.btLeftAngle) else model.btLeftAngle)
                                   , btRightAngle = (if idx == 6 then (if model.btRightAngle >= 1 then model.btRightAngle - 1 else model.btRightAngle) else model.btRightAngle)
                                   }, Cmd.none )
        BTCalculate -> ( {model | btErrorMsg = (if (((model.btLeftSide) > 0) && ((model.btRightSide) > 0) && ((model.btBottom) > 0) )
                                                then "Incorrect Inputs! Please only enter 2 sides!"
                                                else if (model.btLeftSide == 0 && model.btRightSide == 0 && model.btBottom == 0)
                                                then "Not Enough Information, Provide  Two Sides!"
                                                else if ((model.btLeftSide == 0 && model.btRightSide == 0) || (model.btRightSide == 0 && model.btBottom == 0) || (model.btLeftSide == 0 && model.btBottom == 0))
                                                then "Not Enough Information, Provide one more Side!"
                                                else if ((model.btLeftSide >= model.btRightSide && model.btRightSide/=0) || (model.btBottom >= model.btRightSide && model.btRightSide/=0))
                                                then "The Hypoteneuse must be the Largest Side!"
                                                else " ")
                                                
                                     
                                   , btLeftSide = (if((model.btBottom < model.btRightSide) && (model.btRightSide > 0 && model.btBottom > 0))
                                                 then ((((toFloat (round (100*(sqrt((model.btRightSide * model.btRightSide)-(model.btBottom * model.btBottom))))))/100)))
                                                 else model.btLeftSide)
                                   
                                   , btRightSide = (if((model.btBottom > 0 && model.btLeftSide > 0))
                                                 then ((((toFloat (round (100*(sqrt((model.btLeftSide * model.btLeftSide)+(model.btBottom * model.btBottom))))))/100)))
                                                 else model.btRightSide)
                                                 
                                   , btBottom = (if((model.btLeftSide < model.btRightSide) && (model.btRightSide > 0 && model.btLeftSide > 0))
                                                 then ((((toFloat (round (100*(sqrt((model.btRightSide * model.btRightSide)-(model.btLeftSide * model.btLeftSide))))))/100)))
                                                 else model.btBottom)
                                   , btTopAngle = findAngle model.btTopAngle model.btBottom model.btLeftSide model.btRightSide
                                   , btRightAngle = findAngle model.btRightAngle model.btLeftSide model.btBottom model.btRightSide
                                   }, Cmd.none)
                                 
                                                                   
        BTReset -> ( {model | btTopAngle = 0 
                              , btLeftAngle = 90
                              , btRightAngle = 0
                              , btLeftSide = 0
                              , btRightSide = 0
                              , btBottom = 0
                              }, Cmd.none)

findAngle angle opp adj hyp = 
  (if (opp > 0 && adj > 0) then (toFloat (round (18000*(atan2 (opp) (adj))/pi))/100)
   else if (opp > 0 && hyp > 0) then (toFloat (round (18000*(asin (opp/hyp))/pi))/100)
   else if (adj > 0 && hyp > 0) then (toFloat (round (18000*(acos (adj/hyp))/pi))/100)
   else angle)
   
scqCmdScreen model = Random.map5 SCQUpdateScreen scqRandX1 scqRandY1 scqRandX2 scqRandY2 (scqRandPos model)
             |> Random.generate ( identity )
cmdScreen model = Random.map5 UpdateScreen randX randY randFormat randTrig (randPos model)
             |> Random.generate ( identity )

-- RANDOM
-- FOR SOHCAHTOA QUIZ
randY = Random.int 20 50
randX = Random.int 20 90
randFormat = Random.uniform Ratio [Side, Angle]
randTrig = Random.uniform Sin [Cos, Tan]
randPos model = Random.int 0 (model.limit - 1)
-- FOR SINE COSINE QUIZ
scqRandX1 = Random.int (-40) (-20)
scqRandY1 = Random.int (-40) (-20)
scqRandX2 = Random.int (20) (40)
scqRandY2 = Random.int (-40) (-20)
scqRandPos model = Random.int 0 (model.scqLimit - 1)


drawButton h txt lightClr darkClr = group
  [ rect 45 h |> filled lightClr --(rgb 213 139 232)  
  , rect 45 h |> outlined (solid 1) darkClr -- (rgb 213 139 232) darkPurple
  , text txt |> size 5 |> centered |> filled black  |> move (0,-2)
  ]

drawRButton h txt lightClr darkClr = group
  [ roundedRect 45 h 10 |> filled lightClr --(rgb 213 139 232)  
  , roundedRect 45 h 10 |> outlined (solid 1) darkClr -- (rgb 213 139 232) darkPurple
  , text txt |> size 5 |> centered |> filled black  |> move (0,-2)
  ]
  
--SOURCECODE FOR STATES:
-- SOHCAHTOA QUIZ STATE
drawSCT model = group
  [ rect 192 128 |> filled model.clr |> makeTransparent 0.5
  , text ("Score : " ++ String.fromInt model.score) |> alignRight |> filled black |> move (90,50)
  , text model.result |> alignRight |> filled black |> move (-15, 45)
  , drawButton 20 "Back to Menu" lightGreen darkGreen |> move (70, -50) |> notifyTap ToMenu
  , drawButton 20 "Increase Difficulty" lightGreen darkGreen |> move (70, -25) |> notifyTap GenerateQuestion
  , drawInfo
  , drawTrigQuestion model |> move (0, 2)
  , drawTAnswer (toFloat model.sideX) (toFloat model.sideY) model.qFormat model.trig model.answerPos 0 0 (model.limit) |> group |> move (-75, 35)
  , drawTriangle (toFloat model.sideX) (toFloat model.sideY) (model.qFormat) (toFloat model.score)|> move (-80, -55)
  --, text (getAnswer (toFloat model.sideX) (toFloat model.sideY) model.qFormat model.trig) |> size 5 |> filled black |> move (20, 50)
  ]

drawInfo = group 
  [ rect 45 50 |> outlined (solid 1) black |> move (70, 15)
  , text "Formulas:" |> size 10 |> filled black |> move (50, 30)
  , text "Sin = opposite/hypotenuse" |> size 4 |> filled black |> move (49, 20)
  , text "Cos = adjacent/hypotenuse" |> size 4 |> filled black |> move (49, 10)
  , text "Tan = opposite/adjacent" |> size 4 |> filled black |> move (51, 0)
  ]

drawTrigQuestion model = group
  [ roundedRect 100 15 5 |> filled (rgb 255 253 208) |> move (-40, 50)
  , text (changeQuestionText model.qFormat model.trig) |> centered |> size 8 |> filled black |> move (-40, 47)
  ] 

changeQuestionText format trig = 
  case format of 
    (Ratio) ->
       (String.append (String.append "Find the ratio of " (Debug.toString trig)) " X.")
    (Side) ->
      "Find the missing side."
    (Angle) ->
      "Find angle X."

drawTriangle x y format score = group
  [ polygon [ (0,0) , (x,0) , (0,y) ]
    |> filled lightGreen
  , polygon [ (0,0) , (x,0) , (0,y) ]
    |> outlined (solid 1) darkGreen
  , rect 4 4
    |> outlined (solid 1) darkGreen
    |> move (2, 2)
  , drawAuxInfo x y format score
  ]

drawAuxInfo x y format score =
  let 
    version = (modBy 2 (round (x + y)) == 0)
    version2 = (modBy 2 (round (x + y + score)) == 0)
    version3 = modBy 3 (round (x + y))
    z = (toFloat (round (10 * sqrt (x^2 + y^2))))/10
  in 
  case format of 
    (Ratio) -> group --One of angles should be Letter, 
      [ text "X"|> centered |> size 5 |> filled black |> move (if version then (-6, y) else (x, -6))
      , text (String.fromFloat y) |> centered |> size 5 |> filled black |> move (-6, y/2)
      , text (String.fromFloat x) |> centered |> size 5 |> filled black |> move (x/2, -6)
      , text (String.fromFloat z) |> centered |> size 5 |> filled black |> move (x/2 + 4, y/2 + 4)
      ]
    (Side) ->
      case version3 of
        (1) -> group
          [ text "A" |> centered |> size 5 |> filled black |> move (-6, y/2)
          , text (String.append (String.fromFloat (getAngle y x)) ("°"))|> centered |> size (if version2 then 5 else 0) |> filled black |> move (-6, y)
          , text (String.append (String.fromFloat (getAngle x y)) ("°"))|> centered |> size (if version2 then 0 else 5) |> filled black |> move (x, -6)
          , text (String.fromFloat x) |> centered |> size (if version then 0 else 5) |> filled black |> move (x/2, -6)
          , text (String.fromFloat z) |> centered |> size (if version then 5 else 0) |> filled black |> move (x/2 + 4, y/2 + 4)
          ]
        (2) -> group
          [ text "B" |> centered |> size 5 |> filled black |> move (x/2, -6)
          , text (String.append (String.fromFloat (getAngle y x)) ("°"))|> centered |> size (if version2 then 5 else 0) |> filled black |> move (-6, y)
          , text (String.append (String.fromFloat (getAngle x y)) ("°"))|> centered |> size (if version2 then 0 else 5) |> filled black |> move (x, -6)
          , text (String.fromFloat y) |> centered |> size (if version then 0 else 5) |> filled black |> move (-6, y/2)
          , text (String.fromFloat z) |> centered |> size (if version then 5 else 0) |> filled black |> move (x/2 + 4, y/2 + 4)
          ]
        (_) -> group
          [ text "C" |> centered |> size 5 |> filled black |> move (x/2 + 4, y/2 + 4)
          , text (String.append (String.fromFloat (getAngle y x)) ("°"))|> centered |> size (if version2 then 5 else 0) |> filled black |> move (-6, y)
          , text (String.append (String.fromFloat (getAngle x y)) ("°"))|> centered |> size (if version2 then 0 else 5) |> filled black |> move (x, -6)
          , text (String.fromFloat y) |> centered |> size (if version then 5 else 0) |> filled black |> move (-6, y/2)
          , text (String.fromFloat x) |> centered |> size (if version then 0 else 5) |> filled black |> move (x/2, -6)
          ]
    (Angle) -> group
      [ text "X"|> centered |> size 5 |> filled black |> move (if version then (-6, y) else (x, -6))
      , text (String.fromFloat y) |> centered |> size (if version3 == 0 then 0 else 5) |> filled black |> move (-6, y/2)
      , text (String.fromFloat x) |> centered |> size (if version3 == 1 then 0 else 5) |> filled black |> move (x/2, -6)
      , text (String.fromFloat z) |> centered |> size (if version3 == 2 then 0 else 5) |> filled black |> move (x/2 + 4, y/2 + 4)
      ]
      
getAngle x y  = (toFloat (round (1800*(atan2 y x)/pi)))/10
getWAngle x y e =  (toFloat (round (e + (1800*(atan2 y x)/pi)))/10)

getAnswer x y format trig = 
  let 
    version = (modBy 2 (round (x + y)) == 0)
    version3 = modBy 3 (round (x + y))
    z = (toFloat (round (10 * sqrt (x^2 + y^2))))/10
  in 
  case format of 
    (Ratio) ->
      case trig of
        (Sin) -> (if version then getRatio x z else getRatio y z)
        (Cos) -> (if version then getRatio y z else getRatio x z)
        (Tan) -> (if version then getRatio x y else getRatio y x)
    (Side) -> 
      case version3 of
        (1) -> (String.fromFloat y)
        (2) -> (String.fromFloat x)
        (_) -> (String.fromFloat z)
    (Angle) -> 
      (if version then (String.append (String.fromFloat (getAngle y x)) ("°")) 
        else (String.append (String.fromFloat (getAngle x y)) ("°")))
        
getWAnswer x y format trig error = 
  let 
    version = (modBy 2 (round (x + y)) == 0)
    version3 = modBy 3 (round (x + y))
    z = (toFloat (round (10 * sqrt (x^2 + y^2))))/10
    e = toFloat error
  in 
  case format of 
    (Ratio) ->
      case trig of
        (Sin) -> (if version then getWRatio x z e else getWRatio y z e)
        (Cos) -> (if version then getWRatio y z e else getWRatio x z e)
        (Tan) -> (if version then getWRatio x y e else getWRatio y x e)
    (Side) -> 
      case version3 of
        (1) -> (String.fromFloat (y + e))
        (2) -> (String.fromFloat (x + e))
        (_) -> (String.fromFloat (z + e))
    (Angle) -> 
      (if version then (String.append (String.fromFloat (getWAngle y x e)) ("°")) 
        else (String.append (String.fromFloat (getWAngle x y e)) ("°")))

getRatio top bottom = 
  let 
    t = round top
    b = (toFloat (round (10 * bottom)))/10
  in
    (String.append (String.append (String.fromInt t) " / ") (Debug.toString b))

getWRatio top bottom error = 
  let 
    t = round (top + error)
    b = (toFloat (round (10 * bottom)))/10
  in
    (String.append (String.append (String.fromInt t) " / ") (Debug.toString b))

drawTAnswer x y format trig randOrder posX i l=
  if i < l then
    case randOrder of
      (0) ->
        [roundedRect 30 10 5 |> filled (rgb 255 253 208) |> move (if i < 3 then (posX, 0) else if i < 6 then (posX - 105, -15) else (posX - 210, -30) ) 
        |> notifyTap RightAnswer
        , text (getAnswer x y format trig) |> centered |> size 5 |> filled black |> move (if i < 3 then (posX, -2) else if i < 6 then (posX - 105, -17) else (posX - 210, -32)) 
        |> notifyTap RightAnswer
        ]
        ++ drawTAnswer x y format trig (randOrder-1) (posX + 35) (i+1) l
      (_) ->
        [roundedRect 30 10 5 |> filled (rgb 255 253 208) |> move (if i < 3 then (posX, 0) else if i < 6 then (posX - 105, -15) else (posX - 210, -30) )
        |> notifyTap WrongAnswer
        , text (getWAnswer x y format trig randOrder) |> centered |> size 5 |> filled black |> move (if i < 3 then (posX, -2) else if i < 6 then (posX - 105, -17) else (posX - 210, -32))
        |> notifyTap WrongAnswer
        ]
        ++ drawTAnswer x y format trig (randOrder-1) (posX + 35) (i+1) l
  else
    []
    

--SINE COSINE QUIZ STATE
drawSCQ model = group
  [ rect 192 128 |> filled model.scqClr |> makeTransparent 0.5
  , text ("Score : " ++ String.fromInt model.scqScore) |> alignRight |> filled black |> move (90,50)
  , text model.scqResult |> alignRight |> filled black |> move (-15, 45)
  , drawButton 20 "Back to Menu" (rgb 213 139 232) darkPurple |> move (70, -50) |> notifyTap ToMenu
  , drawButton 20 "Increase Difficulty" (rgb 213 139 232) darkPurple |> move (70, -25) |> notifyTap SCQGenerateQuestion
  , drawSCQInfo
  , drawSCQQuestion model |> move (0, 2)
  , drawSCQTAnswer (toFloat model.scqSideX1, toFloat model.scqSideY1) (toFloat model.scqSideX2, toFloat model.scqSideY2) model.scqQFormat model.scqAnswerPos 0 0 (model.scqLimit) |> group |> move (-75, 35)
  , drawSCQTriangle (getSCQCoords model 1) (getSCQCoords model 2) (model.scqQFormat)|> move (centreCentroid (getSCQCoords model 1) (getSCQCoords model 2))
  ]

drawSCQInfo = group 
  [ rect 45 50 |> outlined (solid 1) black |> move (70, 15)
  , text "Formulas:" |> size 10 |> filled black |> move (50, 30)
  , text "Sine Law:" |> size 4 |> centered |> filled black |> move (70, 20)
  , text "Sin(A)/a = Sin(B)/b" |> size 4 |> centered |> filled black |> move (70, 15)
  , text "Cosine Law:" |> size 4 |> centered |> filled black |> move (70, 5)
  , text "C^2 = A^2 + B^2 - 2AB*cos(c)" |> size 3.25 |> centered |> filled black |> move (70, 0)
  ]

drawSCQQuestion model = group
  [ roundedRect 100 15 5 |> filled (rgb 255 253 208) |> move (-40, 50)
  , text (changeSCQText model.scqQFormat model.scqSideX1 model.scqSideX2) |> centered |> size 8 |> filled black |> move (-40, 47)
  ] 

changeSCQText format x1 x2 =
  let
    version = modBy 3 (x1 + x2)
    var = if version == 1 then "A" else if version == 2 then "B" else "C"
  in
  case format of 
    (Sine) ->
       String.append "Find side " var
    (Cosine) ->
       String.append "Find angle " var
       
drawSCQTAnswer (x1,y1) (x2,y2) format randOrder posX i l=
  if i < l then
    case randOrder of
      (0) ->
        [roundedRect 30 10 5 |> filled (rgb 255 253 208) |> move (if i < 3 then (posX, 0) else if i < 6 then (posX - 105, -15) else (posX - 210, -30) ) 
        |> notifyTap SCQRightAnswer
        , text (getSCQAnswer (x1, y1) (x2, y2) format) |> centered |> size 5 |> filled black |> move (if i < 3 then (posX, -2) else if i < 6 then (posX - 105, -17) else (posX - 210, -32)) 
        |> notifyTap SCQRightAnswer
        ]
        ++ drawSCQTAnswer (x1,y1) (x2,y2) format (randOrder-1) (posX + 35) (i+1) l
      (_) ->
        [roundedRect 30 10 5 |> filled (rgb 255 253 208) |> move (if i < 3 then (posX, 0) else if i < 6 then (posX - 105, -15) else (posX - 210, -30) )
        |> notifyTap SCQWrongAnswer
        , text (getWSCQAnswer (x1, y1) (x2, y2) format randOrder) |> centered |> size 5 |> filled black |> move (if i < 3 then (posX, -2) else if i < 6 then (posX - 105, -17) else (posX - 210, -32))
        |> notifyTap SCQWrongAnswer
        ]
        ++ drawSCQTAnswer (x1,y1) (x2,y2) format (randOrder-1) (posX + 35) (i+1) l
  else
    []

centreCentroid (x1, y1) (x2, y2) = 
  let
    cx = (x1 + x2)/3
    cy = (y1 + y2)/3
  in
    (negate (43 + cx), negate (37 + cy)) 

drawSCQTriangle (x1, y1) (x2, y2) format = 
  let
    cx = (x1 + x2)/3
    cy = (y1 + y2)/3
    dx = x2 - x1
  in group
  [ polygon [ (0,0) , (x1,y1) , (x2,y2) ]
    |> filled (rgb 215 139 232)
  , polygon [ (0,0) , (x1,y1) , (x2,y2) ]
    |> outlined (solid 1) darkPurple
  , drawSCQTriangleInfo (x1,y1) (x2,y2) format
  ]
  
getSCQCoords model n =
  case n of
    (1) ->
      ((toFloat model.scqSideX1), (toFloat model.scqSideY1))
    (_) ->
      ((toFloat model.scqSideX2), (toFloat model.scqSideY2))

sideLength (x1, y1) (x2, y2) = 
  let 
    dx = abs (x1 - x2)
    dy = abs (y1 - y2)
  in
    toFloat (round (100 * sqrt (dx^2 + dy^2)))/100
    
drawSCQTriangleInfo (x1,y1) (x2,y2) format =
  let 
    cx = (x1 + x2)/3
    cy = (y1 + y2)/3 
    version = modBy 3 (round (x1 + x2))
    version2 = (modBy 2 (round (y1 + y2)) == 0)
  in 
  case format of 
    (Sine) -> 
      case version of
        (1) -> group
          [ text "A" |> size 5 |> filled black |> move (cx - 4, 2*cy)
          , text (String.fromFloat (sideLength (0, 0) (x2, y2))) |> size (if version2 then 5 else 0) |> filled black |> move (x2/2 + 10, cy + 10)
          , text (String.fromFloat (sideLength (x1, y1) (0, 0))) |> size (if version2 then 0 else 5) |> alignRight |> filled black |> move (x1/2 - 10, cy + 10)
          , auxInfo (x1, y1) (x2, y2) format]
        (2) -> group
          [ text "B" |> size 5 |> filled black |> move (x2/2 + 10, cy + 10) 
          , text (String.fromFloat (sideLength (x1, y1) (x2, y2))) |> size (if version2 then 5 else 0) |> filled black |> move (cx - 4, 2*cy)
          , text (String.fromFloat (sideLength (x1, y1) (0, 0))) |> size (if version2 then 0 else 5) |> alignRight |> filled black |> move (x1/2 - 10, cy + 10)
          , auxInfo (x1, y1) (x2, y2) format]
        (_) -> group
          [ text "C" |> size 5 |> filled black |> move (x1/2 - 10, cy + 10)
          , text (String.fromFloat (sideLength (x1, y1) (x2, y2))) |> size (if version2 then 5 else 0) |> filled black |> move (cx - 4, 2*cy)
          , text (String.fromFloat (sideLength (0, 0) (x2, y2))) |> size (if version2 then 0 else 5) |> filled black |> move (x2/2 + 10, cy + 10)
          , auxInfo (x1, y1) (x2, y2) format]
    (Cosine) -> 
      case version of
        (1) -> group
          [ text "A" |> size 5 |> filled black |> move (1, 1)
          , auxInfo (x1, y1) (x2, y2) format]
        (2) -> group
          [ text "B" |> size 5 |> alignRight |> filled black |> move (x1 - 1, y1)
          , auxInfo (x1, y1) (x2, y2) format]
        (_) -> group
          [ text "C" |> size 5 |> filled black |> move (x2 + 2, y2)
          , auxInfo (x1, y1) (x2, y2) format]

auxInfo (x1,y1) (x2,y2) format = 
  let 
    cx = (x1 + x2)/3
    cy = (y1 + y2)/3 
  in 
  case format of
    (Cosine) -> group
      [ text (String.fromFloat (sideLength (x1, y1) (x2, y2))) |> size 5 |> filled black |> move (cx - 4, 2*cy)
      , text (String.fromFloat (sideLength (0, 0) (x2, y2))) |> size 5 |> filled black |> move (x2/2 + 10, cy + 10)
      , text (String.fromFloat (sideLength (0, 0) (x1, y1))) |> size 5 |> alignRight |> filled black |> move (x1/2 - 10, cy + 10)]
    (_) -> group
      [ text (String.append (String.fromFloat (getSCQAngle (0, 0) (x1, y1) (x2,y2))) "°") |> size 5 |> centered |> filled black |> move (1, 1)
      , text (String.append (String.fromFloat (getSCQAngle (x1, y1) (x2,y2) (0, 0))) "°") |> size 5 |> alignRight |> filled black |> move (x1, y1)
      , text (String.append (String.fromFloat (getSCQAngle (x2,y2) (x1, y1) (0, 0))) "°") |> size 5 |> alignLeft |> filled black |> move (x2 + 2, y2)]

getSCQAngle a b c =
  let
    side1 = sideLength a b
    side2 = sideLength a c
    oppositeSide = sideLength b c
  in
  toFloat (round ((18000/pi)*(acos ((1/(2*side1 * side2))*(side1^2 + side2^2 - oppositeSide^2)))))/100
  
getSCQAnswer (x1, y1) (x2, y2) format = 
  let 
    version = modBy 3 (round (x1 + x2))
  in 
  case format of 
    (Sine) ->
      case version of
        (1) -> String.fromFloat (sideLength (x1, y1) (x2, y2))
        (2) -> String.fromFloat (sideLength (0, 0) (x2, y2))
        (_) -> String.fromFloat (sideLength (0, 0) (x1, y1))
    (_) -> 
      case version of
        (1) -> String.append (String.fromFloat (getSCQAngle (0, 0) (x1,y1) (x2,y2))) "°"
        (2) -> String.append (String.fromFloat (getSCQAngle (x1,y1) (0, 0) (x2,y2))) "°"
        (_) -> String.append (String.fromFloat (getSCQAngle (x2,y2) (0, 0) (x1,y1))) "°"

getWSCQAnswer (x1, y1) (x2, y2) format error = 
  let 
    version = modBy 3 (round (x1 + x2))
    e = toFloat error
  in 
  case format of 
    (Sine) ->
      case version of
        (1) -> String.fromFloat ((toFloat (round (100*(e + sideLength (x1, y1) (x2, y2)))))/100)
        (2) -> String.fromFloat ((toFloat (round (100*(e + sideLength (0, 0) (x2, y2)))))/100)
        (_) -> String.fromFloat ((toFloat (round (100*(e + sideLength (0, 0) (x1, y1)))))/100)
    (_) -> 
      case version of
        (1) -> String.append (String.fromFloat ((toFloat (round (100*(e + getSCQAngle (0, 0) (x1,y1) (x2,y2)))))/100)) "°"
        (2) -> String.append (String.fromFloat ((toFloat (round (100*(e + getSCQAngle (x1,y1) (0, 0) (x2,y2)))))/100)) "°"
        (_) -> String.append (String.fromFloat ((toFloat (round (100*(e + getSCQAngle (x2,y2) (0, 0) (x1,y1)))))/100)) "°"

--FUNCTION VISUALIZER STATE

drawFuncVis model = group
  [ rect 192 130 |> filled (rgb 199 233 242)
  , drawButton 15 "Sine Animation" lightBlue blue |> scale 0.9 |> move(-73,55) 
  , drawButton2 model.animationButton |> move (40, -100)
  , group [
        group[
            circle 10
                  |> outlined (dotted 1) (black)
                  |> move (-70,0)
            ]
              |> move (20,0),
       group [
            drawXYline
            , 
            group [
              line (-100, 10) (100,10)
                |> outlined (solid 0.25) black
                |> makeTransparent 0.5
                |> move (50,0)
              ,line (-100, -10) (100,-10)
                |> outlined (solid 0.25) black
                |> makeTransparent 0.5
                |> move (50,0)
            ]
            |> move (0, 0.5)
            ,
            drawXlable
            ]
            |> move (-50,0)
         , drawSin(0, 0) (pi*4, 0) (2) blue
           |> scale 10
           |> move (-50,0)
         , drawDot red model.posRed
         , drawDot black model.posS
         , text model.debug |> alignRight |> filled (rgb 0 0 255)
     ]
       |> move (0,35)
  , drawButton 15 "Cosine Animation" lightBlue blue |> scale 0.9 |> move (-73,5)
  , group [
        group[
            circle 10
                  |> outlined (dotted 1) (black)
                  |> move (-70,0)
            ]
              |> move (20,0),
       group [
            drawXYline
            , 
            group [
              line (-100, 10) (100,10)
                |> outlined (solid 0.25) black
                |> makeTransparent 0.5
                |> move (50,0)
              ,line (-100, -10) (100,-10)
                |> outlined (solid 0.25) black
                |> makeTransparent 0.5
                |> move (50,0)
            ]
            |> move (0, 0.5)
            ,
            drawXlable
            ]
            |> move (-50,0)
         , drawCos(0, 1) (pi*4, 1) (2) blue
           |> scale 10
           |> move (-50,0)
         , drawDot darkGreen model.posRed
         , drawDot black model.posC
     ]
     |> move (0,-15)
  , drawButton 20 "Back to Menu" lightBlue blue |> move (70, -50) |> notifyTap ToMenu
  ]

drawButton2 status =
  case status of
    Play ->
      group[
       circle 7 |> filled (rgb 199 233 242) |>addOutline (solid 1) black,
       triangle 4 |> filled black
      ]
        |> move (-10, 50)
        |> notifyMouseUp StartAnimating
      
    Stop -> 
      group[
        circle 7 |> filled (rgb 199 233 242) |>addOutline (solid 1) black,
        rect 2 6 |> filled black |> move (-2,0),
        rect 2 6 |> filled black |> move (2,0)
        ]
        |> move (-10, 50)
        |> notifyMouseUp StopAnimating

drawXYline = 
  group[
    line (-100,0) (100,0)
      |> outlined (solid 0.75) black
      |> move (50,0)
    , line (0,-20) (0,20)
      |> outlined (solid 0.75) black
    , line (10 * pi, -20) (10 * pi, 20) 
      |> outlined (solid 0.25) black
    , line (20 * pi, -20) (20 * pi, 20) 
      |> outlined (solid 0.25) black
    , line (30 * pi, -20) (30 * pi, 20) 
      |> outlined (solid 0.25) black
    , line (40 * pi, -20) (40 * pi, 20) 
      |> outlined (solid 0.25) black
  ]
 
drawXlable =
  group[
            text "π/2"
            |>  filled black
            |> scale 0.5
            |> move (10*pi, -5)
            ,
            text "π"
            |>  filled black
            |> scale 0.5
            |> move (20*pi, -5)
            ,
            text "3π/2"
            |>  filled black
            |> scale 0.5
            |> move (30*pi, -5)
            ,
            text "2π"
            |>  filled black
            |> scale 0.5
            |> move (40*pi, -5)
            ,
            text "0"
            |>  filled black
            |> scale 0.5
            |> move (1, -5)
            ,
            text "1"
            |>  filled black
            |> scale 0.5
            |> move (1, 11)
            ,
            text "-1"
            |>  filled black
            |> scale 0.5
            |> move (1, -14)
    ]

drawSin (x0, y0) (x1, y1) height colour = 
  let
    dist = x0+x1
  in
    curve (x0, y0) [Pull (dist/4, height) (dist/2, 0), Pull (dist*3/4, (-1)*height) (x1, y1)]
      |> outlined (solid 0.1) colour     

drawCos (x0, y0) (x1, y1) height colour = 
  let
    dist = x0+x1
  in
    curve (x0, y0) 
      [Pull (y0, y0) (dist/4, 0)
       , Pull (dist/2, (-1)*height) (dist*3/4, 0)
       , Pull (x1-y1, y0) (x1, y1)           
      ]
      |> outlined (solid 0.1) colour  
      
drawDot clr pos = 
  circle 1.5
    |> filled clr
    |> move pos

moveRedDot angle (x,y) = (-50+  10* cos angle, 10 * sin angle )

moveSinDot angle (x,y) = 
    ( -50 + angle*pi*6.4 , 10 * sin angle)
    
moveCosDot angle (x,y) =
    ( -50 + angle*pi*6.4 , 10 * cos angle)

visGetAngle angle (x,y) = 
  if x > 76 then
    0
  else
    angle + 0.005

--SineCosine STATE
drawSC model = group
  [ rect 500 500|> filled (rgb 255 154 155) |> makeTransparent 0.5
  , drawCSTriangle model |> move (0, 7)
  , drawCSInfo |> scale 0.79 |> move (20, 20)
  , text "Cosine/Sine Law" |> filled black |> move(-80,45)
  , drawButton 20 "Calculate" (rgb 255 154 155) red |> move (-65, -50)
    |> notifyTap (Calculate)
    |> notifyTap (Calculate)
    |> notifyTap (Calculate)
  , drawButton 20 "Reset" (rgb 255 154 155) red |> move (-10, -50)
    |> notifyTap (Reset)
  , drawButton 20 "To Quiz" (rgb 255 154 155) red |> move (70, -25) 
    |> notifyTap (ToSCQUIZ)
  , drawButton 20 "Back to Menu" (rgb 255 154 155) red |> move (70, -50) 
    |> notifyTap (ToMenu)
  ]
  
drawCSTriangle model = group
  [ triangle 30
    |> outlined (solid 1) red
    |> rotate (degrees -30)
    |> scaleX 1.5
    |> move(-40,-3)
  , triangle 30
    |> filled (rgb 255 154 155)
    |> rotate (degrees -30)
    |> scaleX 1.5
    |> move(-40,-3)
  , rect 7 7 |> outlined (solid 0.2) black |> move(-15,10)
  , rect 7 7 |> outlined (solid 0.2) black |> move(-65,10)
  , rect 7 7 |> outlined (solid 0.2) black |> move(-40,-25)
  , rect 7 7 |> outlined (solid 0.2) black |> move(-15,-12)
  , rect 7 7 |> outlined (solid 0.2) black |> move(-65,-12)
  , rect 7 7 |> outlined (solid 0.2) black |> move(-43,15)
  , buttons 1 |> move(32,23.5) -- rightSide
  , buttons 2 |> move(-32,23) -- leftSide
  , buttons 3 |> move(7,-11.5) -- bottom
  , buttons 4 |> move(4,28.5) -- topAngle
  , buttons 5 |> move(-18,2) -- leftAngle
  , buttons 6 |> move(18,2) -- rightAngle   
  , text (String.fromFloat model.rightSide) |> filled black |> scale 0.35 |> move(-18,8.5)
  , text (String.fromFloat model.leftSide) |> filled black |> scale 0.35 |> move(-68,8.5)
  , text (String.fromFloat model.bottom) |> filled black |> scale 0.35 |> move(-43,-26.5)
  , text (String.fromFloat model.topAngle) |> filled black |> scale 0.35 |> move(-46,13.5)
  , text (String.fromFloat model.leftAngle) |> filled black |> scale 0.35 |> move(-68,-13.5)
  , text (String.fromFloat model.rightAngle) |> filled black |> scale 0.35 |> move(-18,-13.5)
  , text (model.errorMsg) |> filled (rgb 250 250 150) |> scale 0.35 |> move(-80,35)
  , text "Leave the unknowns to 0." |> filled black |> scale 0.4 |> move(-65,-35)
  ]
  
drawCSInfo = group
    [ rect 57 75 |> outlined (solid 1) black |> move(63,0)
    , text "Facts!" |> filled black |> move(40,25)
    , text "- Use cosine law when" |> filled black |> scale 0.35 |> move(40,17)
    , text "  3 sides are given." |> filled black |> scale 0.35 |> move(40,11)
    , text "- Use cosine law when" |> filled black |> scale 0.35 |> move(40,2)
    , text "  2 sides and a corresponding" |> filled black |> scale 0.35 |> move(40,-3.5)
    , text "  angle is given." |> filled black |> scale 0.35 |> move(40,-8.7)
    , text "- Use sine law when" |> filled black |> scale 0.35 |> move(40,-19)
    , text "  2 sides and a corresponding " |> filled black |> scale 0.35 |> move(40,-24)
    , text "  angle is given (vice versa)." |> filled black |> scale 0.35 |> move(40,-29)
    ]

buttons idx = group
  [
  triangle 2
    |> filled black
    |> rotate (degrees 90)
    |> move(-40,-12)
    |> notifyTap (Increase idx)
    ,
    triangle 2
    |> filled black
    |> rotate (degrees 90)
    |> scaleY -1
    |> move(-40,-15)
    |> notifyTap (Decrease idx)
  ]

--BASIC TRIG STATE
drawBT model = group
  [ rect 500 500 |> filled lightYellow |> makeTransparent 0.5
  , text "Basic Trigonometry" |> filled black |> move(-85,45)
  , drawBTInfo |> scale 0.79 |> move (20, 20)
  , drawBTTriangle model |> move (10, 7)
  , text "Leave the unknowns to 0." |> filled black |> scale 0.4 |> move(-65,-35)
  , drawButton 20 "Calculate" lightYellow darkYellow |> move (-65, -50) |> notifyTap (BTCalculate)
  , drawButton 20 "Reset" lightYellow darkYellow |> move (-10, -50) |> notifyTap (BTReset)
  , drawButton 20 "To Quiz" lightYellow darkYellow |> move (70, -25) |> notifyTap ToSOHQuiz 
  , drawButton 20 "Back to Menu" lightYellow darkYellow |> move (70, -50) |> notifyTap ToMenu
  ]

drawBTInfo = group
    [ rect 57 75 |> outlined (solid 1) black |> move(63,0)
    , text "Facts!" |> filled black |> move(40,25)
    , text "- Use SOH when" |> filled black |> scale 0.35 |> move(40,17)
    , text "  Opposite and Hypoteneuse" |> filled black |> scale 0.35 |> move(40,12)
    , text "  sides are given." |> filled black |> scale 0.35 |> move(40,7)
    , text "- Use CAH when" |> filled black |> scale 0.35 |> move(40,1)
    , text "  Adjacent and Hypoteneuse" |> filled black |> scale 0.35 |> move(40,-3.5)
    , text "  sides are given. " |> filled black |> scale 0.35 |> move(40,-8.7)
    , text "- Use TOA when" |> filled black |> scale 0.35 |> move(40,-16)
    , text "  Opposite and Adjacent" |> filled black |> scale 0.35 |> move(40,-22)
    , text "  sides are given" |> filled black |> scale 0.35 |> move(40,-27)
    ]

drawBTTriangle model = group
  [ rightTriangle 45 40 |> outlined (solid 1) darkYellow |> scaleX 1.5 |> move(-75,-15)
  , rightTriangle 45 40 |> filled lightYellow |> scaleX 1.5 |> move(-75,-15)
  , buttons 1 |> move(-1,23.5) -- rightSide
  , buttons 2 |> move(-52,23) -- leftSide
  , buttons 3 |> move(-6,-11.5) -- bottom
  , text (String.fromFloat model.btRightSide) |> filled black |> scale 0.35 |> move(-38,8.5)
  , text (String.fromFloat model.btLeftSide) |> filled black |> scale 0.35 |> move(-88,8.5)
  , text (String.fromFloat model.btBottom) |> filled black |> scale 0.35 |> move(-43,-26.5)
  , text (String.append (String.fromFloat model.btTopAngle) " °") |> filled black |> scale 0.35 |> move(-85,25)
  , text (String.append (String.fromFloat model.btRightAngle) " °") |> filled black |> scale 0.35 |> move(-5,-20)
  , text (String.append (String.fromFloat model.btLeftAngle) " °") |> filled black |> scale 0.35 |> move(-85,-20)
  , text (model.btErrorMsg) |> filled red |> scale 0.35 |> move(-80,30)
  ]
  
main : EllieAppWithTick () Model Msg
main =
    ellieAppWithTick Tick
        { init = \ _ -> ( init  -- this is the initial state, like you are used to
                        , scqCmdScreen init)
        , update = update
        , view = \ model -> { title = "TrigApp", body = view model }
        , subscriptions = \_ -> Sub.none
        }

view model = collage 192 128 (myShapes model)
