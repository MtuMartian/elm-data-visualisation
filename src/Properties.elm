module Properties exposing (..)


type alias Property = (String, String)

margin : String -> Property
margin amt = ("margin", amt)

title : String -> Property
title str = ("title", str)

vertAxisTitle : String -> Property
vertAxisTitle str = ("vertTitle", str)

horiAxisTitle : String -> Property
horiAxisTitle str = ("horiTitle", str)

ticks : String -> Property
ticks num = ("ticks", num)

max : String -> Property
max amt = ("max", amt)

min : String -> Property
min amt = ("min", amt)

partitionLeft : String -> Property
partitionLeft amt = ("partitionLeft", amt)

partitionRight : String -> Property
partitionRight amt = ("partitionRight", amt)

partitionAbove : String -> Property
partitionAbove amt = ("partitionAbove", amt)

partitionBelow : String -> Property
partitionBelow amt = ("partitionBelow", amt)

bubbleSize : String -> Property
bubbleSize amt = ("bubbleSize", amt)
